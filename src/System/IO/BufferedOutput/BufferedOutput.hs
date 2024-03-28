{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module System.IO.BufferedOutput.BufferedOutput
    ( BufferedOutput (..)
    , openBufferedOutputFile
    , openBufferedOutputStdout
    , openBufferedOutputStderr
    , toBufOutStr
    , defaultBufferedOutputSize
    , closeBufferedOutput
    , pushBufferedOutStr
    , pushBufferedOutStrLn
    ) where

import           Control.Concurrent.MVar
import           Control.Debounce
import           Control.Monad                           (when)
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra           (Next (..))
import qualified Data.ByteString.Builder.Extra           as BBE
import           Data.ByteString.Internal
import           Data.IORef
import           Data.Maybe                              (isJust)
import           Data.Word
import           Foreign.ForeignPtr                      (withForeignPtr)
import           Foreign.Marshal.Alloc                   (allocaBytes, free, mallocBytes)
import           Foreign.Ptr                             (Ptr, plusPtr)
import           GHC.IO.Device                           (close)
import           GHC.IO.FD                               (FD, openFile, stderr, stdout, writeRawBufferPtr)
import           GHC.IO.IOMode                           (IOMode (..))


import           System.IO.BufferedOutput.BufferedOutStr


type BufSize = Int
type Buffer = Ptr Word8
data OutBuf = OutBuf !BufSize (MVar Buffer) (IORef BufferedOutStr)
data BufferedOutput = BufferedOutput (Maybe FilePath) (IORef FD) OutBuf (IO ())


-- | The default buffer size (1 MB).
defaultBufferedOutputSize :: BufSize
defaultBufferedOutputSize = 1000 * 1024


newOutBuf :: BufSize -> IO OutBuf
newOutBuf size = OutBuf size <$> (mallocBytes size >>= newMVar) <*> newIORef mempty

-- | Creating a new 'OutputSet' using a file.
openBufferedOutputFile :: BufSize -> FilePath -> IO BufferedOutput
openBufferedOutputFile size file = openFileFD >>= newFDBufferedOutput size (Just file)
  where
    openFileFD = fst <$> openFile file AppendMode False

-- | Creating a new 'BufferedOutputSet' using stdout.
openBufferedOutputStdout :: BufSize -> IO BufferedOutput
openBufferedOutputStdout size = newFDBufferedOutput size Nothing stdout

-- | Creating a new 'BufferedOutputSet' using stderr.
openBufferedOutputStderr :: BufSize -> IO BufferedOutput
openBufferedOutputStderr size = newFDBufferedOutput size Nothing stderr


-- | Creating a new 'BufferedOutput' using a FD.
newFDBufferedOutput :: BufSize -> Maybe FilePath -> FD -> IO BufferedOutput
newFDBufferedOutput size mfile fd = do
  buf <- newOutBuf (max 1 size)
  fref <- newIORef fd
  flush <- mkDebounce defaultDebounceSettings {debounceAction = flushOut fref buf}
  return $ BufferedOutput mfile fref buf flush


flushOut :: IORef FD -> OutBuf -> IO ()
flushOut fdref (OutBuf size mbuf lref) = do
    msg <- atomicModifyIORef' lref (mempty,)
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeBufferedOutStr fdref buf size msg


-- | Writting 'BufferedOutStr' using a buffer in blocking mode.
--   The size of 'BufferedOutStr' must be smaller or equal to
--   the size of buffer.
writeBufferedOutStr :: IORef FD -> Buffer -> BufSize -> BufferedOutStr -> IO ()
writeBufferedOutStr fdref buf size (BufferedOutStr len builder)
  | size < len = error "writeBufferedOutStr"
  | otherwise  = toBufIOWith buf size (write fdref) builder

write :: IORef FD -> Buffer -> Int -> IO ()
write fdref = loop
  where
    loop bf !len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (written < len) $
            loop (bf `plusPtr` written) (len - written)

writeRawBufferPtr2FD :: IORef FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD fdref bf len = do
    fd <- readIORef fdref
    fromIntegral <$> writeRawBufferPtr "write" fd bf 0 (fromIntegral len)

toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop $ BBE.runBuilder builder
  where
    loop writer = do
        (len, next) <- writer buf size
        io buf len
        case next of
             Done -> return ()
             More minSize writer'
               | size < minSize -> error "toBufIOWith: More: minSize"
               | otherwise      -> loop writer'
             Chunk (PS fptr off siz) writer' ->
               withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz >> loop writer'


-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
closeBufferedOutput :: BufferedOutput -> IO ()
closeBufferedOutput (BufferedOutput mfile fdref outBuf _) = do
  flushOut fdref outBuf
  freeIt outBuf
  fd <- readIORef fdref
  when (isJust mfile) $ close fd
  where
    freeIt (OutBuf _ mbuf _) = do
        takeMVar mbuf >>= free


-- | Writing a message to the corresponding buffer.
--   If the buffer becomes full, the messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushBufferedOutStr :: (ToBufferedOutStr txt) => BufferedOutput -> txt -> IO ()
pushBufferedOutStr (BufferedOutput _ fdref outBuf flush) msg = do
  pushOut fdref outBuf . toBufOutStr $ msg
  flush

-- | Same as 'pushBufferedOutStr' but also appends a newline.
pushBufferedOutStrLn :: (ToBufferedOutStr txt) => BufferedOutput -> txt -> IO ()
pushBufferedOutStrLn buf str = pushBufferedOutStr buf (toBufOutStr str <> "\n")


pushOut :: IORef FD -> OutBuf -> BufferedOutStr -> IO ()
pushOut fdref buffer@(OutBuf size mbuf ref) nmsg@(BufferedOutStr nlen nbuilder)
  | nlen > size = do
      flushOut fdref buffer
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      allocaBytes nlen $ \buf -> withMVar mbuf $ \_ ->
        toBufIOWith buf nlen (write fdref) nbuilder
  | otherwise = do
    mmsg <- atomicModifyIORef' ref checkBuf
    case mmsg of
        Nothing  -> return ()
        Just msg -> withMVar mbuf $ \buf -> writeBufferedOutStr fdref buf size msg
  where
    checkBuf omsg@(BufferedOutStr olen _)
      | size < olen + nlen = (nmsg, Just omsg)
      | otherwise          = (omsg <> nmsg, Nothing)
