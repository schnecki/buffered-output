
import           Control.Monad
import           Data.IORef
import           System.IO.BufferedOutput
import           System.IO.Unsafe         (unsafePerformIO)

main :: IO ()
main =


  putStrLn "Test suite not yet implemented"


testFilepath :: FilePath
testFilepath = "testfile.txt"

testBufSize :: Int
testBufSize = 1000 * 1024  -- 1 MB

testFile :: IORef BufferedOutput
testFile = unsafePerformIO $ openBufferedOutputFile testBufSize testFilepath >>= newIORef
{-# NOINLINE testFile #-}

openBuffer :: IO ()
openBuffer = openBufferedOutputFile testBufSize testFilepath >>= writeIORef testFile

getTestFile :: IO BufferedOutput
getTestFile = readIORef testFile

writeTest :: Int -> String -> String -> IO ()
writeTest per info1 info2 = getTestFile >>= writeTest'
  where
    writeTest' buf = do
      when (per == 1) $ resetTest buf
      pushBufferedOutStrLn buf (info1 <> "\t" <> info2)
    resetTest buf = do
      closeBufferedOutput buf
      writeFile testFilepath "TestCol1\tTestCol2\n"
      openBuffer
