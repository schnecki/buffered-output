{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module System.IO.BufferedOutput.BufferedOutStr
    ( BufferedOutStr (..)
    , ToBufferedOutStr (..)
    , outStrLen
    , fromBufferedOutStr
    , mkMinBufferedOutStrLen
    ) where

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as S8
import qualified Data.ByteString.Lazy    as BL
import           Data.String             (IsString (..))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Int
import           Data.Word

toBuilder :: BS.ByteString -> Builder
toBuilder = B.byteString

fromBuilder :: Builder -> BS.ByteString
fromBuilder = BL.toStrict . B.toLazyByteString


-- | Out message builder. Use ('<>') to append two BufferedOutStr in O(1).
data BufferedOutStr = BufferedOutStr !Int Builder

instance Semigroup BufferedOutStr where
    {-# INLINE (<>) #-}
    BufferedOutStr s1 b1 <> BufferedOutStr s2 b2 = BufferedOutStr (s1 + s2) (b1 <> b2)

instance Monoid BufferedOutStr where
    mempty = BufferedOutStr 0 (toBuilder BS.empty)

instance IsString BufferedOutStr where
    {-# INLINE fromString #-}
    fromString = toBufOutStr . TL.pack

-- | Converting 'BufferedOutStr' to 'ByteString'.
fromBufferedOutStr :: BufferedOutStr -> BS.ByteString
fromBufferedOutStr (BufferedOutStr _ builder) = fromBuilder builder

outStrLen :: BufferedOutStr -> Int
outStrLen (BufferedOutStr l _) = l

mkMinBufferedOutStrLen :: Int -> BufferedOutStr -> BufferedOutStr
mkMinBufferedOutStrLen minLen outStr@(BufferedOutStr l _)
  | len > 0 = outStr <> BufferedOutStr len (toBuilder $ BS.replicate (minLen - l) space)
  | otherwise = outStr
  where
    len = minLen - l
    space = 32


-- | Types that can be converted to a 'BufferedOutStr'. Instances for
-- types from the @text@ library use a UTF-8 encoding. Instances
-- for numerical types use a decimal encoding.
class ToBufferedOutStr msg where
    toBufOutStr :: msg -> BufferedOutStr

instance ToBufferedOutStr BufferedOutStr where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = id
instance ToBufferedOutStr S8.ByteString where
    {-# INLINE toBufOutStr #-}
    toBufOutStr bs = BufferedOutStr (BS.length bs) (toBuilder bs)
instance ToBufferedOutStr BL.ByteString where
    {-# INLINE toBufOutStr #-}
    toBufOutStr b = BufferedOutStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToBufferedOutStr Builder where
    {-# INLINE toBufOutStr #-}
    toBufOutStr x = let b = B.toLazyByteString x in BufferedOutStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToBufferedOutStr String where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . TL.pack
instance ToBufferedOutStr T.Text where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . T.encodeUtf8
instance ToBufferedOutStr TL.Text where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . TL.encodeUtf8

-- | @since 2.4.14
instance ToBufferedOutStr Int where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.intDec
-- | @since 2.4.14
instance ToBufferedOutStr Int8 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.int8Dec
-- | @since 2.4.14
instance ToBufferedOutStr Int16 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.int16Dec
-- | @since 2.4.14
instance ToBufferedOutStr Int32 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.int32Dec
-- | @since 2.4.14
instance ToBufferedOutStr Int64 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.int64Dec

-- | @since 2.4.14
instance ToBufferedOutStr Word where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.wordDec
-- | @since 2.4.14
instance ToBufferedOutStr Word8 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.word8Dec
-- | @since 2.4.14
instance ToBufferedOutStr Word16 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.word16Dec
-- | @since 2.4.14
instance ToBufferedOutStr Word32 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.word32Dec
-- | @since 2.4.14
instance ToBufferedOutStr Word64 where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.word64Dec

-- | @since 2.4.14
instance ToBufferedOutStr Integer where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.integerDec
-- | @since 2.4.14
instance ToBufferedOutStr Float where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.floatDec
-- | @since 2.4.14
instance ToBufferedOutStr Double where
    {-# INLINE toBufOutStr #-}
    toBufOutStr = toBufOutStr . B.doubleDec

instance Show BufferedOutStr where
  show = show . T.decodeUtf8 . fromBufferedOutStr

instance Eq BufferedOutStr where
  a == b = fromBufferedOutStr a == fromBufferedOutStr b
