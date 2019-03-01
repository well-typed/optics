{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.ByteString.Optics
  ( IsByteString(..)
  , unpackedBytes
  , unpackedChars
  , pattern Bytes
  , pattern Chars
  ) where

import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy.Optics as Lazy
import qualified Data.ByteString.Strict.Optics as Strict

import Optics.Core

-- | Traversals for ByteStrings.
class IsByteString t where
  -- | 'Data.ByteString.pack' (or 'Data.ByteString.unpack') a list of bytes into
  -- a strict or lazy 'ByteString'.
  --
  -- @
  -- 'Data.ByteString.pack' x ≡ x '^.' 'packedBytes'
  -- 'Data.ByteString.unpack' x ≡ x '^.' 're' 'packedBytes'
  -- 'packedBytes' ≡ 're' 'unpackedBytes'
  -- @
  packedBytes :: Iso' [Word8] t

  -- | 'Data.ByteString.Char8.pack' (or 'Data.ByteString.Char8.unpack') a list
  -- of characters into a strict or lazy 'ByteString'.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char' lies
  -- between @'\x00'@ and @'\xff'@.
  --
  -- @
  -- 'Data.ByteString.Char8.pack' x ≡ x '^.' 'packedChars'
  -- 'Data.ByteString.Char8.unpack' x ≡ x '^.' 're' 'packedChars'
  -- 'packedChars' ≡ 're' 'unpackedChars'
  -- @
  packedChars :: Iso' String t

  -- | Traverse each 'Word8' in a strict or lazy 'ByteString'
  --
  --
  -- This 'Traversal' walks each strict 'ByteString' chunk in a tree-like
  -- fashion enable zippers to seek to locations more quickly and accelerate
  -- many monoidal queries, but up to associativity (and constant factors) it is
  -- equivalent to the much slower:
  --
  -- @
  -- 'bytes' ≡ 'unpackedBytes' '.' 'traversed'
  -- @
  --
  -- @
  -- 'anyOf' 'bytes' ('==' 0x80) :: 'ByteString' -> 'Bool'
  -- @
  bytes :: IxTraversal' Int64 t Word8

  -- | Traverse the individual bytes in a strict or lazy 'ByteString' as
  -- characters.
  --
  -- When writing back to the 'ByteString' it is assumed that every 'Char' lies
  -- between @'\x00'@ and @'\xff'@.
  --
  -- This 'Traversal' walks each strict 'ByteString' chunk in a tree-like
  -- fashion enable zippers to seek to locations more quickly and accelerate
  -- many monoidal queries, but up to associativity (and constant factors) it is
  -- equivalent to the much slower:
  --
  -- @
  -- 'chars' ≡ 'unpackedChars' '.' 'traversed'
  -- @
  --
  -- @
  -- 'anyOf' 'chars' ('==' \'c\') :: 'ByteString' -> 'Bool'
  -- @
  chars :: IxTraversal' Int64 t Char

-- | 'Data.ByteString.unpack' (or 'Data.ByteString.pack') a 'ByteString' into a
-- list of bytes.
--
-- @
-- 'unpackedBytes' ≡ 're' 'packedBytes'
-- 'Data.ByteString.unpack' x ≡ x '^.' 'unpackedBytes'
-- 'Data.ByteString.pack' x ≡  x '^.' 're' 'unpackedBytes'
-- @
--
-- @
-- 'unpackedBytes' :: 'Iso'' 'Data.ByteString.ByteString' ['Word8']
-- 'unpackedBytes' :: 'Iso'' 'Data.ByteString.Lazy.ByteString' ['Word8']
-- @
unpackedBytes :: IsByteString t => Iso' t [Word8]
unpackedBytes = re packedBytes
{-# INLINE unpackedBytes #-}

pattern Bytes :: IsByteString t => [Word8] -> t
pattern Bytes b <- (view unpackedBytes -> b) where
  Bytes b = review unpackedBytes b

pattern Chars :: IsByteString t => [Char] -> t
pattern Chars b <- (view unpackedChars -> b) where
  Chars b = review unpackedChars b

-- | 'Data.ByteString.Char8.unpack' (or 'Data.ByteString.Char8.pack') a list of
-- characters into a strict (or lazy) 'ByteString'
--
-- When writing back to the 'ByteString' it is assumed that every 'Char' lies
-- between @'\x00'@ and @'\xff'@.
--
-- @
-- 'unpackedChars' ≡ 're' 'packedChars'
-- 'Data.ByteString.Char8.unpack' x ≡ x '^.' 'unpackedChars'
-- 'Data.ByteString.Char8.pack' x ≡ x '^.' 're' 'unpackedChars'
-- @
--
-- @
-- 'unpackedChars' :: 'Iso'' 'Data.ByteString.ByteString' 'String'
-- 'unpackedChars' :: 'Iso'' 'Data.ByteString.Lazy.ByteString' 'String'
-- @
unpackedChars :: IsByteString t => Iso' t String
unpackedChars = re packedChars
{-# INLINE unpackedChars #-}

instance IsByteString Strict.ByteString where
  packedBytes = Strict.packedBytes
  packedChars = Strict.packedChars
  bytes       = Strict.bytes
  chars       = Strict.chars
  {-# INLINE packedBytes #-}
  {-# INLINE packedChars #-}
  {-# INLINE bytes #-}
  {-# INLINE chars #-}

instance IsByteString Lazy.ByteString where
  packedBytes = Lazy.packedBytes
  packedChars = Lazy.packedChars
  bytes       = Lazy.bytes
  chars       = Lazy.chars
  {-# INLINE packedBytes #-}
  {-# INLINE packedChars #-}
  {-# INLINE bytes #-}
  {-# INLINE chars #-}
