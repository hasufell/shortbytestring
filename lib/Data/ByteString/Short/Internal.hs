{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Short.Internal where

import Prelude hiding
    ( length )
import qualified Data.Word16 as W16
import GHC.Exts
import GHC.Word
import GHC.ST
    ( ST (ST) )
import qualified Data.List as List
#if !MIN_VERSION_base(4,13,0)
import Foreign.C.String hiding (newCWString)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
#endif
import Foreign.Marshal.Array (withArray0, peekArray0, newArray0, withArrayLen, peekArray)
import "bytestring" Data.ByteString.Short.Internal
import Control.Exception ( throwIO )
import Control.Monad.ST


create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)
{-# INLINE create #-}


asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#



data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)


newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray (I# len#) =
    ST $ \s -> case newPinnedByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

copyAddrToByteArray :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                 s' -> (# s', () #)


-- this is a copy-paste from bytestring
#if !MIN_VERSION_bytestring(0,10,9)
------------------------------------------------------------------------
-- Primop replacements

-- ---------------------------------------------------------------------
--
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize


-- ---------------------------------------------------------------------
--
-- Uses our C code
--

-- | /O(n)./ Construct a new @ShortByteString@ from a @CString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
--
-- @since 0.10.10.0
packCString :: CString -> IO ShortByteString
packCString cstr = do
  len <- c_strlen cstr
  packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen (cstr, len) | len >= 0 = createFromPtr cstr len
packCStringLen (_, len) =
  moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString bs action =
  allocaBytes (l+1) $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen bs action =
  allocaBytes l $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length bs


#endif


-- | /O(n)./ Construct a new @ShortByteString@ from a @CWString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CWString@, and is managed on the Haskell heap. The original
-- @CWString@ must be null terminated.
--
-- @since 0.10.10.0
packCWString :: Ptr Word16 -> IO ShortByteString
packCWString cwstr = do
  cs <- peekArray0 W16._nul cwstr
  return (packWord16 cs)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CWStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CWStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCWStringLen :: (Ptr Word16, Int) -> IO ShortByteString
packCWStringLen (cp, len) = do
  cs <- peekArray len cp
  return (packWord16 cs)


-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CWString@.  The @CWString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWString :: ShortByteString -> (Ptr Word16 -> IO a) -> IO a
useAsCWString = withArray0 W16._nul . unpackWord16

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWStringLen :: ShortByteString -> ((Ptr Word16, Int) -> IO a) -> IO a
useAsCWStringLen bs action = withArrayLen (unpackWord16 bs) $ \ len ptr -> action (ptr, len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
newCWString :: ShortByteString -> IO (Ptr Word16)
newCWString = newArray0 W16._nul . unpackWord16




 -- ---------------------------------------------------------------------
-- Internal utilities

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString.Short." ++ fun ++ ':':' ':msg

packWord16 :: [Word16] -> ShortByteString
packWord16 cs = packLenWord16 (List.length cs) cs

packLenWord16 :: Int -> [Word16] -> ShortByteString
packLenWord16 len ws0 =
    create (len * 2) (\mba -> go mba 0 ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba i w
      go mba (i+2) ws


unpackWord16 :: ShortByteString -> [Word16]
unpackWord16 sbs = go len []
  where
    len = length sbs
    go !i !acc
      | i < 1     = acc
      | otherwise = let !w = indexWord16Array (asBA sbs) (i - 2)
                    in go (i - 2) (w:acc)

packWord16Rev :: [Word16] -> ShortByteString
packWord16Rev cs = packLenWord16Rev ((List.length cs) * 2) cs

packLenWord16Rev :: Int -> [Word16] -> ShortByteString
packLenWord16Rev len ws0 =
    create len (\mba -> go mba len ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba (i - 2) w
      go mba (i - 2) ws


-- | This isn't strictly Word16 array write. Instead it's two consecutive Word8 array
-- writes to avoid endianness issues due to primops doing automatic alignment based
-- on host platform. We want to always write LE to the byte array.
writeWord16Array :: MBA s
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
                 -> ST s ()
writeWord16Array (MBA# mba#) (I# i#) (W16# w#) =
  case encodeWord16LE# w# of
    (# lsb#, msb# #) ->
      (ST $ \s -> case writeWord8Array# mba# i# lsb# s of
          s' -> (# s', () #)) >>
      (ST $ \s -> case writeWord8Array# mba# (i# +# 1#) msb# s of
          s' -> (# s', () #))

-- | This isn't strictly Word16 array read. Instead it's two Word8 array reads
-- to avoid endianness issues due to primops doing automatic alignment based
-- on host platform. We expect the byte array to be LE always.
indexWord16Array :: BA
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
indexWord16Array (BA# ba#) (I# i#) = 
  case (# indexWord8Array# ba# i#, indexWord8Array# ba# (i# +# 1#) #) of
    (# lsb#, msb# #) -> W16# ((decodeWord16LE# (# lsb#, msb# #)))


encodeWord16LE# :: Word# -- ^ Word16
                -> (# Word#, Word# #) -- ^ Word8 (LSB, MSB)
encodeWord16LE# x# = (# (x# `and#` int2Word# 0xff#)
                     ,  ((x# `and#` int2Word# 0xff00#) `shiftRL#` 8#) #)

decodeWord16LE# :: (# Word#, Word# #) -- ^ Word8 (LSB, MSB)
                -> Word#              -- ^ Word16
decodeWord16LE# (# lsb#, msb# #) = ((msb# `shiftL#` 8#) `or#` lsb#)


