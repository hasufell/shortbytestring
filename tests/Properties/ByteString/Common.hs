-- |
-- Module      : Properties.ByteString
-- Copyright   : (c) Andrew Lelechenko 2021
-- License     : BSD-style

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- We are happy to sacrifice optimizations in exchange for faster compilation,
-- but need to test rewrite rules. As one can check using -ddump-rule-firings,
-- rewrite rules do not fire in -O0 mode, so we use -O1, but disable almost all
-- optimizations. It roughly halves compilation time.
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules
  -fmax-simplifier-iterations=1 -fsimplifier-phases=0
  -fno-call-arity -fno-case-merge -fno-cmm-elim-common-blocks -fno-cmm-sink
  -fno-cpr-anal -fno-cse -fno-do-eta-reduction -fno-float-in -fno-full-laziness
  -fno-loopification -fno-specialise -fno-strictness #-}

#ifdef WORD8
module Properties.ByteString.Short (tests) where
import Data.Word8 (isSpace, _nul)
import qualified "shortbytestring" Data.ByteString.Short as B
#else
module Properties.ByteString.Short.Word16 (tests) where
import Data.Word16 (isSpace, _nul)
import qualified "shortbytestring" Data.ByteString.Short.Word16 as B
#endif
import "shortbytestring" Data.ByteString.Short (ShortByteString)

import Data.Word

import Control.Arrow
import Data.Foldable
import Data.List as L
import Data.Semigroup
import Data.Tuple
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Text.Show.Functions ()

#ifdef WORD16
toElem :: Word16 -> Word16
toElem = id

swapW :: Word16 -> Word16
swapW = byteSwap16

sizedByteString :: Int -> Gen ShortByteString
sizedByteString n = do m <- choose(0, n)
                       fmap B.pack $ vectorOf m arbitrary

instance Arbitrary ShortByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (B.drop n bs) -- to give us some with non-0 offset

instance CoArbitrary ShortByteString where
  coarbitrary s = coarbitrary (B.unpack s)

#else
toElem :: Word8 -> Word8
toElem = id

swapW :: Word8 -> Word8
swapW = id


sizedByteString :: Int -> Gen ShortByteString
sizedByteString n = do m <- choose(0, n)
                       fmap B.pack $ vectorOf m arbitrary

instance Arbitrary ShortByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (B.drop n bs) -- to give us some with non-0 offset
  shrink = map B.pack . shrink . B.unpack

instance CoArbitrary ShortByteString where
  coarbitrary s = coarbitrary (B.unpack s)

#endif

tests :: [TestTree]
tests =
  [ testProperty "pack . unpack" $
    \x -> x === B.pack (B.unpack x)
  , testProperty "unpack . pack" $
    \(map toElem -> xs) -> xs === B.unpack (B.pack xs)
  , testProperty "read . show" $
    \x -> (x :: ShortByteString) === read (show x)

  , testProperty "==" $
    \x y -> (x == y) === (B.unpack x == B.unpack y)
  , testProperty "== refl" $
    \x -> (x :: ShortByteString) == x
  , testProperty "== symm" $
    \x y -> ((x :: ShortByteString) == y) === (y == x)
  , testProperty "== pack unpack" $
    \x -> x == B.pack (B.unpack x)

  , testProperty "compare" $
    \x y -> compare x y === compare (swapW <$> B.unpack x) (swapW <$> B.unpack y)
  , testProperty "compare EQ" $
    \x -> compare (x :: ShortByteString) x == EQ
  , testProperty "compare GT" $
    \x (toElem -> c) -> compare (B.snoc x c) x == GT
  , testProperty "compare LT" $
    \x (toElem -> c) -> compare x (B.snoc x c) == LT
  , testProperty "compare GT empty" $
    \x -> not (B.null x) ==> compare x B.empty == GT
  , testProperty "compare LT empty" $
    \x -> not (B.null x) ==> compare B.empty x == LT
  , testProperty "compare GT concat" $
    \x y -> not (B.null y) ==> compare (x <> y) x == GT
  , testProperty "compare char" $
    \(toElem -> c) (toElem -> d) -> compare (swapW c) (swapW d) == compare (B.singleton c) (B.singleton d)
  , testProperty "compare unsigned" $ once $
    compare (B.singleton 255) (B.singleton 127) == GT

  , testProperty "null" $
    \x -> B.null x === null (B.unpack x)
  , testProperty "empty 0" $ once $
    B.length B.empty === 0
  , testProperty "empty []" $ once $
    B.unpack B.empty === []
  , testProperty "mempty 0" $ once $
    B.length mempty === 0
  , testProperty "mempty []" $ once $
    B.unpack mempty === []

  , testProperty "concat" $
    \xs -> B.unpack (B.concat xs) === concat (map B.unpack xs)
  , testProperty "concat [x,x]" $
    \x -> B.unpack (B.concat [x, x]) === concat [B.unpack x, B.unpack x]
  , testProperty "concat [x,[]]" $
    \x -> B.unpack (B.concat [x, B.empty]) === concat [B.unpack x, []]
  , testProperty "mconcat" $
    \xs -> B.unpack (mconcat xs) === mconcat (map B.unpack xs)
  , testProperty "mconcat [x,x]" $
    \x -> B.unpack (mconcat [x, x]) === mconcat [B.unpack x, B.unpack x]
  , testProperty "mconcat [x,[]]" $
    \x -> B.unpack (mconcat [x, B.empty]) === mconcat [B.unpack x, []]

  , testProperty "null" $
    \x -> B.null x === null (B.unpack x)
  , testProperty "reverse" $
    \x -> B.unpack (B.reverse x) === reverse (B.unpack x)
  , testProperty "all" $
    \f x -> B.all f x === all f (B.unpack x)
  , testProperty "all ==" $
    \(toElem -> c) x -> B.all (== c) x === all (== c) (B.unpack x)
  , testProperty "any" $
    \f x -> B.any f x === any f (B.unpack x)
  , testProperty "any ==" $
    \(toElem -> c) x -> B.any (== c) x === any (== c) (B.unpack x)
  , testProperty "append" $
    \x y -> B.unpack (B.append x y) === B.unpack x ++ B.unpack y
  , testProperty "mappend" $
    \x y -> B.unpack (mappend x y) === B.unpack x `mappend` B.unpack y
  , testProperty "<>" $
    \x y -> B.unpack (x <> y) === B.unpack x <> B.unpack y
  , testProperty "stimes" $
    \(Positive n) x -> stimes (n :: Int) (x :: ShortByteString) === mtimesDefault n x

  , testProperty "break" $
    \f x -> (B.unpack *** B.unpack) (B.break f x) === break f (B.unpack x)
  , testProperty "break ==" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (== c) x) === break (== c) (B.unpack x)
  , testProperty "break /=" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (/= c) x) === break (/= c) (B.unpack x)
  , testProperty "break span" $
    \f x -> B.break f x === B.span (not . f) x
  , testProperty "breakEnd" $
    \f x -> B.breakEnd f x === swap ((B.reverse *** B.reverse) (B.break f (B.reverse x)))
  , testProperty "breakEnd" $
    \f x -> B.breakEnd f x === B.spanEnd (not . f) x
#ifndef WORD16
  , testProperty "break breakSubstring" $
    \(toElem -> c) x -> B.break (== c) x === B.breakSubstring (B.singleton c) x
  , testProperty "breakSubstring" $
    \x y -> not (B.null x) ==> B.null (snd (B.breakSubstring x y)) === not (B.isInfixOf x y)
  , testProperty "breakSubstring empty" $
    \x -> B.breakSubstring B.empty x === (B.empty, x)
#endif
  , testProperty "break isSpace" $
    \x -> (B.unpack *** B.unpack) (B.break isSpace x) === break isSpace (B.unpack x)

  , testProperty "singleton" $
    \(toElem -> c) -> B.unpack (B.singleton c) === [c]
  , testProperty "cons" $
    \(toElem -> c) x -> B.unpack (B.cons c x) === c : B.unpack x
  , testProperty "cons []" $
    \(toElem -> c) -> B.unpack (B.cons c B.empty) === [c]
  , testProperty "snoc" $
    \(toElem -> c) x -> B.unpack (B.snoc x c) === B.unpack x ++ [c]
  , testProperty "snoc []" $
    \(toElem -> c) -> B.unpack (B.snoc B.empty c) === [c]

  , testProperty "drop" $
    \n x -> B.unpack (B.drop n x) === drop (fromIntegral n) (B.unpack x)
  , testProperty "drop 10" $
    \x -> B.unpack (B.drop 10 x) === drop 10 (B.unpack x)
  , testProperty "dropWhile" $
    \f x -> B.unpack (B.dropWhile f x) === dropWhile f (B.unpack x)
  , testProperty "dropWhile ==" $
    \(toElem -> c) x -> B.unpack (B.dropWhile (== c) x) === dropWhile (== c) (B.unpack x)
  , testProperty "dropWhile /=" $
    \(toElem -> c) x -> B.unpack (B.dropWhile (/= c) x) === dropWhile (/= c) (B.unpack x)
  , testProperty "dropWhile isSpace" $
    \x -> B.unpack (B.dropWhile isSpace x) === dropWhile isSpace (B.unpack x)

  , testProperty "take" $
    \n x -> B.unpack (B.take n x) === take (fromIntegral n) (B.unpack x)
  , testProperty "take 10" $
    \x -> B.unpack (B.take 10 x) === take 10 (B.unpack x)
  , testProperty "takeWhile" $
    \f x -> B.unpack (B.takeWhile f x) === takeWhile f (B.unpack x)
  , testProperty "takeWhile ==" $
    \(toElem -> c) x -> B.unpack (B.takeWhile (== c) x) === takeWhile (== c) (B.unpack x)
  , testProperty "takeWhile /=" $
    \(toElem -> c) x -> B.unpack (B.takeWhile (/= c) x) === takeWhile (/= c) (B.unpack x)

  , testProperty "takeWhile isSpace" $
    \x -> B.unpack (B.takeWhile isSpace x) === takeWhile isSpace (B.unpack x)

  , testProperty "dropEnd" $
    \n x -> B.dropEnd n x === B.take (B.length x - n) x
  , testProperty "dropWhileEnd" $
    \f x -> B.dropWhileEnd f x === B.reverse (B.dropWhile f (B.reverse x))
  , testProperty "takeEnd" $
    \n x -> B.takeEnd n x === B.drop (B.length x - n) x
  , testProperty "takeWhileEnd" $
    \f x -> B.takeWhileEnd f x === B.reverse (B.takeWhile f (B.reverse x))

  , testProperty "length" $
    \x -> B.length x === fromIntegral (length (B.unpack x))
  , testProperty "count" $
    \(toElem -> c) x -> B.count c x === fromIntegral (length (elemIndices c (B.unpack x)))
  , testProperty "filter" $
    \f x -> B.unpack (B.filter f x) === filter f (B.unpack x)
  , testProperty "filter compose" $
    \f g x -> B.filter f (B.filter g x) === B.filter (\c -> f c && g c) x
  , testProperty "filter ==" $
    \(toElem -> c) x -> B.unpack (B.filter (== c) x) === filter (== c) (B.unpack x)
  , testProperty "filter /=" $
    \(toElem -> c) x -> B.unpack (B.filter (/= c) x) === filter (/= c) (B.unpack x)
  , testProperty "partition" $
    \f x -> (B.unpack *** B.unpack) (B.partition f x) === partition f (B.unpack x)

  , testProperty "find" $
    \f x -> B.find f x === find f (B.unpack x)
  , testProperty "findIndex" $
    \f x -> B.findIndex f x === fmap fromIntegral (findIndex f (B.unpack x))
  , testProperty "findIndices" $
    \f x -> B.findIndices f x === fmap fromIntegral (findIndices f (B.unpack x))
  , testProperty "findIndices ==" $
    \(toElem -> c) x -> B.findIndices (== c) x === fmap fromIntegral (findIndices (== c) (B.unpack x))

  , testProperty "elem" $
    \(toElem -> c) x -> B.elem c x === elem c (B.unpack x)
  , testProperty "not elem" $
    \(toElem -> c) x -> not (B.elem c x) === notElem c (B.unpack x)
  , testProperty "elemIndex" $
    \(toElem -> c) x -> B.elemIndex c x === fmap fromIntegral (elemIndex c (B.unpack x))
  , testProperty "elemIndices" $
    \(toElem -> c) x -> B.elemIndices c x === fmap fromIntegral (elemIndices c (B.unpack x))

  , testProperty "isPrefixOf" $
    \x y -> B.isPrefixOf x y === isPrefixOf (B.unpack x) (B.unpack y)
  , testProperty "stripPrefix" $
    \x y -> fmap B.unpack (B.stripPrefix x y) === stripPrefix (B.unpack x) (B.unpack y)
  , testProperty "isSuffixOf" $
    \x y -> B.isSuffixOf x y === isSuffixOf (B.unpack x) (B.unpack y)
  , testProperty "stripSuffix" $
    \x y -> fmap B.unpack (B.stripSuffix x y) === stripSuffix (B.unpack x) (B.unpack y)
  , testProperty "isInfixOf" $
    \x y -> B.isInfixOf x y === isInfixOf (B.unpack x) (B.unpack y)

  , testProperty "map" $
    \f x -> B.unpack (B.map (toElem . f) x) === map (toElem . f) (B.unpack x)
  , testProperty "map compose" $
    \f g x -> B.map (toElem . f) (B.map (toElem . g) x) === B.map (toElem . f . toElem . g) x
  , testProperty "replicate" $
    \n (toElem -> c) -> B.unpack (B.replicate (fromIntegral n) c) === replicate n c
  , testProperty "replicate 0" $
    \(toElem -> c) -> B.unpack (B.replicate 0 c) === replicate 0 c

  , testProperty "span" $
    \f x -> (B.unpack *** B.unpack) (B.span f x) === span f (B.unpack x)
  , testProperty "span ==" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (== c) x) === span (== c) (B.unpack x)
  , testProperty "span /=" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (/= c) x) === span (/= c) (B.unpack x)
  , testProperty "spanEnd" $
    \f x -> B.spanEnd f x === swap ((B.reverse *** B.reverse) (B.span f (B.reverse x)))
  , testProperty "split" $
    \(toElem -> c) x -> map B.unpack (B.split c x) === split c (B.unpack x)
  , testProperty "split empty" $
    \(toElem -> c) -> B.split c B.empty === []
  , testProperty "splitWith" $
    \f x -> map B.unpack (B.splitWith f x) === splitWith f (B.unpack x)
  , testProperty "splitWith split" $
    \(toElem -> c) x -> B.splitWith (== c) x === B.split c x
  , testProperty "splitWith empty" $
    \f -> B.splitWith f B.empty === []
  , testProperty "splitWith length" $
    \f x -> let splits = B.splitWith f x; l1 = fromIntegral (length splits); l2 = B.length (B.filter f x) in
      (l1 == l2 || l1 == l2 + 1) && sum (map B.length splits) + l2 == B.length x
  , testProperty "splitAt" $
    \n x -> (B.unpack *** B.unpack) (B.splitAt n x) === splitAt (fromIntegral n) (B.unpack x)

  , testProperty "head" $
    \x -> not (B.null x) ==> B.head x === head (B.unpack x)
  , testProperty "last" $
    \x -> not (B.null x) ==> B.last x === last (B.unpack x)
  , testProperty "tail" $
    \x -> not (B.null x) ==> B.unpack (B.tail x) === tail (B.unpack x)
  , testProperty "tail length" $
    \x -> not (B.null x) ==> B.length x === 1 + B.length (B.tail x)
  , testProperty "init" $
    \x -> not (B.null x) ==> B.unpack (B.init x) === init (B.unpack x)
  , testProperty "init length" $
    \x -> not (B.null x) ==> B.length x === 1 + B.length (B.init x)

  , testProperty "foldl" $
    \f (toElem -> c) x -> B.foldl ((toElem .) . f) c x === foldl ((toElem .) . f) c (B.unpack x)
  , testProperty "foldl'" $
    \f (toElem -> c) x -> B.foldl' ((toElem .) . f) c x === foldl' ((toElem .) . f) c (B.unpack x)
  , testProperty "foldr" $
    \f (toElem -> c) x -> B.foldr ((toElem .) . f) c x === foldr ((toElem .) . f) c (B.unpack x)
  , testProperty "foldr'" $
    \f (toElem -> c) x -> B.foldr' ((toElem .) . f) c x === foldr' ((toElem .) . f) c (B.unpack x)

  , testProperty "foldl cons" $
    \x -> B.foldl (flip B.cons) B.empty x === B.reverse x
  , testProperty "foldr cons" $
    \x -> B.foldr B.cons B.empty x === x
  , testProperty "foldl special" $
    \x (toElem -> c) -> B.unpack (B.foldl (\acc t -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldl (\acc t -> if t == c then acc else t : acc) [] (B.unpack x)
  , testProperty "foldr special" $
    \x (toElem -> c) -> B.unpack (B.foldr (\t acc -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldr (\t acc -> if t == c then acc else t : acc) [] (B.unpack x)

  , testProperty "foldl1" $
    \f x -> not (B.null x) ==> B.foldl1 ((toElem .) . f) x === foldl1 ((toElem .) . f) (B.unpack x)
  , testProperty "foldl1'" $
    \f x -> not (B.null x) ==> B.foldl1' ((toElem .) . f) x === foldl1' ((toElem .) . f) (B.unpack x)
  , testProperty "foldr1" $
    \f x -> not (B.null x) ==> B.foldr1 ((toElem .) . f) x === foldr1 ((toElem .) . f) (B.unpack x)
  , testProperty "foldr1'" $ -- there is not Data.List.foldr1'
    \f x -> not (B.null x) ==> B.foldr1' ((toElem .) . f) x === foldr1 ((toElem .) . f) (B.unpack x)

  , testProperty "foldl1 const" $
    \x -> not (B.null x) ==> B.foldl1 const x === B.head x
  , testProperty "foldl1 flip const" $
    \x -> not (B.null x) ==> B.foldl1 (flip const) x === B.last x
  , testProperty "foldr1 const" $
    \x -> not (B.null x) ==> B.foldr1 const x === B.head x
  , testProperty "foldr1 flip const" $
    \x -> not (B.null x) ==> B.foldr1 (flip const) x === B.last x
  , testProperty "foldl1 max" $
    \x -> not (B.null x) ==> B.foldl1 max x === B.foldl max minBound x
  , testProperty "foldr1 max" $
    \x -> not (B.null x) ==> B.foldr1 max x === B.foldr max minBound x

  , testProperty "intercalate" $
    \x ys -> B.unpack (B.intercalate x ys) === intercalate (B.unpack x) (map B.unpack ys)
  , testProperty "intercalate 'c' [x,y]" $
    \(toElem -> c) x y -> B.unpack (B.intercalate (B.singleton c) [x, y]) === intercalate [c] [B.unpack x, B.unpack y]
  , testProperty "intercalate split" $
    \(toElem -> c) x -> B.intercalate (B.singleton c) (B.split c x) === x

  , testProperty "index" $
    \(NonNegative n) x -> fromIntegral n < B.length x ==> B.index x (fromIntegral n) === B.unpack x !! n
  , testProperty "indexMaybe" $
    \(NonNegative n) x -> fromIntegral n < B.length x ==> B.indexMaybe x (fromIntegral n) === Just (B.unpack x !! n)
  , testProperty "indexMaybe Nothing" $
    \n x -> (n :: Int) < 0 || fromIntegral n >= B.length x ==> B.indexMaybe x (fromIntegral n) === Nothing
  , testProperty "!?" $
    \n x -> B.indexMaybe x (fromIntegral (n :: Int)) === x B.!? (fromIntegral n)

  , testProperty "unfoldrN" $
    \n f (toElem -> c) -> B.unpack (fst (B.unfoldrN n (fmap (first toElem) . f) c)) ===
      take (fromIntegral n) (unfoldr (fmap (first toElem) . f) c)
  , testProperty "unfoldrN replicate" $
    \n (toElem -> c) -> fst (B.unfoldrN n (\t -> Just (t, t)) c) === B.replicate n c
  , testProperty "unfoldr" $
    \n a (toElem -> c) -> B.unpack (B.unfoldr (\x -> if x <= 100 * n then Just (c, x + 1 :: Int) else Nothing) a) ===
      unfoldr (\x -> if x <= 100 * n then Just (c, x + 1) else Nothing) a

  --, testProperty "unfoldr" $
  --  \n f (toElem -> a) -> B.unpack (B.take (fromIntegral n) (B.unfoldr (fmap (first toElem) . f) a)) ===
  --    take n (unfoldr (fmap (first toElem) . f) a)
  --
#ifdef WORD16
  , testProperty "useAsCWString str packCWString == str" $
    \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCWString x B.packCWString >>= \x' -> pure (x === x'))
  , testProperty "useAsCWStringLen str packCWStringLen == str" $
    \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCWStringLen x B.packCWStringLen >>= \x' -> pure (x === x'))
#else
  , testProperty "useAsCString str packCString == str" $
    \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCString x B.packCString >>= \x' -> pure (x === x'))
  , testProperty "useAsCStringLen str packCStringLen == str" $
    \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCStringLen x B.packCStringLen >>= \x' -> pure (x === x'))
#endif
  ]

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix x y = fmap reverse (stripPrefix (reverse x) (reverse y))

split :: Eq a => a -> [a] -> [[a]]
split c = splitWith (== c)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f ys = go [] ys
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
      | f x       = reverse acc : go [] xs
      | otherwise = go (x : acc) xs

