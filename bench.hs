{-# LANGUAGE BangPatterns #-}

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Int
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString          as A
import qualified Data.Attoparsec.ByteString.Lazy     as AL
import           Data.Binary                         (Binary)
import qualified Data.Binary                         as Binary
import qualified Data.ByteString.Char8               as S
import qualified Data.ByteString.Lazy.Char8          as L
import           Data.Hashable
import qualified Data.HashMap.Strict                 as HMS
import           Data.Monoid
import qualified Data.Serialize                      as Cereal
import           System.Random

instance (Eq key, Hashable key, Binary key, Binary val)
         => Binary (HMS.HashMap key val) where
  put = Binary.put . HMS.toList
  get = HMS.fromList <$> Binary.get

instance (Eq key, Hashable key, Cereal.Serialize key, Cereal.Serialize val)
         => Cereal.Serialize (HMS.HashMap key val) where
  put = Cereal.put . HMS.toList
  get = HMS.fromList <$> Cereal.get

fromHMS :: MapType -> Builder
fromHMS hm =
  fromInt64le (fromIntegral $ HMS.size hm) <>
  mconcat (map (\(k,v) -> fromByteString' k <> fromByteString' v) $ HMS.toList hm)
  where
    fromByteString' bs =
      fromInt64le (fromIntegral $ S.length bs) <> fromByteString bs

encodeBlazeLazy :: MapType -> L.ByteString
encodeBlazeLazy = toLazyByteString . fromHMS

encodeBlazeStrict :: MapType -> S.ByteString
encodeBlazeStrict = toByteString . fromHMS

decodeBlazeLazy :: L.ByteString -> MapType
decodeBlazeLazy bs = ret where
  AL.Done _ ret = AL.parse parseHMS bs

decodeBlazeStrict :: S.ByteString -> MapType
decodeBlazeStrict bs = ret where
  A.Done _ ret = A.parse parseHMS bs

parseHMS = do
  len <- fromIntegral <$> anyWord64le
  HMS.fromList <$> replicateM len ((,) <$> parseBS <*> parseBS)

parseBS = do
  len <- fromIntegral <$> anyWord64le
  A.take len

----

instance NFData L.ByteString where
  rnf bs = L.length bs `seq` ()

instance NFData S.ByteString where
  rnf bs = bs `seq` ()

type MapType = HMS.HashMap S.ByteString S.ByteString

genStr :: IO S.ByteString
genStr = do
  len <- randomRIO (4, 16)
  S.pack <$> replicateM len (randomRIO ('a', 'z'))

main :: IO ()
main = do
  !hms <- HMS.fromList <$> replicateM 4096 ((,) <$> genStr <*> genStr)

  !bin  <- return $!! Binary.encode hms
  !cerS <- return $!! Cereal.encode hms
  !cerL <- return $!! Cereal.encodeLazy hms
  !blaS <- return $!! encodeBlazeStrict hms
  !blaL <- return $!! encodeBlazeLazy hms

  print $ L.length bin
  print $ L.length cerL
  print $ L.length blaL

  defaultMain
    [ bgroup "binary"
      [ bench "encode" $ nf Binary.encode hms
      , bench "decode" $ nf (Binary.decode :: L.ByteString -> MapType) bin
      ]
    , bgroup "cereal"
      [ bench "encode-strict" $ nf Cereal.encode hms
      , bench "encode-lazy"   $ nf Cereal.encodeLazy hms
      , bench "decode-strict" $ nf (Cereal.decode :: S.ByteString -> Either String MapType) cerS
      , bench "decode-lazy"   $ nf (Cereal.decodeLazy :: L.ByteString -> Either String MapType) cerL
      ]
    , bgroup "blaze/attoparsec"
      [ bench "encode-strict" $ nf encodeBlazeStrict hms
      , bench "encode-lazy"   $ nf encodeBlazeLazy hms
      , bench "decode-strict" $ nf decodeBlazeStrict blaS
      , bench "decode-lazy"   $ nf decodeBlazeLazy blaL
      ]
    ]
