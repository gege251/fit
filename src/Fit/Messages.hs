{-# LANGUAGE LambdaCase #-}

{- |
Module      : Fit.Messages
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

The Messages API abstracts over the structure of a FIT file slightly and presents
the FIT file as just the sequence of data messages in the file. The Messages API
also abstracts over the various FIT base types (for example, signed/unsigned integers
of different sizes) to give a simpler set of types to work with.

If you need to know about the specifics of the FIT file structure, use the API in
"Fit.Internal.FitFile" instead. However, for pulling information out of a FIT file
this API is much more convenient.
-}
module Fit.Messages (
  readMessages,
  readFileMessages,
  parseMessages,
  Messages (..),
  Message (..),
  Field (..),
  Value (..),
  SingletonValue (..),
  ArrayValue (..),
) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (pack, readFile)
import qualified Data.Foldable as F (toList)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map (empty, insert)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (fromList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Word (Word8)
import qualified Fit.Internal.FitFile as FF
import qualified Fit.Internal.Parse as FF

import Prelude

-- | The collection of data messages from the FIT file.
newtype Messages = Messages
  { _messages :: Seq Message
  }
  deriving (Show)

-- | A FIT data message
data Message = Message
  { _mNumber :: !Int
  -- ^ The global message number, as found in the FIT profile
  , _mFields :: IntMap Field
  -- ^ The fields in the message, mapped from field number to 'Field'
  }
  deriving (Show)

-- | A single field in a FIT data message
data Field = Field
  { _fNumber :: !Int
  -- ^ The field number, as found in the FIT profile
  , _fValue :: Value
  }
  deriving (Show, Eq)

-- | FIT values can either contain a single piece of data or an array. FIT arrays are homogenous
data Value
  = Singleton SingletonValue
  | Array ArrayValue
  deriving (Show, Eq)

-- | A singleton value. In the Messages API we abstract over the specific FIT base type of the field. For example, the FIT types uint8, sint8, uint16, etc. are all presented as an 'IntValue'. FIT strings (ie. character arrays) are presented as singleton 'TextValue's. If you need to know the specific base type of a field you can use the API in "Fit.Internal.FitFile".
data SingletonValue
  = IntValue !Int
  | RealValue !Double
  | ByteValue !Word8
  | TextValue Text
  deriving (Show, Eq)

-- | Array values. Like singleton values these ignore the specific FIT base type to present a simpler interface. Byte arrays are presented as strict 'ByteString's. There are no character arrays, since the singleton 'TextValue' handles that case.
data ArrayValue
  = IntArray (Seq Int)
  | RealArray (Seq Double)
  | ByteArray ByteString
  deriving (Show, Eq)

-- | Parse a strict 'ByteString' containing the FIT data into its 'Messages'
readMessages :: ByteString -> Either String Messages
readMessages bs = toMessages <$> FF.readFitRaw bs

-- | Parse the given FIT file into its 'Messages'
readFileMessages :: FilePath -> IO (Either String Messages)
readFileMessages fp = B.readFile fp <&> readMessages

-- | An Attoparsec parser for 'Messages'
parseMessages :: Parser Messages
parseMessages = fmap toMessages FF.parseFit

toMessages :: FF.Fit -> Messages
toMessages rFit = (Messages . S.fromList) (mapMaybe toMessage (FF.fMessages rFit))

toMessage :: FF.Message -> Maybe Message
toMessage (FF.DefM _) = Nothing
toMessage (FF.DataM _ gmt fields) = Just $ Message gmt (foldr go Map.empty fields)
  where
    getNum (FF.SingletonField num _) = num
    getNum (FF.ArrayField num _) = num

    go rawF fieldMap = case toField rawF of
      Just f -> Map.insert (getNum rawF) f fieldMap
      Nothing -> fieldMap

toField :: FF.Field -> Maybe Field
toField (FF.SingletonField num value)
  | isInvalidValue value = Nothing
  | otherwise = Just . Field num . Singleton $ fromSingletonValue value
toField (FF.ArrayField num array) = Just . Field num . Array $ fromArray array

isInvalidValue :: FF.Value -> Bool
isInvalidValue = \case
  FF.EnumValue i -> i == 0xFF
  FF.SInt8Value i -> i == 0x7F
  FF.UInt8Value i -> i == 0xFF
  FF.SInt16Value i -> i == 0x7FFF
  FF.UInt16Value i -> i == 0xFFFF
  FF.SInt32Value i -> i == 0x7FFFFFFF
  FF.UInt32Value i -> i == 0xFFFFFFFF
  FF.StringValue t -> t == mempty
  FF.Float32Value f -> f == 0xFFFFFFFF
  FF.Float64Value f -> f == 0xFFFFFFFFFFFFFFFF
  FF.UInt8ZValue i -> i == 0x00
  FF.UInt16ZValue i -> i == 0x0000
  FF.UInt32ZValue i -> i == 0x00000000
  FF.ByteValue b -> b == 0xFF
  FF.SInt64Value i -> i == 0x7FFFFFFFFFFFFFFF
  FF.UInt64Value i -> i == 0xFFFFFFFFFFFFFFFF
  FF.UInt64ZValue i -> i == 0x0000000000000000

fromSingletonValue :: FF.Value -> SingletonValue
fromSingletonValue v =
  case v of
    FF.EnumValue i -> IntValue (fromIntegral i)
    FF.SInt8Value i -> IntValue (fromIntegral i)
    FF.UInt8Value i -> IntValue (fromIntegral i)
    FF.SInt16Value i -> IntValue (fromIntegral i)
    FF.UInt16Value i -> IntValue (fromIntegral i)
    FF.SInt32Value i -> IntValue (fromIntegral i)
    FF.UInt32Value i -> IntValue (fromIntegral i)
    FF.StringValue t -> TextValue t
    FF.Float32Value f -> RealValue (fromRational (toRational f))
    FF.Float64Value f -> RealValue f
    FF.UInt8ZValue i -> IntValue (fromIntegral i)
    FF.UInt16ZValue i -> IntValue (fromIntegral i)
    FF.UInt32ZValue i -> IntValue (fromIntegral i)
    FF.ByteValue b -> ByteValue b
    FF.SInt64Value i -> IntValue (fromIntegral i)
    FF.UInt64Value i -> IntValue (fromIntegral i)
    FF.UInt64ZValue i -> IntValue (fromIntegral i)

fromArray :: FF.Array -> ArrayValue
fromArray a =
  case a of
    FF.EnumArray xs -> intArray $ Seq.filter (/= 0xFF) xs
    FF.SInt8Array xs -> intArray $ Seq.filter (/= 0x7F) xs
    FF.UInt8Array xs -> intArray $ Seq.filter (/= 0xFF) xs
    FF.SInt16Array xs -> intArray $ Seq.filter (/= 0x7FFF) xs
    FF.UInt16Array xs -> intArray $ Seq.filter (/= 0xFFFF) xs
    FF.SInt32Array xs -> intArray $ Seq.filter (/= 0x7FFFFFFF) xs
    FF.UInt32Array xs -> intArray $ Seq.filter (/= 0xFFFFFFFF) xs
    FF.Float32Array xs -> realArray $ Seq.filter (/= 0xFFFFFFFF) xs
    FF.Float64Array xs -> realArray $ Seq.filter (/= 0xFFFFFFFFFFFFFFFF) xs
    FF.UInt8ZArray xs -> intArray $ Seq.filter (/= 0x00) xs
    FF.UInt16ZArray xs -> intArray $ Seq.filter (/= 0x0000) xs
    FF.UInt32ZArray xs -> intArray $ Seq.filter (/= 0x00000000) xs
    FF.ByteArray bs -> ByteArray . B.pack . F.toList $ Seq.filter (/= 0x0000000000000000) bs
    FF.SInt64Array xs -> intArray $ Seq.filter (/= 0xFF) xs
    FF.UInt64Array xs -> intArray $ Seq.filter (/= 0x7FFFFFFFFFFFFFFF) xs
    FF.UInt64ZArray xs -> intArray $ Seq.filter (/= 0xFFFFFFFFFFFFFFFF) xs
  where
    intArray is = IntArray $ fmap fromIntegral is
    realArray rs = RealArray $ fmap (fromRational . toRational) rs
