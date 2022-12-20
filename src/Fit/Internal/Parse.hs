{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Fit.Internal.Parse
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental
-}
module Fit.Internal.Parse (
  readFitRaw,
  parseFit,

  -- * Parsers for components of FIT files
  parseHeader,
  parseMessages,
  -- parseMessage,
  parseMessageDef,
  parseFieldDef,
  parseDataMessage,
  parseField,
  parseValue,
  parseArray,
  parseSeq,
  parseString,
  parseCTDataMessage,
  mkHeader,
) where

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString (Parser, endOfInput)
import qualified Data.Attoparsec.ByteString as A (anyWord8, parseOnly, runScanner, string, word8)
import qualified Data.Attoparsec.Combinator as A (count, many', manyTill')
import Data.Bits (shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (init)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (fromList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32, Word8)
import Fit.Internal.Architecture (Arch (ArchBig, ArchLittle))
import Fit.Internal.FitFile (
  Array (ByteArray, EnumArray, Float32Array, Float64Array, SInt16Array, SInt32Array, SInt8Array, UInt16Array, UInt16ZArray, UInt32Array, UInt32ZArray, UInt8Array, UInt8ZArray),
  BaseType (FitByte, FitEnum, FitFloat32, FitFloat64, FitSInt16, FitSInt32, FitSInt8, FitString, FitUInt16, FitUInt16Z, FitUInt32, FitUInt32Z, FitUInt8, FitUInt8Z),
  DevDataIdx (DevDataIdx),
  DevDataMsg (DevDataMsg),
  DevFieldDef (DevFieldDef),
  Field (ArrayField, SingletonField),
  FieldDef (FieldDef),
  FieldDefNum (FieldDefNum),
  Fit (Fit),
  FitHeader (FH),
  LocalMessageType,
  Message (DataM, DefM),
  MessageDefinition (MessageDef),
  MessageHeader (CTDataHeader, DataHeader, DefHeader),
  TimeOffset,
  Timestamp (Timestamp, unTimestamp),
  Value (ByteValue, EnumValue, Float32Value, Float64Value, SInt16Value, SInt32Value, SInt8Value, StringValue, UInt16Value, UInt16ZValue, UInt32Value, UInt32ZValue, UInt8Value, UInt8ZValue),
  btSize,
  mkLocalMessageType,
  mkTimeOffset,
 )
import Fit.Internal.FitParser (
  FitParser,
  addDevFieldMsgs,
  addMessageDef,
  archFloat32,
  archFloat64,
  archInt16,
  archInt32,
  archInt64,
  archWord16,
  archWord32,
  int8,
  lookupDevField,
  lookupMessageDef,
  runFitParser,
  storeTimestamp,
  updateTimestamp,
  withArchitecture,
  word8,
 )
import Prelude

-- TODO ignores final CRC bytes

-- | Parse a strict 'ByteString' containing the FIT data into a 'Fit' value
readFitRaw :: ByteString -> Either String Fit
readFitRaw bs =
  let noCrc = B.init (B.init bs) -- Drop CRC bytes
   in A.parseOnly parseFit noCrc

-- | An Attoparsec parser for 'Fit' values
parseFit :: Parser Fit
parseFit = runFitParser $ Fit <$> parseHeader <*> parseMessages

-- Numbers in the FIT header are all little-endian
parseHeader :: FitParser FitHeader
parseHeader = withArchitecture ArchLittle $ do
  headerSize <- word8
  protocolVersion <- word8
  profileVersion <- archWord16
  dataSize <- archWord32
  marker <- lift $ A.string ".FIT"
  crc <-
    if headerSize == 14
      then Just <$> archWord16
      else return Nothing

  return $ FH headerSize protocolVersion profileVersion dataSize marker crc

parseMessages :: FitParser [Message]
parseMessages = mconcat <$> A.manyTill' parseMessageGroup (lift endOfInput)

parseMessageGroup :: FitParser [Message]
parseMessageGroup = do
  messageDef <- parseDefHeader
  msgs <- A.many' parseContent

  addDevFieldMsgs $ mapMaybe convertToDevDataMsg msgs
  pure $ DefM messageDef : msgs

parseDefHeader :: FitParser MessageDefinition
parseDefHeader = do
  headerByte <- lift A.anyWord8
  let header = mkHeader headerByte

  case header of
    DefHeader t d -> do
      msgDef <- parseMessageDef t d
      addMessageDef msgDef
      pure msgDef
    _ -> fail "Not a def header"

parseContent :: FitParser Message
parseContent = do
  headerByte <- lift A.anyWord8
  let header = mkHeader headerByte

  case header of
    DataHeader t -> lookupMessageDef t >>= parseDataMessage
    CTDataHeader t o -> lookupMessageDef t >>= parseCTDataMessage o
    _ -> fail "Not a content header"

parseMessageDef :: LocalMessageType -> Bool -> FitParser MessageDefinition
parseMessageDef lmt hasDeveloperData = do
  _ <- word8 -- Unused reserved byte
  arch <-
    word8 >>= \case
      0 -> return ArchLittle
      1 -> return ArchBig
      invalid -> fail $ "Architecture neither 0 nor 1: " ++ show invalid
  globalNum <- fromIntegral <$> withArchitecture arch archInt16
  numFields <- fromIntegral <$> word8
  fieldDefs <- replicateM numFields parseFieldDef
  devFields <-
    if hasDeveloperData
      then do
        devNumFields <- fromIntegral <$> word8
        replicateM devNumFields parseDevFieldDef
      else pure []

  return $ MessageDef lmt globalNum arch fieldDefs devFields

parseFieldDef :: FitParser FieldDef
parseFieldDef = FieldDef <$> num <*> size <*> baseType
  where
    num = fromIntegral <$> word8
    size = fromIntegral <$> word8
    baseType = word8 >>= toBaseType

toBaseType :: (MonadFail m) => Word8 -> m BaseType
toBaseType = \case
  0x00 -> pure FitEnum
  0x01 -> pure FitSInt8
  0x02 -> pure FitUInt8
  0x83 -> pure FitSInt16
  0x84 -> pure FitUInt16
  0x85 -> pure FitSInt32
  0x86 -> pure FitUInt32
  0x07 -> pure FitString
  0x88 -> pure FitFloat32
  0x89 -> pure FitFloat64
  0x0A -> pure FitUInt8Z
  0x8B -> pure FitUInt16Z
  0x8C -> pure FitUInt32Z
  0x0D -> pure FitByte
  0x8E -> fail "New field format 0x8E: sint64"
  0x8F -> fail "New field format 0x8F: uint64"
  0x90 -> fail "New field format 0x90: uint64z"
  invalid -> fail ("Invalid base type field: " ++ show invalid)

{- | Specialised version of @toMessage@ to parse dev data messages only
 TODO: We cannot use the toMessage function, as that would cause a circular dependency
 but should find a less ad hoc way to achieve this
-}
convertToDevDataMsg :: Message -> Maybe DevDataMsg
convertToDevDataMsg (DefM _) = Nothing
convertToDevDataMsg (DataM _ gmt fields)
  | gmt == 206 =
      let fieldMap = foldr go Map.empty fields
       in DevDataMsg
            <$> (Map.lookup 0 fieldMap >>= fmap DevDataIdx . getUInt8)
            <*> (Map.lookup 1 fieldMap >>= fmap FieldDefNum . getUInt8)
            <*> (Map.lookup 2 fieldMap >>= getBaseType)
            <*> (Map.lookup 3 fieldMap >>= getString)
            <*> pure (Map.lookup 8 fieldMap >>= getString)
            <*> pure (Map.lookup 14 fieldMap >>= getUInt16)
            <*> pure (Map.lookup 15 fieldMap >>= getUInt8)
  | otherwise = Nothing
  where
    go (SingletonField num value) fieldMap = Map.insert num value fieldMap
    go (ArrayField _ _) fieldMap = fieldMap -- fail "Developer data message should not include arrays."
    getUInt8 = \case UInt8Value i -> Just (fromIntegral i); _ -> Nothing
    getUInt16 = \case UInt16Value i -> Just (fromIntegral i); _ -> Nothing
    getBaseType = \case UInt8Value i -> toBaseType i; _ -> Nothing
    getString = \case StringValue str -> Just str; _ -> Nothing

parseDevFieldDef :: FitParser DevFieldDef
parseDevFieldDef = DevFieldDef <$> num <*> size <*> dataIndex
  where
    num = FieldDefNum . fromIntegral <$> word8
    size = fromIntegral <$> word8
    dataIndex = DevDataIdx . fromIntegral <$> word8

parseDataMessage :: MessageDefinition -> FitParser Message
parseDataMessage (MessageDef lmt gmt arch fieldDefs devFieldDefs) = withArchitecture arch $ do
  fields <- mapM parseField fieldDefs
  devFields <- mapM parseDevField devFieldDefs
  return (DataM lmt gmt (fields <> devFields))

parseField :: FieldDef -> FitParser Field
parseField (FieldDef num size bt) = do
  let numValues = size `div` btSize bt

  field <-
    if numValues == 1 || (bt == FitString)
      then SingletonField num <$> parseValue numValues bt
      else ArrayField num <$> parseArray numValues bt

  case field of
    TimestampField t -> storeTimestamp (Timestamp t) >> return field
    _ -> return field

parseDevField :: DevFieldDef -> FitParser Field
parseDevField (DevFieldDef fi@(FieldDefNum num) size ddi) = do
  DevDataMsg _ _ bt _ _ _ _ <- lookupDevField ddi fi

  let numValues = size `div` btSize bt

  field <-
    if numValues == 1 || (bt == FitString)
      then SingletonField num <$> parseValue num bt
      else ArrayField num <$> parseArray numValues bt

  case field of
    TimestampField t -> storeTimestamp (Timestamp t) >> return field
    _ -> return field

parseValue :: Int -> BaseType -> FitParser Value
parseValue n bt =
  case bt of
    FitEnum -> EnumValue <$> word8
    FitSInt8 -> SInt8Value <$> int8
    FitUInt8 -> UInt8Value <$> word8
    FitSInt16 -> SInt16Value <$> archInt16
    FitUInt16 -> UInt16Value <$> archWord16
    FitSInt32 -> SInt32Value <$> archInt32
    FitUInt32 -> UInt32Value <$> archWord32
    FitString -> StringValue <$> lift (parseString n)
    FitFloat32 -> Float32Value <$> archFloat32
    FitFloat64 -> Float64Value <$> archFloat64
    FitUInt8Z -> UInt8ZValue <$> word8
    FitUInt16Z -> UInt16ZValue <$> archWord16
    FitUInt32Z -> UInt32ZValue <$> archWord32
    FitByte -> ByteValue <$> word8

{- | This function will fail if the 'BaseType' is 'FitString'. This implementation
 currently doesn't support arrays of strings, but treats char arrays as always
 being a single string.
-}
parseArray ::
  -- | The number of elements in the array
  Int ->
  -- | The base element type
  BaseType ->
  FitParser Array
parseArray n bt =
  let seqOf = parseSeq n
   in case bt of
        FitEnum -> EnumArray <$> seqOf word8
        FitSInt8 -> SInt8Array <$> seqOf int8
        FitUInt8 -> UInt8Array <$> seqOf word8
        FitSInt16 -> SInt16Array <$> seqOf archInt16
        FitUInt16 -> UInt16Array <$> seqOf archWord16
        FitSInt32 -> SInt32Array <$> seqOf archInt32
        FitUInt32 -> UInt32Array <$> seqOf archWord32
        FitString -> fail "String arrays not supported -- how was this line reached?"
        FitFloat32 -> Float32Array <$> seqOf (fmap fromIntegral archInt32)
        FitFloat64 -> Float64Array <$> seqOf (fmap fromIntegral archInt64)
        FitUInt8Z -> UInt8ZArray <$> seqOf word8
        FitUInt16Z -> UInt16ZArray <$> seqOf archWord16
        FitUInt32Z -> UInt32ZArray <$> seqOf archWord32
        FitByte -> ByteArray <$> seqOf word8

-- | Run a 'FitParser' @n@ times to parse a 'Seq' of values
parseSeq :: Int -> FitParser a -> FitParser (Seq a)
parseSeq n p = S.fromList <$> A.count n p

-- | Parse a null-terminated UTF-8 string.
parseString :: Int -> Parser Text
parseString n = do
  (result, size) <- A.runScanner 0 go
  decodeUtf8
    <$> ( if size == n
            then pure result
            else A.word8 0 $> result
        )
  where
    go :: Int -> Word8 -> Maybe Int
    go size ch
      | size < n && ch /= 0 = Just $ succ size
      | otherwise = Nothing

{- | Parse a compressed-timestamp message, using the 'TimeOffset' from the
 compressed-timestamp message header.
-}
parseCTDataMessage :: TimeOffset -> MessageDefinition -> FitParser Message
parseCTDataMessage offset (MessageDef lmt gmt arch fieldDefs devFieldDefs) = withArchitecture arch $ do
  fields <- mapM parseField fieldDefs
  devFields <- mapM parseDevField devFieldDefs
  newTimestamp <- updateTimestamp offset
  let timestampField = TimestampField (unTimestamp newTimestamp)
  return $ DataM lmt gmt (timestampField : fields <> devFields)

-- | Transform a FIT message header byte into a 'MessageHeader'
mkHeader :: Word8 -> MessageHeader
mkHeader byte =
  if isNormalHeader
    then
      if isDefMessage
        then defHeader
        else dataHeader
    else ctDataHeader
  where
    isNormalHeader = not (testBit byte 7) -- 0 indicates normal header
    isDefMessage = testBit byte 6 -- 0 indicates data message
    hasDeveloperData = testBit byte 5 -- indicates extended record format with custom developer data
    defHeader = DefHeader normalLmt hasDeveloperData
    dataHeader = DataHeader normalLmt
    ctDataHeader = CTDataHeader ctLmt ctOffset
    normalLmt = mkLocalMessageType byte -- in a normal header the lmt is already low 4 bits
    ctLmt = mkLocalMessageType $ (byte `shiftR` 5) .&. 0x3 -- in CT header the lmt is bits 5 and 6
    ctOffset = mkTimeOffset byte -- in CT header the time offset is already low 5 bits

-- Originally this was exported from Fit.Internal.Types, but haddock chokes on patterns
-- between modules. I think the bug is fixed in GHC 7.8.4, so can move this back to
-- that module once we're off 7.8.3.
pattern TimestampField :: Word32 -> Field
pattern TimestampField t = SingletonField 253 (UInt32Value t)
