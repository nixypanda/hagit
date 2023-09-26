{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Packfile (
    DeltaContent (..),
    DeltafiedObj (..),
    Instruction (..),
    InstructionType (..),
    PackObjHeader (..),
    PackObjType (..),
    PackObject (..),
    PackfileHeader (..),
    PackfileWithDeltas (..),
    getDeltified,
    getUndeltified,
    mkDeltifiedObj,
    mkPackObjHeader,
    objHeaderRepr,
    packObjLen,
    instructionType,
) where

import Crypto.Hash (Digest, SHA1)
import Data.Bits ((.&.))
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Word8 (Word8)
import Object (GitObject (..), ObjectType (..), objBodyLen)
import Prelude hiding (take)

data PackfileWithDeltas = PackfileWithDeltas
    { packfileHeader :: PackfileHeader
    , packfileObjects :: [PackObject]
    , receivedChecksum :: Digest SHA1
    }
    deriving (Show)

data PackfileHeader = PackfileHeader
    { magicString :: BL.ByteString
    , packfileVersion :: Int
    , objectsInPackfile :: Int
    }
    deriving (Show)

data PackObject
    = Undeltafied GitObject
    | Deltafied DeltafiedObj
    deriving (Show, Eq)

packObjLen :: PackObject -> Int
packObjLen (Undeltafied go) = objBodyLen go
packObjLen (Deltafied (DeltafiedObj (PackObjHeader _ size) _ _)) = size

getDeltified :: PackObject -> Maybe DeltafiedObj
getDeltified (Deltafied x) = Just x
getDeltified _ = Nothing

getUndeltified :: PackObject -> Maybe GitObject
getUndeltified (Undeltafied x) = Just x
getUndeltified _ = Nothing

data DeltafiedObj = DeltafiedObj
    { deltaObjHeader :: PackObjHeader
    , deltaObjData :: BL.ByteString
    , deltaObjParentSha1 :: Digest SHA1
    }
    deriving (Show, Eq)

mkDeltifiedObj :: PackObjHeader -> Digest SHA1 -> BL.ByteString -> PackObject
mkDeltifiedObj objHeader parentSha1 deltaObjData =
    Deltafied $ DeltafiedObj objHeader deltaObjData parentSha1

data PackObjHeader = PackObjHeader
    { packObjType :: PackObjType
    , packObjSize :: Int
    }
    deriving (Show, Eq)

mkPackObjHeader :: ObjectType -> Int -> PackObjHeader
mkPackObjHeader BlobType objSize = PackObjHeader OBJ_BLOB objSize
mkPackObjHeader TreeType objSize = PackObjHeader OBJ_TREE objSize
mkPackObjHeader CommitType objSize = PackObjHeader OBJ_COMMIT objSize

objHeaderRepr :: PackObjHeader -> BL.ByteString
objHeaderRepr PackObjHeader{..} = BLC.pack $ packObjTypeRepr packObjType <> " " <> show packObjSize

data PackObjType
    = OBJ_COMMIT
    | OBJ_TREE
    | OBJ_BLOB
    | OBJ_TAG
    | OBJ_OFS_DELTA
    | OBJ_REF_DELTA
    deriving (Show, Eq)

packObjTypeRepr :: PackObjType -> String
packObjTypeRepr OBJ_COMMIT = "commit"
packObjTypeRepr OBJ_TREE = "tree"
packObjTypeRepr OBJ_BLOB = "blob"
packObjTypeRepr OBJ_TAG = "tag"
packObjTypeRepr _ = error "Has no representation"

-- Packfile Deltafied Object Instructions

data InstructionType = CopyType | AddNewType deriving (Show, Eq)

instructionType :: Word8 -> InstructionType
instructionType firstByte
    | firstByte .&. 0x80 == 0 = AddNewType
    | otherwise = CopyType

data Instruction
    = Copy Int Int
    | AddNew BL.ByteString
    deriving (Show, Eq)

data DeltaContent = DeltaContent
    { baseObjSize :: Int
    , reconstructedObjSize :: Int
    , instructions :: [Instruction]
    }
    deriving (Show, Eq)
