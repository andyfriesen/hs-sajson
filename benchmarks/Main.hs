{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Criterion
import Criterion.Main

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)

import qualified Sajson as Sajson
import qualified Sajson.FromJson as Sajson
import qualified Sajson.ToJson as Sajson
import Sajson.FromJson (getKey, withObject)
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import Control.DeepSeq (NFData (..), force)
import qualified Data.ByteString as BS

newtype Utf8 = Utf8 { unUtf8 :: BS.ByteString }
    deriving (Show, Eq, IsString)

instance Sajson.ToJson Utf8 where
    toJson (Utf8 bs) = Sajson.unsafeUtf8ByteString bs

instance Sajson.FromJson Utf8 where
    fromJson val =
        case Sajson.asByteString val of
            Just v -> Right $Utf8 v
            Nothing -> Left "boop"

data EyeColor = Green | Blue | Brown
    deriving (Eq, Show)
data Gender = Male | Female
    deriving (Eq, Show)
data Fruit = Apple | Strawberry | Banana
    deriving (Eq, Show)
data Friend string = Friend
    { fId :: !Int
    , fName :: !string
    } deriving (Eq, Show)

data User string = User
    { uId   :: !string
    , uIndex    :: !Int
    , uGuid :: !string
    , uIsActive :: !Bool
    , uBalance  :: !string
    , uPicture  :: !string
    , uAge  :: !Int
    , uEyeColor :: !EyeColor
    , uName :: !string
    , uGender   :: !Gender
    , uCompany  :: !string
    , uEmail    :: !string
    , uPhone    :: !string
    , uAddress  :: !string
    , uAbout    :: !string
    , uRegistered   :: !string -- UTCTime?
    , uLatitude :: !Double
    , uLongitude    :: !Double
    , uTags :: ![Text]
    , uFriends  :: ![Friend string]
    , uGreeting :: !string
    , uFavouriteFruit   :: !Fruit
    } deriving (Eq, Show)

instance NFData EyeColor
instance NFData Gender
instance NFData Fruit

instance NFData str => NFData (Friend str) where
    rnf Friend {..} = (rnf fId) `seq` (rnf fName) `seq` ()

instance NFData a => NFData (User a) where
    rnf User {..} = (rnf uId) `seq` (rnf uIndex) `seq` (rnf uGuid) `seq` (rnf uIsActive) `seq` (rnf uBalance) `seq` (rnf uPicture) `seq` (rnf uAge) `seq` (rnf uEyeColor) `seq` (rnf uName) `seq` (rnf uGender) `seq` (rnf uCompany) `seq` (rnf uEmail) `seq` (rnf uPhone) `seq` (rnf uAddress) `seq` (rnf uAbout) `seq` (rnf uRegistered) `seq` (rnf uLatitude) `seq` (rnf uLongitude) `seq` (rnf uTags) `seq` (rnf uFriends) `seq` (rnf uGreeting) `seq` (rnf uFavouriteFruit) `seq` ()

eyeColorTable :: [(Text, EyeColor)]
eyeColorTable = [("brown", Brown), ("green", Green), ("blue", Blue)]

genderTable :: [(Text, Gender)]
genderTable = [("male", Male), ("female", Female)]

fruitTable :: [(Text, Fruit)]
fruitTable = [("apple", Apple), ("strawberry", Strawberry), ("banana", Banana)]

enumFromJson :: Monad m => String -> [(Text, enum)] -> (json -> m Text) -> json -> m enum
enumFromJson enumName table extract v = do
    s <- extract v
    case lookup s table of
        Just r -> return r
        Nothing -> fail $ "Bad " ++ enumName ++ ": " ++ show s

instance Sajson.FromJson EyeColor where
    fromJson = enumFromJson "EyeColor" eyeColorTable Sajson.fromJson

instance Sajson.FromJson Gender where
    fromJson = enumFromJson "Gender" genderTable Sajson.fromJson

instance Sajson.FromJson Fruit where
    fromJson v =
        let asStr = Sajson.asByteString v
        in case asStr of
            Just s | "apple"      == s -> Right Apple
                   | "strawberry" == s -> Right Strawberry
                   | "banana"     == s -> Right Banana
                   | otherwise         -> Left "Got bad string decoding Fruit"
            Nothing -> Left "Got nonstring decoding Fruit"

instance Sajson.FromJson str => Sajson.FromJson (Friend str) where
    fromJson v = withObject v $ \o -> do
        fId <- getKey o "id"
        fName <- getKey o "name"
        return Friend {..}

instance Sajson.FromJson str => Sajson.FromJson (User str) where
    fromJson v = withObject v $ \o -> do
        uId <- getKey o "_id"
        uIndex <- getKey o "index"
        uGuid <- getKey o "guid"
        uIsActive <- getKey o "isActive"
        uBalance <- getKey o "balance"
        uPicture <- getKey o "picture"
        uAge <- getKey o "age"
        uEyeColor <- getKey o "eyeColor"
        uName <- getKey o "name"
        uGender <- getKey o "gender"
        uCompany <- getKey o "company"
        uEmail <- getKey o "email"
        uPhone <- getKey o "phone"
        uAddress <- getKey o "address"
        uAbout <- getKey o "about"
        uRegistered <- getKey o "registered"
        uLatitude <- getKey o "latitude"
        uLongitude <- getKey o "longitude"
        uTags <- getKey o "tags"
        uFriends <- getKey o "friends"
        uGreeting <- getKey o "greeting"
        uFavouriteFruit <- getKey o "favoriteFruit"
        return User {..}

instance Sajson.ToJson EyeColor where
    toJson ec = Sajson.toJson $ case ec of
        Green -> "green" :: Utf8
        Blue -> "blue"
        Brown -> "brown"

instance Sajson.ToJson Gender where
    toJson g = Sajson.toJson $ case g of
        Male -> "male" :: Utf8
        Female -> "female"

instance Sajson.ToJson Fruit where
    toJson f = Sajson.toJson $ case f of
        Apple      -> "apple" :: Utf8
        Banana     -> "banana"
        Strawberry -> "strawberry"

instance Sajson.ToJson str => Sajson.ToJson (Friend str) where
    toJson Friend{..} = {-# SCC "friend_to_json" #-} Sajson.toJson $ {-# SCC "friend_newobject" #-} Sajson.newObject
        `Sajson.add` ({-# SCC "friend_id" #-} Sajson.mkBSPair "id" fId)
        `Sajson.add` ({-# SCC "friend_name" #-} Sajson.mkBSPair "name" ({-# SCC "fName" #-} fName))
    {-# INLINE toJson #-}

instance Sajson.ToJson str => Sajson.ToJson (User str) where
    toJson User {..} = {-# SCC "user_to_json" #-} Sajson.toJson $ Sajson.newObject
        `Sajson.add` Sajson.mkBSPair "_id" uId
        `Sajson.add` Sajson.mkBSPair "index" uIndex
        `Sajson.add` Sajson.mkBSPair "guid" uGuid
        `Sajson.add` Sajson.mkBSPair "isActive" uIsActive
        `Sajson.add` Sajson.mkBSPair "balance" uBalance
        `Sajson.add` Sajson.mkBSPair "picture" uPicture
        `Sajson.add` Sajson.mkBSPair "age" uAge
        `Sajson.add` Sajson.mkBSPair "eyeColor" uEyeColor
        `Sajson.add` Sajson.mkBSPair "name" uName
        `Sajson.add` Sajson.mkBSPair "gender" uGender
        `Sajson.add` Sajson.mkBSPair "company" uCompany
        `Sajson.add` Sajson.mkBSPair "email" uEmail
        `Sajson.add` Sajson.mkBSPair "phone" uPhone
        `Sajson.add` Sajson.mkBSPair "address" uAddress
        `Sajson.add` Sajson.mkBSPair "about" uAbout
        `Sajson.add` Sajson.mkBSPair "registered" uRegistered
        `Sajson.add` Sajson.mkBSPair "latitude" uLatitude
        `Sajson.add` Sajson.mkBSPair "longitude" uLongitude
        `Sajson.add` Sajson.mkBSPair "tags" uTags
        `Sajson.add` Sajson.mkBSPair "friends" uFriends
        `Sajson.add` Sajson.mkBSPair "greeting" uGreeting
        `Sajson.add` Sajson.mkBSPair "favoriteFruit" uFavouriteFruit
    {-# INLINE toJson #-}

instance Aeson.FromJSON EyeColor where
    parseJSON = enumFromJson "EyeColor" eyeColorTable Aeson.parseJSON

instance Aeson.FromJSON Gender where
    parseJSON = enumFromJson "Gender" genderTable Aeson.parseJSON

instance Aeson.FromJSON Fruit where
    parseJSON = enumFromJson "Fruit" fruitTable Aeson.parseJSON

instance Aeson.FromJSON str => Aeson.FromJSON (Friend str) where
    parseJSON = Aeson.withObject "Friend" $ \o -> do
        fId <- o .: "id"
        fName <- o .: "name"
        return Friend {..}

instance Aeson.FromJSON str => Aeson.FromJSON (User str) where
    parseJSON = Aeson.withObject "User" $ \o -> do
        uId <- o .: "_id"
        uIndex <- o .: "index"
        uGuid <- o .: "guid"
        uIsActive <- o .: "isActive"
        uBalance <- o .: "balance"
        uPicture <- o .: "picture"
        uAge <- o .: "age"
        uEyeColor <- o .: "eyeColor"
        uName <- o .: "name"
        uGender <- o .: "gender"
        uCompany <- o .: "company"
        uEmail <- o .: "email"
        uPhone <- o .: "phone"
        uAddress <- o .: "address"
        uAbout <- o .: "about"
        uRegistered <- o .: "registered"
        uLatitude <- o .: "latitude"
        uLongitude <- o .: "longitude"
        uTags <- o .: "tags"
        uFriends <- o .: "friends"
        uGreeting <- o .: "greeting"
        uFavouriteFruit <- o .: "favoriteFruit"
        return User {..}

instance Aeson.ToJSON EyeColor where
    toJSON ec = Aeson.toJSON $ case ec of
        Green -> "green" :: Text
        Blue -> "blue"
        Brown -> "brown"

instance Aeson.ToJSON Gender where
    toJSON g = Aeson.toJSON $ case g of
        Male -> "male" :: Text
        Female -> "female"

instance Aeson.ToJSON Fruit where
    toJSON f = Aeson.toJSON $ case f of
        Apple -> "apple" :: Text
        Banana -> "banana"
        Strawberry -> "strawberry"

instance Aeson.ToJSON str => Aeson.ToJSON (Friend str) where
    toJSON Friend {..} = Aeson.object
        [ "id" .= fId
        , "name" .= fName
        ]

instance Aeson.ToJSON str => Aeson.ToJSON (User str) where
    toJSON User{..} = Aeson.object
        [ "_id" .= uId
        , "index" .= uIndex
        , "guid" .= uGuid
        , "isActive" .= uIsActive
        , "balance" .= uBalance
        , "picture" .= uPicture
        , "age" .= uAge
        , "eyeColor" .= uEyeColor
        , "name" .= uName
        , "gender" .= uGender
        , "company" .= uCompany
        , "email" .= uEmail
        , "phone" .= uPhone
        , "address" .= uAddress
        , "about" .= uAbout
        , "registered" .= uRegistered
        , "latitude" .= uLatitude
        , "longitude" .= uLongitude
        , "tags" .= uTags
        , "friends" .= uFriends
        , "greeting" .= uGreeting
        , "favoriteFruit" .= uFavouriteFruit
        ]

assumeSuccess :: Either a b -> b
assumeSuccess (Right r) = r
assumeSuccess _ = error "assumeSuccess"

main :: IO ()
main = do
    content <- B.readFile "test.json"
    let contentText = force $ decodeUtf8 content
    let lazyContent = force $ BSL.fromChunks [content]

    let parsedUserList :: [User Text]
        Right parsedUserList = Sajson.fromJson (assumeSuccess $ Sajson.parse contentText)

        parsedUserList' :: [User Utf8]
        Right parsedUserList' = Sajson.fromJson (assumeSuccess $ Sajson.parse contentText)

    defaultMain [ bgroup "parse"
                    [ bench "sajson" $ whnf Sajson.parse contentText
                    , bench "aeson" $ nf (Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value) lazyContent
                    ]
                , bgroup "extract"
                    [ bench "sajson" $ whnf (Sajson.fromJson :: Sajson.Value -> Either Sajson.DecodeError [User Text]) (assumeSuccess $ Sajson.parse contentText)
                    , bench "aeson" $ whnf (Aeson.decode :: BSL.ByteString -> Maybe [User Text]) lazyContent
                    ]
                , bgroup "render"
                    [ bench "sajson" $ nf Sajson.encodeJsonStrict parsedUserList
                    , bench "sajson2" $ nf Sajson.encodeJsonStrict parsedUserList'
                    , bench "aeson" $ nf Aeson.encode parsedUserList
                    ]
                ]
