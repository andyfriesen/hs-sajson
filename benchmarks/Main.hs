{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
import Data.Tuple (swap)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import Control.DeepSeq (NFData (..), force)

data EyeColor = Green | Blue | Brown
    deriving (Eq, Show)
data Gender = Male | Female
    deriving (Eq, Show)
data Fruit = Apple | Strawberry | Banana
    deriving (Eq, Show)
data Friend = Friend
    { fId :: !Int
    , fName :: !Text
    } deriving (Eq, Show)

data User = User
    { uId   :: !Text
    , uIndex    :: !Int
    , uGuid :: !Text
    , uIsActive :: !Bool
    , uBalance  :: !Text
    , uPicture  :: !Text
    , uAge  :: !Int
    , uEyeColor :: !EyeColor
    , uName :: !Text
    , uGender   :: !Gender
    , uCompany  :: !Text
    , uEmail    :: !Text
    , uPhone    :: !Text
    , uAddress  :: !Text
    , uAbout    :: !Text
    , uRegistered   :: !Text -- UTCTime?
    , uLatitude :: !Double
    , uLongitude    :: !Double
    , uTags :: ![Text]
    , uFriends  :: ![Friend]
    , uGreeting :: !Text
    , uFavouriteFruit   :: !Fruit
    } deriving (Eq, Show)

instance NFData EyeColor
instance NFData Gender
instance NFData Fruit

instance NFData Friend where
    rnf Friend {..} = (rnf fId) `seq` (rnf fName) `seq` ()

instance NFData User where
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

enumToJson :: (Show a, Eq a)
           => String -> [(b, a)] -> (b -> t) -> a -> t
enumToJson enumName table encode v = do
    case lookup v (map swap table) of
        Just n -> encode n
        Nothing -> error $ "Failed to encode " ++ enumName ++ ": " ++ show v

instance Sajson.FromJson EyeColor where
    fromJson = enumFromJson "EyeColor" eyeColorTable Sajson.fromJson

instance Sajson.FromJson Gender where
    fromJson = enumFromJson "Gender" genderTable Sajson.fromJson

instance Sajson.FromJson Fruit where
    fromJson = enumFromJson "Fruit" fruitTable Sajson.fromJson

instance Sajson.FromJson Friend where
    fromJson v = withObject v $ \o -> do
        fId <- getKey o "id"
        fName <- getKey o "name"
        return Friend {..}

instance Sajson.FromJson User where
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
    toJson = enumToJson "EyeColor" eyeColorTable Sajson.toJson

instance Sajson.ToJson Gender where
    toJson = enumToJson "Gender" genderTable Sajson.toJson

instance Sajson.ToJson Fruit where
    toJson = enumToJson "Fruit" fruitTable Sajson.toJson

instance Sajson.ToJson Friend where
    toJson Friend{..} = {-# SCC "friend_to_json" #-} Sajson.toJson $ {-# SCC "friend_newobject" #-} Sajson.newObject
        `Sajson.add` ({-# SCC "friend_id" #-} Sajson.mkPair "id" fId)
        `Sajson.add` ({-# SCC "friend_name" #-} Sajson.mkPair "name" fName)

instance Sajson.ToJson User where
    toJson User {..} = {-# SCC "user_to_json" #-} Sajson.toJson $ Sajson.newObject
        `Sajson.add` Sajson.mkPair "_id" uId
        `Sajson.add` Sajson.mkPair "index" uIndex
        `Sajson.add` Sajson.mkPair "guid" uGuid
        `Sajson.add` Sajson.mkPair "isActive" uIsActive
        `Sajson.add` Sajson.mkPair "balance" uBalance
        `Sajson.add` Sajson.mkPair "picture" uPicture
        `Sajson.add` Sajson.mkPair "age" uAge
        `Sajson.add` Sajson.mkPair "eyeColor" uEyeColor
        `Sajson.add` Sajson.mkPair "name" uName
        `Sajson.add` Sajson.mkPair "gender" uGender
        `Sajson.add` Sajson.mkPair "company" uCompany
        `Sajson.add` Sajson.mkPair "email" uEmail
        `Sajson.add` Sajson.mkPair "phone" uPhone
        `Sajson.add` Sajson.mkPair "address" uAddress
        `Sajson.add` Sajson.mkPair "about" uAbout
        `Sajson.add` Sajson.mkPair "registered" uRegistered
        `Sajson.add` Sajson.mkPair "latitude" uLatitude
        `Sajson.add` Sajson.mkPair "longitude" uLongitude
        `Sajson.add` Sajson.mkPair "tags" uTags
        `Sajson.add` Sajson.mkPair "friends" uFriends
        `Sajson.add` Sajson.mkPair "greeting" uGreeting
        `Sajson.add` Sajson.mkPair "favoriteFruit" uFavouriteFruit

instance Aeson.FromJSON EyeColor where
    parseJSON = enumFromJson "EyeColor" eyeColorTable Aeson.parseJSON

instance Aeson.FromJSON Gender where
    parseJSON = enumFromJson "Gender" genderTable Aeson.parseJSON

instance Aeson.FromJSON Fruit where
    parseJSON = enumFromJson "Fruit" fruitTable Aeson.parseJSON

instance Aeson.FromJSON Friend where
    parseJSON = Aeson.withObject "Friend" $ \o -> do
        fId <- o .: "id"
        fName <- o .: "name"
        return Friend {..}

instance Aeson.FromJSON User where
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
    toJSON = enumToJson "EyeColor" eyeColorTable Aeson.toJSON

instance Aeson.ToJSON Gender where
    toJSON = enumToJson "Gender" genderTable Aeson.toJSON

instance Aeson.ToJSON Fruit where
    toJSON = enumToJson "Fruit" fruitTable Aeson.toJSON

instance Aeson.ToJSON Friend where
    toJSON Friend {..} = Aeson.object
        [ "id" .= fId
        , "name" .= fName
        ]

instance Aeson.ToJSON User where
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

    let Right parsedUserList = (Sajson.fromJson :: Sajson.Value -> Either Sajson.DecodeError [User]) (assumeSuccess $ Sajson.parse contentText)

    defaultMain [ bgroup "parse"
                    [ bench "sajson" $ whnf Sajson.parse contentText
                    , bench "aeson" $ nf (Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value) lazyContent
                    ]
                , bgroup "extract"
                    [ bench "sajson" $ whnf (Sajson.fromJson :: Sajson.Value -> Either Sajson.DecodeError [User]) (assumeSuccess $ Sajson.parse contentText)
                    , bench "aeson" $ whnf (Aeson.decode :: BSL.ByteString -> Maybe [User]) lazyContent
                    ]
                , bgroup "render"
                    [ bench "sajson" $ nf Sajson.encodeJsonStrict parsedUserList
                    , bench "aeson" $ nf Aeson.encode parsedUserList
                    ]
                ]
