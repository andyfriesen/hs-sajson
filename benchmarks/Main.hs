module Main where

import Criterion
import Criterion.Main

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)

import qualified Sajson as Sajson
import qualified Data.Aeson as Aeson
import Control.DeepSeq (force)

main :: IO ()
main = do
    content <- B.readFile "test.json"
    let contentText = force $ decodeUtf8 content
    let lazyContent = force $ BSL.fromChunks [content]
    defaultMain [ bgroup "sajson"
                    [ bench "parse" $ whnf Sajson.parse contentText ]
                , bgroup "aeson"
                    [ bench "parse" $ whnf (Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value) lazyContent ]
                ]