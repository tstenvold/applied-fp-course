{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           ResponseReceived, lazyRequestBody,
                                           pathInfo, rawQueryString,
                                           requestMethod, responseLBS)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status Json lbs = responseLBS status [("Context-Type", renderContentType Json)] lbs
mkResponse status PlainText lbs = responseLBS status [("Context-Type", renderContentType PlainText)] lbs

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest txt lbs =  do
  topic <- mkTopic txt
  comment <- mkCommentText $ lazyByteStringToStrictText lbs
  return $ AddRq topic comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t = ViewRq <$> mkTopic t

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownCommand = resp404 PlainText "Unknown Command"
mkErrorResponse EmptyTopic     = resp400 PlainText "Invalid Topic"
mkErrorResponse EmptyComment   = resp400 PlainText "Empty Comment Text"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest request =
  case (requestMethod request, pathInfo request) of
  ("POST", [topic, "add"]) -> do
    body <- lazyRequestBody request
    return $ mkAddRequest topic body
  ("GET", [topic, "view"]) -> return $ mkViewRequest topic
  ("GET", ["list"])        -> return mkListRequest
  (_, _)                   -> return $ Left UnknownCommand


  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "AddRq not implemented yet"
handleRequest (ViewRq _) = Right $ resp200 PlainText "ViewRq not implemented yet"
handleRequest ListRq      = Right $ resp200 PlainText "ListRq not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app :: Application
app request respond = do
  req <- mkRequest request
  case req of
    Right rqType -> let reps = handleRequest rqType in
      case reps of
        Right re -> respond re
        Left err -> respond $ mkErrorResponse err
    Left errorRep   ->  respond $ mkErrorResponse errorRep



runApp :: IO ()
runApp = run 3000 app
