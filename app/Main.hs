{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

import Web.Spock
    ( delete,
      get,
      post,
      put,
      spock,
      (<//>),
      runSpock,
      json,
      jsonBody,
      setStatus,
      var,
      SpockM,
      HasSpock(runQuery, SpockConn),
      SpockAction,
      ActionCtxT )
import Web.Spock.Config
    ( SpockCfg(SpockCfg, spc_initialState, spc_database,
               spc_sessionCfg, spc_maxRequestSize, spc_logError,
               spc_csrfProtection, spc_csrfHeaderName, spc_csrfPostName),
      defaultSpockCfg,
      PoolOrConn(PCPool) )

import Data.Aeson ( object, Value(String), KeyValue((.=)) )
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics ()

import Network.HTTP.Types (Status, status201, status204, status400, status404)

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist ( selectList, SelectOpt(Asc) )
import Database.Persist.Postgresql
    ( runSqlPool,
      runSqlConn,
      runMigration,
      selectList,
      SelectOpt(Asc),
      SqlPersistT,
      BackendKey(SqlBackendKey),
      SqlBackend,
      createPostgresqlPool )
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
import qualified Database.Persist as P

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible to ToJSON and FromJSON instances for us.
  name Text 
  age Int 
  deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object 
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

commonErrorJson :: Status -> ActionCtxT () IO ()
commonErrorJson status =
  json $
    object 
    [
      "result" .= String "failure"
    , "error" .= object ["message" .= message]
    ]
      where
        message = String $ if status == status404 then "Resource not found." else "Unknown error."

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool "postgresql://localhost:5432/spock?user=postgres&password=example" 5
  baseConfig <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = SpockCfg (spc_initialState baseConfig) (spc_database baseConfig) (spc_sessionCfg baseConfig) (spc_maxRequestSize baseConfig) commonErrorJson (spc_logError baseConfig) (spc_csrfProtection baseConfig) (spc_csrfHeaderName baseConfig) (spc_csrfPostName baseConfig)
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople

  get ("people" <//> var) $ \personId -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus status404
        errorJson 2 "Could not find a person with matching id"
      Just thePerson -> json thePerson

  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus status400
        errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        setStatus status201 
        newId <- runSQL $ P.insert thePerson
        json $ object ["result" .= String "success", "id" .= newId]

  put ("people" <//> var) $ \personId -> do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus status400
        errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        setStatus status201
        _ <- runSQL $ P.replace personId thePerson
        json $ object ["result" .= String "success"]

  delete ("people" <//> var) $ \personId -> do
    _ <- runSQL $ P.delete (personId :: P.Key Person) :: ApiAction ()
    json $ object ["result" .= String "success"]
