{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Stack.Sig.Hackage
Description : Call Hackage APIs for User/Package Data
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Hackage (packagesForMaintainer) where

import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson (eitherDecode)
import           Data.Aeson.TH (deriveFromJSON, defaultOptions)
import           Data.List.Split (splitOn)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Network.HTTP.Conduit
  (parseUrl, withManager, httpLbs, requestHeaders, responseBody)
import           Stack.Types

data UserDetail = UserDetail
    { groups :: [String]
    , username :: String
    , userid :: Integer
    } deriving (Show,Eq)

$(deriveFromJSON defaultOptions ''UserDetail)

-- | Given a hackage.org maintainer username, fetch all their
-- @PackageIdentifier@s.
packagesForMaintainer
    :: forall (m :: * -> *).
       (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadLogger m)
    => String -> m [PackageName]
packagesForMaintainer uname = do
    req <- parseUrl ("https://hackage.haskell.org/user/" <> uname)
    res <-
        withManager
            (httpLbs
                 (req
                  { requestHeaders = [("Accept", "application/json")]
                  }))
    let groups' = groups <$> eitherDecode (responseBody res)
        splits = map (filter (/= "") . splitOn "/") <$> groups'
        maybeName ["package",p,"maintainers"] = Just p
        maybeName _ = Nothing
        pkgNames = mapMaybe maybeName <$> splits
    case pkgNames of
        Left e ->
            throwM
                (HackageAPIException
                     ("Cloudn't retrieve packages for user " <> uname <> ": " <>
                      show e))
        Right p -> mapM parsePackageNameFromString p
