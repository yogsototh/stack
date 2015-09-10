{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Stack.Sig.Cabal
Description : Cabal Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.Cabal
       (cabalFilePackageId, packagesFromIndex, getPackageTarballPath)
       where

import qualified Codec.Archive.Tar as Tar
import           Conduit ((=$), ($$), runResourceT, sourceFile, sinkList,
                         linesUnboundedC, decodeUtf8C, concatMapC)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy as BL
import           Data.List (stripPrefix, isSuffixOf)
import           Data.List.Split (splitOn)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Version as V
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Verbosity as D
import           Stack.Types
import           System.Directory (doesFileExist, getAppUserDataDirectory)
import           System.FilePath ((</>))

-- | Extract the @PackageIdentifier@ given an exploded haskell package
-- path.
cabalFilePackageId
    :: (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => FilePath -> m PackageIdentifier
cabalFilePackageId fp =
    liftIO (D.readPackageDescription D.silent fp) >>=
    toStackPI . D.package . D.packageDescription
  where
    toStackPI (D.PackageIdentifier (D.PackageName name) ver) = do
        name' <- parsePackageNameFromString name
        ver' <- parseVersionFromString (V.showVersion ver)
        pure (PackageIdentifier name' ver')

-- | Extract all the @PackageIdentifier@s from the cabal package
-- index.
packagesFromIndex
    :: (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => m [PackageIdentifier]
packagesFromIndex = do
    indexPath <- getPackageIndexPath
    indexExists <- liftIO (doesFileExist indexPath)
    unless
        indexExists
        (throwM
             (CabalIndexException
                  ("Cabal index \"" <> indexPath <>
                   "\" is missing. Please run `cabal update` first.")))
    filePathsFromTarball [] . Tar.read =<<
        liftIO . BL.readFile =<< getPackageIndexPath
  where
    filePathsFromTarball _ (Tar.Fail err) =
        throwM
            (CabalIndexException
                 ("Unable to read the Cabal package index: " <> show err))
    filePathsFromTarball pkgs Tar.Done = (return . catMaybes) pkgs
    filePathsFromTarball pkgs (Tar.Next entry es) =
        case Tar.entryContent entry of
            Tar.NormalFile _ _
              | ".cabal" `isSuffixOf` Tar.entryPath entry ->
                  case splitOn "/" (Tar.entryPath entry) of
                      [] -> filePathsFromTarball pkgs es
                      [_] -> filePathsFromTarball pkgs es
                      (k:v:_) ->
                          filePathsFromTarball
                              (parsePackageIdentifierFromString (k <> "-" <> v) :
                               pkgs)
                              es
            _ -> filePathsFromTarball pkgs es

-- | The hackage package index on disk.
getPackageIndexPath
    :: (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => m FilePath
getPackageIndexPath = do
    cabalCacheDir <- getCabalCacheDir
    return (cabalCacheDir </> "hackage.haskell.org" </> "00-index.tar")

-- | The hackage package tarball location on disk given the
-- @PackageIdentifier@.
getPackageTarballPath
    :: (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => PackageIdentifier -> m FilePath
getPackageTarballPath (PackageIdentifier name ver) = do
    let pName = show name
        pVersion = show ver
    cabalCacheDir <- getCabalCacheDir
    return
        (cabalCacheDir </> "hackage.haskell.org" </> pName </> pVersion </>
         (pName <> "-" <> pVersion <> ".tar.gz"))

-- | The cabal cache directory on disk.
getCabalCacheDir
    :: (Applicative m, MonadCatch m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadLogger m, MonadThrow m)
    => m FilePath
getCabalCacheDir = do
    c <- liftIO (getAppUserDataDirectory "cabal")
    configLines <-
        runResourceT $
        sourceFile (fromString (c </> "config")) $$ decodeUtf8C =$
        linesUnboundedC =$
        concatMapC (getRemoteCache . T.unpack) =$
        sinkList
    case configLines of
        [x] -> return x
        [] ->
            throwM
                (CabalIndexException
                     "No remote-repo-cache found in Cabal config file")
        _ ->
            throwM
                (CabalIndexException
                     "Multiple remote-repo-cache entries found in Cabal config file")
  where
    getRemoteCache s = do
        ("remote-repo-cache",stripPrefix ": " -> Just v) <-
            Just (break (== ':') s)
        Just v
