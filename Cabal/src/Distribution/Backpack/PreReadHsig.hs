{-# LANGUAGE DataKinds #-}

-- | Pre-read @.hsig@ signature files so their declarations can be
-- shown in error messages. This is done in IO before entering the
-- pure 'LogProgress' monad used by the Backpack linking pipeline.
module Distribution.Backpack.PreReadHsig
  ( HsigDecls
  , readHsigDecls
  , extractDeclarations
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
  ( BuildInfo (hsSourceDirs)
  , Library (libBuildInfo, signatures)
  , PackageDescription
  , allLibraries
  )
import Distribution.Utils.Path
  ( CWD
  , FileOrDir (Dir)
  , Pkg
  , Source
  , SymbolicPath
  , getSymbolicPath
  )

import qualified Data.Map as Map
import System.Directory (doesFileExist)

-- | Map from module name to extracted declaration lines from @.hsig@ files.
type HsigDecls = Map.Map ModuleName [String]

-- | Pre-read all @.hsig@ files referenced by the package's library
-- signatures, extracting their declarations for use in error messages.
--
-- If a @.hsig@ file cannot be found (e.g. it comes from an external
-- dependency), that module is simply omitted from the map.
readHsigDecls
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDescription
  -> IO HsigDecls
readHsigDecls mbWorkDir pkg_descr = do
  entries <- concat <$> traverse (readLibHsigDecls mbWorkDir) (allLibraries pkg_descr)
  return (Map.fromList entries)

readLibHsigDecls
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> Library
  -> IO [(ModuleName, [String])]
readLibHsigDecls mbWorkDir lib = do
  let srcDirs = hsSourceDirs (libBuildInfo lib)
      sigs = signatures lib
  catMaybes <$> traverse (findAndReadHsig mbWorkDir srcDirs) sigs

findAndReadHsig
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> [SymbolicPath Pkg (Dir Source)]
  -> ModuleName
  -> IO (Maybe (ModuleName, [String]))
findAndReadHsig mbWorkDir srcDirs modName = do
  let candidates =
        [ (dir, ext)
        | dir <- if null srcDirs then ["."] else map getSymbolicPath srcDirs
        , ext <- [".hsig", ".lhsig"]
        ]
  go candidates
  where
    go [] = return Nothing
    go ((dir, ext) : rest) = do
      let relPath = dir ++ "/" ++ ModuleName.toFilePath modName ++ ext
          fullPath = case mbWorkDir of
            Nothing -> relPath
            Just wd -> getSymbolicPath wd ++ "/" ++ relPath
      exists <- doesFileExist fullPath
      if exists
        then do
          contents <- readFile fullPath
          let decls = extractDeclarations contents
          return (Just (modName, decls))
        else go rest

-- | Extract declaration lines from the contents of a @.hsig@ file.
--
-- This is a simple text-based extraction: we keep non-blank lines after
-- @where@, filtering out:
--
-- * The @signature ... where@ header
-- * Pragmas (@{-\# ... \#-}@)
-- * Import lines
-- * Line comments (@--@)
extractDeclarations :: String -> [String]
extractDeclarations contents =
  let ls = lines contents
      -- Find lines after the "where" keyword
      afterWhere = case break (elem "where" . words) ls of
        (_, []) -> [] -- no "where" found
        (_, (_ : rest)) -> rest
      -- Filter and clean
      isKeepable line =
        let stripped = dropWhile (== ' ') line
         in not (null stripped)
              && not ("{-#" `isPrefixOf` stripped)
              && not ("import " `isPrefixOf` stripped)
              && not ("--" `isPrefixOf` stripped)
   in map (dropWhile (== ' ')) (filter isKeepable afterWhere)
