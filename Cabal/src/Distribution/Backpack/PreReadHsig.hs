{-# LANGUAGE DataKinds #-}

-- | Pre-read @.hsig@ signature files so their declarations can be
-- shown in error messages. This is done in IO before entering the
-- pure 'LogProgress' monad used by the Backpack linking pipeline.
module Distribution.Backpack.PreReadHsig
  ( HsigDecls
  , readHsigDecls
  , extractDeclarations
  , extractDeclName
  , ModuleDefinedNames
  , extractDefinedNames
  , readModuleDefinedNames
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
  ( BuildInfo (hsSourceDirs)
  , Library (exposedModules, libBuildInfo, signatures)
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
import qualified Data.Set as Set
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

-- | Extract the declared name from a single @.hsig@ declaration line.
--
-- Examples:
--
-- * @"data A"@ → @Just "A"@
-- * @"fun :: A -> A"@ → @Just "fun"@
-- * @"type T"@ → @Just "T"@
-- * @"class C a where"@ → @Just "C"@
-- * @"newtype N"@ → @Just "N"@
extractDeclName :: String -> Maybe String
extractDeclName line =
  case words line of
    (kw : name : _)
      | kw `elem` ["data", "type", "newtype", "class"] -> Just name
    (name : "::" : _) -> Just name
    _ -> Nothing

-- | Map from module name to the set of top-level defined names in a @.hs@ file.
type ModuleDefinedNames = Map.Map ModuleName (Set.Set String)

-- | Extract top-level defined names from the contents of a @.hs@ file.
--
-- Considers two sources of names:
--
-- 1. The module export list (if present): names like @Foo(..)@, @bar@ are
--    extracted. This catches re-exports from imported modules.
--
-- 2. Top-level definitions after the module header: @data@/@type@/@newtype@/@class@
--    declarations, type signatures, and function definitions.
extractDefinedNames :: String -> Set.Set String
extractDefinedNames contents =
  let ls = lines contents
      -- Collect the module header text (everything up to and including "where")
      (headerLines, bodyLines) = case break (elem "where" . words) ls of
        (_, []) -> ([], ls) -- no module header
        (before, (whereLine : rest)) -> (before ++ [whereLine], rest)
      headerText = unwords (map (dropWhile (== ' ')) headerLines)
      -- Extract names from the export list in the module header
      exportNames = extractExportListNames headerText
      -- Only consider non-indented, non-blank lines for definitions
      isTopLevel l = case l of
        (c : _) -> c /= ' ' && c /= '\t'
        [] -> False
      extractName l =
        let ws = words l
         in case ws of
              (kw : name : _)
                | kw `elem` ["data", "type", "newtype", "class"] -> Just name
              (name : "=" : _) -> Just name
              (name : "::" : _) -> Just name
              -- Pattern like "name arg1 arg2 = ..."
              (name@(c : _) : _)
                | any (== '=') l
                , isIdentStart c ->
                    Just name
              _ -> Nothing
      isIdentStart c = (c >= 'a' && c <= 'z') || c == '_'
      defNames = Set.fromList (mapMaybe extractName (filter isTopLevel bodyLines))
   in Set.union exportNames defNames

-- | Extract names from a module export list.
--
-- Given a module header like @"module Foo ( bar , Baz(..) , qux ) where"@,
-- extracts @{"bar", "Baz", "qux"}@. This handles re-exports.
-- Correctly handles nested parentheses from @(..)@ patterns.
extractExportListNames :: String -> Set.Set String
extractExportListNames headerText =
  case dropWhile (/= '(') headerText of
    ('(' : rest) ->
      let -- Take everything up to the matching closing paren, respecting nesting
          exportText = takeBalanced 0 rest
          -- Split on top-level commas (not inside parens)
          items = splitTopLevel exportText
          extractItem item =
            case words item of
              (name : _) ->
                let cleaned = takeWhile (\c -> c /= '(' && c /= ')') name
                 in if not (null cleaned)
                      then Just cleaned
                      else Nothing
              [] -> Nothing
       in Set.fromList (mapMaybe extractItem items)
    _ -> Set.empty

-- | Take characters up to the matching closing parenthesis at depth 0.
takeBalanced :: Int -> String -> String
takeBalanced _ [] = []
takeBalanced 0 (')' : _) = []
takeBalanced depth (')' : cs) = ')' : takeBalanced (depth - 1) cs
takeBalanced depth ('(' : cs) = '(' : takeBalanced (depth + 1) cs
takeBalanced depth (c : cs) = c : takeBalanced depth cs

-- | Split a string on commas that are not inside parentheses.
splitTopLevel :: String -> [String]
splitTopLevel = go 0 ""
  where
    go :: Int -> String -> String -> [String]
    go _ acc [] = [reverse acc]
    go 0 acc (',' : cs) = reverse acc : go 0 "" cs
    go depth acc ('(' : cs) = go (depth + 1) ('(' : acc) cs
    go depth acc (')' : cs) = go (max 0 (depth - 1)) (')' : acc) cs
    go depth acc (c : cs) = go depth (c : acc) cs


-- | Read @.hs@ files for modules whose names match known signatures,
-- extracting their defined names. Only reads local (in-package) modules.
readModuleDefinedNames
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDescription
  -> Set.Set ModuleName
  -- ^ The set of signature module names to look for
  -> IO ModuleDefinedNames
readModuleDefinedNames mbWorkDir pkg_descr sigNames = do
  entries <- concat <$> traverse (readLibDefinedNames mbWorkDir sigNames) (allLibraries pkg_descr)
  return (Map.fromList entries)

readLibDefinedNames
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> Set.Set ModuleName
  -> Library
  -> IO [(ModuleName, Set.Set String)]
readLibDefinedNames mbWorkDir sigNames lib = do
  let srcDirs = hsSourceDirs (libBuildInfo lib)
      -- Only look at exposed modules whose names match a signature
      mods = filter (`Set.member` sigNames) (exposedModules lib)
  catMaybes <$> traverse (findAndReadModule mbWorkDir srcDirs) mods

findAndReadModule
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> [SymbolicPath Pkg (Dir Source)]
  -> ModuleName
  -> IO (Maybe (ModuleName, Set.Set String))
findAndReadModule mbWorkDir srcDirs modName = do
  let candidates =
        [ (dir, ext)
        | dir <- if null srcDirs then ["."] else map getSymbolicPath srcDirs
        , ext <- [".hs", ".lhs"]
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
          fileContents <- readFile fullPath
          let names = extractDefinedNames fileContents
          return (Just (modName, names))
        else go rest
