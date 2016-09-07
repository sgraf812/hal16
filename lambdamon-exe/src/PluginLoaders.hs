{-# LANGUAGE ScopedTypeVariables #-}

module PluginLoaders where

import Types
import Paths
import Lens.Micro.Platform
import Data.Maybe (fromJust)
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua
import Language.Haskell.Interpreter (Interpreter)
import qualified Language.Haskell.Interpreter as Hint
import System.Plugins.DynamicLoader (DynamicModule)
import qualified System.Plugins.DynamicLoader as DL
import System.Directory (doesFileExist, canonicalizePath)
import Data.Typeable

luaPlugin :: FilePath -> IO (Maybe Plugin)
luaPlugin script = do
  l <- Lua.newstate
  Lua.openlibs l
  return . Just . Plugin $ selectMove l
  where
    selectMove :: LuaState -> GameState -> IO Move
    selectMove l state = do
      Lua.loadfile l script
      Lua.call l 0 1
      Lua.pushinteger l . fromIntegral . health $ state
      Lua.pushinteger l . fromIntegral . damageMultiplier $ state
      Lua.pushinteger l . fromIntegral . concentration $ state
      Lua.pushboolean l . hasCoffee $ state
      Lua.call l 4 1
      move <- fromJust <$> Lua.peek l 1
      Lua.pop l 1
      return (toEnum move)

hintPlugin :: FilePath -> IO (Maybe Plugin)
hintPlugin script = return . Just . Plugin $ plugin
  where
    plugin :: GameState -> IO Move
    plugin st =
      fmap (either (error . show) id) . Hint.runInterpreter $
      do Hint.set
           [ Hint.languageExtensions Hint.:=
             [Hint.RecordWildCards, Hint.MultiWayIf]
           , Hint.searchPath Hint.:= [".", srcPath]]
         scriptFile <- Hint.liftIO (canonicalizePath script)
         Hint.loadModules [scriptFile]
         Hint.setTopLevelModules ["Main"]
         read <$> Hint.eval ("chooseMove " ++ Hint.parens (show st))

dllPlugin :: FilePath -> IO (Maybe Plugin)
dllPlugin pkgPath = do
  pkg <- DL.loadPackage "lambdamon-dll" (Just pkgPath) Nothing Nothing
  DL.resolveFunctions
  v :: Int <- DL.loadQualifiedFunction "Version.version"
  print v
  chooseMove <- DL.loadQualifiedFunction "Optimal.chooseMove"
  return (Just (Plugin (return . chooseMove)))