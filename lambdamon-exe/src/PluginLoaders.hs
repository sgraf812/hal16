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
import System.Plugins.Load (LoadStatus(..))
import qualified System.Plugins.Load as Plugins
import System.Directory (doesFileExist, canonicalizePath)
import Data.Typeable

luaPlugin :: FilePath -> IO (Maybe Plugin)
luaPlugin script = do
  l <- Lua.newstate
  return . Just . Plugin $ selectMove l
  where
    selectMove :: LuaState -> GameState -> IO Move
    selectMove l state = do
      -- Load the script contents on the stack
      Lua.loadfile l script
      -- Execute the script file (it's a thunk semantically)
      Lua.call l 0 {-arguments-} 1 {-result on stack-}
      -- The result (which now lies on top of the stack) should be an anonymous
      -- function, taking 4 arguments and returning an integer in [0..3] for
      -- the chosen move
      Lua.pushinteger l . fromIntegral . health $ state
      Lua.pushinteger l . fromIntegral . damageMultiplier $ state
      Lua.pushinteger l . fromIntegral . concentration $ state
      Lua.pushboolean l . hasCoffee $ state
      Lua.call l 4 1
      -- 1 is the index of the topmost stack element, which is the return val.
      move :: Int <- fromJust <$> Lua.peek l 1
      Lua.pop l 1
      return (toEnum move)

hintPlugin :: FilePath -> IO (Maybe Plugin)
hintPlugin script = return . Just . Plugin $ plugin
  where
    plugin :: GameState -> IO Move
    plugin st =
      fmap (either (error . show) id) . Hint.runInterpreter $
      do scriptFile <- Hint.liftIO (canonicalizePath script)
         Hint.loadModules [scriptFile]
         Hint.setTopLevelModules ["Plugin"]
         read <$> Hint.eval ("chooseMove " ++ Hint.parens (show st))

dynamicLoaderPlugin :: FilePath -> IO (Maybe Plugin)
dynamicLoaderPlugin pluginPath = do
  ar <- DL.loadArchiveFromPath pluginPath
  DL.resolveFunctions
  chooseMove <- DL.loadQualifiedFunction "Optimal.chooseMove"
  return (Just (Plugin (return . chooseMove)))

pluginsPlugin :: FilePath -> IO (Maybe Plugin)
pluginsPlugin pluginPath = do
  status :: LoadStatus Int <-
    Plugins.pdynload "Version.o" [pluginPath] [] "Prelude.Int" "version"
  case status of
    LoadSuccess module_ v -> print v
    LoadFailure errors -> putStrLn (unlines errors)
  return Nothing
