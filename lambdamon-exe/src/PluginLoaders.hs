module PluginLoaders where

import Types
import Lens.Micro.Platform
import Data.Maybe (fromJust)
import Scripting.Lua (LuaState)
import qualified Scripting.Lua as Lua

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
      Lua.pushinteger l (state ^. health . to fromIntegral)
      Lua.pushinteger l (state ^. damageMultiplier . to fromIntegral)
      Lua.pushinteger l (state ^. concentration . to fromIntegral)
      Lua.pushboolean l (state ^. hasCoffee)
      Lua.call l 4 1
      move <- fromJust <$> Lua.peek l 1
      Lua.pop l 1
      return (toEnum move)
