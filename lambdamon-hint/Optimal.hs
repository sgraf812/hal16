import Types

chooseMove st@GameState{..} =
  if | concentration < 2 && hasCoffee -> Coffee
     | health > 200 && damageMultiplier < 2 -> Rename
     | health <= 50 -> Capture
     | otherwise -> Reduce
