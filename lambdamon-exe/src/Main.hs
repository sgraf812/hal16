{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Brick
import Brick.AttrMap
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.ProgressBar
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad (forever, replicateM, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (chr, ord)
import Data.Default
import Data.Maybe
import Data.Tuple.Extra
import Debug.Trace (traceShowId)
import Graphics.Vty
import Lens.Micro.Platform
import Numeric.Extra
import System.Random (randomIO)
import System.Environment (getArgs)
import Types (Plugin(..), GameState(GameState), Move(..))
import PluginLoaders
import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock

data ExprStats = ExprStats
  { _curHealth :: Int
  , _maxHealth :: Int
  , _damageMultiplier :: Int
  , _varChar :: Char
  } deriving (Eq, Ord, Show)

makeLenses ''ExprStats

descr :: Move -> String
descr Reduce = "Beta Reduce"
descr Rename = "Rename"
descr Capture = "Capture"
descr Coffee = "Drink Coffee"

data Phase
  = ChooseMove
  | Animation Move
              Int
  | EndTurn Move
            Bool
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _concentration :: Int
  , _expr :: ExprStats
  , _score :: Int
  , _nextMove :: Dialog Move
  , _phase :: Phase
  , _remainingCoffee :: Int
  }

makeLenses ''AppState

reductionDmg :: AppState -> Int
reductionDmg s = s ^. expr . damageMultiplier * 50

newDialog :: Dialog Move
newDialog = dialog Nothing (Just (0, map (descr &&& id) moves)) (fst dimensions)

initialState :: AppState
initialState =
  AppState
  { _concentration = 10
  , _score = 0
  , _expr = undefined
  , _nextMove = newDialog
  , _phase = ChooseMove
  , _remainingCoffee = 1
  }

moves :: [Move]
moves = enumFrom minBound

data MyEvent
  = Tick
  | PluginChoseMove Move

dimensions :: (Int, Int)
dimensions = (72, 30)

draw :: AppState -> [Widget a]
draw s = [conc, health, ball, trainer, variable, botDialog]
  where
    botDialog =
      case s ^. phase of
        ChooseMove -> moveDialog
        Animation _ _ -> textDialog " "
        EndTurn Reduce False ->
          textDialog $ "Reduction brought the variables health down by " ++
          show (reductionDmg s)
        EndTurn Reduce True ->
          textDialog
            "Great! Reduction got rid of the variable! Let's tame another one."
        EndTurn Rename _ ->
          textDialog $ "Renaming raised the damage multiplier to " ++
          show (s ^. expr . damageMultiplier)
        EndTurn Coffee _ ->
          textDialog
            "Ahh... the coffee got you psyched again. Who needs to sleep anyway?"
        EndTurn Capture False ->
          textDialog "Capturing failed. The variable broke free!"
        EndTurn Capture True ->
          textDialog "Yeah! Captured the variable! Let's tame another one."
    layoutDims = centerLayer . hLimit (fst dimensions) . vLimit (snd dimensions)
    layoutDialog = layoutDims . marginTop Max
    moveDialog =
      layoutDialog . renderDialog (s ^. nextMove) . hCenter . padAll 1 . str $
      "Choose a move:"
    layoutGraphics = layoutDims . marginBottom (Pad 6)
    conc =
      layoutGraphics . marginTop Max . marginLeft Max .
      (str ("score: " ++ show (s ^. score)) <=>) .
      (str ("remaining coffee: " ++ show (s ^. remainingCoffee)) <=>) .
      (str ("concentration: " ++ show (s ^. concentration) ++ "/10") <=>) .
      hLimit 20 $
      progressBar Nothing ((s ^. concentration . to intToFloat) / 10.0)
    health =
      layoutGraphics . marginBottom Max . marginRight Max .
      (<=> str
             ("health: " ++ show (s ^. expr . curHealth) ++ "/" ++
              show (s ^. expr . maxHealth))) .
      hLimit 20 $
      progressBar
        Nothing
        (s ^. expr . curHealth . to intToFloat / s ^. expr . maxHealth . to intToFloat)
    trainer =
      layoutGraphics . marginTop Max . marginRight Max . marginLeft (Pad 2) $ trainerSprite
    ball =
      case s ^. phase of
        Animation Capture n ->
          let interp =
                min 1 . (* 1.5) $ (intToDouble n / intToDouble animationFrames)
              start = (24, 20)
              end = (56, 5)
              position =
                ( round $ interp * fst end + (1 - interp) * fst start
                , round $ interp * snd end + (1 - interp) * snd start)
          in layoutGraphics . marginRight Max . marginBottom Max .
             marginLeft (Pad (fst position)) .
             marginTop (Pad (snd position)) $
             lambdaSprite (s ^. expr . varChar)
        _ -> emptyWidget
    animateVar (Animation Rename 0) c = c
    animateVar (Animation Rename 1) c = nextVar c
    animateVar (Animation Rename 2) c = c
    animateVar (Animation Rename 3) c = nextVar c
    animateVar (Animation Rename 4) c = c
    animateVar (Animation Rename 5) c = nextVar c
    animateVar (Animation Rename 6) c = nextVar c
    animateVar (Animation Rename 7) c = c
    animateVar (Animation Rename 8) c = nextVar c
    animateVar (Animation Rename 9) c = nextVar c
    animateVar (Animation Rename _) c = nextVar c
    animateVar _ c = c
    variable =
      layoutGraphics . marginBottom Max . marginLeft Max . marginRight (Pad 4) .
      marginTop (Pad 2) .
      varSprite .
      animateVar (s ^. phase) $
      (s ^. expr . varChar)
    textDialog =
      layoutDialog . renderDialog (dialog Nothing Nothing (fst dimensions)) . hCenter .
      padBottom (Pad 1) .
      padAll 1 .
      str

topLeftCorner :: Widget a -> Widget a
topLeftCorner child =
  Widget Greedy Greedy $
  do ctx <- getContext
     childResult <- render child
     return $ childResult & imageL %~
       resize (ctx ^. availWidthL) (ctx ^. availHeightL)

marginLeft :: Padding -> Widget a -> Widget a
marginLeft padding p =
  let (pad, sz) =
        case padding of
          Max -> (id, Greedy)
          Pad i -> (const i, hSize p)
  in Widget sz (vSize p) $
     do (availWidth, lim) <- calculateLim padding availWidthL
        result <- render $ hLimit lim p
        let offsetX = pad (availWidth - result ^. imageL . to imageWidth)
            offset = Location (offsetX, 0)
        return $ addResultOffset offset $ result & imageL %~ translateX offsetX

marginRight :: Padding -> Widget a -> Widget a
marginRight padding p =
  let (pad, sz) =
        case padding of
          Max -> (id, Greedy)
          Pad i -> (const i, hSize p)
  in Widget sz (vSize p) $
     do (availWidth, lim) <- calculateLim padding availWidthL
        result <- render $ hLimit lim p
        let childWidth = result ^. imageL . to imageWidth
            width = childWidth + pad (availWidth - childWidth)
        return $ result & imageL %~ resizeWidth width

marginTop :: Padding -> Widget a -> Widget a
marginTop padding p =
  let (pad, sz) =
        case padding of
          Max -> (id, Greedy)
          Pad i -> (const i, vSize p)
  in Widget (hSize p) sz $
     do (availHeight, lim) <- calculateLim padding availHeightL
        result <- render $ vLimit lim p
        let offsetY = pad (availHeight - result ^. imageL . to imageHeight)
            offset = Location (0, offsetY)
        return $ addResultOffset offset $ result & imageL %~ translateY offsetY

marginBottom :: Padding -> Widget a -> Widget a
marginBottom padding p =
  let (pad, sz) =
        case padding of
          Max -> (id, Greedy)
          Pad i -> (const i, vSize p)
  in Widget (hSize p) sz $
     do (availHeight, lim) <- calculateLim padding availHeightL
        result <- render $ vLimit lim p
        let childHeight = result ^. imageL . to imageHeight
            height = childHeight + pad (availHeight - childHeight)
        return $ result & imageL %~ resizeHeight height

calculateLim :: Padding -> Lens' Context Int -> RenderM n (Int, Int)
calculateLim padding widthOrHeight = do
  c <- getContext
  let lim =
        case padding of
          Max -> c ^. widthOrHeight
          Pad i -> c ^. widthOrHeight - i
  return (c ^. widthOrHeight, lim)

bottomRightCorner :: Widget a -> Widget a
bottomRightCorner child =
  Widget Greedy Greedy $
  do ctx <- getContext
     childResult <- render child
     let childHeight = childResult ^. imageL . to imageHeight
         childWidth = childResult ^. imageL . to imageWidth
         offsetX = ctx ^. availWidthL - childWidth
         offsetY = ctx ^. availHeightL - childHeight
         offset = Location (offsetX, offsetY)
     return $ addResultOffset offset $ childResult & imageL %~
       translate offsetX offsetY

animationFrames :: Int
animationFrames = 10

gameState :: AppState -> GameState
gameState s =
  GameState
    (s ^. expr . curHealth)
    (s ^. expr . damageMultiplier)
    (s ^. concentration)
    (s ^. remainingCoffee . to (> 0))

handleEvent :: (GameState -> IO ())
            -> AppState
            -> BrickEvent n MyEvent
            -> EventM () (Next AppState)
handleEvent _ s (VtyEvent (EvKey KEsc _)) = halt s
handleEvent askPlugin s (VtyEvent (EvKey KEnter _)) =
  case s ^. phase of
    Animation _ _ -> continue s
    EndTurn mv _ ->
      if s ^. concentration <= 1
        then halt s
        else continue $ s & phase .~ ChooseMove & concentration %~ subtract 1 &
             case mv of
               Coffee -> remainingCoffee %~ subtract 1
               _ -> id
    ChooseMove -> handleEvent askPlugin s (AppEvent (PluginChoseMove move))
      where Just move = s ^. nextMove . to dialogSelection
handleEvent _ s (VtyEvent ev) =
  case s ^. phase of
    ChooseMove -> continue =<< handleEventLensed s nextMove handleDialogEvent ev
    _ -> continue s
handleEvent _ s (AppEvent (PluginChoseMove move)) =
  continue $ s &
  if move == Coffee && s ^. remainingCoffee <= 0
    then id
    else phase .~ Animation move 0
handleEvent askPlugin s (AppEvent Tick) =
  case s ^. phase of
    Animation Coffee n ->
      if s ^. concentration == 10
        then continue $ s & phase .~ EndTurn Coffee True
        else continue $ s & concentration %~ (+ 1) & phase .~
             Animation Coffee (n + 1)
    Animation Capture n ->
      if n >= animationFrames
        then do
          newExpr <- newExprWhenCaptured s
          continue $ s & maybe id (\e -> (expr .~ e) . (score %~ (+ 2))) newExpr &
            phase .~
            EndTurn Capture (isJust newExpr)
        else continue (s & phase .~ Animation Capture (n + 1))
    Animation Reduce n -> do
      let dead = s ^. expr . curHealth <= 0
      if n >= animationFrames || dead
        then do
          let s' = s & phase .~ EndTurn Reduce dead
          if dead
            then do
              e <- newExpr s'
              continue $ s' & score %~ (+ 1) & expr .~ e
            else continue s'
        else continue $ s & calculateReduction n & phase .~
             Animation Reduce (n + 1)
    Animation Rename n ->
      if n >= animationFrames
        then continue $ s & expr . damageMultiplier %~ (+ 1) & expr . varChar %~
             nextVar &
             phase .~
             EndTurn Rename True
        else continue (s & phase .~ Animation Rename (n + 1))
    ChooseMove -> do
      liftIO . askPlugin . gameState $ s
      continue s
    _ -> continue s

calculateReduction :: Int -> AppState -> AppState
calculateReduction n s = s & expr . curHealth %~ hurt
  where
    totalDmg :: Int
    totalDmg = s ^. expr . damageMultiplier * 50
    hurt :: Int -> Int
    hurt hp =
      max
        0
        (hp + (n * totalDmg `div` animationFrames) -
         ((n + 1) * totalDmg `div` animationFrames))

newExpr :: AppState -> EventM a ExprStats
newExpr s = do
  health <- (+ 180) . (`mod` 51) <$> liftIO randomIO
  c <- chr . (ord 'x' +) . (`mod` 3) <$> liftIO randomIO
  return $ ExprStats health health 1 c

newExprWhenCaptured :: AppState -> EventM a (Maybe ExprStats)
newExprWhenCaptured s = do
  rolls <- replicateM 3 (liftIO randomIO)
  let success roll = roll `mod` s ^. expr . maxHealth + 1 > s ^. expr . curHealth
  if all success rolls
    then Just <$> newExpr s
    else return Nothing

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (dialogAttr, white `on` blue)
    , (buttonAttr, black `on` white)
    , (buttonSelectedAttr, bg yellow)
    , (progressCompleteAttr, bg green)
    , (progressIncompleteAttr, bg red)]

app :: (GameState -> IO ()) -> App AppState MyEvent ()
app askPlugin =
  App
  { appDraw = draw
  , appHandleEvent = handleEvent askPlugin
  , appStartEvent = \s -> (\e -> (expr .~ e) s) <$> newExpr s
  , appAttrMap = const theMap
  , appChooseCursor = showFirstCursor
  }

determinePlugin :: IO (Maybe Plugin)
determinePlugin = do
  args <- getArgs
  if length args < 2
    then return Nothing
    else case head args of
           "--lua" -> luaPlugin (args !! 1)
           "--hint" -> hintPlugin (args !! 1)
           "--dynamic-loader" -> dynamicLoaderPlugin (args !! 1)
           "--plugins" -> pluginsPlugin (args !! 1)
           _ -> return Nothing

main :: IO ()
main = do
  events <- newBChan 1000
  choosingMove <- Lock.new
  plugin <- determinePlugin
  forkIO $ forever $
    do threadDelay 100000
       Lock.with choosingMove (writeBChan events Tick)
  let askPlugin st =
        case plugin of
          Nothing -> return ()
          Just p ->
            Lock.with choosingMove $
            do move <- runPlugin p st
               writeBChan events (PluginChoseMove move)
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  s <- customMain initialVty buildVty (Just events) (app askPlugin) initialState
  putStrLn $ "Zzzz... You fell asleep. Your score was " ++ show (s ^. score)
  return ()

trainerSprite :: Widget n
trainerSprite =
  vBox
    [ l 12 "%%*../#&"
    , l 8 "&%//########(%"
    , l 6 "%/*****/(######(("
    , l 5 "#*********((((###/&"
    , l 4 "%***,******//((((%%//(%"
    , l 5 "#,.  .********(%%#/(&"
    , l 5 "%.,,,,,,, ,.  (#/*"
    , l 6 ",        #%,#@@#%"
    , l 6 "&/       /%@@@@%#"
    , l 8 "(,  .*###%%%#"
    , l 3 "&//((((/*,,,,./%"
    , l 2 "//##((((((//,.*%%#%"
    , l 0 "&/****/////*.,*,/@@&##"
    , l 0 ".*..(%/.***.**,(####@#(@@#(#%"
    , l 0 " .******,,..**,(#/*#%@@(/&@@#"]
  where
    l margin string = marginLeft (Pad margin) $ str string

xSprite :: Widget n
xSprite =
  vBox
    [ l 0 "`7M'   `MF'"
    , l 2 "`VA ,V'"
    , l 4 "XMX"
    , l 2 ",V' VA."
    , l 0 ".AM.   .MA."]
  where
    l margin string = marginLeft (Pad margin) $ str string

ySprite :: Widget n
ySprite =
  vBox
    [ l 0 "`7M'   `MF'"
    , l 2 "VA   ,V"
    , l 3 "VA ,V"
    , l 4 "VVV"
    , l 4 ",V"
    , l 3 ",V"
    , l 0 "00b\""]
  where
    l margin string = marginLeft (Pad margin) $ str string

zSprite :: Widget n
zSprite =
  vBox [l 0 "M\"\"\"MMV", l 0 "'  AMV", l 2 "AMV", l 1 "AMV  ,", l 0 "AMMmmmM"]
  where
    l margin string = marginLeft (Pad margin) $ str string

varSprite :: Char -> Widget n
varSprite 'x' = xSprite
varSprite 'y' = ySprite
varSprite 'z' = zSprite
varSprite c = error $ "No sprite for var " ++ show c

nextVar :: Char -> Char
nextVar 'x' = 'y'
nextVar 'y' = 'z'
nextVar 'z' = 'x'
nextVar c = error $ "nextVar " ++ show c ++ " not supported"

lambdaSprite :: Char -> Widget n
lambdaSprite c = vBox [l 1 "/‾‾‾\\", l 0 ("( λ" ++ [c] ++ ". )"), l 1 "\\___/"]
  where
    l margin string = marginLeft (Pad margin) $ str string
