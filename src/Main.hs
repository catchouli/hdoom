{-# LANGUAGE OverloadedStrings #-}
module Main where

import Line
import Lens.Micro ((^.))
import SDL (($=), (^*), _x, _y)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (unless)
import Control.Monad.State (StateT(..), runStateT, liftIO, get, put)
import qualified SDL
import qualified Data.Text as T


-- | The target framerate in hz
targetFps :: Float
targetFps = 60.0


-- | The time step in seconds
timeStep :: Float
timeStep = 1.0 / targetFps


walls :: [Seg Float]
walls = [ Seg (SDL.V2 350 250) (SDL.V2 350 350)
        , Seg (SDL.V2 350 350) (SDL.V2 250 350)
        , Seg (SDL.V2 250 350) (SDL.V2 250 250)
        , Seg (SDL.V2 250 250) (SDL.V2 350 250)
        ]


-- | The game data
data Mode = Mode_2D | Mode_3D

data Game = Game { _renderMode :: Mode            -- ^ Whether to render the 2d overview or the 3d view
                 , _playerPos  :: SDL.V2 Float    -- ^ Player position
                 , _playerDir  :: Float           -- ^ Player direction in radians clockwise from vertical
                 }


-- | Start the game
main :: IO ()
main = do
  -- Create window
  (window, renderer) <- createWindow "DOOOOOOOM"

  -- Start main loop
  let initialGameState = Game Mode_2D (SDL.V2 300 300) 0
  let mainLoop = loop (window, renderer)
  runStateT mainLoop initialGameState

  -- Clean up
  destroyWindow (window, renderer)


-- | Main loop
loop :: (SDL.Window, SDL.Renderer) -> StateT Game IO ()
loop (window, renderer) = do
  -- Handle events
  events <- SDL.pollEvents
  let eventPayloads = map SDL.eventPayload events
  let quitting = not . null . filter (==SDL.QuitEvent) $ eventPayloads

  -- Update game
  updateGame timeStep

  -- Render game
  get >>= liftIO . renderGame renderer

  -- Continue if we want to
  unless quitting (loop (window, renderer))


-- | Create a window and rendering context
createWindow :: T.Text -> IO (SDL.Window, SDL.Renderer)
createWindow title = do
  -- Initialise graphics and other subsystems
  SDL.initializeAll

  -- Create window with opengl flag
  let windowDesc = SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL }
  window <- SDL.createWindow title windowDesc

  -- Create renderer with hardware acceleration and vsync enabled
  let rendererDesc = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  renderer <- SDL.createRenderer window (-1) rendererDesc

  return (window, renderer)


-- | Destroy the window and rendering context
destroyWindow :: (SDL.Window, SDL.Renderer) -> IO ()
destroyWindow (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window


-- | Update the game
updateGame :: Float -> StateT Game IO ()
updateGame timeStep = do
  -- Get keyboard state
  keyState <- liftIO $ SDL.getKeyboardState


  let leftPressed = keyState SDL.ScancodeLeft
  let rightPressed = keyState SDL.ScancodeRight
  let upPressed = keyState SDL.ScancodeUp
  let downPressed = keyState SDL.ScancodeDown

  -- Update player position
  let rotateSpeed = case () of () | leftPressed && not rightPressed -> -5
                                  | rightPressed && not leftPressed ->  5
                                  | otherwise                       ->  0
  let moveSpeed = case () of ()   | upPressed && not downPressed    ->  2
                                  | downPressed && not upPressed    -> -2
                                  | otherwise                       ->  0

  game@(Game m (SDL.V2 x y) r) <- get

  -- The player position and facing direction
  let playerPoint = _playerPos game
  let playerDir = angleToDir . _playerDir $ game

  -- Desired position after movement, will then be clipped by walls for collision
  let desiredPos = playerPoint + playerDir ^* moveSpeed

  -- Clip movement to a position by a wall
  let clipMovement desiredPos wall =
        let isect = intersects wall (Seg playerPoint desiredPos)
            dirSame = SDL.dot (playerDir ^* moveSpeed) (normal wall) < 0
        in if isect && dirSame
              then desiredPos + (normal wall ^* (distance wall desiredPos * 1.01))
              else desiredPos

  -- Clip movement to walls. extends walls out by 1 pixel to prevent
  -- movement through corners, which might cause problems in two sided walls.
  -- A simple solution for that would be to just generate an extra 45 degree wall
  -- when there's a corner
  -- Also, flip the walls around so that there's intersection testing with both
  -- sides (normally it's only tested clockwise to the wall)
  -- TODO: check the dot product with the walls normal and movement direction
  -- to prevent being pushed through a wall that intersects another wall
  let flipWall (Seg a b) = Seg b a
  let wallsToClip = walls ++ map (flipWall) walls
  let newPos = foldl clipMovement desiredPos (map (extend 5.0) wallsToClip)

  put $ Game m newPos (r+rotateSpeed*0.01)


-- | Render the game
renderGame :: SDL.Renderer -> Game -> IO ()
renderGame renderer game = do
  -- Clear buffer
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
  SDL.clear renderer

  render3DView renderer game
  render2DView renderer game

  --case (_renderMode game) of
  --     Mode_2D -> render2DView renderer game
  --     Mode_3D -> render3DView renderer game

  -- Present buffer
  SDL.present renderer


-- | Render the 2d view
render2DView :: SDL.Renderer -> Game -> IO ()
render2DView renderer game = do
  -- Draw player
  let playerPoint = _playerPos game
  let playerAngle = _playerDir game
  let playerDir = angleToDir playerAngle

  let drawPoint p = SDL.fillRect renderer $ Just (SDL.Rectangle (SDL.P . fmap round $ p - SDL.V2 3 3) (SDL.V2 7 7))

  SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
  drawPoint playerPoint
  SDL.drawLine renderer (SDL.P . fmap round $ playerPoint) (SDL.P $ fmap round (playerPoint + playerDir * 15))

  let drawWall (Seg a b) = SDL.drawLine renderer (SDL.P . fmap round $ a) (SDL.P . fmap round $ b)
  mapM_ drawWall walls

  -- Find the wall we're looking at
  let intersectionPoint = castRay playerPoint playerDir

  -- Draw intersection point
  SDL.rendererDrawColor renderer $= SDL.V4 0 255 0 255
  case castRay playerPoint playerDir of
    Just (SDL.V3 x y z) -> drawPoint (SDL.V2 x y)
    Nothing             -> return ()


-- | Render the 3d view
render3DView :: SDL.Renderer -> Game -> IO ()
render3DView renderer game = do
  let playerPoint = _playerPos game
  let playerAngle = _playerDir game
  SDL.rendererDrawColor renderer $= SDL.V4 255 0 255 255
  (flip mapM_) [0..799]
    (\w -> do let dx = (w - 400.0) / 800.0
                  dy = -1
                  dir = SDL.V2 (dx * sin (-playerAngle) - dy * cos (-playerAngle)) (dx * cos (-playerAngle) + dy * sin (-playerAngle))
                  (Just (SDL.V3 x y z)) = castRay playerPoint dir
              SDL.drawLine renderer (SDL.P $ SDL.V2 (round w) (round $ 300-(5000.0 / z))) (SDL.P $ SDL.V2 (round w) (round $ 300+(5000.0 / z)))
    )


-- | Cast a ray from a point in a direction and find a wall
castRay :: SDL.V2 Float -> SDL.V2 Float -> Maybe (SDL.V3 Float)
castRay origin direction =
  let playerRay = Seg origin (origin + direction)
      findPoints = map (intersectionPoints playerRay)
      filterPoints = filter (\(a, b) -> (a >= 0 && b >= 0 && b <= 1))
      remapPoints = map (\(a,_) -> let (V2 x y) = origin + direction ^* a in (SDL.V3 x y a))
      points = remapPoints . filterPoints . findPoints $ walls
  in listToMaybe points


-- | Extend a line segment
extend :: (Num a, Floating a, SDL.Epsilon a) => a -> Seg a -> Seg a
extend dist (Seg a b) = let dir = SDL.normalize (b - a)
                        in Seg (a - dir ^* dist) (b + dir ^* dist)


-- | Angle to direction
angleToDir :: Floating a => a -> SDL.V2 a
angleToDir angle = SDL.V2 (cos angle) (sin angle)
