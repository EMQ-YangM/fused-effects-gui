{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import SDL
import SDL.Font as SF
import SDL.Framerate
import SDL.Primitive

main :: IO ()
main = do
  initializeAll
  SF.initialize
  window <-
    createWindow
      "resize"
      WindowConfig
        { windowBorder = True,
          windowHighDPI = False,
          windowInputGrabbed = False,
          windowMode = Windowed,
          windowGraphicsContext = NoGraphicsContext,
          windowPosition = Wherever,
          windowResizable = True,
          windowInitialSize = V2 800 600,
          windowVisible = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  addEventWatch $ \ev ->
    case eventPayload ev of
      WindowSizeChangedEvent sizeChangeData ->
        putStrLn $ "eventWatch windowSizeChanged: " ++ show sizeChangeData
      _ -> return ()
  fm <- manager
  set fm 30
  font <- load "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf" 20
  appLoop renderer font

blue = V4 0 0 255 255

appLoop :: Renderer -> Font -> IO ()
appLoop renderer font = do
  waitEvent >>= go
  where
    go :: Event -> IO ()
    go ev = do
      rendererDrawColor renderer $= V4 255 255 255 255
      clear renderer
      rendererDrawColor renderer $= V4 255 0 0 255
      -- drawLine renderer (P $ V2 0 0) (P $ V2 400 300)
      -- thickLine renderer 0 300 10 blue
      -- fillCircle renderer 300 20 blue
      let val = "wellcome haskell!!!!!!"
      renderFont font renderer val (P 30) blue
      present renderer
      case eventPayload ev of
        WindowSizeChangedEvent sizeChangeData -> do
          putStrLn $ "waitEvent windowSizeChanged: " ++ show sizeChangeData
          waitEvent >>= go
        KeyboardEvent keyboardEvent
          | keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ ->
            return ()
        MouseMotionEvent mv@(MouseMotionEventData _ _ _ p _) -> do
          let val = pack (show p)
          renderFont font renderer val (fmap fromIntegral p) blue
          present renderer
          waitEvent >>= go
        _ -> waitEvent >>= go

renderFont :: Font -> Renderer -> Text -> Point V2 CInt -> SF.Color -> IO ()
renderFont font renderer val pos color = do
  (w, h) <- size font val
  sur <- blended font color val
  texture <- createTextureFromSurface renderer sur
  copy
    renderer
    texture
    Nothing
    ( Just $
        Rectangle
          pos
          (V2 (fromIntegral w) (fromIntegral h))
    )
  freeSurface sur
  destroyTexture texture