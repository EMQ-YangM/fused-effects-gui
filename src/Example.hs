{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Example where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Optics (use, (%=), (.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import Data.Kind
import Data.Text (Text, pack)
import Data.Word (Word8)
import MyLib
import Optics ((%))
-- import Optics
import SDL
import SDL.Font as SF
import SDL.Framerate
import SDL.Primitive
import Widget

data Body = Body

bodyWidget' :: Widget Body
bodyWidget' =
  Widget
    { _width = 100,
      _heigh = 100,
      _model = Body,
      _backgroundColor = 255,
      _frontColor = 90,
      _visible = True,
      _path = [],
      _children = []
    }

instance WidgetRender Body where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    clear renderer

instance WidgetHandler Body where
  handler e a = do
    case eventPayload e of
      (MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos)) -> do
        cs <- use $ bodyWidget % children'
        let newmw = modelWidget [length cs]
        bodyWidget % children' %= ((fmap fromIntegral pos, SomeWidget newmw) :)
      _ -> return ()
    return a

makeUIState :: UIState
makeUIState =
  UIState
    { _bodyWidget = SomeWidget bodyWidget',
      _focus = []
    }

newtype Model = Model Int deriving (Show)

modelWidget :: [Int] -> Widget Model
modelWidget path =
  Widget
    { _width = 100,
      _heigh = 100,
      _model = Model 1,
      _backgroundColor = 30,
      _frontColor = V4 0 0 0 255,
      _visible = True,
      _path = path,
      _children = []
    }

instance WidgetRender Model where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    font <- asks _font
    liftIO $ do
      renderFont font renderer (pack $ show _model ++ show _path) (fmap fromIntegral bp) _frontColor

instance WidgetHandler Model where
  handler e a = do
    return a

initGUI :: IO (Renderer, Font)
initGUI = do
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
  SDL.Framerate.set fm 30
  font <- load "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf" 20
  return (renderer, font)

appLoop1 :: forall sig m. (UI sig m, MonadIO m) => m ()
appLoop1 = go
  where
    go = do
      e <- liftIO waitEvent
      SomeWidget bodyW <- gets _bodyWidget
      handler e Body

      render 0 bodyW

      renderer <- asks _renderer
      present renderer
      go

main :: IO ()
main = do
  (r, f) <- initGUI
  runReader (UIEnv r f) $ runState makeUIState appLoop1
  return ()