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

module Widget where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import Data.Kind
import Data.Text (Text, pack)
import Data.Word (Word8)
import MyLib
import Optics
import SDL
import SDL.Font as SF
import SDL.Framerate
import SDL.Primitive

{-
Widget m

-}

data UIEnv = UIEnv
  { _renderer :: Renderer,
    _font :: Font
  }

data UIState = UIState
  { _bodyWidges :: SomeWidget,
    _focus :: [Int]
  }

type UI sig m = Has (Reader UIEnv :+: State UIState) sig m --- very cool!!!!!!

--
--
--
--  WidgetMoveChild in event
--

-- type Event' = Int

class WidgetHandler a where
  handler :: (UI sig m, MonadIO m) => Event -> a -> m a

--   handler :: Event' -> Widget sig m a -> m ()

type BasePositon = Point V2 Int

class WidgetRender a where
  renderSelf :: (UI sig m, MonadIO m) => BasePositon -> Widget a -> m ()

  render :: (UI sig m, MonadIO m) => BasePositon -> Widget a -> m ()
  render bp w = do
    when (_visible w) $ do
      renderer <- asks _renderer
      liftIO $ do
        rendererDrawColor renderer $= _backgroundColor w
        drawRect renderer (Just $ Rectangle (fmap fromIntegral bp) (V2 (fromIntegral (_width w)) (fromIntegral (_height w))))
      renderSelf bp w
      forM_ (_children w) $ \(bpc, SomeWidget w) -> render (bp + bpc) w

data Widget model = Widget
  { _width :: Int,
    _height :: Int,
    _model :: model,
    _backgroundColor :: V4 Word8,
    _frontColor :: V4 Word8,
    _visible :: Bool,
    _path :: [Int],
    _children :: [(BasePositon, SomeWidget)]
  }

data SomeWidget
  = forall a.
    ( WidgetRender a,
      WidgetHandler a
    ) =>
    SomeWidget (Widget a)

makeLenses ''Widget
makeLenses ''UIState
makeLenses ''UIEnv

updatePos :: Int -> SomeWidget -> SomeWidget
updatePos i (SomeWidget w) = SomeWidget (w {_width = i})

data Body = Body

bodyWidget :: Widget Body
bodyWidget =
  Widget
    { _width = 100,
      _height = 100,
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

instance WidgetHandler w where
  handler e a = return a

makeUIState :: UIState
makeUIState =
  UIState
    { _bodyWidges = SomeWidget bodyWidget,
      _focus = []
    }

newtype Model = Model Int deriving (Show)

modelWidget :: [Int] -> Widget Model
modelWidget path =
  Widget
    { _width = 100,
      _height = 100,
      _model = Model 1,
      _backgroundColor = 30,
      _frontColor = 90,
      _visible = True,
      _path = path,
      _children = []
    }

instance WidgetRender Model where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    font <- asks _font
    liftIO $ do
      rendererDrawColor renderer $= _frontColor
      renderFont font renderer (pack $ show _model) (fmap fromIntegral bp)

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
      SomeWidget bodyW <- gets _bodyWidges
      handler e Body

      render 0 bodyW
      go

tmain :: IO ()
tmain = do
  (r, f) <- initGUI
  runReader (UIEnv r f) $ runState makeUIState appLoop1
  undefined