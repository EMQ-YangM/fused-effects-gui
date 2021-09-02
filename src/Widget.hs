{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Widget where

import Control.Algebra
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
import SDL.Font

{-
Widget m

-}

data UIEnv = UIEnv
  { _renderer :: Renderer,
    _font :: Font
  }

data UIState sig m = UIState
  { _bodyWidges :: [SomeWidget sig m],
    _focus :: [Int]
  }

type UI sig m = Has (Reader UIEnv :+: State (UIState sig m)) sig m --- very cool!!!!!!

--
--
--
--  WidgetMoveChild in event
--

class WidgetHandler

type BasePositon = Point V2 Int

class (UI sig m, MonadIO m) => WidgetRender sig m a where
  renderSelf :: BasePositon -> Widget sig m a -> m ()

  render :: BasePositon -> Widget sig m a -> m ()
  render bp w = do
    when (_visible w) $ do
      renderer <- asks _renderer
      liftIO $ do
        rendererDrawColor renderer $= _backgroundColor w
        drawRect renderer (Just $ Rectangle (fmap fromIntegral bp) (V2 (fromIntegral (_width w)) (fromIntegral (_height w))))
      renderSelf bp w
      forM_ (_children w) $ \(bpc, SomeWidget w) -> render (bp + bpc) w

data Widget sig m model = Widget
  { _width :: Int,
    _height :: Int,
    _model :: model,
    _backgroundColor :: V4 Word8,
    _frontColor :: V4 Word8,
    _visible :: Bool,
    _children :: [(BasePositon, SomeWidget sig m)]
  }

data SomeWidget sig m = forall a. (WidgetRender sig m a) => SomeWidget (Widget sig m a)

makeLenses ''Widget
makeLenses ''UIState
makeLenses ''UIEnv

updatePos :: Int -> SomeWidget sig m -> SomeWidget sig m
updatePos i (SomeWidget w) = SomeWidget (w {_width = i})

newtype Model = Model Int deriving (Show)

modelWidget :: Widget sig m Model
modelWidget =
  Widget
    { _width = 100,
      _height = 100,
      _model = Model 1,
      _backgroundColor = 30,
      _frontColor = 90,
      _visible = True,
      _children = []
    }

instance (UI sig m, MonadIO m) => WidgetRender sig m Model where
  renderSelf bp w@Widget {..} = do
    renderer <- asks _renderer
    font <- asks _font
    liftIO $ do
      rendererDrawColor renderer $= _frontColor
      renderFont font renderer (pack $ show _model) (fmap fromIntegral bp)
