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
  { _bodyWidget :: SomeWidget,
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

children' :: Lens' SomeWidget [(BasePositon, SomeWidget)]
children' = lens (\(SomeWidget w) -> w ^. children) (\(SomeWidget w) a -> SomeWidget (w & children .~ a))