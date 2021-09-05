{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NewWidget where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable (forM_)
import Data.IORef
import Data.Kind
import Data.Text (Text, pack)
import Data.Typeable
import Data.Word (Word8)
import MyLib
import Optics
import SDL
import SDL.Font as SF
import SDL.Framerate
import SDL.Primitive
import System.IO.Unsafe

{-
Widget m

-}

data UIEnv = UIEnv
  { _renderer :: Renderer,
    _font :: Font,
    _manager :: Manager,
    _getUserEvent :: Event -> IO (Maybe Dynamic)
  }

data UIState = UIState
  { _bodyWidget :: Int, -- SomeWidget,
    _focus :: [Int]
  }

type UI sig m = Has (Reader UIEnv :+: State UIState) sig m

data Widget model = forall parent a.
  Widget
  { rect :: Rectangle Int,
    model :: (IORef a, Lens' a model),
    children :: [SomeWidget],
    parent :: Widget parent
  }

instance Show model => Show (Widget model) where
  show (Widget rec (ref, m) children _) =
    show rec ++ " \n"
      ++ unsafePerformIO
        ( do
            v <- readIORef ref
            return (show $ v ^. m)
        )
      ++ " \n"
      ++ show children

data SomeWidget = forall model. Show model => SomeWidget (Widget model)

instance Show SomeWidget where
  show (SomeWidget a) = show a

data A = A
  { _va :: Int,
    _vb :: String
  }
  deriving (Show)

makeLenses ''A

da = A 10 "nice"

-- >>> show $ da & id' %~ (& va .~ 10000)
-- "A {_va = 10000, _vb = \"nice\"}"
id' :: Lens' a a
id' = lens id (\a b -> b)

widgetCreate :: Show b => IORef a -> Lens' a b -> SomeWidget
widgetCreate ref len =
  SomeWidget $
    Widget
      { rect = Rectangle 30 40,
        model = (ref, len),
        children = [],
        parent = undefined
      }

t :: IO ()
t = do
  refA <- newIORef (A 10 "nice")
  let w =
        Widget
          { rect = Rectangle 30 40,
            model = (refA, id'),
            children =
              [ widgetCreate refA va,
                widgetCreate refA vb
              ],
            parent = undefined
          }
  --   nw = w {parent = undefined}
  print w


data T a = T
  { val :: a,
    tl :: [T a]
  }

data C a = C
  { pv :: a,
    pl :: [T a],
    cv :: T a,
    pr :: [T a]
  }