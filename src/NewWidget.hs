{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Data.Functor (Functor)
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

data Widget model = forall a.
  Widget
  { rect :: Rectangle Int,
    model :: (IORef a, Lens' a model),
    children :: [SomeWidget]
  }

instance Show model => Show (Widget model) where
  show (Widget rec (ref, m) children) =
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
        children = []
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
              ]
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

data Zipper a = Zipper
  { _zval :: a,
    _left :: [Zipper a],
    _center :: Zipper a,
    _right :: [Zipper a]
  }

makeLenses ''Zipper

-- instance Show a => Show (Zipper a) where
--   show (Zipper ls c rs) = show ls ++ " [" ++ show c ++ "] " ++ show rs

-- leftMove :: Zipper a -> Zipper a
-- leftMove (Zipper (l : ls) c rs) = Zipper ls l (c : rs)
-- leftMove a = a

-- rightMove :: Zipper a -> Zipper a
-- rightMove (Zipper ls c (r : rs)) = Zipper (c : ls) r rs
-- rightMove a = a

-- >>> tz
-- >>> leftMove tz
-- >>> rightMove $ rightMove tz
-- [3,2,1] [4] [5,6,7]
-- [2,1] [3] [4,5,6,7]
-- [5,4,3,2,1] [6] [7]
-- tz :: Zipper Int
-- tz = Zipper [3, 2, 1] 4 [5, 6, 7]

-- type ZZ a = Zipper (Zipper a)

-- >>> tzz
-- >>> tzz & center %~ leftMove
-- >>> leftMove  tzz
-- [[3,2,1] [4] [5,6,7]] [[3,2,1] [4] [5,6,7]] [[3,2,1] [4] [5,6,7]]
-- [[3,2,1] [4] [5,6,7]] [[2,1] [3] [4,5,6,7]] [[3,2,1] [4] [5,6,7]]
-- [] [[3,2,1] [4] [5,6,7]] [[3,2,1] [4] [5,6,7],[3,2,1] [4] [5,6,7]]
-- tzz :: ZZ Int
-- tzz = Zipper [tz] tz [tz]

-- tk = tzz & center %~ leftMove

-- data Fix f a
--   = Fix (f (Fix f a))
--   | End a
--   deriving (Functor)

-- instance (Foldable f, Show a) => Show (Fix f a) where
--   show (End a) = "End " ++ show a
--   show (Fix v) = "Fix " ++ concatMap show v

-- type ZF a = Fix Zipper a

-- -- >>> tzf
-- -- Fix Fix End 0End 10End 100End 1000End 1End 2Fix End 3
-- tzf :: ZF Int
-- tzf =
--   Fix
--     ( Zipper
--         [Fix (Zipper [End 0, End 10, End 100, End 1000] (End 1) [])]
--         (End 2)
--         [Fix (Zipper [] (End 3) [])]
--     )
