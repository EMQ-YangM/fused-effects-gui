{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module LZipper where

import Data.IORef
import Data.Tree
import Data.Tree.Zipper
import GHC.IOArray (newIOArray)
import Optics

t :: Tree Int
t =
  Node
    10
    [ Node 9 [],
      Node 8 [Node 81 [Node 82 []], Node 88 []],
      Node 7 []
    ]

dt = putStrLn $ drawTree $ fmap show t

-- >>> children tp
-- Loc {_content = E, _before = [], _after = [Node {rootLabel = 9, subForest = []},Node {rootLabel = 8, subForest = [Node {rootLabel = 81, subForest = [Node {rootLabel = 82, subForest = []}]},Node {rootLabel = 88, subForest = []}]},Node {rootLabel = 7, subForest = []}], _parents = [([],10,[])]}
tp = fromTree t

class Render a

class Handler a

data SomeModel = forall a b. (Render b, Handler b) => SomeModel (IORef a, Lens' a b)

instance Show SomeModel where
  show _ = "SomeModel"

data Widget' = Widget'
  { _pos :: Int,
    _size :: Int,
    _model :: SomeModel
  }
  deriving (Show)

data A = A
  { _va :: Int,
    _vb :: String
  }
  deriving (Show)

makeLenses ''A

df = A 10 "nice"

instance Render A

instance Handler A

id' :: Lens' s s
id' = lens id (\s a -> a)

mkA :: IORef A -> Widget'
mkA ref =
  Widget'
    { _pos = 1,
      _size = 2,
      _model = SomeModel (ref, id')
    }

mkW ::
  (Handler b, Render b) =>
  IORef a ->
  Lens' a b ->
  Widget'
mkW ref len =
  Widget'
    { _pos = 1,
      _size = 2,
      _model = SomeModel (ref, len)
    }

type Widget = Tree Widget'

instance Handler Int

instance Render Int

instance Handler String

instance Render String

wt :: IO ()
wt = do
  ref <- newIORef df
  let w' = mkA ref
      w =
        Node
          w'
          [ Node (mkW ref va) [],
            Node (mkW ref vb) []
          ]
  undefined
