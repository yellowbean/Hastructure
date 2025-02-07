{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

 module WebUI where
-- 
-- import Web.Hyperbole
-- import Data.Text (Text)
-- 
-- import qualified Accounts as A
-- import Types
-- 
-- import Web.Hyperbole
-- import Data.Text (Text)
-- import GHC.Generics (Generic)
-- 
-- -- Define your ADT
-- data Shape = 
--     Circle Double
--   | Rectangle Double Double
--   | Triangle Double Double Double
--   deriving (Show, Generic, HyperView)
-- 
-- instance HyperView Shape where
--   data Action Shape = 
--       SetShape Text
--     | SetCircleRadius Double
--     | SetRectangleWidth Double
--     | SetRectangleHeight Double
--     | SetTriangleA Double
--     | SetTriangleB Double
--     | SetTriangleC Double
--     deriving (Show, Read, ViewAction)
-- 
--   view _ s = do
--     col id $ do
--       -- Dropdown for shape selection
--       select "shape" (viewShapeName s) SetShape [("Circle", "Circle"), ("Rectangle", "Rectangle"), ("Triangle", "Triangle")]
--       case s of
--         Circle r -> do
--           field "radius" r SetCircleRadius
--         Rectangle w h -> do
--           field "width" w SetRectangleWidth
--           field "height" h SetRectangleHeight
--         Triangle a b c -> do
--           field "side a" a SetTriangleA
--           field "side b" b SetTriangleB
--           field "side c" c SetTriangleC
-- 
--   update (SetShape "Circle") _ = Circle 0
--   update (SetShape "Rectangle") _ = Rectangle 0 0
--   update (SetShape "Triangle") _ = Triangle 0 0 0
--   update (SetCircleRadius r) _ = Circle r
--   update (SetRectangleWidth w) (Rectangle _ h) = Rectangle w h
--   update (SetRectangleHeight h) (Rectangle w _) = Rectangle w h
--   update (SetTriangleA a) (Triangle _ b c) = Triangle a b c
--   update (SetTriangleB b) (Triangle a _ c) = Triangle a b c
--   update (SetTriangleC c) (Triangle a b _) = Triangle a b c
--   update _ s = s  -- Default case, no change
-- 
-- -- Helper function to convert Shape to text for the select dropdown
-- viewShapeName :: Shape -> Text
-- viewShapeName (Circle _) = "Circle"
-- viewShapeName (Rectangle _ _) = "Rectangle"
-- viewShapeName (Triangle _ _ _) = "Triangle"
-- 
-- -- Define a simple page
-- page :: (Hyperbole :> es) => Eff es (Page '[Shape])
-- page = do
--   pure $ col id $ do
--     hyper ShapeForm $ view ShapeForm (Circle 0)
-- 
-- 
-- 
-- main :: IO ()
-- main = do
--   run 3001 $ do
--     liveApp (basicDocument "Example") (runPage page)
-- -- 
-- -- page :: Eff es (Page '[])
-- -- page = do
-- --   pure $ do
-- --     col (pad 10) $ do
-- --       hyper MonthEnd dpView "A"