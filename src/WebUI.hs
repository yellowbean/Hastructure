{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WebUI where

import Web.Hyperbole


import qualified Accounts as A
import Types

-- instance HyperView A.ReserveAmount es where
--   data Action A.ReserveAmount 
--     = SetMessage Text
--     deriving (Show, Read, ViewAction)
-- 
--   update (SetMessage msg) =
--     pure $ messageView msg

instance HyperView DatePattern es where
  data Action DatePattern
    = SetDatePattern DatePattern
    deriving (Show, Read, ViewAction)

  update (SetDatePattern dp) =
    pure $ dpView dp


dpView :: DatePattern -> View context ()
dpView msg =
  el bold (text msg)


main :: IO ()
main = do
  run 3001 $ do
    liveApp (basicDocument "Example") (runPage page)

page :: Eff es (Page '[])
page = do
  pure $ do
    col (pad 10) $ do
      el bold "Hello World"