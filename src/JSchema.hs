{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}




module JSchema

where 

import Types
import GHC.Generics
import GHC.Real
import Data.OpenApi hiding(Server) 
import Servant.OpenApi






instance ToSchema (Ratio Integer)
instance ToSchema (TsPoint Balance)
instance ToSchema (TsPoint Rational)
instance ToSchema (TsPoint Bool)
instance ToSchema (TsPoint IRate)
instance ToSchema Ts


