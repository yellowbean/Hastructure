{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Deal.DealValidation (validateRun)
  where 

import Deal.DealBase
import Types


validatePreRun :: TestDeal a -> Date -> (Bool,[ResultComponent])
validatePreRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM} d 
  = let 
      errors = []
      warnings = []
      flag = True
      -- waterfall action coverage check 

      -- run result scan
    in 
      (flag, errors ++ warnings)


validateRun :: TestDeal a -> Date -> [ResultComponent]
validateRun t@TestDeal{waterfall=waterfallM
                      ,accounts =accM 
                      ,fees = feeM 
                      ,bonds = bondM 
                      ,collects = aggRule 
                      ,liqProvider = liqProviderM 
                      ,rateSwap = rsM 
                      ,triggers = triggerM
                      ,ledgers = ledgerM} d 
  = let 
      errors = []
      warnings = []
      -- waterfall action coverage check 

      -- run result scan
    in 
      errors ++ warnings