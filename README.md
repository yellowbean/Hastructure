[![Actions Status](https://github.com/yellowbean/Hastructure/workflows/Haskell%20CI/badge.svg)](https://github.com/yellowbean/Hastructure/actions)

# Hastructure
* :dollar: A structured finance cashflow engine written in Haskell 
* :coffee: Easy integration with Java/C#/C++/JavaScript/Python
* :bricks: A building block engine to model cashflows of structured product
* :car: In-house and white-label friendly 
* :snake: [Python wrapper](https://github.com/yellowbean/PyABS) on the way   
### Features
* Integration
  * Built-in REST API services :heavy_check_mark:
    * Language independent, can be integrated to various programming language stack.
  * Docker Support (W.I.P)
* Asset class coverage
  * Mortgage  :heavy_check_mark:
  * Student Loan
  * Auto Loan
  * Rentals
  * Corp Loan
* Pool Assumptions
  * Mortgage (Prepay Default Recovery Lag/Rate) :heavy_check_mark:
* Multiple Waterfalls
  * User defined waterfalls and supported any point of time during the running cycle
* Accounts
  * Liquidity Facility
  * Reserve Account  :heavy_check_mark:
  * Bank Account (with interest) :heavy_check_mark:
  * Pay a single fee with a list of Accounts :heavy_check_mark:
* Bonds/Tranches
  * Float Index rate :heavy_check_mark:
  * Sequential :heavy_check_mark:
  * Prepay Lockout :heavy_check_mark:
  * PAC Bond Support :heavy_check_mark:
  * Z Bond Support :heavy_check_mark:
  * Bond Pricing
    * IRR :heavy_check_mark:
    * WAL :heavy_check_mark:
    * Duration :heavy_check_mark:
    * Accural Int in pricing
* Call
  * Pool/Bond Balance :heavy_check_mark:
  * Bond/Pool Factor :heavy_check_mark:
  * On Date/or after :heavy_check_mark:
* Fees
  * Pool / Bond balanced based fees  :heavy_check_mark:
  * Pool Interest pct :heavy_check_mark:
  * Fix Amount Fees  :heavy_check_mark:
  * Custom Fee Flow :heavy_check_mark:

### Online Demo

The demo only cover very limit features of this engine and subject to UI performance issue due to rapid prototype design of web component

* [Here](https://deal-bench.xyz)


### Others
* [Why yet another cashflow engine](https://github.com/yellowbean/Hastructure/wiki/Why-Yet-Anohter-Cashflow-Engine)
