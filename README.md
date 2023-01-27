[![Actions Status](https://github.com/yellowbean/Hastructure/workflows/Haskell%20CI/badge.svg)](https://github.com/yellowbean/Hastructure/actions)
[![Docker Build](https://img.shields.io/docker/v/yellowbean/hastructure?color=green&label=docker)](https://hub.docker.com/r/yellowbean/hastructure)

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
    * Public server status -> [here](https://absbox.org)
  * Docker Support :heavy_check_mark:
* Asset class coverage
  * Mortgage  :heavy_check_mark:
  * Student Loan
  * Auto Loan
  * Rentals :heavy_check_mark:
  * Corp Loan :heavy_check_mark:
  * Consumer Installment :heavy_check_mark:
* Pool Assumptions
  * Mortgage (Prepay Default Recovery Lag/Rate) :heavy_check_mark:
  * Installment (Prepay Default Recovery Lag/Rate) :heavy_check_mark:
  * Corp Loan (Prepay Default Recovery Lag/Rate) :heavy_check_mark:
  * Rentals (Gaps between leases,Rental Curve Assumption) :heavy_check_mark:
* Multiple Waterfalls
  * User defined waterfalls and supported any point of time during the running cycle
    * Waterfall at Distribution Day :heavy_check_mark:
      * Waterfall before EOD :heavy_check_mark:
      * Waterfall when Event of Acceleration :heavy_check_mark:
      * Waterfall when Event of Default :heavy_check_mark:
    * Waterfall at End of Pool Collection Day :heavy_check_mark:
    * Waterfall for Clean Up Call  :heavy_check_mark:
* Accounts
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
    * Accrual Int in pricing :heavy_check_mark:
* Call
  * Pool/Bond Balance :heavy_check_mark:
  * Bond/Pool Factor :heavy_check_mark:
  * On Date/or after :heavy_check_mark:
* Fees
  * Pool / Bond balanced based fees  :heavy_check_mark:
  * Pool Interest pct :heavy_check_mark:
  * Fix Amount Fees  :heavy_check_mark:
  * Custom Fee Flow :heavy_check_mark:
* Liquidity Provider 
  * line of credit/ Unlimit support  :heavy_check_mark:
* Trigger 
  * Base on Date :heavy_check_mark:
  * Base on Formula :heavy_check_mark:
    * Pool performance :heavy_check_mark:
    * Bond stats :heavy_check_mark:
* Scenario Analysis
  * Running multiple scenarios :heavy_check_mark:
* Formula Support 
  * User is able to using var of deal ( Pool Balance,Account balance .. ) to construct formula.
* Misc
  * Support user define pay dates & pool collection dates :heavy_check_mark:

### Online Demo

The demo only cover very limit features of this engine and subject to UI performance issue due to rapid prototype design of web component

**Pls noted that the web demo is far behind latest development/stable version**

* [Here](https://deal-bench.xyz)


### Others
* [Why yet another cashflow engine](https://github.com/yellowbean/Hastructure/wiki/Why-Yet-Anohter-Cashflow-Engine)
