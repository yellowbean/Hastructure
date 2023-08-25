[![Actions Status](https://github.com/yellowbean/Hastructure/workflows/Haskell%20CI/badge.svg)](https://github.com/yellowbean/Hastructure/actions)
[![Docker Build](https://img.shields.io/docker/v/yellowbean/hastructure?color=green&label=docker)](https://hub.docker.com/r/yellowbean/hastructure)

# Hastructure
* :dollar: A structured finance cashflow engine written in Haskell 
* :coffee: Easy integration with Java/C#/C++/JavaScript/Python with RESTful interface and Docker image ready
* :bricks: A building block engine to model cashflows of structured product, all the formula and variables are exposed.
* :car: In-house and white-label friendly.
* :snake: [Python wrapper](https://github.com/yellowbean/PyABS) on the way   

### Features
* Integration
  * Built-in REST API services
    * Language independent, integration friendly.
    * Swagger -> [here](https://github.com/yellowbean/Hastructure/blob/master/swagger.json)
    * Public server status -> [here](https://absbox.org)
  * Docker Support 
* Asset class coverage (Mortgage/Student Loan/Auto Loan/Rentals/Corp Loan/Consumer Installment)
* Pool Assumptions
  * Mortgage (Prepay,Prepay Penalty,Default,Recovery Lag/Rate)
  * Installment (Prepay Default Recovery Lag/Rate) 
  * Corp Loan (Prepay Default Recovery Lag/Rate)
  * Rentals (Gaps between leases,Rental Curve Assumption) 
* Multiple Waterfalls
  * Clean up waterfall/ Pre,Post Enforcement waterfall
  * Pool collection waterfall
* Accounts
  * Reserve Account/Bank Account (with interest)/Cash Account/ Ledger(PDL)
* Bonds/Tranches
  * Float Index rate / Step Up coupon type / Fix Rate
  * Sequential / Prepay Lockout /PAC Bond Support /Z Bond Support 
  * Bond Pricing (IRR /WAL /Duration /Accrual Int)
* Call
  * call by Pool/Bond Balance amount;Bond/Pool Factor;On Date/or after
* Fees
  * Pool / Bond balanced based fees 
  * Fix Amount Fees / Custom Fee Flow / Number Type Fee of a deal / Formula based fee rate 
* Liquidity Provider 
  * line of credit/ Unlimit support 
  * interest charge or fee charge on the credit used & unused
* Trigger 
  * Base on Date 
  * Base on Free Formula : Bond /Pool metrics
  * Base on Pool performance
  * Base on any combination above
* Interest Swap
  * Float to Float/ Fix to Float support
* Scenario Analysis
  * Running multiple scenarios on single deal
  * Pricing on single asset 
  * Revoving Buy Analysis 
* Formula Support 
  * User is able to using statistics of deal ( Pool Balance,Account balance ,total Bond Balance of , A factor of .. ) to construct formula which used to specify the amount of cash to transfer , pay out to fee or liabilities etc.
* Misc
  * Support user define pay dates & pool collection dates 
### Online Demo

The demo only cover very limit features of this engine and subject to UI performance issue due to rapid prototype design of web component

**Pls noted that the web demo is far behind latest development/stable version**

* [Here](https://deal-bench.xyz)


### Others
* [Why yet another cashflow engine](https://github.com/yellowbean/Hastructure/wiki/Why-Yet-Anohter-Cashflow-Engine)
