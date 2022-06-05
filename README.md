# Hastructure
a structured finance cashflow engine written in Haskell
### Road Map

* Mortgage Default/Prepay/Recovery Assumption :construction_worker:
* Float Interest Rate Support (Bond and Assets)
* Sinking Fund/PAC Bond

### Features
* built-in REST API services :heavy_check_mark:
* Asset class coverage
  * Mortgage  :heavy_check_mark:
  * Student Loan
  * Auto Loan
  * Rentals
* Pool Assumptions
  * Mortgage (Prepay Default Recovery) 
* Accounts
  * Liquidity Facility
  * Reserve Account  :heavy_check_mark:
  * Bank Account (with interest) :heavy_check_mark:
* Bonds
  * Float Index rate
  * Passthrough :heavy_check_mark:
  * Sinking fund
  * Term bond
* Fees
  * Pool / Bond balanced based fees  :heavy_check_mark:
  * Fix Amount Fees  :heavy_check_mark:

#### Why yet another cashflow engine

I've tried with `clojure` and `python` to write cashflow engine for structured finance 
while it is quite challenging : cashflow in structured finance involves hundreds of data types(either pool asset type and liability type), it also involves with differnt fee types and account types.
all of these combination types will boost the complexity of computation and poses a huge burden on brain.

`clojure` prevails in nested data structure using `specter` which is a killer in dealing with `deal/transaction` 
as it has nested components like `pool` `assets` `bond/liabilities` `fees` etc. But I failed to continue expanding as the code
base as it is heavily relying on `core.match`/pattern match, unfortunately that package seems slowing and no-one is fixing backlog issues.

`python` has introduced the pattern matching in 3.10 to solve that and having a great community support like `numpy/pandas` library.
While it failed in maintenance cost : it is hard to refactor and scale up the code base in line with the complexity world of structured finance.
A minor changes in code may secretly introduce bugs if there isn't any rigorous testing framework in place.

After months of hunting(actually years) => `haskell` is quite nice fit IMHO.
* `type /compiler` check ease the brain work and let me focus the business logic instead of breaches caused by refactoring
* built-in support for pattern matching
* high performance 

