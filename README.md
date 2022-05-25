# Hastructure
a structured finance cashflow engine written in Haskell

### Features
* built-in REST API services
* Asset class coverage
  * Mortgage  (/)
  * Student Loan
  * Auto Mortgage
  * Rentals
* Accounts
  * Liquidation Facility
  * Reserve Account
* Bonds
  * Passthrough
  * Sinking fund
  * Term bond

### Why yet another cashflow engine

I've tried with `clojure` and `python` to write cashflow engine for structured finance 
which is most challenging : cashflow in structured finance involves hundreds of data types(either pool asset type and liability type)  

`clojure` prevails in nested data structure using `specter` which is a killer in dealing with `deal/transaction` 
as it has nested components like `pool` `assets` `bond/liabilities` `fees` etc. But I failed to continue expanding as the code
base is heavily rely on `core.match`/pattern match, unfortunately that package seems slowing and no-one is fixing backlog issues
which is causing blocks to future work.

`python` has introduced the pattern matching in 3.10 to solve that and having a great community support like `numpy/pandas` library.
While it failed in maintenance cost : it is hard to refactor and scale up the code base in line with the complexity world of structured finance.

After months of hunting(actually years) => `haskell` is quite nice fit IMHO.
* `type /compiler` check ease the brain work and let me focus the business logic instead of breaches caused by refactoring
* built-in support for pattern matching
* high performance 
* nice FFI support
    * working-in-progress version of `Python` wrapper is under way

