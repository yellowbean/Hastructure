# Changelog for Hastructure

## 0.22.0
### 2023-10-15
* BREAK: cashflow now with `Cumulative Stats` ( cumulative default/delinq/loss/prepayment/principal/recovery)
* NEW: expose `inspect` in waterfall action to observe variables during a waterfall execution
* NEW: `stepup` now accpet a `pre` instead of a `date` to switch rate
* ENHANCE: auto patch `issuance balance` for `PreClosing` Deal
* ENHANCE: implement `pre-run check` and `post-run check`, patch check logs in result
* FIX: fix bug on `prepay penalty` when using `stepDown`

## 0.21.5
### 2023-10-8
* ENHANCE: in the revolving buy , now buy amount is no longer a multipler of revolving assets face value
* FIX: now revolving asset may have remtain term ==  original term

## 0.21.4
### 2023-9-27
* ENHANCE: require a new status when defining a deal in `preClosing` stage
* FIX: fix a bug when reading financial report logs 

## 0.21.3
### 2023-9-26
* NEW: include a `default`/`delinq`/`loss` status map when projecting cashflow
* NEW: implement `haircut` as extra stress projecting `mortgage` 
* ENHANCE: include `called` deal status, which will be set when deal was triggered with a clean up call assumption
* ENHANCE: expose `runAsset` endpoint
* ENHANCE: expose formula query on `deal status` as well as `trigger status`
* ENHANCE: add `rampUp` deal status
* FIX: adjust bond reset date from `cutoff date` to `closing date`

## 0.21.1
### 2023-9-21
* BREAK: seperate `performance assumption`
* BREAK: add `delinquency` projection on mortgage as well as schedule mortgage cashflow

## 0.20.3
### 2023-9-4
* ENHANCE: now user can include boolean/int/balance/rate type query in `inspect` field

## 0.20.2
### 2023-8-31
* BREAK: move `Trigger` from `list` into a `map` with a name
* ENHANCE: add `CumulatiePoolDefaultedRateTill` to query default rate as of collection period N , then support query last one,last two default rates in the past as a rolling basis..
* ENHANCE: add `queryBool` with test logic of `any` `or` which will test all predicates or any predicates are/is satisfied. With new included aforementioned formula above, the engine can have a predicate like `last 2 period cumulative defaulte rates are all lower than 5%`, `any last 2 period cumulative defaulte rates is higher than 5%`  

## 0.20.1
### 2023-8-29
* ENHANCE: add `LedgerTxnAmt` , allow user to query transaction amount for a ledger by `comment`
* ENHANCE: expose `Abs` in formula , which will get absolute value of another formula

## 0.20.0
### 2023-8-25
* BREAK: refactor `payInt` and `payFee` which includes `extraSupport` from either another `account` or `liquidation provider`, with option to book `PDL draw` on ledger
* NEW: expose `Cumulative Net Loss` `Cumulative Net Loss Ratio` `Bond Rate` `Bond Weight Average Rate` in formula
* NEW: expose `Avg` in formula ,which can calculate average value from a list of deal stats.
* NEW: expose `RefRate` in bond , now bond can be setup interest rate which reference to a value of deal , could be like 100% of Pool WAC coupon , or average of bond rate of bonds etc.
* ENHANCE: add `liquidity provider` `interest swap` to `balance sheet repot`
* ENHANCE: add new bool query `is_most_senior_bond`
* ENHANCE: add new balance query `PoolCurCollection` returns target pool source balance in last collected period
* ENHANCE: refactor account transfer by `target reserve amount`


## 0.19.15
### 2023-8-20
* ENHANCE: add `reserve account excess/gap` to formula
* ENHANCE: refactor bond `step up` coupon by date ,which pertains to Euro deals
* ENHANCE: add comments to souce code and prepare to release to `Hackage`

## 0.19.12
### 2023-8-17
* NEW: Add `Step Up By Date` /`Cap`/`Floor` coupon type for bond
* NEW: Add `Prepay Penalty` attribute on `Mortgage`, penalty types includes:
  * `rate0` before `term N` and `rate1` after `term N`
  * Fixed amount in lifetime or before `term N`
  * Fixed pct in life time or bfore `term N`
  * Sliding from `rate0` by step of `rate1`
  * Ladder type like first `12` periods with Pct of `Rate0` , next `12` periods with Pct of `Rate1`
* ENHANCE: refactor `liquidity provider`
  * include a maybe `valid date` of the agreement
  * include floater index

## 0.19.11
### 2023-8-14
* ENHANCE: add `calcAndPay` action for fee
* ENHANCE: expose new assumption on expense projection
* ENHANCE: include a new `NO_IE` type to generate dates vector
* FIX: Fix missing periods of `recurr` type of fee

## 0.19.10
### 2023-8-7
* NEW : add a new expense type: `TargetBalanceFee`, which due amount = `<formula 1> - <formula 2>`
* ENHANCE: add query total txn amount for account/bond/expense with optional `comment` as a filter
* ENHANCE: expoese query on `cumulative pool` on `recoveries` `principal` `interest` `prepayment` 
* ENHANCE: expoese query on `beg balance` on pool


## 0.19.8
### 2023-7-24
* ENHANCE: trancate payments records for bond with 0 balance and 0 due interest/due pricipal

## 0.19.7
### 2023-7-19
* ENHANCE: expose query on `cumulative pool recoveries`
* ENHANCE: expose `factor` in query
* ENHANCE: ensure principal payment is cap via bond oustanding balance 

## 0.19.6
### 2023-7-18
* FIX: update PDL Ledger balance after `bookBy` action

## 0.19.4
#### 2023-7-17
* FIX: fix pricing error if bond flow size is 0

## 0.19.0
#### 2023-7-1
* BREAK : seperate `payInt` action and `accrueInt` action
* ENHANCE: optimize `Z-spread` calculation
* ENHANCE: re arrange `deal.hs` , break down code logic into seperate files.
* NEW: include `ledgers` to accomodate `PDL` feature (Principal Deficiency Ledger)
* ENHANCE: expose `rounding` on `deal stats`, which rounds interest rate change by a factor of fix amount ,or pay principal on balance by a factor of fix amount.
* ENAHNCE: expose `runDate` endpoint as sandbox for user to play with `<datePattern>`


## 0.18.9
#### 2023-6-23
* NEW : add `floorAndCap` formula to set upper or lower bound of formula value
* NEW : add formula based fee rate for `pct` and `annual` type of fee

## 0.18.1
#### 2023-6-21
* NEW : Project cashflow for a list of asset, with performance assumption
* ENHANCE : Add `limit` for revolving buy action
* ENHANCE : Add `default` waterfall 
* NEW : Add "IF-ELSE" in waterfall action

## 0.18.0
#### 2023-6-8
* NEW "Major" : expose revolving assumption !
* NEW : `Pre` now support comparing with a `balance` type formula ,not limited to a balance number


## 0.17.2
#### 2023-5-24
* NEW: expose `exclude dates` and `offset by days` <date pattern>


## 0.17.1
#### 2023-5-21
* NEW: expose trigger status in `Inspect`

## 0.17.0
#### 2023-5-21
* NEW: expose `BalanceSheet Report` and `Cashflow Report`, user can query them via set flags in assumption
* BREAK: normalized some account comments to be analysed when compling `Cashflow Report`


## 0.16.0
#### 2023-5-13
* NEW: expose bond with `Step-Up coupon` feature 
* BREAK:using `DatePattern` to annotate reset date for floater bonds
* FIX: data query in the trigger

## 0.15.4
#### 2023-5-6
* FIX: Fix cashflow projection logic for `Installment`
* Include `DefaultedRecovery` assumption for defaulted assets.

## 0.15
#### 2023-5-1
* Introduce new asset : `AdjustRateMortgage` with assumption:
    * init period,first reset cap, periodic reset cap,lifetime cap, lifetime floor
* Docker hub will host each stable releases of Hastructure
