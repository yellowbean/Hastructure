# Changelog for Hastructure


## 0.17.3
#### 2023-5-
* NEW "Major" : expose revolving assumption ! 


## 0.17.2
#### 2023-5-24
* NEW: expose "exclude dates" and "offset by days" <date pattern>


## 0.17.1
#### 2023-5-21
* NEW: expose trigger status in "Inspect"

## 0.17.0
#### 2023-5-21
* NEW: expose BalanceSheet Report and Cashflow Report, user can query them via set flags in assumption
* BREAK: normalized some account comments to be analysed when compling Cashflow Report


## 0.16.0
#### 2023-5-13
* NEW: expose bond with Step-Up coupon feature 
* BREAK:using <DatePattern> to annotate reset date for floater bonds
* FIX: data query in the trigger

## 0.15.4
#### 2023-5-6
* Fix cashflow projection logic for `Installment`
* Include `DefaultedRecovery` assumption for defaulted assets.

## 0.15
#### 2023-5-1
* Introduce new asset : AdjustRateMortgage with assumption:
    * init period,first reset cap, periodic reset cap,lifetime cap, lifetime floor
* Docker hub will host each stable releases of Hastructure
