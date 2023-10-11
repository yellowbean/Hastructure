## How Cashflow Projection Work

### Steps

* determine the projection dates
  
  * start date -> There is a field in asset present the `origination date` ,which means the date when asset came into existance.
  * (original/scheduled) payment dates -> Then, cashflow engine will generate a vector of payment dates bases on `origination date` and `payment frequency`
  * remaining payment dates -> base on the input field `remaining terms`, engine will trancate the `payment dates` to get `remaining payment dates`

* project cashflow with assumptions
  
  * projected cashflow -> Given `remaining payment dates` and `current balance` , then engine will calculate cashflow with assumption starting from `remaining payment dates`

* truncate the cashflow via `cutoff date`

  * `projected cashflow` was truncated via `cutoff date` ,that's the cashflow of asset which flows into the SPV

