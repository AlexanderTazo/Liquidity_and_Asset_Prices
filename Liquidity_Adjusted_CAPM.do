/*-------------------------------------------------------------------------------
Liquidity-Adjusted Capital Asset Pricing Model
-------------------------------------------------------------------------------
This is the code written in Stata for the Master Thesis: Liquidity and Asset
Prices by Alexander Tazo and Heda Tazojeva. Please note that this code is only
meant for the supervisor, Jørgen Haug, and for the external examiner. Below we
provide the code for our main testing model for the sake of brevity. The full
code is over 5000 lines long and can be provided to the examinisers if they
wish so. 
For further questions please contact the following Email:

Alexander Tazo
alex_taz@live.no 
*/

* SECTION 4: METHODOLIGY

*----------------------------------------------------
      * Section 4.1  AMIHUD ILLIQUIDITY RATIO
*----------------------------------------------------	


use "C:\Users\ATazo\Desktop\LCAPM\OriginalData.dta",clear

* drop zero issues
drop if SharesIssue == 0

* Winsorize returns
winsor2 ri, replace

* drop uncessary variables
drop Bid-Last
drop Symbol-SecurityName
drop logRegAdj
drop logReturn
drop Vwap
drop OffTurnover

* drop small stocks
drop if AdjLast < 1
* Declare variables in global macros

global daily_date date // here date is the date variable in the dataset
global monthly_date mofd // mofd is monthly variable in the dataset
global year_date year
global firm_marketcap firm_marketcap
global market_total_cap market_total_cap
global shares_outstanding SharesIssue // replace SharesIssue with actual variable name in the dataset
global share_price AdjLast // replace AdjLast with actual variable name in the dataset
global firm_id SecuritiyID // replace SecuritiyID with actual variable name in the dataset
global trading_volume OffShareTurnover // replace OffTurnover with actual variable name in the dataset
global daily_returns returns // replace returns with actual variable name in the dataset

* Create a monthly date in Stata format
gen monthly_date = mofd($daily_date)

* Format the data as monthly data
format monthly_date %tm

gen year =year($daily_date)


* Find market capitalization for each firm
gen firm_marketcap = $shares_outstanding * $share_price

* Find market wide capitalizaiton
bys $daily_date: egen market_total_cap = total(firm_marketcap)

* lag the market_total_cap by one month
preserve
keep monthly_date market_total_cap
bys monthly_date: keep if _n == _N
tsset monthly_date
gen lag_market_total_cap = L.market_total_cap
drop market_total_cap
save temp, replace
restore
merge m:1 monthly_date using temp

* Create the variable PM - the ratio of lagged market value to first value of the market in the sample
sum $daily_date
loc firstDate = `r(min)'
sum market_total_cap if $daily_date == `firstDate'


gen PM = lag_market_total_cap / `r(mean)'
* drop uncessary variables
drop market_total_cap _merge
rm temp.dta





*Generate the dollar volume***
gen float dollar_volume = $trading_volume * $share_price

bys $firm_id monthly_date : gen obs =_N
bys $firm_id monthly_date : egen z = count($daily_returns) if $daily_returns==0
bys $firm_id monthly_date : egen zero = min(z)
gen pzret = zero / obs
drop if pzret>.8 & pzret!=.
drop  z
bys $firm_id monthly_date : egen sumtrades = count($daily_date)
drop if sumtrades<15
drop sumtrades
*Scaled Amihud measure of illiquidity, dollar volume converted into thousands

bys $firm_id $daily_date : gen double illiq = 1000000*(abs($daily_returns)/(dollar_volume)) if dollar_volume>0

drop if illiq==.
bys $firm_id monthly_date : egen firm_Amihud = mean(illiq)

* winsorize for extreme values
winsor2 firm_Amihud, replace
drop obs zero pzret


* set the normalizaiton parameters
global ac = .25
global bc = .35
globa maxc = 30

gen c = $ac + ($bc * firm_Amihud) * PM

replace c = min(c, $maxc)

save "C:\Users\ATazo\Desktop\LCAPM\illiq.dta", replace

*------------------------------------------------------------
      * Section 4.3  PORTFOLIOS CONSTRUCTION AND EVALUATION
*------------------------------------------------------------	
use "C:\Users\ATazo\Desktop\LCAPM\illiq.dta",clear 		   

/*We form 5 illiquidity portfolios for each year y during the period 1998 to 2017 by
sorting stocks with price, at the beginning of the year, excluding only stocks less than NOK 1,
and return and volume data in year y - 1 for at least 100 days.
*/

* Find annual average illiquidity for each stock
bys $firm_id year: egen yearly_ILLIQ = mean(c)

* Find annual standard deviation of daily illiquidity for each stock
bys $firm_id year: egen yearly_SD_ILLIQ = sd(c)

* Find annual mean of firms market capitalizations
bys $firm_od year: egen yearly_size = mean(firm_marketcap)



* reduce the data to yearly frequency
bys $firm_id year : keep if _n == _N 

* keep relevant variables
keep $firm_id year yearly_ILLIQ yearly_SD_ILLIQ

* lag the illiquidty variable by one year
tsset $firm_id year

gen lag_illiq = L.yearly_ILLIQ

gen lag_SD_illiq = L.yearly_SD_ILLIQ

* Make 5 portfolios from the illiquidity and market capitalization

bys year: astile portf_illiq = lag_illiq, nq(5)

bys year: astile portf_SD_illiq = lag_SD_illiq, nq(5)

bys year: astile portf_size = yearly_size, nq(5)

* save in temporary file
save temp, replace

* Merge with the main data set
use "C:\Users\ATazo\Desktop\LCAPM\illiq.dta",clear 

merge m:1  $firm_id year using temp
drop _merge

*----------------------------------------------------------------------
       *CREATING TABLE 2 Second Part(All values except Betas)
*----------------------------------------------------------------------

* Find monthly returns for each stock
bys $firm_id monthly_date: asrol ret, stat(product) add(1) gen(monthly_ret)
winsor2 monthly_ret, replace

* Find yearly sd  for each stock
bys $firm_id year: asrol ret, stat(sd)  gen(yearly_sd_firms)


* Find monthly turnover for each stock
bys $firm_id monthly_date: asrol $trading_volume, stat(sum) gen(monthly_turnover)

* winsorize it
winsor2 monthly_turnover, replace

* Convert monthly turnover to  millions
replace monthly_turnover = monthly_turnover / 10000000

* reduce the data to monthly frequency
bys portf_illiq monthly_date : keep if _n == _N

* Find portfolio returns
bys portf_illiq monthly_date: egen illiq_port_ret = mean(monthly_ret)

* SD Returns
bys portf_illiq monthly_date: egen illiq_port_sd = mean(yearly_sd_firms)


* Find portfolio illiquidity
bys portf_illiq monthly_date: egen illiq_port_c = mean(c)

*Standard deviation of portfolio illiquidity
bys portf_illiq monthly_date: egen illiq_sd_c = sd(c)


* Turnover
bys portf_illiq monthly_date: egen illiq_port_turn = mean(monthly_turnover)

* Market cap
bys portf_illiq monthly_date: egen illiq_port_mktcap = mean(firm_marketcap)

* Convert to millions
replace illiq_port_mktcap = illiq_port_mktcap / 1000000000


*Need to substract monthly Risk-free rate from the monthly returns
merge m:1 mofd using Risk_free_monthly
drop _merge
replace monthly_ret = monthly_ret - rf1m 

*-------------------------------------------------------------------
*               MARKET ILLIQUIDITY AND MARKET RETURNS
*-------------------------------------------------------------------

*Market illiquidity as a mean of cross-sectional illiquidity
bys monthly_date : egen market_illiq =mean(c)
replace market_illiq = market_illiq * PM

*Market Returns- From Ødegård Database(or can just average our monthly returns)
*bys monthly_date: egen monthly_rm = mean(monthly_ret)

merge m:1 monthly_date using MonthlyMarketReturns
rename ew monthly_rm_EW
rename vw monthly_rm_VW

drop _merge allshare obx 

*---------------------------------------------------------------------
*          Output of Second Part of Table 2
*---------------------------------------------------------------------
preserve
* renma evariables for output
ren (illiq_port_c illiq_sd_c illiq_port_ret illiq_port_sd illiq_port_turn illiq_port_mktcap)\\\
(E(s^p) sigma(s^p) E(r^e,p) sigma(r^p) Turn Size)

tabstat E(s^p) sigma(s^p) E(r^e,p) sigma(r^p) Turn Size, by(portf_illiq)

restore
save Table2_Part2, replace

*In next part we need to reshape our portfolios to run estimate our Betas
*Application of DVECH MGARCH does not work on panal data.

* keep only relevant data
keep portf_illiq monthly_date year PM market_illiq monthly_rm_EW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

* Save the data in long format

save "C:\Users\ATazo\Desktop\LCAPM\portf_illiq_long.dta", replace

use"C:\Users\ATazo\Desktop\LCAPM\portf_illiq_long.dta", clear


preserve
keep if portf_illiq == 1

rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P1_ret P1_c  P1_sd P1_turn P1_mcap)
save p1, replace
restore

drop PM market_illiq monthly_rm year




forv i = 2 / 5 {
preserve
keep if portf_illiq == `i'
* rename all variables
rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P`i'_ret P`i'_c  P`i'_sd P`i'_turn P`i'_mcap)
 save p`i', replace
restore
}

use "C:\Users\ATazo\Desktop\LCAPM\p1.dta",clear
drop portf_illiq
forv i = 2 / 5 {
merge 1:1 mofd using p`i'
cap drop _merge
cap drop portf_illiq
}

save "C:\Users\ATazo\Desktop\LCAPM\illiq_portf.dta", replace

*---------------------------------------------------------------------------------------
* SECTION 4.4 ESTIMATING INNOVATIONS IN ILLIQUIDITY AND RETURNS USING DVECH MGARCH
*----------------------------------------------------------------------------------------        

tsset monthly_date
rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = ($maxc - $ac)/($bc * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)




*Conditional Covariance of Portfolio Returns with Market- i.e Beta 1 Numerator
	cap drop  h1_* h2_* h3_* h4_*
	mgarch dvech (P1_ret monthly_rm_EW =), arch(1) iterate(100)
	predict h1* if e(sample), variance
	
 	mgarch dvech (P2_ret monthly_rm_EW =), arch(1/2) iterate(100)
	predict h2* if e(sample), variance

	mgarch dvech (P3_ret monthly_rm_EW =), arch(1) iterate(100)
	predict h3* if e(sample), variance

	mgarch dvech (P4_ret monthly_rm_EW =), arch(1) iterate(100)
	predict h4* if e(sample), variance
	
	mgarch dvech (P5_ret monthly_rm_EW =), arch(1) iterate(100)
	predict h5* if e(sample), variance


* Denominator of our Betas	in Equations 11-14
	gen Rm_PIm = monthly_rm_EW - market_illiq
	
	
* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	
*Covariance of Portfolio Illiquidity with Market Illiqudity, i.e Beta 2 Numerator
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

*Covariance of Portfolio Return with Market Illiqudity-i.e Beta 3 Numerator
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

	
*Covariance of Portfolio illiquidity with Market Returns-i.e Beta 4 Numerator
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_EW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	
*-----------------------------------------------------------------------------------
*      Constructing Betas for our Illiquidity Sorted Portfolios, i.e Table 2-Part1	
*-----------------------------------------------------------------------------------


*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_P`p'_ret / v1_Rm_PIm_Rm_PIm
}

*Beta 2 : Portfolio Illiq and Market Illiq

forv p = 1 / 5 {
	gen B2_P`p' = p`p'_market_illiq_P`p'_c / v1_Rm_PIm_Rm_PIm
}

*Beta 3 : Portfolio Returns and Market Illiq
forv p = 1 / 5 {
	gen B3_P`p' = rp`p'_market_illiq_P`p'_ret / v1_Rm_PIm_Rm_PIm
}


*Beta 4 : Portfolio Illiq and Market Returns
forv p = 1 / 5 {
	gen B4_P`p' = prm`p'_monthly_rm_P`p'_c / v1_Rm_PIm_Rm_PIm
}	

Tabstat B1* B2* B3* B4*

save Table2_Part1

*------------------------------END OF TABLR 2----------------------------------

*-----------------------------------------------------------------------------
*  GENERALIZED METHOD OF MOMEMNTS(GMM) REGRESSION FOR MAIN TESTING MODEL- TABLE9
*-----------------------------------------------------------------------------
*Please note that between the estimation of Betas and until the point of running
*GMM there is a lot of work and different tests, while also the need to bring
*back the Betas into the original file.

bys portf_illiq: egen Ec= mean(illiq_port_c)
egen h = mean( illiq_port_turn)

gen hEc= h*Ec
gen Ret_excess =  illiq_port_ret - hEc
gen B_net= B1 + B2 - B3 - B4


gmm (Ret_excess -      {cons}  -{lambda_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{free_h}*(Ec) - {lambda_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{lambda_1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{lambda_1}*(B1) - {lambda_net}*(B_net)), inst( B* B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{free_h}*(Ec)  -{lambda_1}*(B1) - {lambda_net}*(B_net)), inst( B_net B* Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{lambda_1}*(B1) - {lambda_net}*(B_net)), inst( B* B_net Ec) winit(identity)
gmm (Ret_excess -      {cons}  -{lambda_1}*(B1) - {lambda_2}*(B2) + {lambda_3}*(B3)\\\
+ {lambda_4}*(B4)), inst( B*) winit(identity)
gmm (illiq_port_ret -  {cons}  -{free_h}*(Ec) -{lambda1}*(B1) - {lambda_2}*(B2)\\\
 + {lambda3}*(B3) + {lambda_4}*(B4)), inst( B1 B2 B3 B4 Ec) winit(identity)

