	*NEW VERSION OF LIQUIDITY ADJUSTED CAPM- LIQUIDITY RISK AT OSLO STOCK EXCHANGE

use "C:\Users\ATazo\Desktop\LCAPM\OriginalData.dta",clear


/*We start off dropping the values and variables we do not need(we could have done
this from the start in Børsprosjektet, but I am writing this in retospect knowing which
variables was used and which were unimportant, in additon to what values to drop and etc.
*/

drop Symbol ISIN SecurityName Bid Offer Last Vwap logReturn OffTurnover logRegAdj

*Dropping zero issues
drop if SharesIssue == 0

*Drop 1th and 99th percentile of returns 
winsor2 ri, replace


*We dropp the smallest stocks
drop if AdjLast < 1
drop if AdjLast > 10000


* Creating monthly date in Stata format

gen mofd = mofd(date)

*Format data as monthly data

format mofd %tm

*Same for year
gen year = year(date)

format year %ty

*Must create a market portfolio in the start of the sample

* Create market capitalization for each firm in the sample

gen firm_marketcap = SharesIssue * AdjLast

* Finding market wide capitalization and sort it daily starting from 01.January 1998

bys date: egen marketVal= total(firm_marketcap)

*Generating equal weighted illiquidity portfolio
bys date: gen weight= firm_marketcap/ marketVal


* We need to lag marketval by one month( we use last months marketwide cap to make the measure of illiquidity stationary)

preserve
keep mofd marketVal
bys mofd: keep if _n == _N
tsset mofd
gen lag_marketVal = L.marketVal
drop marketVal
save temp, replace
restore
merge m:1 mofd using temp


* Create the variable PM - the ratio of lagged market value to first value of the market in the sample
sum date
loc firstDate = `r(min)'
sum marketVal if date == `firstDate'

gen PM = lag_marketVal / `r(mean)'
* drop uncessary variables
drop marketVal _merge
rm temp.dta


*Next we generate the dollar volume 
gen float dollar_volume = OffShareTurnover * AdjLast




bys SecuritiyID mofd : gen obs =_N
bys SecuritiyID mofd : egen z = count(returns) if returns==0
bys SecuritiyID mofd : egen zero = min(z)
gen pzret = zero / obs
drop if pzret>.8 & pzret!=.
drop  z
bys SecuritiyID mofd : egen sumtrades = count(date)
drop if sumtrades<15


*Scaling Amihud Illiquidity ratio to make it more presentable, 10^6, i.e NOK volume converted into millions

bys SecuritiyID date : gen double illiq = 1000000*(abs(returns)/(dollar_volume)) if dollar_volume>0

drop if illiq==.
bys SecuritiyID mofd : egen firm_Amihud = mean(illiq)


*we have to take into account extreme values, meaning we drop 1th and 99th quantile of the observations
winsor2 firm_Amihud, replace
drop obs zero pzret


*need to set normalization parameters so we dont get crazy results that are driven by extreme observations 
global ac = .25
global bc = .35
globa maxc = 30

gen c = $ac + ($bc * firm_Amihud) * PM

replace c = min(c, $maxc)

save "C:\Users\ATazo\Desktop\LCAPM\illiq.dta", replace
*So far so good, get same results each time

* We are now done creating Amihud Illiquidity for each firm in our sample, next step to find market illiquidity and market returns


/*---- Next step is creating 5 illiquidity portfolios for each year y during the
period of 01.January 1998 to 31. December 2017 by sorting stocks based on their price
at the beginning of each year, between NOK10 and NOK 10 000, and return and volume data
in year y-1 for at least 100 days
amount of stocks listed on OSE- but up for discussion in the end(well see)
Our first data is generated in year 1999 since we use last years data to balance out this
years results and so on. - For my main testing i only got ridd of stocks less than NOK 10
*/

use "C:\Users\ATazo\Desktop\LCAPM\illiq", clear

* First we start off by finding average annual illiquidity for each stock
bys SecuritiyID year: egen yearly_ILLIQ = mean(c)

*Next we find annual standard deviation of daily illiquidities for each stock
bys SecuritiyID year: egen yearly_SD_ILLIQ = sd(c)

bys SecuritiyID year: egen yearly_size = mean(firm_marketcap)

*Getting data to yearly frequency(_N means its yearly in this case because we sorted it on year) i.e reducing it to only yearly data
bys SecuritiyID year : keep if _n == _N 

*Here we have our illiquidity data
keep SecuritiyID year yearly_ILLIQ yearly_SD_ILLIQ yearly_size


*We need to lag the illiquidity variable by one year as stated in intro to this section
tsset SecuritiyID year

gen lag_illiq = L.yearly_ILLIQ

gen lag_SD_illiq = L.yearly_SD_ILLIQ



*We make 5 portfolios from our constructed illiquidity variable

bys year: astile portf_illiq = lag_illiq, nq(5)

bys year: astile portf_SD_illiq = lag_SD_illiq, nq(5)

bys year: astile portf_size = yearly_size, nq(5)
*save in temporary file
 save temp, replace
 
*Merge with our main dataset

use "C:\Users\ATazo\Desktop\LCAPM\illiq.dta",clear
merge m:1  SecuritiyID year using temp
drop _merge
 
 
 *Finding monthly returns for each stock in our data by using rolling window command(this makes the data stable over time and is most widely used method when dealing with financial data), we need to add 1 to get weekly, monthly and yearly returns, per usual cumulative returns for a month
bys SecuritiyID mofd: asrol returns, stat(product) add(1) gen(monthly_ret)
winsor2 monthly_ret, replace

*Cumulative Returns- Meaning the Momentum Factor- must first tsset data or xtset, does not work with bys
ascol returns, returns(simple) keep(all) tomonth
drop month_id
rename month_returns MOM

*Need to substract monthly Risk-free rate for the monthly returns
merge m:1 mofd using Risk_free_monthly
drop _merge
replace monthly_ret = monthly_ret - rf1m 
 
 * Find yearly standard deviation  for each stocks returns this is s.d(r^p)
bys SecuritiyID year: asrol returns, stat(sd)  gen(yearly_sd_firms)

*Monthly standard deviation for each stocks returns
bys SecuritiyID mofd: asrol returns, stat(sd) gen(monthly_sd_firms)

*Find monthly turnover for each stock
bys SecuritiyID mofd: asrol OffShareTurnover, stat(sum) gen(monthly_turnover)

*Converting monthly turnover to look more presentable, 10^6 i.e millions NOK
replace monthly_turnover = monthly_turnover / 10000000

save temp1, replace




* Creating Table 2-(Second Part)
use temp1, clear

*Portfolio returns meaning E(r^e,p), these are Equally- weighted

bys portf_illiq mofd: egen illiq_port_ret = mean(monthly_ret)

*Portfolio illiquidity meaning E(c^p), also Equally-weighted
bys portf_illiq  mofd: egen illiq_port_c = mean(c)


*Portfolio Standard Deviation, meaning yearly s.d(r^p,e)
bys portf_illiq  mofd: egen illiq_port_sd = mean(yearly_sd_firms)

*Portfolio Standard Deviation,monthly s.d(r^p)
bys portf_illiq mofd: egen illiq_port_sd_monthly = mean( monthly_sd_firms)

*Standard deviation of portfolio innovations, sd(c^p)
bys portf_illiq mofd: egen illiq_sd_innov= sd(c)

*Turnover for each portfolio sorted on motnhly bases
bys portf_illiq  mofd: egen illiq_port_turn = mean(monthly_turnover)


*Market Capitalization i.e Size sorted on monthly bases
bys portf_illiq mofd: egen illiq_port_mktcap = mean(firm_marketcap)

replace illiq_port_mktcap = illiq_port_mktcap / 1000000000

*Portfolio sorted MOMENTUM Factor
bys portf_illiq mofd: egen illiq_port_MOM= mean(MOM)

tabstat illiq_port_c illiq_sd_innov illiq_port_ret illiq_port_sd_monthly illiq_port_turn illiq_port_mktcap illiq_port_MOM, by(portf_illiq)



*----Market Illiquidity and Market Returns---
*Market illiquidity is a mean of cross-sectional illiquidity, so just simply average illiquidity each month for all firms, meaning sorted on monthly data
bys mofd: egen market_illiq= mean(illiq)
replace market_illiq= market_illiq * PM


* Monthly market returns ( 2 options, just mean all our returns, or use Ødegaards)--> we use Ødegaards
*bys mofd: egen monthly_rm = mean(monthly_ret)

merge m:1 mofd using MonthlyMarketReturns
rename ew monthly_rm_EW
rename vw monthly_rm_VW

drop _merge allshare obx 

save part1, replace



/* Find innovations, i.e residuals using AR(2) specification same as litterature
Reshape the data- each portfolio as a variables- 
we reshape into wide-data to make it easier to deal with going forward-
remember our standard model is equally-weighted
*/
keep portf_illiq mofd year PM market_illiq monthly_rm_EW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

save "C:\Users\ATazo\Desktop\LCAPM\portf_illiq_long.dta", replace

use"C:\Users\ATazo\Desktop\LCAPM\portf_illiq_long.dta", clear

bys portf_illiq mofd: keep if _n==_N

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


/*--- ESTIMATING INNOVATIONS IN ILLIQUIDITY
for alle portfolios:
marketPort
IlliquidPort

We start off the Market Portfolios, our model is standarzied in value-weight, 
but we need to make market illiquidity equal weight to reduce effects of large liquid securities

market_Port_ewmcInnov
market_Port_vwmcInnov
*/

use "C:\Users\ATazo\Desktop\LCAPM\illiq_portf.dta", clear
tsset mofd

rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = ($maxc - $ac)/($bc * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)




forv p = 1 / 5 {

	gen P`p'_cs = (30 - 0.25)/(0.30 * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)





*From HERE 

* PREDICT MARKET ILLIQUIDITY using AR(2) specification

gen y  = 0.25 + 0.30*market_illiq*PM

gen x1 = 0.25 + 0.30 * L.market_illiq * PM

gen x2 = 0.25 + 0.30* L2.market_illiq * PM

reg y x1 x2


predict marketPort_ewmcInnov_res,res

*Find innovations in market portfolio returns
reg monthly_rm_EW L.monthly_rm_EW L2.monthly_rm_EW
predict marketPort_ewmretInnov_res,res


/*These are just standard stationarity tests
tsline marketPort_ewmcInnov_res
corrgram marketPort_ewmcInnov_res
ac marketPort_ewmcInnov_res
dfuller marketPort_ewmcInnov_res, noconst regress

dfuller market_illiq, trend regress lags(2)
corrgram market_illiq


actest
tsline marketPort_ewmretInnov_res
arima market_illiq, ar(1/2) ma(1/2)
predict inv_market_illiq, res
*/

forv p= 1/5{
arima P`p'_cs, ar(2)
predict P`p'_c_innov, res
}


* Generate the denominator in equation 13-16
gen adjRetInnov_res = marketPort_ewmretInnov_res - marketPort_ewmcInnov_res

/*
* Find variance of this term
cor adjRetInnov_res, cov
loc var_adjRetInno = `r(Var_1)'

* Find variance of market return innovations
cor marketPort_ewmretInnov_res, cov
loc var_market_retu_inn = `r(Var_1)'


* estimate a regression of portfolio returns with the market innovations in retr
reg P1_ret marketPort_ewmretInnov_res

* Find beta1 by replacing the beta from the above regression with covariance of adjRetInnov_res
gen B1__P1 = _b[marketPort_ewmretInnov_res] * `var_market_retu_inn' / `var_adjRetInno'
*/



*We need a rolling variance for each month of our sample based on INNOVATIONS- GET IT!
*To HERE is irrelevant for my thesis- Tried AR(2), but didnt work, so went back after turning in my thesis to see if why.


asrol P1_c_innov, stat(sd) window(mofd 2)
forv p=2/5{
asrol P`p'_c_innov, stat(sd) window(mofd 2)
}

*forv p=1/5{
*replace sd2_P`p'_c_innov = sd2_P`p'_c_innov^2
*}

asrol marketPort_ewmcInnov_res, stat(sd) window(mofd 2)
*replace sd2_marketPort_ewmcInnov_res = sd2_marketPort_ewmcInnov_res^2

asrol marketPort_ewmretInnov_res, stat(sd) window(mofd 2)
*replace sd2_marketPort_ewmretInnov_res= sd2_marketPort_ewmretInnov_res^2

asrol adjRetInnov_res, stat(sd) window(mofd 2)
*replace  sd2_adjRetInnov_res=  sd2_adjRetInnov_res^2

forv p=1/5{
asrol P`p'_ret, stat(sd) window(mofd 2)
}

*forv p=1/5{
*replace sd2_P`p'_ret = sd2_P`p'_ret^2
*}



gen B1_P1= (sd2_P1_ret * sd2_marketPort_ewmretInnov_res)/sd2_adjRetInnov_res
forv p=2/5 {
gen B1_P`p'= (sd2_P`p'_ret * sd2_marketPort_ewmretInnov_res)/sd2_adjRetInnov_res
}

forv p=1/5{
gen B2_P`p'= (sd2_P`p'_c * sd2_marketPort_ewmcInnov_res)/sd2_adjRetInnov_res
}
forv p=1/5{
gen B3_P`p'= (sd2_P`p'_ret* sd2_marketPort_ewmcInnov_res)/sd2_adjRetInnov_res
}

forv p=1/5{
gen B4_P`p'= (sd2_P`p'_c* sd2_marketPort_ewmretInnov_res)/sd2_adjRetInnov_res
}





*INNOVATIONS OF ILLIQUIDITY PORTFOLIOS- SINCE USING AR(2) leads to failure of convergence, we use MGARCH and derive the static version- This is first part of Table2
*****************************************************/


*Conditional Covariance of Portfolio Returns with Market
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


	
	gen Rm_PIm = monthly_rm_EW - market_illiq
	
	
	
	*Covariance of Portfolio Illiquidity with Market Illiqudity
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

	*Covariance of Portfolio Return with Market Illiqudity
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

*Covariance of Portfolio illiquidity with Market Returns
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_EW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	

* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	*Risk premium betas

*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_EW_P`p'_ret / v1_Rm_PIm_Rm_PIm
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
	gen B4_P`p' = prm`p'_monthly_rm_EW_P`p'_c / v1_Rm_PIm_Rm_PIm
}

asdoc sum B1* B2* B3* B4*, stat(mean tstat) replace
save part2, replace



*--------------------------------------------------------------------------------------
/*If you have reached this far, then 90% of your work is done, from now on you only
paste the same code with different assumptions to see if things change and if you get different
results and so on. Basicly, producing the code above took almost all of the time, while
the code below is simply copy paste of code above with some changes. This is standard
academic researching, where the start and middle is hardest and ending is easy. If you
start off slow and try to understand each part, and implement things correctly, that is
the most vital part. Your role as analyst is simply these steps: 
1. Gather Data, cleanse it and drop what you dont need and keep what you need
2. Create the ratios and variables you need based on your data
3. Run a regression or any statistical model and see if the dependent variable is 
affected by the independent variables and so on.. 

REMEMBER : HARDEST PART IS NOT KNOWING WHERE TO START, after that you solve one problem at the time and before you know it you are done. 
Good luck to you T-Dawgh. I am writing this 20.05.2019 and hopefully you understand some of
the struggle that went into this by the time you ask me for this code. PS: Hows my job situation doing in the future? If I am still trapped in the rat race, plz remind me of this message.

*/

*-------------------------------------------------------------------------------------------


keep mofd year P*_ret P*_c P*_sd P*_turn P*_mcap market_illiq_raw monthly_rm_EW market_illiq B*_P*

*Correlations of Betas and between Market Illiquidity and Market Returns- Table5 to Table8
egen B1_mean = rowmean(B1_P*)
bys year: egen B1_yearly= mean(B1_mean)

egen B2_mean= rowmean(B2_P*)
bys year: egen B2_yearly = mean(B2_mean)

egen B3_mean= rowmean(B3_P*)
bys year: egen B3_yearly= mean( B3_mean)

egen B4_mean= rowmean(B4_P*)
bys year: egen B4_yearly = mean(B4_mean)

*Annualized for betas in each portfolio
corr B*_yearly
*Monthly correlation
corr B*_mean

*Correlation between portfolio illiquidity and market illiquidity
corr P*_c market_illiq

*Correlation between returns of portfolios and market illiquidity
corr P*_ret market_illiq

*Correlation between portfolio illiquidity and market returns
corr P*_c monthly_rm_EW

*Correlation between portfolio returns and market returns
corr P*_ret monthly_rm_EW

save part3, replace

*Next we bring back our wide format into long format i.e panel data where we will use
*cross-sectionally regression to obtain our R^2

use part3, clear
forv p=1/5{
preserve
keep year mofd B1_P`p'
rename B1_P`p' B1
gen portf_illiq =`p'
save beta1_p`p', replace
restore
}

forv p=1/5{
preserve
keep year mofd B2_P`p'
rename B2_P`p' B2
gen portf_illiq =`p'
save beta2_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B3_P`p'
rename B3_P`p' B3
gen portf_illiq =`p'
save beta3_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B4_P`p'
rename B4_P`p' B4
gen portf_illiq =`p'
save beta4_p`p', replace
restore
}

use beta1_p1, clear
append using beta1_p2 beta1_p3 beta1_p4 beta1_p5
save B1, replace

use beta2_p1,clear
append using beta2_p2 beta2_p3 beta2_p4 beta2_p5
save B2, replace

use beta3_p1,clear
append using beta3_p2 beta3_p3 beta3_p4 beta3_p5
save B3,replace

use beta4_p1,clear
append using beta4_p2 beta4_p3 beta4_p4 beta4_p5
save B4,replace


*Next we need our portfolio returns and monthly returns and then merge with Betas

use part1, clear
bys portf_illiq mofd: keep if _n==_N
keep portf_illiq mofd weight year monthly_ret MOM illiq_port_ret illiq_port_c illiq_port_turn illiq_port_mktcap
merge m:1 portf_illiq mofd using B1
drop _merge
merge m:1 portf_illiq mofd using B2

drop _merge
merge m:1 portf_illiq mofd using B3

drop _merge
merge m:1 portf_illiq mofd using B4

drop _merge

xtset portf_illiq mofd

save part4, replace



*Next step is to run our GMM Model Cross-sectioanlly- Examining relation between Returns and Liquidity Risk

use part4, clear
*Correlation between individual stocks
corr B1 B2 B3 B4


bys portf_illiq: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0153

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4


gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1 B2 B3 B4) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B_net)), inst( B1 B2 B3 B4) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam2}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)

gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)

gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)



*Our Primary GMM-model with correctly specified instruments, 
*equal to Fama and Mcbeth and Cross-sectional regression
*This is Table9
gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B* B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B_net B* kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B* B_net kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B*)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc)





*The GMM Model below is wrong- the INSTRUMENTS are not classified in correct form, which gives false z- and P-values, specifiying correct instruments will lead to somewhat similar results for regression, cross-regression, 2stepRegression, and regular GMM

use part2, clear
drop h1_P1_ret_P1_ret-v1_Rm_PIm_Rm_PIm

gen k= 0.0153

forv p=1/5{
egen P`p'_c_mean = mean(P`p'_c)
}
forv p=1/5{
gen kEc`p'= k* P`p'_c_mean
}

forv p=1/5{
gen P`p'_ex= P`p'_ret - kEc`p'
}

forv p=1/5{
gen B_net_P`p'= B1_P`p' + B2_P`p' - B3_P`p' - B4_P`p'
}

gmm ( P1_ex  -{cons} -{gam_net}*(B_net_P1))( P2_ex -{cons} - {gam_net}*(B_net_P2))( P3_ex -{cons} - {gam_net}*(B_net_P3))( P4_ex -{cons} - {gam_net}*(B_net_P4))( P5_ex -{cons} - {gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean) - {gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean) - {gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean) - {gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean) - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1))( P2_ret -{cons} - {gam1}*(B1_P2))( P3_ret -{cons} - {gam1}*(B1_P3))( P4_ret -{cons} - {gam1}*(B1_P4))( P5_ret -{cons} - {gam1}*(B1_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ex  -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ex  -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ex  -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ex  -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ex  -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean)- {gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean)- {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean)- {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean)- {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean)- {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

*Long data better for Equation 7 and 8
gmm( P1_ex -{cons} - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ex -{cons} - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ex -{cons} - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ex -{cons} - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ex -{cons} - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

gmm( P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ret -{cons}-{gam0}*(P2_c_mean)  - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ret -{cons}-{gam0}*(P3_c_mean)  - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ret -{cons}-{gam0}*(P4_c_mean)  - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ret -{cons}-{gam0}*(P5_c_mean)  - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P*)winit(identity)


*Running single cross-sectional regression to get R^2 --> get almost 0 in all cases, we have no use for them in thesis
use part4,clear

bys portf_illiq: egen Ec= mean(illiq_port_c)
gen B_net = B1+B2-B3-B4
gen k= 0.0153


xtreg illiq_port_ret kEc  B_net, fe
xtreg illiq_port_ret Ec B_net

xtreg illiq_port_ret B1

xtreg illiq_port_ret kEc B1 B_net, fe
xtreg illiq_port_ret kEc B1 B_net
xtreg illiq_port_ret B1 B_net

xtreg illiq_port_ret kEc B1 B2 B3 B4,fe
xtreg illiq_port_ret Ec B1 B2 B3 B4




*Volatility of Illiquidity Innovations, Table10 (Using same notations to not fk up the long as model). Btw- for some reason my portfolios become upside down, meaning most liquid in Portfolio 5 and least liquid in Portfolio 1, I donno why, and I did not have time to investigate further. As long as you know thats the case it does not matter too much but was confusing.
*--------------------------------------------------------------------------------------*

* Creating Table 3- Results are the same as for regular Illiquidity
use temp1, clear

*Portfolio returns meaning E(r^e,p), these are Equally- weighted

bys portf_SD_illiq mofd: egen illiq_port_ret = mean(monthly_ret)

*Portfolio illiquidity meaning E(c^p), also Equally-weighted
bys portf_SD_illiq  mofd: egen illiq_port_c = mean(c)


*Portfolio Standard Deviation, meaning yearly s.d(r^p,e)
bys portf_SD_illiq  mofd: egen illiq_port_sd = mean(yearly_sd_firms)

*Portfolio Standard Deviation,monthly s.d(r^p)
bys portf_SD_illiq mofd: egen illiq_port_sd_monthly = mean( monthly_sd_firms)

*Standard deviation of portfolio innovations, sd(c^p)
bys portf_SD_illiq mofd: egen illiq_sd_innov= sd(c)

*Turnover for each portfolio sorted on motnhly bases
bys portf_SD_illiq  mofd: egen illiq_port_turn = mean(monthly_turnover)


*Market Capitalization i.e Size sorted on monthly bases
bys portf_SD_illiq mofd: egen illiq_port_mktcap = mean(firm_marketcap)

replace illiq_port_mktcap = illiq_port_mktcap / 1000000000

tabstat illiq_port_c illiq_sd_innov illiq_port_ret illiq_port_sd_monthly illiq_port_turn illiq_port_mktcap, by(portf_SD_illiq)

*----Market Illiquidity and Market Returns---


*Market illiquidity is a mean of cross-sectional illiquidity, so just simply average illiquidity each month for all firms, meaning sorted on monthly data
bys mofd: egen market_illiq= mean(illiq)
replace market_illiq= market_illiq * PM


* Monthly market returns ( 2 options, just mean all our returns, or use Ødegaards)--> we use Ødegaards
*bys mofd: egen monthly_rm = mean(monthly_ret)

merge m:1 mofd using MonthlyMarketReturns
rename ew monthly_rm_EW
rename vw monthly_rm_VW

drop _merge allshare obx 

save part1.1, replace

use part1.1, clear 

keep portf_SD_illiq mofd year PM market_illiq monthly_rm_EW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

bys portf_SD_illiq mofd: keep if _n==_N



preserve
keep if portf_SD_illiq == 1

rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P1_ret P1_c  P1_sd P1_turn P1_mcap)
save p1, replace
restore

drop PM market_illiq monthly_rm year


forv i = 2 / 5 {
preserve
keep if portf_SD_illiq == `i'
* rename all variables
rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P`i'_ret P`i'_c  P`i'_sd P`i'_turn P`i'_mcap)
 save p`i', replace
restore
}

use "C:\Users\ATazo\Desktop\LCAPM\p1.dta",clear
drop portf_SD_illiq
forv i = 2 / 5 {
merge 1:1 mofd using p`i'
cap drop _merge
cap drop portf_SD_illiq
}


tsset mofd

rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = (30 - 0.25)/(0.30 * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)




*Conditional Covariance of Portfolio Returns with Market
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


	
	gen Rm_PIm = monthly_rm_EW - market_illiq
	
	
	
	*Covariance of Portfolio Illiquidity with Market Illiqudity
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

	*Covariance of Portfolio Return with Market Illiqudity
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

*Covariance of Portfolio illiquidity with Market Returns
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_EW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	

* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	*Risk premium betas

*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_EW_P`p'_ret / v1_Rm_PIm_Rm_PIm
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
	gen B4_P`p' = prm`p'_monthly_rm_EW_P`p'_c / v1_Rm_PIm_Rm_PIm
}

asdoc sum B1* B2* B3* B4*, stat(mean tstat) replace

save part2.1, replace


*Asset Pricing Model Testing- Volatility Illiquidity Innovations Sorted Portfolios
use part2.1, clear

forv p=1/5{
preserve
keep year mofd B1_P`p'
rename B1_P`p' B1
gen portf_SD_illiq =`p'
save beta1_p`p', replace
restore
}

forv p=1/5{
preserve
keep year mofd B2_P`p'
rename B2_P`p' B2
gen portf_SD_illiq =`p'
save beta2_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B3_P`p'
rename B3_P`p' B3
gen portf_SD_illiq =`p'
save beta3_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B4_P`p'
rename B4_P`p' B4
gen portf_SD_illiq =`p'
save beta4_p`p', replace
restore
}

use beta1_p1, clear
append using beta1_p2 beta1_p3 beta1_p4 beta1_p5
save B1, replace

use beta2_p1,clear
append using beta2_p2 beta2_p3 beta2_p4 beta2_p5
save B2, replace

use beta3_p1,clear
append using beta3_p2 beta3_p3 beta3_p4 beta3_p5
save B3,replace

use beta4_p1,clear
append using beta4_p2 beta4_p3 beta4_p4 beta4_p5
save B4,replace


use part1.1, clear

bys portf_SD_illiq mofd: keep if _n==_N
keep portf_SD_illiq mofd year monthly_ret illiq_port_ret illiq_port_c illiq_port_turn
merge m:1 portf_SD_illiq mofd using B1
drop _merge
merge m:1 portf_SD_illiq mofd using B2

drop _merge
merge m:1 portf_SD_illiq mofd using B3

drop _merge
merge m:1 portf_SD_illiq mofd using B4

drop _merge

xtset portf_SD_illiq mofd

save part4.1, replace


use part4.1, clear
bys portf_SD_illiq: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0153

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4


gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B*) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1 B2 B3 B4) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B_net)), inst( B1 B2 B3 B4) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)

*GMM with correctly classified instruments- TABLE 10
gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)



*My old GMM Model- IGNORE IT or Dont, but the instruments are not correctly classified
use part2.1, clear

gen k= 0.0153

forv p=1/5{
egen P`p'_c_mean = mean(P`p'_c)
}
forv p=1/5{
gen kEc`p'= k* P`p'_c_mean
}

forv p=1/5{
gen P`p'_ex= P`p'_ret - kEc`p'
}

forv p=1/5{
gen B_net_P`p'= B1_P`p' + B2_P`p' - B3_P`p' - B4_P`p'
}

gmm ( P1_ex  -{cons} -{gam_net}*(B_net_P1))( P2_ex -{cons} - {gam_net}*(B_net_P2))( P3_ex -{cons} - {gam_net}*(B_net_P3))( P4_ex -{cons} - {gam_net}*(B_net_P4))( P5_ex -{cons} - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean) - {gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean) - {gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean) - {gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean) - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1))( P2_ret -{cons} - {gam1}*(B1_P2))( P3_ret -{cons} - {gam1}*(B1_P3))( P4_ret -{cons} - {gam1}*(B1_P4))( P5_ret -{cons} - {gam1}*(B1_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ex  -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ex  -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ex  -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ex  -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ex  -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean)- {gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean)- {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean)- {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean)- {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean)- {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

*Long data better for Equation 7 and 8
gmm (P1_ex -{cons} - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ex -{cons} - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ex -{cons} - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ex -{cons} - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ex -{cons} - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

gmm (P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ret -{cons}-{gam0}*(P2_c_mean)  - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ret -{cons}-{gam0}*(P3_c_mean)  - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ret -{cons}-{gam0}*(P4_c_mean)  - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ret -{cons}-{gam0}*(P5_c_mean)  - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P*)winit(identity)





* --Value-Weighted Illiquidity portfolios and Equal Weighted Market--Table 3--
*------------------------------------------------------------------------------*
use temp1, clear
*Must also create Value-weighted returns
bys portf_illiq mofd: egen illiq_port_ret = sum(weight*monthly_ret)

*Portfolios illiquidity Value-weighted and Value-weighted Market Portfolios
bys portf_illiq mofd: egen illiq_port_c = sum(weight*c)

**Portfolio Standard Deviation, meaning yearly s.d(r^p,e), VW
bys portf_illiq mofd: egen illiq_port_sd= sum(yearly_sd_firms*weight)

**Portfolio Standard Deviation,monthly s.d(r^p), VW
bys portf_illiq mofd: egen illiq_port_sd_monthly_VW = sum( monthly_sd_firms*weight)

*VW sd(c^p)
bys portf_illiq mofd: egen illiq_sd_innov= sd(c * weight)


*Turnover for each portfolio sorted on motnhly bases
bys portf_illiq  mofd: egen illiq_port_turn = mean(monthly_turnover)


*Market Capitalization i.e Size sorted on monthly bases
bys portf_illiq mofd: egen illiq_port_mktcap = mean(firm_marketcap)

replace illiq_port_mktcap = illiq_port_mktcap / 1000000000


tabstat illiq_port_c illiq_sd_innov illiq_port_ret illiq_port_sd_monthly illiq_port_turn illiq_port_mktcap, by(portf_illiq)

*----Market Illiquidity and Market Returns---


*Market illiquidity is a mean of cross-sectional illiquidity, so just simply average illiquidity each month for all firms, meaning sorted on monthly data
bys mofd: egen market_illiq= mean(illiq)
replace market_illiq= market_illiq * PM


* Monthly market returns ( 2 options, just mean all our returns, or use Ødegaards)--> we use Ødegaards
*bys mofd: egen monthly_rm = mean(monthly_ret)

merge m:1 mofd using MonthlyMarketReturns
rename ew monthly_rm_EW
rename vw monthly_rm_VW

drop _merge allshare obx 

save part1.2 , replace


keep portf_illiq mofd year PM market_illiq monthly_rm_EW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

bys portf_illiq mofd: keep if _n==_N



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


tsset mofd

rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = (30 - 0.25)/(0.30 * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)




*Conditional Covariance of Portfolio Returns with Market
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


	
	gen Rm_PIm = monthly_rm_EW - market_illiq
	
	
	
	*Covariance of Portfolio Illiquidity with Market Illiqudity
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

	*Covariance of Portfolio Return with Market Illiqudity
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

*Covariance of Portfolio illiquidity with Market Returns
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_EW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	

* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	*Risk premium betas

*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_EW_P`p'_ret / v1_Rm_PIm_Rm_PIm
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
	gen B4_P`p' = prm`p'_monthly_rm_EW_P`p'_c / v1_Rm_PIm_Rm_PIm
}

asdoc sum B1* B2* B3* B4*, stat(mean tstat) replace

save part2.2, replace


use part2.2, clear


forv p=1/5{
preserve
keep year mofd B1_P`p'
rename B1_P`p' B1
gen portf_illiq =`p'
save beta1_p`p', replace
restore
}

forv p=1/5{
preserve
keep year mofd B2_P`p'
rename B2_P`p' B2
gen portf_illiq =`p'
save beta2_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B3_P`p'
rename B3_P`p' B3
gen portf_illiq =`p'
save beta3_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B4_P`p'
rename B4_P`p' B4
gen portf_illiq =`p'
save beta4_p`p', replace
restore
}

use beta1_p1, clear
append using beta1_p2 beta1_p3 beta1_p4 beta1_p5
save B1, replace

use beta2_p1,clear
append using beta2_p2 beta2_p3 beta2_p4 beta2_p5
save B2, replace

use beta3_p1,clear
append using beta3_p2 beta3_p3 beta3_p4 beta3_p5
save B3,replace

use beta4_p1,clear
append using beta4_p2 beta4_p3 beta4_p4 beta4_p5
save B4,replace


use part1.2, clear

bys portf_illiq mofd: keep if _n==_N
keep portf_illiq mofd year monthly_ret illiq_port_ret illiq_port_c illiq_port_turn
merge m:1 portf_illiq mofd using B1
drop _merge
merge m:1 portf_illiq mofd using B2

drop _merge
merge m:1 portf_illiq mofd using B3

drop _merge
merge m:1 portf_illiq mofd using B4

drop _merge

xtset portf_illiq mofd

save part4.2, replace


*Asset Pricing Model Testing- Value-Weighted Portfolios and Equal-Weighted Market

use part4.2, clear
bys portf_illiq: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0153

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4


gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B*) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)



*Different version of GMM above - think this is the right version with right classified instruments--> gives same answers as above
*Table 11
gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)



*My Old model, which performs best but wayyyy too overidentified
use part2.2, clear

gen k= 0.0153

forv p=1/5{
egen P`p'_c_mean = mean(P`p'_c)
}
forv p=1/5{
gen kEc`p'= k* P`p'_c_mean
}

forv p=1/5{
gen P`p'_ex= P`p'_ret - kEc`p'
}

forv p=1/5{
gen B_net_P`p'= B1_P`p' + B2_P`p' - B3_P`p' - B4_P`p'
}

gmm ( P1_ex  -{cons} -{gam_net}*(B_net_P1))( P2_ex -{cons} - {gam_net}*(B_net_P2))( P3_ex -{cons} - {gam_net}*(B_net_P3))( P4_ex -{cons} - {gam_net}*(B_net_P4))( P5_ex -{cons} - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean) - {gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean) - {gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean) - {gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean) - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1))( P2_ret -{cons} - {gam1}*(B1_P2))( P3_ret -{cons} - {gam1}*(B1_P3))( P4_ret -{cons} - {gam1}*(B1_P4))( P5_ret -{cons} - {gam1}*(B1_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ex  -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ex  -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ex  -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ex  -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ex  -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean)- {gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean)- {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean)- {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean)- {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean)- {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

*Long data better for Equation 7 and 8
gmm (P1_ex -{cons} - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ex -{cons} - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ex -{cons} - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ex -{cons} - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ex -{cons} - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

gmm (P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ret -{cons}-{gam0}*(P2_c_mean)  - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ret -{cons}-{gam0}*(P3_c_mean)  - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ret -{cons}-{gam0}*(P4_c_mean)  - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ret -{cons}-{gam0}*(P5_c_mean)  - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P*)winit(identity)





*Equally-Weighted Portfolios and Value-Weighted Market Portfolio 
*-----------------------------------------------------------------------------------*
use part1, clear


keep portf_illiq mofd year PM market_illiq monthly_rm_VW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

bys portf_illiq mofd: keep if _n==_N



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


tsset mofd

rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = (30 - 0.25)/(0.30 * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq_EW = rowmean(P*_cs)

egen marketVal = rowtotal(P*_mcap)
forv p= 1/5{
gen weight_P`p' = P`p'_mcap/ marketVal
}

forv p= 1/5{

gen P`p'_cs_w = P`p'_cs * weight_P`p'
}

egen market_illiq_VW= rowtotal( P*_cs_w)

drop market_illiq_EW-P5_cs_w

rename market_illiq_VW market_illiq



*Conditional Covariance of Portfolio Returns with Market
	cap drop  h1_* h2_* h3_* h4_*
	mgarch dvech (P1_ret monthly_rm_VW =), arch(1) iterate(100)
	predict h1* if e(sample), variance
	
 	mgarch dvech (P2_ret monthly_rm_VW =), arch(1/2) iterate(100)
	predict h2* if e(sample), variance

	mgarch dvech (P3_ret monthly_rm_VW =), arch(1) iterate(100)
	predict h3* if e(sample), variance

	mgarch dvech (P4_ret monthly_rm_VW =), arch(1) iterate(100)
	predict h4* if e(sample), variance
	
	mgarch dvech (P5_ret monthly_rm_VW =), arch(1) iterate(100)
	predict h5* if e(sample), variance


	
	gen Rm_PIm = monthly_rm_VW - market_illiq
	
	
	
	*Covariance of Portfolio Illiquidity with Market Illiqudity
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

	*Covariance of Portfolio Return with Market Illiqudity
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

*Covariance of Portfolio illiquidity with Market Returns
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_VW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	

* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	*Risk premium betas

*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_VW_P`p'_ret / v1_Rm_PIm_Rm_PIm
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
	gen B4_P`p' = prm`p'_monthly_rm_VW_P`p'_c / v1_Rm_PIm_Rm_PIm
}

asdoc sum B1* B2* B3* B4*, stat(mean tstat) replace

save part2.4, replace


forv p=1/5{
preserve
keep year mofd B1_P`p'
rename B1_P`p' B1
gen portf_illiq =`p'
save beta1_p`p', replace
restore
}

forv p=1/5{
preserve
keep year mofd B2_P`p'
rename B2_P`p' B2
gen portf_illiq =`p'
save beta2_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B3_P`p'
rename B3_P`p' B3
gen portf_illiq =`p'
save beta3_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B4_P`p'
rename B4_P`p' B4
gen portf_illiq =`p'
save beta4_p`p', replace
restore
}

use beta1_p1, clear
append using beta1_p2 beta1_p3 beta1_p4 beta1_p5
save B1, replace

use beta2_p1,clear
append using beta2_p2 beta2_p3 beta2_p4 beta2_p5
save B2, replace

use beta3_p1,clear
append using beta3_p2 beta3_p3 beta3_p4 beta3_p5
save B3,replace

use beta4_p1,clear
append using beta4_p2 beta4_p3 beta4_p4 beta4_p5
save B4,replace


*Next we need our portfolio returns and monthly returns and then merge with Betas

use part1, clear
bys portf_illiq mofd: keep if _n==_N
keep portf_illiq mofd year monthly_ret illiq_port_ret illiq_port_c illiq_port_turn
merge m:1 portf_illiq mofd using B1
drop _merge
merge m:1 portf_illiq mofd using B2

drop _merge
merge m:1 portf_illiq mofd using B3

drop _merge
merge m:1 portf_illiq mofd using B4

drop _merge

xtset portf_illiq mofd

save part4.4, replace


*ASSET PRICING TESTING
use part4.4, clear
bys portf_illiq: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0153

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4



*THIS IS THE MAIN GMM TESTING MODEL- equalivent to cross-sectionally regssion or Fama and Mcbeth, but GMM allows serial correlation in error term and take into account pre-estimation of betas
*Table 12
gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B_net B1 kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B1 B_net kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B*) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc) winit(identity)



use part2.4, clear
drop h1_P1_ret_P1_ret-v1_Rm_PIm_Rm_PIm

gen k= 0.0153

forv p=1/5{
egen P`p'_c_mean = mean(P`p'_c)
}
forv p=1/5{
gen kEc`p'= k* P`p'_c_mean
}

forv p=1/5{
gen P`p'_ex= P`p'_ret - kEc`p'
}

forv p=1/5{
gen B_net_P`p'= B1_P`p' + B2_P`p' - B3_P`p' - B4_P`p'
}

/*We ignore this model, produces same results as the long data when instruments are 
classified correctly(beware: instruments are NOT correctly clasiffied here, 
therefor leading too overidenfied model leading to false z-statistics and P-values
--> IF YOU CLASSIFY THE INSTRUMENTS CORRECTLY FOR EACH MODEL, IT WILL PRODUCE SAME RESULTS
AS ABOVE GMM and xtRegression(cross-sectional regression)
*/
gmm ( P1_ex  -{cons} -{gam_net}*(B_net_P1))( P2_ex -{cons} - {gam_net}*(B_net_P2))( P3_ex -{cons} - {gam_net}*(B_net_P3))( P4_ex -{cons} - {gam_net}*(B_net_P4))( P5_ex -{cons} - {gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean) - {gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean) - {gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean) - {gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean) - {gam_net}*(B_net_P5)), inst(B_net_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1))( P2_ret -{cons} - {gam1}*(B1_P2))( P3_ret -{cons} - {gam1}*(B1_P3))( P4_ret -{cons} - {gam1}*(B1_P4))( P5_ret -{cons} - {gam1}*(B1_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ex  -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ex  -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ex  -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ex  -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ex  -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)), inst(B1_P* B2_P* B3_P* B4_P*) winit(identity)
gmm ( P1_ret -{cons} -{gam0}*(P1_c_mean)- {gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} -{gam0}*(P2_c_mean)- {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} -{gam0}*(P3_c_mean)- {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} -{gam0}*(P4_c_mean)- {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} -{gam0}*(P5_c_mean)- {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)
gmm ( P1_ret -{cons} -{gam1}*(B1_P1) -{gam_net}*(B_net_P1))( P2_ret -{cons} - {gam1}*(B1_P2) -{gam_net}*(B_net_P2))( P3_ret -{cons} - {gam1}*(B1_P3) -{gam_net}*(B_net_P3))( P4_ret -{cons} - {gam1}*(B1_P4) -{gam_net}*(B_net_P4))( P5_ret -{cons} - {gam1}*(B1_P5) -{gam_net}*(B_net_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

*Long data better for Equation 7 and 8
gmm (P1_ex -{cons} - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ex -{cons} - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ex -{cons} - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ex -{cons} - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ex -{cons} - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P* )winit(identity)

gmm (P1_ret -{cons} -{gam0}*(P1_c_mean) - {gam1}*(B1_P1) - {gam2}*(B2_P1) + {gam3}*(B3_P1) + {gam4}*(B4_P1))( P2_ret -{cons}-{gam0}*(P2_c_mean)  - {gam1}*(B1_P2) - {gam2}*(B2_P2) + {gam3}*(B3_P2) + {gam4}*(B4_P2))( P3_ret -{cons}-{gam0}*(P3_c_mean)  - {gam1}*(B1_P3) - {gam2}*(B2_P3) + {gam3}*(B3_P3) + {gam4}*(B4_P3))( P4_ret -{cons}-{gam0}*(P4_c_mean)  - {gam1}*(B1_P4) - {gam2}*(B2_P4) + {gam3}*(B3_P4) + {gam4}*(B4_P4))( P5_ret -{cons}-{gam0}*(P5_c_mean)  - {gam1}*(B1_P5) - {gam2}*(B2_P5) + {gam3}*(B3_P5) + {gam4}*(B4_P5)),inst(B1_P* B2_P* B3_P* B4_P*)winit(identity)





*SIZE Sorted Portfolios- Table 4 (The reason I dont change names is because i simply can copy paste the code below to create the sorted table
*--------------------------------------------------------------------------------**
use temp1, clear
bys portf_size mofd: egen illiq_port_ret = mean(monthly_ret)

*Portfolio illiquidity meaning E(c^p), also Equally-weighted
bys portf_size  mofd: egen illiq_port_c = mean(c)


*Portfolio Standard Deviation, meaning yearly s.d(r^p,e)
bys portf_size  mofd: egen illiq_port_sd = mean(yearly_sd_firms)

*Portfolio Standard Deviation,monthly s.d(r^p)
bys portf_size mofd: egen illiq_port_sd_monthly = mean( monthly_sd_firms)

*Standard deviation of portfolio innovations, sd(c^p)
bys portf_size mofd: egen illiq_sd_innov= sd(c)

*Turnover for each portfolio sorted on motnhly bases
bys portf_size  mofd: egen illiq_port_turn = mean(monthly_turnover)


*Market Capitalization i.e Size sorted on monthly bases
bys portf_size mofd: egen illiq_port_mktcap = mean(firm_marketcap)

replace illiq_port_mktcap = illiq_port_mktcap / 1000000000


tabstat illiq_port_c illiq_sd_innov illiq_port_ret illiq_port_sd_monthly illiq_port_turn illiq_port_mktcap, by(portf_size)

*----Market Illiquidity and Market Returns---


*Market illiquidity is a mean of cross-sectional illiquidity, so just simply average illiquidity each month for all firms, meaning sorted on monthly data
bys mofd: egen market_illiq= mean(illiq)
replace market_illiq= market_illiq * PM


* Monthly market returns ( 2 options, just mean all our returns, or use Ødegaards)--> we use Ødegaards
*bys mofd: egen monthly_rm = mean(monthly_ret)

merge m:1 mofd using MonthlyMarketReturns
rename ew monthly_rm_EW
rename vw monthly_rm_VW

drop _merge allshare obx 

save part1.3, replace

keep portf_size mofd year PM market_illiq monthly_rm_EW illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap

bys portf_size mofd: keep if _n==_N



preserve
keep if portf_size == 1

rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P1_ret P1_c  P1_sd P1_turn P1_mcap)
save p1, replace
restore

drop PM market_illiq monthly_rm year


forv i = 2 / 5 {
preserve
keep if portf_size == `i'
* rename all variables
rename (illiq_port_ret illiq_port_c illiq_port_sd illiq_port_turn illiq_port_mktcap) ///
		(P`i'_ret P`i'_c  P`i'_sd P`i'_turn P`i'_mcap)
 save p`i', replace
restore
}

use "C:\Users\ATazo\Desktop\LCAPM\p1.dta",clear
drop portf_size
forv i = 2 / 5 {
merge 1:1 mofd using p`i'
cap drop _merge
cap drop portf_size
}


tsset mofd

rename market_illiq market_illiq_raw

*Next we standarize illiquidity in each portfolio

forv p = 1 / 5 {

	gen P`p'_cs = (30 - 0.25)/(0.30 * PM)
	
	replace P`p'_cs = P`p'_c  if  P`p'_cs >= P`p'_c
}
egen market_illiq = rowmean(P*_cs)




*Conditional Covariance of Portfolio Returns with Market
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


	
	gen Rm_PIm = monthly_rm_EW - market_illiq
	
	
	
	*Covariance of Portfolio Illiquidity with Market Illiqudity
	cap drop  p1_* p2_* p3_* p4_*
	
	forv p = 1 / 5 {
	mgarch dvech (P`p'_c market_illiq =), iterate(100)  arch(1)
	predict p`p'* if e(sample), variance
	}

	*Covariance of Portfolio Return with Market Illiqudity
	forv p = 1 / 5 {
		mgarch dvech (P`p'_ret market_illiq =),  iterate(100) arch(1)
		predict rp`p'* if e(sample), variance
	}

*Covariance of Portfolio illiquidity with Market Returns
	forv p = 1 / 5 {
		mgarch dvech (P`p'_c monthly_rm_EW =), iterate(100)  arch(1)
		predict prm`p'* if e(sample), variance
	}
 	

* The conditional variance
	mgarch dvech (Rm_PIm =), arch(1) iterate(100) 
	predict v1* if e(sample), variance

	*Risk premium betas

*Beta 1 for each portfolio: Covariance of Portfolio returns with market return
forv p = 1 / 5 {
	gen B1_P`p' = h`p'_monthly_rm_EW_P`p'_ret / v1_Rm_PIm_Rm_PIm
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
	gen B4_P`p' = prm`p'_monthly_rm_EW_P`p'_c / v1_Rm_PIm_Rm_PIm
}

asdoc sum B1* B2* B3* B4*, stat(mean tstat) replace

save part2.3, replace

use part2.3, clear

forv p=1/5{
preserve
keep year mofd B1_P`p'
rename B1_P`p' B1
gen portf_size =`p'
save beta1_p`p', replace
restore
}

forv p=1/5{
preserve
keep year mofd B2_P`p'
rename B2_P`p' B2
gen portf_size =`p'
save beta2_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B3_P`p'
rename B3_P`p' B3
gen portf_size =`p'
save beta3_p`p', replace
restore
}


forv p=1/5{
preserve
keep year mofd B4_P`p'
rename B4_P`p' B4
gen portf_size =`p'
save beta4_p`p', replace
restore
}

use beta1_p1, clear
append using beta1_p2 beta1_p3 beta1_p4 beta1_p5
save B1, replace

use beta2_p1,clear
append using beta2_p2 beta2_p3 beta2_p4 beta2_p5
save B2, replace

use beta3_p1,clear
append using beta3_p2 beta3_p3 beta3_p4 beta3_p5
save B3,replace

use beta4_p1,clear
append using beta4_p2 beta4_p3 beta4_p4 beta4_p5
save B4,replace


*Next we need our portfolio returns and monthly returns and then merge with Betas

use part1.3, clear

bys portf_size mofd: keep if _n==_N
keep portf_size mofd year monthly_ret illiq_port_ret illiq_port_c illiq_port_turn
merge m:1 portf_size mofd using B1
drop _merge
merge m:1 portf_size mofd using B2

drop _merge
merge m:1 portf_size mofd using B3

drop _merge
merge m:1 portf_size mofd using B4

drop _merge

xtset portf_size mofd

save part4.3, replace


use part4.3, clear
bys portf_size: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0146

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4



*Our Primary GMM-model with correctly specified instruments, equal to Fama and Mcbeth and Cross-sectional regression
gmm (Ret_excess -      {cons}  -{gam_net}*(B_net)), inst( B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net)), inst( B1 B_net Ec) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)), inst( B1) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B* B_net) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B_net B* kEc) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)), inst( B* B_net kEc) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B*)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)), inst( B1 B2 B3 B4 kEc)






*Controlling for SIZE and MOMENTUM in our Illiquidity Sorted Portfolios
*-----------------------------------------------------------------*

use part4, clear

bys portf_illiq: egen Ec= mean(illiq_port_c)
egen k = mean( illiq_port_turn)
replace k= 0.0153

gen kEc= k*Ec
gen Ret_excess =  illiq_port_ret - kEc
gen B_net= B1 + B2 - B3 - B4

gen ln_size = ln(weight)

rename illiq_port_MOM MOM


*Controlling for SIZE

gmm (Ret_excess -      {cons}  -{gam_net}*(B_net) -{gam5}*(ln_size)), inst( B_net ln_size) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net) -{gam5}*(ln_size)), inst( B1 B_net Ec ln_size) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)-{gam5}*(ln_size)), inst( B1 ln_size) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)), inst( B* B_net ln_size) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)), inst( B_net B* kEc ln_size) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)), inst( B* B_net kEc ln_size) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)-{gam5}*(ln_size)), inst( B* ln_size)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)-{gam5}*(ln_size)), inst( B1 B2 B3 B4 kEc ln_size)


*Controlling for Momentum and SIZE - Table 14

gmm (Ret_excess -      {cons}  -{gam_net}*(B_net) -{gam5}*(ln_size) -{gam6}*(MOM)), inst( B_net ln_size MOM) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) - {gam_net}*(B_net) -{gam5}*(ln_size)-{gam6}*(MOM)), inst( B1 B_net Ec ln_size MOM) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B1 ln_size MOM) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B* B_net ln_size MOM) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec)  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B_net B* kEc ln_size MOM) winit(identity)
gmm (illiq_port_ret -  {cons}  -{gam1}*(B1) - {gam_net}*(B_net)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B* B_net kEc ln_size MOM) winit(identity)
gmm (Ret_excess -      {cons}  -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B* ln_size MOM)
gmm (illiq_port_ret -  {cons}  -{gam0}*(Ec) -{gam1}*(B1) - {gam2}*(B2) + {gam3}*(B3) + {gam4}*(B4)-{gam5}*(ln_size)-{gam6}*(MOM)), inst( B1 B2 B3 B4 kEc ln_size MOM)




*NEXT- PRODUCING APPENDIX- With alternative filtering(Meaning we drop <NOK 10 000 and >NOK10

*NEW DO-FILE??









	


	

