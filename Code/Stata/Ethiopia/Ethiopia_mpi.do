***********************************************************************************************************************

** Human Development Report Office (HDRO), United Nations Development Programme
** Multidimensional Poverty Index 2019 release

** Methodology developed in partnership with the Oxford Poverty and Human Development Initiative, University of Oxford

************************************************************************************************************************


clear all 
set more off
set maxvar 10000
set mem 500m
cap log close


*** Working Folder Path ***
global path_in "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\MPI 2.0\Ethiopia 2016_DHS\" 
global path_out "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\MPI 2.0\Ethiopia 2016_DHS\"
global path_logs "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\MPI 2.0\Ethiopia 2016_DHS\"
global path_qc "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\MPI 2.0\Ethiopia 2016_DHS\"
global path_ado "C:"


*** Log file *** 
log using "$path_logs/eth_dhs16_dataprep.log", replace	


********************************************************************************
*** Ethiopia DHS 2016 ***
********************************************************************************


********************************************************************************
*** Step 1: Data preparation 
*** Selecting variables from KR, BR, IR, & MR recode & merging with PR recode 
********************************************************************************

	
/* Ethiopia DHS 2016: Height and weight were measured for all 
children age 0-59 months, women age 15-49, and men age 15-59 */


********************************************************************************
*** Step 1.1 KR - CHILDREN's RECODE (under 5)
********************************************************************************

use "$path_in/ETPR70FL.DTA", clear 

*** Generate individual unique key variable required for data merging
*** v001=cluster number; 
*** v002=household number; 
*** b16=child's line number in household
gen double ind_id = hv001*1000000 + hv002*100 + hvidx
format ind_id %20.0g
label var  ind_id "Individual ID"

duplicates report ind_id
duplicates tag ind_id, gen(duplicates)

tab hc13 if duplicates!=0

keep if hv120==1

gen child_KR=1 
	//Generate identification variable for observations in KR recode

keep hc1 hc2 hc3 hc13 hc15 hv120 hv104 hv105 ind_id child_KR hc18 hc17 hc19 hc30 hc16 hc31 hv005 hv807c hc1a

*** Next, indicate to STATA where the igrowup_restricted.ado file is stored:
	***Source of ado file: http://www.who.int/childgrowth/software/en/
adopath + "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\WHO igrowup STATA\"


*** We will now proceed to create three nutritional variables: 
	*** weight-for-age (underweight),  
	*** weight-for-height (wasting) 
	*** height-for-age (stunting)

	
/* We use 'reflib' to specify the package directory where the .dta files 
containing the WHO Child Growth Standards are stored. Note that we use 
strX to specify the length of the path in string. If the path is long, 
you may specify str55 or more, so it will run. */	
gen str100 reflib="C:\igrowup_stata"
lab var reflib "Directory of reference tables"


/* We use datalib to specify the working directory where the input STATA 
dataset containing the anthropometric measurement is stored. */
gen str100 datalib = "$path_out" 
lab var datalib "Directory for datafiles"


/* We use datalab to specify the name that will prefix the output files that 
will be produced from using this ado file (datalab_z_r_rc and datalab_prev_rc)*/
gen str30 datalab = "children_nutri_eth" 
lab var datalab "Working file"


*** Next check the variables that WHO ado needs to calculate the z-scores:
*** sex, age, weight, height

*** Variable: SEX ***
tab hv104, miss 
	//"1" for male ;"2" for female
tab  hv104, nol 
clonevar gender = hv104
desc gender
tab gender


*** Variable: AGE ***
tab hc1, miss 
codebook hc1
	//Age is measured in months
clonevar age_months = hc1  
desc age_months
summ age_months
gen  str6 ageunit = "months" 
lab var ageunit "Months"
gen mdate = mdy(hc18, hc17, hc19)
gen bdate = mdy(hc30, hc16, hc31) if hc16 <= 31
	//Calculate birth date in days from date of interview
replace bdate = mdy(hc30, 15, hc31) if hc16 > 31 
	//If date of birth of child has been expressed as more than 31, we use 15
gen age = (mdate-bdate)/30.4375
	//Calculate age in months with days expressed as decimals
replace age = 0 if age<0

gen age2=hc1a/30.4375
compare age age2
drop age2

	
*** Variable: BODY WEIGHT (KILOGRAMS) ***
codebook hc2, tab (10000)
gen	weight = hc2/10 
	//We divide it by 10 in order to express it in kilograms 
tab hc2 if hc2>9990,m nol   
	//Missing values are 994 to 996
replace weight = . if hc2>=9990
	//All missing values or out of range are replaced as "."
tab	hc13 hc2 if hc2>=9990 | hc2==., miss 
	//hc13: result of the measurement
desc weight 
summ weight


*** Variable: HEIGHT (CENTIMETERS)
codebook hc3, tab (10000)
gen	height = hc3/10 
	//We divide it by 10 in order to express it in centimeters
tab hc3 if hc3>9990,m nol   
	//Missing values are 9994 to 9996
replace height = . if hc3>=9990 
	//All missing values or out of range are replaced as "."
tab	hc13 hc3   if hc3>=9990 | hc3==., miss
desc height 
summ height


*** Variable: MEASURED STANDING/LYING DOWN ***
codebook hc15
gen measure = "l" if hc15==1 
	//Child measured lying down
replace measure = "h" if hc15==2 
	//Child measured standing up
replace measure = " " if hc15==9 | hc15==0 | hc15==. 
	//Replace with " " if unknown
desc measure
tab measure


	
*** Variable: OEDEMA ***
lookfor oedema
gen str1 oedema = "n"  
	//It assumes no-one has oedema
desc oedema
tab oedema	


*** Variable: INDIVIDUAL CHILD SAMPLING WEIGHT ***
gen  sw = hv005/1000000 
	//For DHS sample weight has to be divided 1000000
desc sw
summ sw


/*We now run the command to calculate the z-scores with the adofile */
igrowup_restricted reflib datalib datalab gender age ageunit weight height ///
measure oedema sw


/*We now turn to using the dta file that was created and that contains 
the calculated z-scores to create the child nutrition variables following WHO 
standards */
use "$path_out/children_nutri_eth_z_rc.dta", clear 



*** Standard MPI indicator ***
	//Takes value 1 if the child is under 2 stdev below the median & 0 otherwise
	
gen	underweight = (_zwei < -2.0) 
replace underweight = . if _zwei == . | _fwei==1
lab var underweight  "Child is undernourished (weight-for-age) 2sd - WHO"
tab underweight, miss


gen stunting = (_zlen < -2.0)
replace stunting = . if _zlen == . | _flen==1
lab var stunting "Child is stunted (length/height-for-age) 2sd - WHO"
tab stunting, miss


gen wasting = (_zwfl < - 2.0)
replace wasting = . if _zwfl == . | _fwfl == 1
lab var wasting  "Child is wasted (weight-for-length/height) 2sd - WHO"
tab wasting, miss


	//Retain relevant variables:
keep ind_id child_KR underweight stunting wasting 

order ind_id child_KR underweight stunting wasting 

sort ind_id

duplicates report ind_id


	//Erase files from folder:
erase "$path_out/children_nutri_eth_z_rc.xls"
erase "$path_out/children_nutri_eth_prev_rc.xls"
erase "$path_out/children_nutri_eth_z_rc.dta"


	//Save a temp file for merging with PR:
save "$path_out/eth16_KR.dta", replace
	

********************************************************************************
*** Step 1.2  BR - BIRTH RECODE 
*** (All females 15-49 years who ever gave birth)  
********************************************************************************

use "$path_in/ETBR70FL.dta", clear

		
*** Generate individual unique key variable required for data merging
*** v001=cluster number;  
*** v002=household number; 
*** v003=respondent's line number
gen double ind_id = v001*1000000 + v002*100 + v003 
format ind_id %20.0g
label var ind_id "Individual ID"


desc b3 b7	
gen date_death = b3 + b7
	//Date of death = date of birth (b3) + age at death (b7)
gen mdead_survey = v008 - date_death
	//Months dead from survey = Date of interview (v008) - date of death
gen ydead_survey = mdead_survey/12
	//Years dead from survey

	
codebook b5, tab (10)	
gen child_died = 1 if b5==0
	//Redefine the coding and labels (1=child dead; 0=child alive)
replace child_died = 0 if b5==1
replace child_died = . if b5==.
label define lab_died 1 "child has died" 0 "child is alive"
label values child_died lab_died
tab b5 child_died, miss
	

	/*NOTE: For each woman, sum the number of children who died and compare to 
	the number of sons/daughters whom they reported have died */
bysort ind_id: egen tot_child_died = sum(child_died) 
egen tot_child_died_2 = rsum(v206 v207)
	//v206: sons who have died
	//v207: daughters who have died
compare tot_child_died tot_child_died_2
	//In Ethiopia DHS 2016, these figures are identical
	
		
replace child_died=0 if b7>=216 & b7<.
/* counting only deads of children <18y (216 months) */

bysort ind_id: egen tot_child_died_5y=sum(child_died) if ydead_survey<=5
	/*For each woman, sum the number of children who died in the past 5 years 
	prior to the interview date */
	
replace tot_child_died_5y=0 if tot_child_died_5y==. & tot_child_died>=0 & tot_child_died<.
	/*All children who are alive and died longer than 5 years from the interview 
	date are replaced as '0'*/
	
replace tot_child_died_5y=. if child_died==1 & ydead_survey==.
	//Replace as '.' if there is no information on when the child died  

tab tot_child_died tot_child_died_5y, miss

bysort ind_id: egen child_died_per_wom = max(tot_child_died)
lab var child_died_per_wom "Total child death for each women (birth recode)"

bysort ind_id: egen child_died_per_wom_5y = max(tot_child_died_5y)
lab var child_died_per_wom_5y "Total child death for each women in the last 5 years (birth recode)"

	

	//Keep one observation per women
bysort ind_id: gen id=1 if _n==1
keep if id==1
drop id

duplicates report ind_id 

gen women_BR = 1 
	//Identification variable for observations in BR recode

	
	//Retain relevant variables
keep ind_id women_BR b16 child_died_per_wom child_died_per_wom_5y b7
 
order ind_id women_BR b16 child_died_per_wom child_died_per_wom_5y b7

sort ind_id

	//Save a temp file for merging with PR:
save "$path_out/eth16_BR.dta", replace	
	
	
********************************************************************************
*** Step 1.3  IR - WOMEN's RECODE  
*** (All eligible females 15-49 years in the household)
********************************************************************************

use "$path_in/ETIR70FL.dta", clear


*** Generate individual unique key variable required for data merging
*** v001=cluster number;  
*** v002=household number; 
*** v003=respondent's line number
gen double ind_id = v001*1000000 + v002*100 + v003 
format ind_id %20.0g
label var ind_id "Individual ID"

duplicates report ind_id

gen women_IR=1 
	//Identification variable for observations in IR recode


keep ind_id women_IR v003 v005 v012 v201 v206 v207
 
order ind_id women_IR v003 v005 v012 v201 v206 v207

 
sort ind_id

	//Save a temp file for merging with PR:
save "$path_out/eth16_IR.dta", replace


********************************************************************************
*** Step 1.4  IR - WOMEN'S RECODE  
*** (Girls 15-19 years in the household)
********************************************************************************

use "$path_in/ETPR70FL.dta", clear

*** Generate individual unique key variable required for data merging
*** v001=cluster number;  
*** v002=household number; 
*** v003=respondent's line number
gen double ind_id = hv001*1000000 + hv002*100 + hvidx 
format ind_id %20.0g
label var ind_id "Individual ID"

duplicates report ind_id	

keep if hv104==2 & hv105>=15 & hv105<=19
***Variables required to calculate the z-scores to produce BMI-for-age:

keep ind_id ha2 ha3 hv008 ha32 hv104 hv105 hv005 hv807c

*** Variable: SEX ***
gen gender=2 

*** Variable: AGE IN MONTHS ***
*compare hv807c hv008
/* date of biomarker vs date of interview, they should be identical */
*gen age_month=hv008-ha32
gen age_month=hv807c-ha32
lab var age_month "Age in months, individuals 15-19 years"	

	
*** Variable: AGE UNIT ***
gen str6 ageunit = "months" 
lab var ageunit "Months"

		
*** Variable: BODY WEIGHT (KILOGRAMS) ***
codebook ha2, tab (999)
gen weight = ha2/10
	//We divide it by 10 in order to express it in kilograms
replace weight = . if ha2>=9990 
	//All missing values or out of range are replaced as "."
summ weight


*** Variable: HEIGHT (CENTIMETERS)
codebook ha3, tab (999)
gen	height = ha3/10 
	//We divide it by 10 in order to express it in centimeters
replace height = . if ha3>=9990 
	//All missing values or out of range are replaced as "."
summ height


*** Variable: OEDEMA
lookfor oedema
gen oedema = "n"  
tab oedema	


*** Variable: SAMPLING WEIGHT ***
gen  sw = hv005/1000000 
	//For DHS sample weight has to be divided 1000000*
summ sw	
		
	
*** Next, indicate to STATA where the igrowup_restricted.ado file is stored:	
adopath + "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\WHO 2007 stata\"


/* We use 'reflib' to specify the package directory where the .dta files 
containing the WHO Growth reference are stored. Note that we use strX to specity 
the length of the path in string. */		
gen str100 reflib="C:\WHO 2007 Stata"
lab var reflib "Directory of reference tables"


/* We use datalib to specify the working directory where the input STATA data
set containing the anthropometric measurement is stored. */
gen str100 datalib = "$path_out" 
lab var datalib "Directory for datafiles"


/* We use datalab to specify the name that will prefix the output files that 
will be produced from using this ado file*/
gen str30 datalab = "girl_nutri_eth" 
lab var datalab "Working file"
	


/*We now run the command to calculate the z-scores with the adofile */
who2007 reflib datalib datalab gender age_month ageunit weight height oedema sw


/*We now turn to using the dta file that was created and that contains 
the calculated z-scores to compute BMI-for-age*/
use "$path_out/girl_nutri_eth_z.dta", clear 

		
gen	z_bmi = _zbfa
replace z_bmi = . if _fbfa==1 
lab var z_bmi "z-score bmi-for-age WHO"


gen	low_bmiage = (z_bmi < -2.0) 
	/*Takes value 1 if BMI-for-age is under 2 stdev below the median & 0 
	otherwise */
replace low_bmiage = . if z_bmi==.
lab var low_bmiage "Teenage low bmi 2sd - WHO"


gen teen_IR=1 
	//Identification variable for observations in IR recode (only 15-19 years)	


	//Retain relevant variables:	
keep ind_id teen_IR age_month low_bmiage
 
order ind_id teen_IR age_month low_bmiage
 
sort ind_id


	//Erase files from folder:
erase "$path_out/girl_nutri_eth_z.xls"
erase "$path_out/girl_nutri_eth_prev.xls"
erase "$path_out/girl_nutri_eth_z.dta"


	//Save a temp file for merging with PR:
save "$path_out/eth16_IR_girls.dta", replace


********************************************************************************
*** Step 1.5  MR - MEN'S RECODE  
***(All eligible man: 15-59 years in the household) 
********************************************************************************

use "$path_in/ETMR70FL.dta", clear 


*** Generate individual unique key variable required for data merging
	*** mv001=cluster number; 
	*** mv002=household number;
	*** mv003=respondent's line number
gen double ind_id = mv001*1000000 + mv002*100 + mv003 	
format ind_id %20.0g
label var ind_id "Individual ID"

duplicates report ind_id

gen men_MR=1 	
	//Identification variable for observations in MR recode


keep ind_id men_MR mv003 mv005 mv012 mv201 mv206 mv207

order ind_id men_MR mv003 mv005 mv012 mv201 mv206 mv207 

sort ind_id

	//Save a temp file for merging with PR:
save "$path_out/eth16_MR.dta", replace


********************************************************************************
*** Step 1.6a  MR - MEN'S RECODE  
***(Boys 15-19 years in the household) 
********************************************************************************
/*Note: In the case of Ethiopia 2016, anthropometric data was collected 
for men. However, to compute the BMI-for-age, we will need to extract the weight
and height variable from the PR file for boys 15-19 years as it is not present 
in the individual MR file */

use "$path_in/ETPR70FL.DTA", clear
	
gen double ind_id = hv001*1000000 + hv002*100 + hvidx 
format ind_id %20.0g
label var ind_id "Individual ID"
codebook ind_id 

count if hv104==1 & hv105>=15 & hv105<=19
	//Total number of men 15-19 years in the PR recode	
keep if hv104==1 & hv105>=15 & hv105<=19
	//Keep only men between the age 15-19 years to merge with the MR recode	
	
count if hb2!=.	
	//hb2: man's weight in kilograms
count if hb3!=.
	//hb3: man's height in centimeters
	
codebook hb13, tab (10)
	//hb13: result of measurement - height/weight
	
gen teen_MR_temp = 1	
	
keep ind_id teen_MR_temp hb2 hb3 hb13 hb40 hv104 hv105 hb32 hv008 hv005 hv807c
	//Retain the height and weight variables

sort ind_id


***Variables required to calculate the z-scores to produce BMI-for-age:

*** Variable: SEX ***
gen gender=1 
	/*Assign all observations as "1" for male, as the file contains all 
	men, 15-19*/
	

gen age_month = hv807c-hb32
/* this is a more accurate way to calculate age in months using information from the PR file,
the same file from which we extract weight and height info */

*** Variable: AGE UNIT ***
gen str6 ageunit = "months" 
lab var ageunit "Months"


/* some men under 20 years were in the PR file but not in the MR file, we need to
construct the age in months for these men since they do not have info in variables mv from MR file */

*** Variable: BODY WEIGHT (KILOGRAMS) ***
	/*In the case of Ethiopia DHS 2016, the usual mv437 variable (body weight) 
	does not exist in the MR file. Hence we use the hb2 (man's weight in 
	kilograms) variable that were extracted from the PR file. */

codebook hb2, tab (9999)
gen weight = hb2/10
	//We divide it by 10 in order to express it in kilograms 
replace weight = . if hb2>=9990 
	//All missing values or out of range are replaced as "."
summ weight


*** Variable: HEIGHT (CENTIMETERS)
	/*In the case of Ethiopia DHS 2016, the usual mv438 variable (height) 
	does not exist in the MR file. Hence we use the hb3 (man's height in 
	centimeters) variable that were extracted from the PR file. */
	
codebook hb3, tab (9999)
gen	height = hb3/10 
	//We divide it by 10 in order to express it in centimeters
replace height = . if hb3>=9990 
	//All missing values or out of range are replaced as "."
summ height


*** Variable: OEDEMA
gen oedema = "n"  
tab oedema	


*** Variable: SAMPLING WEIGHT ***
gen  sw = hv005/1000000 
	//For DHS sample weight has to be divided 1000000
summ sw


*** Keep only relevant sample: teenagers 15 - 19 years ***
*count if mv012>=15 & mv012<=19
	//Total number of boys in the MR recode	
*keep if mv012>=15 & mv012<=19	
	//Keep only boys between age 15-19 years to compute BMI-for-age

count if age_month<240
keep if age_month<240
	
/* 
For this part of the do-file we use the WHO AnthroPlus software. This is to 
calculate the z-scores for 15-19 years. 
Source of ado file: https://www.who.int/growthref/tools/en/
*/
	
*** Indicate to STATA where the igrowup_restricted.ado file is stored:	
adopath + "C:\Users\cecilia.calderon\Documents\HDRO_MCC\MPI\WHO 2007 stata\"


/* We use 'reflib' to specify the package directory where the .dta files 
containing the WHO Growth reference are stored. Note that we use strX to specity 
the length of the path in string. */		
gen str100 reflib="C:\WHO 2007 Stata"
lab var reflib "Directory of reference tables"


/* We use datalib to specify the working directory where the input STATA data
set containing the anthropometric measurement is stored. */
gen str100 datalib = "$path_out" 
lab var datalib "Directory for datafiles"


/* We use datalab to specify the name that will prefix the output files that 
will be produced from using this ado file*/
gen str30 datalab = "boy_nutri_eth" 
lab var datalab "Working file"	
	
capture drop _merge
	
/*We now run the command to calculate the z-scores with the adofile */
who2007 reflib datalib datalab gender age_month ageunit weight height oedema sw

	
/*We now turn to using the dta file that was created and that contains 
the calculated z-scores to compute BMI-for-age*/
use "$path_out/boy_nutri_eth_z.dta", clear 

		
gen	z_bmi = _zbfa
replace z_bmi = . if _fbfa==1 
lab var z_bmi "z-score bmi-for-age WHO"


	/*Takes value 1 if BMI-for-age is under 2 stdev below the median & 0 
	otherwise */
gen	low_bmiage = (z_bmi < -2.0) 
replace low_bmiage = . if z_bmi==.
lab var low_bmiage "Teenage low bmi 2sd - WHO"


gen teen_MR = 1
	//Identification variable for observations in IR recode (only 15-19 years)

	
	//Retain relevant variables:	
keep ind_id teen_MR age_month low_bmiage
 
order ind_id teen_MR age_month low_bmiage

ren age_month age_month_boys  
ren low_bmiage low_bmiage_boys

sort ind_id

	//erase files from folder:
erase "$path_out/boy_nutri_eth_z.xls"
erase "$path_out/boy_nutri_eth_prev.xls"
erase "$path_out/boy_nutri_eth_z.dta"


	//Save a temp file for merging with PR:
save "$path_out/eth16_MR_boys.dta", replace


********************************************************************************
*** Step 1.7  PR - HOUSEHOLD MEMBER'S RECODE 
********************************************************************************

use "$path_in/ETPR70FL.dta", clear

gen cty		= "Ethiopia" 
gen ccty	= "ETH"  
gen year    = "2016"  
gen survey  = "DHS"
gen ccnum	= 231


*** Generate a household unique key variable at the household level using: 
	***hv001=cluster number 
	***hv002=household number
gen double hh_id = hv001*10000 + hv002 
format hh_id %20.0g
label var hh_id "Household ID"
codebook hh_id  


*** Generate individual unique key variable required for data merging using:
	*** hv001=cluster number; 
	*** hv002=household number; 
	*** hvidx=respondent's line number.
gen double ind_id = hv001*1000000 + hv002*100 + hvidx 
format ind_id %20.0g
label var ind_id "Individual ID"
codebook ind_id


sort hh_id ind_id
	
	
********************************************************************************
*** 1.8 DATA MERGING
********************************************************************************

*** Merging BR Recode 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_BR.dta"

drop _merge

erase "$path_out/eth16_BR.dta"


*** Merging IR Recode 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_IR.dta"

tab women_IR hv117, miss col
tab ha65 if hv117==1 & women_IR ==., miss
	//Total number of eligible women not interviewed
tab ha65 ha13 if women_IR == . & hv117==1, miss  

drop _merge

erase "$path_out/eth16_IR.dta"


*** Merging IR Recode: 15-19 years girls 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_IR_girls.dta"

tab teen_IR hv117 if hv105>=15 & hv105<=19, miss col
tab ha65 if hv117==1 & teen_IR ==. & (hv105>=15 & hv105<=19), miss 
	//Total number of eligible girls not interviewed
tab ha65 ha13 if hv117==1 & teen_IR ==. & (hv105>=15 & hv105<=19), miss 
tab ha40 if ha65==1 & ha13 ==0 & hv117==1 & teen_IR ==. & (hv105>=15 & hv105<=19), miss

drop _merge

erase "$path_out/eth16_IR_girls.dta"


*** Merging MR Recode 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_MR.dta"

tab men_MR hv118, miss col

drop _merge

erase "$path_out/eth16_MR.dta"


*** Merging MR Recode: 15-19 years boys 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_MR_boys.dta"
	
drop _merge

erase "$path_out/eth16_MR_boys.dta"


*** Merging KR Recode 
*****************************************
merge 1:1 ind_id using "$path_out/eth16_KR.dta"
count if b16==0 & child_KR==1
	//The children without household line are unique to the KR recode

drop _merge

erase "$path_out/eth16_KR.dta"


sort ind_id


********************************************************************************
*** Step 1.9 KEEPING ONLY DE JURE HOUSEHOLD MEMBERS ***
********************************************************************************

//Permanent (de jure) household members 
clonevar resident = hv102 
codebook resident, tab (10) 
label var resident "Permanent (de jure) household member"


drop if resident!=1 
tab resident, miss
	/*Note: The Global MPI is based on de jure (permanent) household members 
	only. As such, non-usual residents will be excluded from the sample. 
	
	In the context of Ethiopia DHS 2016, 400 (1.22%) individuals who were 
	non-usual residents were dropped from the sample*/


********************************************************************************
*** 1.10 CONTROL VARIABLES
********************************************************************************

/* Households are identified as having 'no eligible' members if there are no 
applicable population, that is, children 0-5 years, adult women 15-49 years or 
men 15-59 years. These households will not have information on relevant 
indicators of health. As such, these households are considered as non-deprived 
in those relevant indicators.*/


*** No Eligible Women 15-49 years
*****************************************
gen fem_eligible = (hv117==1)
bys hh_id: egen hh_n_fem_eligible = sum(fem_eligible) 	
	//Number of eligible women for interview in the hh
gen no_fem_eligible = (hh_n_fem_eligible==0) 									
	//Takes value 1 if the household had no eligible females for an interview
lab var no_fem_eligible "Household has no eligible women"
tab no_fem_eligible, miss


*** No Eligible Men 15-59 years
*****************************************
gen male_eligible = (hv118==1)
bys hh_id: egen hh_n_male_eligible = sum(male_eligible)  
	//Number of eligible men for interview in the hh
gen no_male_eligible = (hh_n_male_eligible==0) 	
	//Takes value 1 if the household had no eligible males for an interview
lab var no_male_eligible "Household has no eligible man"
tab no_male_eligible, miss


*** No Eligible Children 0-5 years
*****************************************
gen child_eligible = (hv120==1) 
bys hh_id: egen hh_n_children_eligible = sum(child_eligible)  
	//Number of eligible children for anthropometrics
gen no_child_eligible = (hh_n_children_eligible==0) 
	//Takes value 1 if there were no eligible children for anthropometrics
lab var no_child_eligible "Household has no children eligible"
tab no_child_eligible, miss


*** No Eligible Women and Men 
***********************************************
	/*NOTE: In the DHS datasets, we use this variable as a control 
	variable for the child mortality indicator if mortality data was 
	collected from women and men. If child mortality was only colelcted 
	from women, the we use 'no_fem_eligible' as the eligibility criteria */
gen	no_adults_eligible = (no_fem_eligible==1 & no_male_eligible==1) 
	//Takes value 1 if the household had no eligible men & women for an interview
lab var no_adults_eligible "Household has no eligible women or men"
tab no_adults_eligible, miss 


*** No Eligible Children and Women  
***********************************************
	/*NOTE: In the DHS datasets, we use this variable as a control 
	variable for the nutrition indicator if nutrition data is 
	present for children and women.*/
gen	no_child_fem_eligible = (no_child_eligible==1 & no_fem_eligible==1)
lab var no_child_fem_eligible "Household has no children or women eligible"
tab no_child_fem_eligible, miss 


*** No Eligible Women, Men or Children 
***********************************************
	/*NOTE: In the DHS datasets, we use this variable as a control 
	variable for the nutrition indicator if nutrition data is 
	present for children, women and men. */
gen no_eligibles = (no_fem_eligible==1 & no_male_eligible==1 & no_child_eligible==1)
lab var no_eligibles "Household has no eligible women, men, or children"
tab no_eligibles, miss


*** No Eligible Subsample 
*****************************************
	/*hv042 (household selected for hemoglobin) is essentially a variable that 
	indicates whether there is selection of a subsample for anthropometric 
	data. */	
gen hem_eligible =(hv042==1)
bys hh_id: egen hh_n_hem_eligible = sum(hem_eligible) 
gen no_hem_eligible = (hh_n_hem_eligible==0) 
	//Takes value 1 if the HH had no eligible females for hemoglobin test	
lab var no_hem_eligible "Household has no eligible individuals for hemoglobin measurements"
tab no_hem_eligible, miss


drop fem_eligible hh_n_fem_eligible male_eligible hh_n_male_eligible ///
child_eligible hh_n_children_eligible hem_eligible hh_n_hem_eligible 


sort hh_id ind_id


********************************************************************************
*** 1.11 SUBSAMPLE VARIABLE ***
********************************************************************************

/* In Ethiopia DHS 2016, height and weight measurements were
collected from children (0-5), women (15-49) and men (15-59). 
*/

gen subsample=1
label var subsample "Households selected as part of nutrition subsample" 
tab subsample, miss


********************************************************************************
*** 1.12 RENAMING DEMOGRAPHIC VARIABLES ***
********************************************************************************

//Sample weight
desc hv005
clonevar weight = hv005 
label var weight "Sample weight"


//Area: urban or rural	
desc hv025
codebook hv025, tab (5)		
clonevar area = hv025  
replace area=0 if area==2  
label define lab_area 1 "urban" 0 "rural"
label values area lab_area
label var area "Area: urban-rural"


//Relationship to the head of household 
clonevar relationship = hv101 
codebook relationship, tab (20)
recode relationship (1=1)(2=2)(3=3)(11=3)(4/10=4)(15/16=4)(12=5)(17=6)(98=.)
	/*Please ensure recoding follows these 6 broad categories: 1"head" 2"spouse" 
	3"child" 4"extended family" 5"not related" 6"maid" */
label define lab_rel 1"head" 2"spouse" 3"child" 4"extended family" 5"not related" 6"maid"
label values relationship lab_rel
label var relationship "Relationship to the head of household"
tab hv101 relationship, miss


//Sex of household member	
codebook hv104, tab (10)
clonevar sex = hv104  
label var sex "Sex of household member"


//Age of household member
codebook hv105, tab (1000)
clonevar age = hv105  
replace age = . if age>=98
label var age "Age of household member"


//Age group 
recode age (0/4 = 1 "0-4")(5/9 = 2 "5-9")(10/14 = 3 "10-14") ///
		   (15/17 = 4 "15-17")(18/59 = 5 "18-59")(60/max=6 "60+"), gen(agec7)
lab var agec7 "age groups (7 groups)"	
	   
recode age (0/9 = 1 "0-9") (10/17 = 2 "10-17")(18/59 = 3 "18-59") ///
		   (60/max=4 "60+"), gen(agec4)
lab var agec4 "age groups (4 groups)"


//Marital status of household member
clonevar marital = hv115 
codebook marital, tab (20)
recode marital (0=1)(1=2)(8=.)
label define lab_mar 1"never married" 2"currently married" ///
					 3"widowed" 4"divorced" 5"not living together"
label values marital lab_mar	
label var marital "Marital status of household member"
tab hv115 marital, miss


//Total number of de jure hh members in the household
gen member = 1
bysort hh_id: egen hhsize = sum(member)
label var hhsize "Household size"
tab hhsize, miss
drop member


//Subnational region
lookfor region
codebook hv024, tab (100)	
gen region = hv024
lab var region "Region for subnational decomposition"
tab hv024 region, miss


********************************************************************************
***  Step 2 Data preparation  ***
***  Standardization of the 10 Global MPI indicators 
***  Identification of non-deprived & deprived individuals  
********************************************************************************


********************************************************************************
*** Step 2.1 Years of Schooling ***
********************************************************************************

codebook hv108, tab(30)
clonevar  eduyears = hv108   
	*total number of years of education
replace eduyears = . if eduyears>30
	*recode any unreasonable years of highest education as missing value
replace eduyears = . if eduyears>=age & age>0
replace eduyears = 0 if age < 10 
	/*The variable "eduyears" was replaced with a '0' given that the criteria 
	for this indicator is household member aged 10 years or older */

	
	/*A control variable is created on whether there is information on 
	years of education for at least 2/3 of the household members. */
gen temp = 1 if eduyears!=. & age>=10 & age!=.
bysort	hh_id: egen no_missing_edu = sum(temp)
	/*Total household members who are 10 years and older with no missing 
	years of education */
gen temp2 = 1 if age>=10 & age!=.
bysort hh_id: egen hhs = sum(temp2)
	//Total number of household members who are 10 years and older 
replace no_missing_edu = no_missing_edu/hhs
replace no_missing_edu = (no_missing_edu>=2/3)
	/*Identify whether there is information on years of education for at 
	least 2/3 of the household members aged 10 years and older */
tab no_missing_edu, miss
label var no_missing_edu "No missing edu for at least 2/3 of the HH members aged 10 years & older"		
drop temp temp2 hhs


/*The entire household is considered deprived if no household member aged 
10 years or older has completed SIX years of schooling. */

gen	 years_edu6 = (eduyears>=6)
	/* The years of schooling indicator takes a value of "1" if at least someone 
	in the hh has reported 6 years of education or more */
replace years_edu6 = . if eduyears==.
bysort hh_id: egen hh_years_edu6_1 = max(years_edu6)
gen	hh_years_edu6 = (hh_years_edu6_1==1)
replace hh_years_edu6 = . if hh_years_edu6_1==.
replace hh_years_edu6 = . if hh_years_edu6==0 & no_missing_edu==0 
lab var hh_years_edu6 "Household has at least one member with 6 years of edu"



********************************************************************************
*** Step 2.2 Child School Attendance ***
********************************************************************************

codebook hv121, tab (10)
clonevar attendance = hv121 
recode attendance (2=1) 
codebook attendance, tab (10)

replace attendance = 0 if (attendance==8 | attendance==.) & hv109==0 
	/*In some countries, they don't assess attendance for those with no 
	 educational attainment. These are replaced with a '0' */
replace attendance = . if  attendance==8 & hv109!=0
	//Replace missing values
	
	

*** Old & New Standard MPI ***
******************************************************************* 
/*The entire household is considered deprived if any school-aged child is not 
attending school up to class 8. */ 

gen child_schoolage = (age>=7 & age<=15)
	/* Note: In Ethiopia, the official school entrance age is 6 years.  
	So, age range is 7-15 (=7+8)
	Source: http://data.uis.unesco.org/?ReportId=163. */
	
	
	/*A control variable is created on whether there is no information on 
	school attendance for at least 2/3 of the school age children */
count if child_schoolage==1 & attendance==.
	//Understand how many eligible school aged children are not attending school 
gen temp = 1 if child_schoolage==1 & attendance!=.
	/*Generate a variable that captures the number of eligible school aged 
	children who are attending school */
bysort hh_id: egen no_missing_atten = sum(temp)	
	/*Total school age children with no missing information on school 
	attendance */
gen temp2 = 1 if child_schoolage==1	
bysort hh_id: egen hhs = sum(temp2)
	//Total number of household members who are of school age
replace no_missing_atten = no_missing_atten/hhs 
replace no_missing_atten = (no_missing_atten>=2/3)
	/*Identify whether there is missing information on school attendance for 
	more than 2/3 of the school age children */			
tab no_missing_atten, miss
label var no_missing_atten "No missing school attendance for at least 2/3 of the school aged children"		
drop temp temp2 hhs
	
	
bysort hh_id: egen hh_children_schoolage = sum(child_schoolage)
replace hh_children_schoolage = (hh_children_schoolage>0) 
	//Control variable: 
	//It takes value 1 if the household has children in school age
lab var hh_children_schoolage "Household has children in school age"


gen	child_not_atten = (attendance==0) if child_schoolage==1
replace child_not_atten = . if attendance==. & child_schoolage==1
bysort	hh_id: egen any_child_not_atten = max(child_not_atten)
gen	hh_child_atten = (any_child_not_atten==0) 
replace hh_child_atten = . if any_child_not_atten==.
replace hh_child_atten = 1 if hh_children_schoolage==0
replace hh_child_atten = . if hh_child_atten==1 & no_missing_atten==0 
	/*If the household has been intially identified as non-deprived, but has 
	missing school attendance for at least 2/3 of the school aged children, then 
	we replace this household with a value of '.' because there is insufficient 
	information to conclusively conclude that the household is not deprived */
lab var hh_child_atten "Household has all school age children up to class 8 in school"
tab hh_child_atten, miss

	/*Note: The indicator takes value 1 if ALL children in school age are 
	attending school and 0 if there is at least one child not attending. 
	Households with no children receive a value of 1 as non-deprived. The 
	indicator has a missing value only when there are all missing values on 
	children attendance in households that have children in school age. */
	
	
********************************************************************************
*** Step 2.3 Nutrition ***
********************************************************************************


********************************************************************************
*** Step 2.3a Adult Nutrition ***
********************************************************************************
/*Note: Ethiopia DHS 2016 has anthropometric data for men, 15-59 years */

lookfor body mass
codebook ha40 hb40

foreach var in ha40 hb40 {
			 gen inf_`var' = 1 if `var'!=.
			 bys sex: tab age inf_`var' 
			 drop inf_`var'
			 }


*** ELIGIBILITY FOR BMI ***

** WOMEN
gen fem_eligible_bmi = 1 if ha13<.
replace fem_eligible_bmi = 0 if ha13==.
replace fem_eligible_bmi = 0 if age>49 & age<.
bys hh_id: egen hh_n_fem_eligible_bmi = sum(fem_eligible_bmi) 	
	//Number of eligible women for BMI in the hh
gen no_fem_eligible_bmi = (hh_n_fem_eligible_bmi==0) 									
	//Takes value 1 if the household had no eligible females for an interview
lab var no_fem_eligible_bmi "Household has no eligible women"
tab no_fem_eligible_bmi, miss

** MEN
gen male_eligible_bmi = 1 if hb13<.
replace male_eligible_bmi = 0 if hb13==.
replace male_eligible_bmi = 0 if age>59 & age<.
bys hh_id: egen hh_n_male_eligible_bmi = sum(male_eligible_bmi)  
	//Number of eligible men for BMI
gen no_male_eligible_bmi = (hh_n_male_eligible_bmi==0) 	
	//Takes value 1 if the household had no eligible males for an interview
lab var no_male_eligible_bmi "Household has no eligible man"
tab no_male_eligible_bmi, miss

*** No Eligible Women and Men for BMI
***********************************************
	/*NOTE: In the DHS datasets, we use this variable as a control 
	variable for nutrition when anthropometrics is available for women and men */
gen	no_adults_eligible_bmi = (no_fem_eligible_bmi==1 & no_male_eligible_bmi==1) 
	//Takes value 1 if the household had no eligible men & women for BMI
lab var no_adults_eligible_bmi "Household has no eligible women or men"
tab no_adults_eligible_bmi, miss


*** No Eligible Women, Men or Children for BMI
***********************************************
	/*NOTE: In the DHS datasets, we use this variable as a control 
	variable for the nutrition indicator if nutrition data is 
	present for children, women and men. */
gen no_eligibles_bmi = (no_fem_eligible_bmi==1 & no_male_eligible_bmi==1 & no_child_eligible==1)
lab var no_eligibles_bmi "Household has no eligible women, men, or children for BMI"
tab no_eligibles_bmi, miss




*** BMI Indicator for Women 15-49 years ***
******************************************************************* 

gen	f_bmi = ha40/100
	//Low BMI of women 15-49 years	
lab var f_bmi "Women's BMI"

gen	f_low_bmi = (f_bmi<18.5)
replace f_low_bmi = . if f_bmi==. | f_bmi>=99.90
replace f_low_bmi = . if age>49 & age<.
lab var f_low_bmi "BMI of women < 18.5"

bysort hh_id: egen low_bmi = max(f_low_bmi)

gen	hh_no_low_bmi = (low_bmi==0)
	/*Under this section, households take a value of '1' if no women in the 
	household has low bmi */
	
replace hh_no_low_bmi = . if low_bmi==.
	/*Under this section, households take a value of '.' if there is no 
	information from eligible women*/


replace hh_no_low_bmi = 1 if no_fem_eligible_bmi==1
	/*Under this section, households that don't have eligible female population 
	are identified as non-deprived in nutrition. */	
	
drop low_bmi
lab var hh_no_low_bmi "Household has no adult with low BMI"

tab hh_no_low_bmi, miss
	/*Figures are exclusively based on information from eligible adult 
	women (15-49 years) */


*** BMI Indicator for Men 15-59 years ***
******************************************************************* 

gen m_bmi = hb40/100
lab var m_bmi "Male's BMI"

gen m_low_bmi = (m_bmi<18.5)
replace m_low_bmi = . if m_bmi==. | m_bmi>=99.90
replace m_low_bmi = . if age>59 & age<.
lab var m_low_bmi "BMI of male < 18.5"
 
bysort hh_id: egen low_bmi = max(m_low_bmi) 

replace hh_no_low_bmi = 0 if low_bmi==1
	/*Under this section, households take a value of '0' if there's any male 
	with low bmi*/
	
replace hh_no_low_bmi = 1 if low_bmi==0 & hh_no_low_bmi==.
	/*Under this section, households take a value of '1' if no male has low BMI 
	& info is missing for women */
	
drop low_bmi
	
tab hh_no_low_bmi if subsample==1, miss
	/*Figures are based on information from eligible adult women and eligible
	men. For countries that do not have male recode or lack anthropometric data
	for men, then the figures are exclusively from women.  */
	


*** BMI-for-age for individuals 15-19 years and BMI for individuals 20-49 years ***
******************************************************************* 
*replace age_month=age_month_boys if age_month_boys<. & age_month==.
*replace low_bmiage=low_bmiage_boys if low_bmiage_boys<. & low_bmiage==.

gen low_bmi_byage = 0
lab var low_bmi_byage "Individuals with low BMI or BMI-for-age"

replace low_bmi_byage = 1 if f_low_bmi==1
	//Replace variable "low_bmi_byage = 1" if eligible women have low BMI

	
	/*Note: The following command will result in 0 changes when there is no BMI 
	information from men*/
	
replace low_bmi_byage = 1 if low_bmi_byage==0 & m_low_bmi==1 
	//Replace variable "low_bmi_byage = 1" if eligible men have low BMI
	
	
	/*Note: The following command replaces BMI with BMI-for-age for those 
	between the age group of 15-19 by their age in months where information is 
	available */
	
//Replacement for girls: 
replace low_bmi_byage = 1 if low_bmiage==1 & age_month!=.
replace low_bmi_byage = 0 if low_bmiage==0 & age_month!=.

		//Replacements for boys:
replace low_bmi_byage = 1 if low_bmiage_b==1 & age_month_b!=.
replace low_bmi_byage = 0 if low_bmiage_b==0 & age_month_b!=.
	
	
	/*Note: The following control variable is applied when there is BMI 
	information for women and men, as well as BMI-for-age for teenagers */
replace low_bmi_byage = . if f_low_bmi==. & m_low_bmi==. & low_bmiage==. & low_bmiage_b==. 

		
bysort hh_id: egen low_bmi = max(low_bmi_byage)

gen	hh_no_low_bmiage = (low_bmi==0)
	/*Households take a value of '1' if all eligible adults and teenagers in the 
	household has normal bmi or bmi-for-age */
	
replace hh_no_low_bmiage = . if low_bmi==.
	/*Households take a value of '.' if there is no information from eligible 
	individuals in the household */
	
replace hh_no_low_bmiage = 1 if no_adults_eligible_bmi==1
	/*Households take a value of '1' if there is no eligible population.*/


drop low_bmi
lab var hh_no_low_bmiage "Household has no adult with low BMI or BMI-for-age"

tab hh_no_low_bmi if subsample==1, m	
tab hh_no_low_bmiage if subsample==1, m

	/*NOTE that hh_no_low_bmi takes value 1 if: (a) no any eligible adult in the 
	household has (observed) low BMI or (b) there are no eligible adults in the 
	household. One has to check and adjust the dofile so all people who are 
	eligible and/or measured are included. It is particularly important to check 
	if male are measured and what age group among males and females. The 
	variable takes values 0 for those households that have at least one adult 
	with observed low BMI. The variable has a missing value only when there is 
	missing info on BMI for ALL eligible adults in the household */

	

********************************************************************************
*** Step 2.3b Child Nutrition ***
********************************************************************************

*** Child Underweight Indicator ***
************************************************************************

bysort hh_id: egen temp = max(underweight)
gen	hh_no_underweight = (temp==0) 
	//Takes value 1 if no child in the hh is underweight 
replace hh_no_underweight = . if temp==.
replace hh_no_underweight = 1 if no_child_eligible==1 
	/* Households with no eligible children will receive a value of 1 */
lab var hh_no_underweight "Household has no child underweight - 2 stdev"
drop temp


*** Child Stunting Indicator ***
************************************************************************

bysort hh_id: egen temp = max(stunting)
gen	hh_no_stunting = (temp==0) 
	//Takes value 1 if no child in the hh is stunted
replace hh_no_stunting = . if temp==.
replace hh_no_stunting = 1 if no_child_eligible==1 
lab var hh_no_stunting "Household has no child stunted - 2 stdev"
drop temp


*** Child Either Stunted or Underweight Indicator ***
************************************************************************

gen uw_st = 1 if stunting==1 | underweight==1
replace uw_st = 0 if stunting==0 & underweight==0
replace uw_st = . if stunting==. & underweight==.

bysort hh_id: egen temp = max(uw_st)
gen	hh_no_uw_st = (temp==0) 
	//Takes value 1 if no child in the hh is underweight or stunted
replace hh_no_uw_st = . if temp==.
replace hh_no_uw_st = 1 if no_child_eligible==1
	//Households with no eligible children will receive a value of 1 
lab var hh_no_uw_st "Household has no child underweight or stunted"
drop temp



********************************************************************************
*** Step 2.3c Household Nutrition Indicator ***
********************************************************************************


/* The indicator takes value 1 if there is no low BMI-for-age among teenagers, 
no low BMI among adults or no children under 5 underweight or stunted. It also 
takes value 1 for the households that have no eligible adult AND no eligible 
children. The indicator takes a value of missing "." only if all eligible adults 
and eligible children have missing information in their respective nutrition 
variable. */

gen hh_nutrition_uw_st = 1 if (hh_no_low_bmiage==1 & hh_no_uw_st==1) | (hh_no_low_bmiage==. & hh_no_uw_st==1 & no_child_eligible==0) | (hh_no_low_bmiage==1 & hh_no_uw_st==. & no_adults_eligible_bmi==0)
replace hh_nutrition_uw_st = 0 if hh_no_low_bmiage==0 | hh_no_uw_st==0
replace hh_nutrition_uw_st = . if hh_no_low_bmiage==. & hh_no_uw_st==.
replace hh_nutrition_uw_st = 1 if no_eligibles_bmi==1
/*If country have collected anthropometric data from women, 
	child 0-5 & a subsample of men, we only replace households which do not have 
	any of these three applicable population as non-deprived*/
lab var hh_nutrition_uw_st "Household has no child underweight/stunted or adult deprived by BMI/BMI-for-age"

********************************************************************************
*** Step 2.4 Child Mortality ***
********************************************************************************

codebook v206 v207 mv206 mv207
	//v206 or mv206: number of sons who have died 
	//v207 or mv207: number of daughters who have died
	

	//Total child mortality reported by eligible women
egen temp_f = rowtotal(v206 v207), missing
replace temp_f = 0 if v201==0
bysort	hh_id: egen child_mortality_f = sum(temp_f), missing
lab var child_mortality_f "Occurrence of child mortality reported by women"
tab child_mortality_f, miss
drop temp_f
	
	//Total child mortality reported by eligible men	
egen temp_m = rowtotal(mv206 mv207), missing
replace temp_m = 0 if mv201==0
bysort	hh_id: egen child_mortality_m = sum(temp_m), missing
lab var child_mortality_m "Occurrence of child mortality reported by men"
tab child_mortality_m, miss
drop temp_m

egen child_mortality = rowmax(child_mortality_f child_mortality_m)
lab var child_mortality "Total child mortality within household reported by women & men"
tab child_mortality if subsample==1, miss	
	
	
/*Deprived if any children died in the household */
************************************************************************

gen	hh_mortality = (child_mortality==0)
	/*Household is replaced with a value of "1" if there is no incidence of 
	child mortality*/
replace hh_mortality = . if child_mortality==.

replace hh_mortality = 1 if no_adults_eligible==1 
	/*Change eligibility to "no_fem_eligible==1" if child mortality indicator 
	is constructed solely using information from women */
	
lab var hh_mortality "Household had no child mortality"
tab hh_mortality if subsample==1, miss


/*Deprived if any children died in the household in the last 5 years 
from the survey year */
************************************************************************

tab child_died_per_wom_5y, miss
	/* The 'child_died_per_wom_5y' variable was constructed in Step 1.2 using 
	information from individual women who ever gave birth in the BR file. The 
	missing values represent eligible woman who have never ever given birth and 
	so are not present in the BR file. But these 'missing women' may be living 
	in households where there are other women with child mortality information 
	from the BR file. So at this stage, it is important that we aggregate the 
	information that was obtained from the BR file at the household level. This
	ensures that women who were not present in the BR file is assigned with a 
	value, following the information provided by other women in the household.*/

replace child_died_per_wom_5y = 0 if v201==0 
	/*Assign a value of "0" for:
	- all eligible women who never ever gave birth */
replace child_died_per_wom_5y = 0 if no_fem_eligible==1 
	/*Assign a value of "0" for:
	- individuals living in households that have non-eligible women */	
	
bysort hh_id: egen child_mortality_5y = sum(child_died_per_wom_5y), missing

replace child_mortality_5y = 0 if child_mortality_5y==. & child_mortality==0
	/*Replace all households as 0 death if women has missing value and men 
	reported no death in those households */

label var child_mortality_5y "Total child mortality within household past 5 years reported by women"
tab child_mortality_5y if subsample==1, miss

	/*
	The new standard MPI indicator takes a value of "1" if eligible women  
	within the household reported no child mortality or if any child died longer 
	than 5 years from the survey year. The indicator takes a value of "0" if 
	women in the household reported any child mortality in the last 5 years from 
	the survey year. Households were replaced with a value of "1" if eligible 
	men within the household reported no child mortality in the absence of 
	information from women. The indicator takes a missing value if there was 
	missing information on reported death from eligible individuals.
	*/

gen hh_mortality_5y = (child_mortality_5y==0)
replace hh_mortality_5y = . if child_mortality_5y==.
tab hh_mortality_5y if subsample==1, miss	
lab var hh_mortality_5y "Household had no child mortality in the last 5 years"



********************************************************************************
*** Step 2.5 Electricity ***
********************************************************************************
/*Members of the household are considered deprived if the household has no 
electricity */

clonevar electricity = hv206 
codebook electricity, tab (10)
replace electricity = . if electricity==9 
	//Please check that missing values remain missing
label var electricity "Household has electricity"


********************************************************************************
*** Step 2.6 Sanitation ***
********************************************************************************

/*Members of the household are considered deprived if the household's sanitation 
facility is not improved, according to MDG guidelines, or it is improved but 
shared with other household. In cases of mismatch between the MDG guideline and 
country report, we followed the country report. */

clonevar toilet = hv205  
codebook toilet, tab(30) 
codebook hv225, tab(30)  
clonevar shared_toilet = hv225 
	//0=no;1=yes;.=missing

gen toilet_mdg = 1 if (toilet==11 | toilet==12 | toilet==13 | toilet==21 | toilet==22 | toilet==41) & shared_toilet!=1
replace toilet_mdg = 0 if toilet==14 | toilet==15 | toilet==23 | toilet==31 | toilet==42 | toilet==43 | toilet==96
replace toilet_mdg = 0 if shared_toilet==1 
replace toilet_mdg = . if toilet==.  | toilet==99
lab var toilet_mdg "Household has improved sanitation with MDG Standards"
tab toilet toilet_mdg, miss

/* follows DHS report page 20:
              type of toilet facility |      Freq.     Percent        Cum.
--------------------------------------+-----------------------------------
          flush to piped sewer system |        642        0.87        0.87 11 y
                 flush to septic tank |      1,210        1.64        2.51 12 y
                 flush to pit latrine |      1,972        2.67        5.17 13 y
              flush to somewhere else |        174        0.24        5.41 14 n
              flush, don't know where |        145        0.20        5.61 15 n
ventilated improved pit latrine (vip) |        502        0.68        6.29 21 y
                pit latrine with slab |     11,531       15.60       21.89 22 y
    pit latrine without slab/open pit |     28,849       39.04       60.93 23 n
               no facility/bush/field |     27,904       37.76       98.68 31 n
                    composting toilet |        484        0.65       99.34 41 y
                        bucket toilet |         19        0.03       99.37 42 n
               hanging toilet/latrine |        136        0.18       99.55 43 n
                                other |        333        0.45      100.00 96 n
--------------------------------------+-----------------------------------
                                Total |     73,901      100.00 */

********************************************************************************
*** Step 2.7 Drinking Water  ***
********************************************************************************

/*Members of the household are considered deprived if the household does not 
have access to safe drinking water according to MDG guidelines, or safe drinking 
water is more than a 30-minute walk from home roundtrip. In cases of mismatch 
between the MDG guideline and country report, we followed the country report.*/


clonevar water = hv201  
clonevar timetowater = hv204  
codebook water, tab(100)	
clonevar ndwater = hv202  
	//Non-drinking water - no observation

		/*For Ethiopia, we followed the DHS report (page 19) */

/*                  piped into dwelling |      1,263        1.71        1.71 11 y
                     piped to yard/plot |     11,448       15.49       17.20 12 y
                      piped to neighbor |      3,121        4.22       21.42 13 y
                   public tap/standpipe |     10,940       14.80       36.23 14 y
                  tube well or borehole |     10,226       13.84       50.06 21 y
                         protected well |      5,145        6.96       57.03 31 y
                       unprotected well |      4,938        6.68       63.71 32 n
                       protected spring |      6,065        8.21       71.92 41 y
                     unprotected spring |     10,078       13.64       85.55 42 n
river/dam/lake/ponds/stream/canal/irrig |      8,923       12.07       97.63 43 n
                              rainwater |        447        0.60       98.23 51 y
                           tanker truck |        458        0.62       98.85 61 n
                   cart with small tank |        374        0.51       99.36 62 n
                          bottled water |        394        0.53       99.89 71 y/n
                                  other |         81        0.11      100.00 96 n */


gen water_mdg = 1 if water==11 | water==12 | water==13 | water==14 | water==21 |  water==31 | water==41 | water==51 | (water==71 & (ndwater==11 | ndwater==12 | ndwater==13 | ndwater==14 | ndwater==21 | ndwater==31))
replace water_mdg = 0 if water==32 | water==42 | water==43 | water==61 | water==62 | (water==71 & (ndwater==32 | ndwater==61 | ndwater==62 | ndwater==96)) | water==96 
replace water_mdg = 0 if (water_mdg==1 | water_mdg==.) & timetowater >= 30 & timetowater!=. & timetowater!=996 & timetowater!=998 & timetowater!=999 
	//Deprived if water is at more than 30 minutes' walk (roundtrip) 


replace water_mdg = . if water==. | water==99
lab var water_mdg "Household has drinking water with MDG standards (considering distance)"
tab water water_mdg, miss


********************************************************************************
*** Step 2.8 Housing ***
********************************************************************************

/* Members of the household are considered deprived if the household 
has a dirt, sand or dung floor */

clonevar floor = hv213 
codebook floor, tab(99)
gen	floor_imp = 1
replace floor_imp = 0 if floor==11 | floor==12 | floor==13 | floor==96  
	//Deprived if "mud/earth", "sand", "dung", "other" 	
replace floor_imp = . if floor==. | floor==99 
lab var floor_imp "Household has floor that it is not earth/sand/dung"
tab floor floor_imp, miss		



/* Members of the household are considered deprived if the household has wall 
made of natural or rudimentary materials */
clonevar wall = hv214 
codebook wall, tab(99)	
gen	wall_imp = 1 
replace wall_imp = 0 if wall<=26 | wall==96  
	/*Deprived if "no wall" "cane/palms/trunk" "mud/dirt" 
	"grass/reeds/thatch" "pole/bamboo with mud" "stone with mud" "plywood"
	"cardboard" "carton/plastic" "uncovered adobe" "canvas/tent" 
	"unburnt bricks" "reused wood" "other"*/
replace wall_imp = . if wall==. | wall==99 	
lab var wall_imp "Household has wall that it is not of low quality materials"
tab wall wall_imp, miss	
	

	
/* Members of the household are considered deprived if the household has roof 
made of natural or rudimentary materials */
clonevar roof = hv215
codebook roof, tab(99)		
gen	roof_imp = 1 
replace roof_imp = 0 if roof<=26 | roof==96  
	/*Deprived if "no roof" "thatch/palm leaf" "mud/earth/lump of earth" 
	"sod/grass" "plastic/polythene sheeting" "rustic mat" "cardboard" 
	"canvas/tent" "wood planks/reused wood" "unburnt bricks" "other"*/
replace roof_imp = . if roof==. | roof==99 	
lab var roof_imp "Household has roof that it is not of low quality materials"
tab roof roof_imp, miss



/*Household is deprived in housing if the roof, floor OR walls uses 
low quality materials.*/
gen housing_1 = 1
replace housing_1 = 0 if floor_imp==0 | wall_imp==0 | roof_imp==0
replace housing_1 = . if floor_imp==. & wall_imp==. & roof_imp==.
lab var housing_1 "Household has roof, floor & walls that it is not low quality material"
tab housing_1, miss


********************************************************************************
*** Step 2.9 Cooking Fuel ***
********************************************************************************

/* Members of the household are considered deprived if the household cooks with 
solid fuels: wood, charcoal, crop residues or dung. "Indicators for Monitoring 
the Millennium Development Goals", p. 63 */

clonevar cookingfuel = hv226  
codebook cookingfuel, tab(99)


gen cooking_mdg = 1 if cookingfuel<=5 | cookingfuel==95 | cookingfuel==96
replace cooking_mdg = 0 if (cookingfuel>5 & cookingfuel<=11)
replace cooking_mdg = . if cookingfuel==. | cookingfuel==99
lab var cooking_mdg "Househod has cooking fuel according to MDG standards"	 
tab cookingfuel cooking_mdg, miss	


********************************************************************************
*** Step 2.10 Assets ownership ***
********************************************************************************

/* Members of the household are considered deprived if the household does not 
own more than one of: radio, TV, telephone, bike, motorbike or refrigerator and 
does not own a car or truck. */

	//Check that for standard assets in living standards: "no"==0 and yes=="1"
codebook hv208 hv207 hv221 hv243a hv209 hv212 hv210 hv211 hv243c hv243e

clonevar television = hv208 
gen bw_television   = .
clonevar radio = hv207 
clonevar telephone =  hv221 
clonevar mobiletelephone = hv243a  
clonevar refrigerator = hv209 
clonevar car = hv212  	
clonevar bicycle = hv210 
clonevar motorbike = hv211 
clonevar computer = hv243e
clonevar animal_cart = hv243c

* animal cart not available *

foreach var in television radio telephone mobiletelephone refrigerator ///
			   car bicycle motorbike computer animal_cart {
replace `var' = . if `var'==9 | `var'==99 | `var'==8 | `var'==98 
}
	//9 , 99 and 8, 98 are missing values
	

	//Group telephone and mobiletelephone as a single variable
replace telephone=1 if telephone==0 & mobiletelephone==1
replace telephone=1 if telephone==. & mobiletelephone==1


/* Members of the household are considered deprived in assets if the household 
does not own more than one of: radio, TV, telephone, bike, motorbike, 
refrigerator, computer or animal_cart and does not own a car or truck.*/

egen n_small_assets2 = rowtotal(television radio telephone refrigerator bicycle motorbike computer animal_cart), missing
lab var n_small_assets2 "Household Number of Small Assets Owned" 
  
  
gen hh_assets2 = (car==1 | n_small_assets2 > 1) 
replace hh_assets2 = . if car==. & n_small_assets2==.
lab var hh_assets2 "Household Asset Ownership: HH has car or more than 1 small assets incl computer & animal cart"


	
********************************************************************************
*** Step 2.11 Rename and keep variables for MPI calculation 
********************************************************************************
	

	//Retain data on sampling design: 
desc hv022 hv021	
clonevar strata = hv022
clonevar psu = hv021


	//Retain year, month & date of interview:
desc hv007 hv006 hv008
clonevar year_interview = hv007 	
clonevar month_interview = hv006 
clonevar date_interview = hv008
 

*** Rename key global MPI indicators for estimation ***
recode hh_mortality_5y      (0=1)(1=0) , gen(d_cm)
recode hh_nutrition_uw_st 	(0=1)(1=0) , gen(d_nutr)
recode hh_child_atten 		(0=1)(1=0) , gen(d_satt)
recode hh_years_edu6 		(0=1)(1=0) , gen(d_educ)
recode electricity 			(0=1)(1=0) , gen(d_elct)
recode water_mdg 			(0=1)(1=0) , gen(d_wtr)
recode toilet_mdg 			(0=1)(1=0) , gen(d_sani)
recode housing_1 			(0=1)(1=0) , gen(d_hsg)
recode cooking_mdg 			(0=1)(1=0) , gen(d_ckfl)
recode hh_assets2 			(0=1)(1=0) , gen(d_asst)



*** Keep selected variables for global MPI estimation ***
keep hh_id ind_id ccty ccnum cty survey year subsample ///
strata psu weight area relationship sex age agec7 agec4 marital hhsize ///
region year_interview month_interview date_interview ///
d_cm d_nutr d_satt d_educ d_elct d_wtr d_sani d_hsg d_ckfl d_asst hh_mortality_5y hh_nutrition_uw_st hh_child_atten hh_years_edu6 electricity water_mdg toilet_mdg housing_1 cooking_mdg hh_assets2 

order hh_id ind_id ccty ccnum cty survey year subsample ///
strata psu weight area relationship sex age agec7 agec4 marital hhsize ///
region year_interview month_interview date_interview ///
d_cm d_nutr d_satt d_educ d_elct d_wtr d_sani d_hsg d_ckfl d_asst hh_mortality_5y hh_nutrition_uw_st hh_child_atten hh_years_edu6 electricity water_mdg toilet_mdg housing_1 cooking_mdg hh_assets2 



*** Sort, compress and save data for estimation ***
sort ind_id
compress
save "$path_out/eth_dhs16_pov.dta", replace 
log close




********************************************************************************
*** MPI Calculation (TTD file)
********************************************************************************

**SELECT COUNTRY POV FILE RUN ON LOOP FOR MORE COUNTRIES

use "$path_out\eth_dhs16_pov.dta", clear

********************************************************************************
*** Define Sample Weight and total population ***
********************************************************************************
gen sample_weight = weight/1000000 
	//only DHS


gen country = "Ethiopia" 
gen countrycode = "ETH"  


/* change to weight if MICS*/
	
********************************************************************************
*** List of the 10 indicators included in the MPI ***
********************************************************************************

gen edu_1 = hh_years_edu6
gen atten_1 = hh_child_atten
gen cm_1 = hh_mortality_5y
/* change countries with no child mortality 5 year to child mortality ever*/
gen nutri_1 = hh_nutrition_uw_st
gen elec_1 = electricity
gen toilet_1 = toilet_mdg
gen water_1 = water_mdg
gen house_1 = housing_1
gen fuel_1 = cooking_mdg
gen asset_1 = hh_assets2

global est_1 edu_1 atten_1 cm_1 nutri_1 elec_1 toilet_1 water_1 house_1 fuel_1 asset_1

********************************************************************************
*** List of sample without missing values ***
********************************************************************************

foreach j of numlist 1 {
gen sample_`j' = (edu_`j'!=. & atten_`j'!=. & cm_`j'!=. & nutri_`j'!=. & elec_`j'!=. & toilet_`j'!=. & water_`j'!=. & house_`j'!=. & fuel_`j'!=. & asset_`j'!=.)

replace sample_`j' = . if subsample==0
	/* Note: If the anthropometric data was collected from a subsample of the 
	total population that was sampled, then the final analysis only includes the 
	subsample population. */ 

*** Percentage sample after dropping missing values ***
sum sample_`j' [iw = sample_weight]
gen per_sample_weighted_`j' = r(mean)

sum sample_`j'
gen per_sample_`j' = r(mean)
}
***

********************************************************************************
*** Define deprivation matrix 'g0' 
*** which takes values 1 if individual is deprived in the particular 
*** indicator according to deprivation cutoff z as defined during step 2 ***
********************************************************************************
foreach j of numlist 1 {
foreach var in ${est_`j'} {  
	gen	g0`j'_`var' = 1 if `var'==0
	replace g0`j'_`var' = 0 if `var'==1
	}
}
	
*** Raw Headcount Ratios
foreach j of numlist 1 {
foreach var in ${est_`j'}   {  
	sum	g0`j'_`var' if sample_`j'==1 [iw = sample_weight]
	gen	raw`j'_`var' = r(mean)*100
	lab var raw`j'_`var'  "Raw Headcount: Percentage of people who are deprived in `var'"
	}
}
********************************************************************************
*** Define vector 'w' of dimensional and indicator weight ***
********************************************************************************
/*If survey lacks one or more indicators, weights need to be adjusted within /
each dimension such that each dimension weighs 1/3 and the indicator weights
add up to one (100%). CHECK COUNTRY FILE*/

foreach j of numlist 1 {
// DIMENSION EDUCATION 
foreach var in edu_`j' atten_`j' {
capture drop w`j'_`var' 
	gen w`j'_`var' = 1/6
	}

// DIMENSION HEALTH
foreach var in cm_`j' nutri_`j' {
	capture drop w`j'_`var'
	gen w`j'_`var' = 1/6
	}

// DIMENSION LIVING STANDARD
foreach var in elec_`j' toilet_`j' water_`j' house_`j' fuel_`j' asset_`j' {
	
	capture drop w`j'_`var'
	gen w`j'_`var' = 1/18
	}

}
********************************************************************************
*** Generate the weighted deprivation matrix 'w' * 'g0'
********************************************************************************

foreach j of numlist 1 {
foreach var in ${est_`j'} {
	gen	w`j'_g0_`var' = w`j'_`var' * g0`j'_`var' 
	replace w`j'_g0_`var' = . if sample_`j'!=1 
	/*The estimation is based only on observations that have non-missing values 
	for all variables in varlist_pov*/
	}
}
********************************************************************************
*** Generate the vector of individual weighted deprivation count 'c'
********************************************************************************

foreach j of numlist 1 {
egen	c_vector_`j' = rowtotal(w`j'_g0_*)
replace c_vector_`j' = . if sample_`j'!=1
*drop	w_g0_*
}

********************************************************************************
*** Identification step according to poverty cutoff k (20 33.33 50) ***
********************************************************************************

foreach j of numlist 1 {
	foreach k of numlist 20 33 50 {
		gen	multidimensionally_poor_`j'_`k' = (c_vector_`j'>=`k'/100)
		replace multidimensionally_poor_`j'_`k' = . if sample_`j'!=1 
		//Takes value 1 if individual is multidimensional poor
	}
}

********************************************************************************
*** Generate the censored vector of individual weighted deprivation count 'c(k)'
********************************************************************************


foreach j of numlist 1 {
	foreach k of numlist 20 33 50 {
		gen	c_censured_vector_`j'_`k' = c_vector_`j'
		replace c_censured_vector_`j'_`k' = 0 if multidimensionally_poor_`j'_`k'==0 
	}	//Provide a score of zero if a person is not poor
}
*

********************************************************************************
*** Define censored deprivation matrix 'g0(k)' ***
********************************************************************************

foreach j of numlist 1 {
foreach var in ${est_`j'} {
	gen	g0`j'_k_`var' = g0`j'_`var' 
	replace g0`j'_k_`var' = 0 if multidimensionally_poor_`j'_33==0
	replace g0`j'_k_`var' = . if sample_`j'!=1 
	}
}	
********************************************************************************
*** Generates Multidimensional Poverty Index (MPI), 
*** Headcount (H) and Intensity of Poverty (A) ***
********************************************************************************

*** Multidimensional Poverty Index (MPI) ***
foreach j of numlist 1 {
	foreach k of numlist 20 33 50 {
		sum	c_censured_vector_`j'_`k' [iw = sample_weight] if sample_`j'==1
		gen	MPI_`j'_`k' = r(mean)
		lab var MPI_`j'_`k' "MPI with k=`k'"
	}
	
	sum	c_censured_vector_`j'_33 [iw = sample_weight] if sample_`j'==1
	gen	MPI_`j' = r(mean)
	lab var MPI_`j' "`j' Multidimensional Poverty Index (MPI = H*A): Range 0 to 1"

*** Headcount (H) ***
	sum	multidimensionally_poor_`j'_33 [iw = sample_weight] if sample_`j'==1
	gen	H_`j' = r(mean)*100
	lab var H_`j' "`j' Headcount ratio: % Population in multidimensional poverty (H)"

*** Intensity of Poverty (A) ***
	sum	c_censured_vector_`j'_33 [iw = sample_weight] if multidimensionally_poor_`j'_33==1 & sample_`j'==1
	gen	A_`j' = r(mean)*100
	lab var A_`j'  "`j' Intensity of deprivation among the poor (A): Average % of weighted deprivations"

*** Population vulnerable to poverty (who experience 20-32.9% intensity of deprivations) ***
	gen	temp = 0
	replace temp = 1 if c_vector_`j'>=0.2 & c_vector_`j'<0.3332
	replace temp = . if sample_`j'!=1
	sum	temp [iw = sample_weight] 
	gen	vulnerable_`j' = r(mean)*100
	lab var vulnerable_`j'  "`j' % Population vulnerable to poverty (who experience 20-32.9% intensity of deprivations)"
	drop	temp

*** Population in severe poverty (with intensity 50% or higher) *** 
	gen	temp = 0
	replace temp = 1 if c_vector_`j'>0.49
	replace temp = . if sample_`j'!=1
	sum	temp [iw = sample_weight] 
	gen	severe_`j' = r(mean)*100
	lab var severe_`j'  "`j' % Population in severe poverty (with intensity 50% or higher)"
	drop	temp	
}
*

*** Censored Headcount ***

foreach j of numlist 1 {
foreach var in ${est_`j'} {
	sum	g0`j'_k_`var' [iw = sample_weight] if sample_`j'==1
	gen	cen`j'_`var' = r(mean)*100 
	lab var cen`j'_`var'  "Censored Headcount: Percentage of people who are poor and deprived in `var'"
	}
}
*** Dimensional Contribution ***

foreach j of numlist 1 {
foreach var in ${est_`j'} {	
gen	cont`j'_`var' = (w`j'_`var' * cen`j'_`var')/MPI_`j' if sample_`j'==1
	lab var cont`j'_`var'  "% Contribution in MPI of indicator..."
	}
}

*** Prepare results to export ***
keep subsample country year survey per_sample_weighted* per_sample* MPI* H* A* vulnerable* severe* raw* cen* cont* 


keep if subsample==1
gen  temp = (_n)
keep if temp==1
drop temp

order MPI_1 H_1 A_1 vulnerable_1 severe_1 cont1_nutri_1 cont1_cm_1 cont1_edu_1 cont1_atten_1 cont1_fuel_1 cont1_toilet_1 cont1_water_1 cont1_elec_1 cont1_house_1 cont1_asset_1 per_sample_1 per_sample_weighted_1

codebook, compact

clear

