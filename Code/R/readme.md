# we need to readjust the indicators to ensure consistency across years: Use only indicators available in every year (2000, 2005, 2011, 2016).
This usually leaves:

v744a–v744e (attitudes)
maybe v739 (general autonomy)

Drop:

v743a, v743b (do not exist in 2000)
v743c/d/e/f in later waves if missing in early waves
s723c (not in 2000)
v632 (heavily skipped in 2000)

This will give you a stable, comparable AF index across years.

## analysis update : 

The youngest age band (15–24) exhibits substantially more missing values for the agency index (103 observations dropped). This reflects DHS skip patterns: the decision-making variable v743b is only asked of women who are currently married or living with a partner, and marriage rates in this age group are low. In addition, attitudes-to-violence items (v744a–e) show higher non-response among young women. As a result, many women aged 15–24 have no observed agency indicators and therefore no computable c_score. This limits the interpretability of the first-stage DiD estimates for the youngest cohort, since the index captures agency primarily within marriage or cohabitation contexts.
