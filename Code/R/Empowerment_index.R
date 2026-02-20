# ================================================================
# Ethiopia DHS 2016 (IR) - Women's Agency Index (Alkire–Foster style)
# Author: Arthur Martin 
# Last updated: Sys.Date(20.02)

## need to decide on how to recode 'other.' in reference to who decides 
## need to recode / account for missing domains as this will effect the total 
#score

## could add in an ifelse clause where we read in the year and then change the variables
# due to which edition the survey design is. so set a var == avg(v002) if v002 is 
# year of survey. Then we have a logic gate which says that if year == 2016 then select
# vars == 2016 set. so in the recode function and other aspects we set it so we can run the same 
# file but each year the varaible names change ever so slighty. we can create year vectors!! 
# so if  year == 2016 then vars = 2016_set, then this applies the set of functions due to the
# vars in that aspect. 

## v850a can you refuse sex.. hard one to decide on this! 
##  v850b - respondent can ask partner to use a condom - have not included it but it is hard 
# ================================================================



# ---- 0) Packages ----
library(haven)     # read Stata .dta
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(purrr)
library(survey)    # weighted H, A, M0
library(readr)
library(haven)
library(Hmisc)
library(skimr)

user <- Sys.getenv("USERNAME")

if (user == "Arthur.Martin") {
  base_directory <- paste0(
    "C:/Users/", user,
    "/OneDrive - Department of Health and Social Care/Documents/LSE/",
    "DV495_DISSERTATION/DV495_EFLR_Agency"
  )
}

output_directory <- paste0(base_directory, "/Outputs/figures")

path_in <- paste0(
  base_directory,
  "/Data/Data_raw/Ethiopia/ET_2016_DHS_Standard/ETIR71FL.DTA"
)


# ---- 1) Parameters ----
# file_path <- "Data/Data_raw/Ethiopia/ET_2016_DHS_Standard/ETIR71FL.DTA"  # <- EDIT THIS
k_cutoff  <- 0.33                    # AF poverty cut-off for agency (changeable)
domain_wA <- 0.70                    # Decision autonomy domain weight
domain_wB <- 0.30                    # Attitudes-to-violence domain weight

# Optional: sensitivity grids
weight_grid <- tibble(
  domain_wA = c(0.5, 0.7, 0.9),
  domain_wB = 1 - domain_wA
)
k_grid <- c(0.20, 0.33, 0.40)

# ---- 2) Load data ----
ir_raw <- read_dta(path_in)

# DHS weights (v005 is 6-decimal scaled)
ir <- ir_raw %>%
  mutate(wt = as.numeric(v005) / 1e6)

# ---- 3) Helper recode functions ----



## combine the two contraception varaibles into one varaible as asked to whether you use contra or not 
# 1) Combine into one: prefer v632, else v632a
ir <- ir %>%
  mutate(
    v632_comb = coalesce(v632, v632a)  # take v632 if non-missing, else v632a
  )
# ------------------ Helper recode functions ----------------------------------------------------------
var_check <- function(df, x) {
  print(unique(df[[x]]))
}


contraception_vars <- c("v632_comb")

for (v in contraception_vars) {
  cat("\n---", v, "---\n")
  var_check(ir, v)
}

# Recode into deprivation indicator
recode_contra_decision <- function(x) {
  x <- as.numeric(x)
  case_when(
    x %in% c(1, 3) ~ 0L,  # respondent or joint => NOT deprived
    x %in% c(2, 6) ~ 1L,  # husband/partner alone or other => deprived
    TRUE           ~ NA_integer_
  )
}

# Wife-beating attitudes (v744*): deprived = 1 if "Yes, justified"
# DHS usually: 0 = no, 1 = yes, 8 = don't know, 9 = missing
recode_beating <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x == 1  ~ 1L,     # justifies beating -> deprived
    x == 0  ~ 0L,     # does not justify -> not deprived
    TRUE ~ NA_integer_
  )
}
## quick check to ensure all beating values follow this pattern 

beat_vars <- c("v744a", "v744b", "v744c", "v744d", "v744e")

for (v in beat_vars) {
  cat("\n---", v, "---\n")
  var_check(ir, v)
}

## happy with the coding of this

# Decision-making v743*: deprived = 1 if no say (husband/other alone)
# DHS7 codes typically: 1 = respondent alone, 2 = husband alone, 3 = joint, 4 = someone else, 5 = other
desc_vars <- c("v743a", "v743b")

for (v in desc_vars) {
  cat("\n---", v, "---\n")
  var_check(ir, v)
}

recode_decision <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x %in% c(1, 2, 3)       ~ 0L,     # respondent alone or joint -> not deprived
    x %in% c(4, 5, 6)    ~ 1L,     # husband alone / someone else / other -> deprived
    TRUE                 ~ NA_integer_
  )
}

sex_vars <- c("v850a")

for (v in sex_vars) {
  cat("\n---", v, "---\n")
  var_check(ir, v)
}


# Can refuse sex s723c (2005) v850a in 2016: deprived = cannot refuse
# Typical (survey-specific) codes: 1 = yes, 2 = no, 8/9 = dk/missing
recode_v850a <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x  %in% c(1)      ~ 0L,     # can refuse / don't know/ not sure -> not deprived
    x %in% c(0,8)     ~ 1L,     # cannot refuse -> deprived
    TRUE              ~ NA_integer_
  )
}

# ---- 4) Build deprivation indicators (0/1/NA) ----
agency_raw <- ir %>%
  transmute(
    caseid,
    v001, v002, v003,             # cluster/household/line for joins if needed
    region = v024,                # region identifier
    wt,
    
    # --- Domain B: Attitudes to wife-beating (5 items) ---
    B_v744a = recode_beating(v744a), # goes out without telling him
    B_v744b = recode_beating(v744b), # neglects the children
    B_v744c = recode_beating(v744c), # argues with him
    B_v744d = recode_beating(v744d), # refuses sex
    B_v744e = recode_beating(v744e), # burns food
    
    # --- Domain A: Decision autonomy (4 items) ---
    A_v743a = recode_decision(v743a), # who usually decides healthcare
    A_v743b = recode_decision(v743b), # final say large purchases
   # A_v743c = recode_decision(v743c), # in 2016 this is decision 
    A_v632  = recode_contra_decision(v632_comb),# decision maker using contraception
    A_v850a = recode_v850a(v850a)     # can refuse sexual intercourse
  )

# ---- 5) Person-specific reweighting to preserve domain totals ----
A_vars <- c("A_v743a","A_v743b","A_v632","A_v850a")
B_vars <- c("B_v744a","B_v744b","B_v744c","B_v744d","B_v744e")

# function to create weights that sum to the domain weight for observed items
mk_domain_weights <- function(df, vars, domain_total_weight) {
  
  obs <- df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ !is.na(.)))  # TRUE if observed
  
  den <- rowSums(as.matrix(obs))  # count observed in domain, per row
  
  # weights: observed indicators get domain_total_weight/den; missing get 0
  w <- as.data.frame(obs) * 0  # same shape, all zeros
  for (j in seq_along(vars)) {
    w[[j]] <- ifelse(obs[[j]] & den > 0, domain_total_weight / den, 0)
  }
  names(w) <- vars
  w
}

A_w <- mk_domain_weights(agency_raw, A_vars, domain_wA)
B_w <- mk_domain_weights(agency_raw, B_vars, domain_wB)

# Combine back
agency_wide <- bind_cols(agency_raw, 
                         setNames(A_w, paste0(names(A_w), "_w")),
                         setNames(B_w, paste0(names(B_w), "_w")))

# ---- 6) Compute AF deprivation score c_i, identify agency-poor ----
# c_i = sum_j  d_ij * w_ij  with domain-preserving reweights
all_vars <- c(A_vars, B_vars)

agency_scored <- agency_wide %>%
  rowwise() %>%
  mutate(
    n_obs = sum(!is.na(c_across(all_of(all_vars)))),
    c_score = ifelse(
      n_obs == 0,
      NA_real_,
      sum(
        c_across(all_of(all_vars)) * c_across(all_of(paste0(all_vars, "_w"))),
        na.rm = TRUE
      )
    ),
    agency_poor = ifelse(is.na(c_score), NA_integer_, as.integer(c_score >= k_cutoff))
  ) %>%
  ungroup()

###=============================================================================
###=================================== visualisation ===========================
###=============================================================================

# Survey design (weights only add ids/strata later if I Need)
des <- svydesign(ids = ~1, weights = ~wt, data = agency_scored)

# Weighted mean + SE by region
region_means <- svyby(
  ~c_score,
  ~region,
  design = des,
  FUN = svymean,
  na.rm = TRUE,
  vartype = "se"
) %>%
  as.data.frame() %>%
  rename(mean_c_score = c_score, se = se) %>%
  mutate(
    region = as.factor(region),
    ci_low  = mean_c_score - 1.96 * se,
    ci_high = mean_c_score + 1.96 * se
  )

# Plot (bars + 95% CI)
plot1 <- ggplot(region_means, aes(x = region, y = mean_c_score)) +
  geom_col(fill = "#2C7FB8", alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  labs(
    title = "Average women's agency deprivation score (c_score) by region 2016",
    subtitle = "Weighted using DHS sampling weights (v005/1e6)",
    x = "Region",
    y = "Mean c_score (0 = none, 1 = maximum deprivation)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = paste0(output_directory, "/Avg_agency_region_2016.png"),
  plot = plot1,
  width = 9, height = 5, dpi = 300
)

# ##==============================================================================
# ##============================ Part 2 - aggregation of the deprived scores =====
# 
# # this part is not as important for now as we are focused on who is deprived,
# # care less about thier aggregation. 
# 
# # ---- 7) AF aggregates: H, A, M0 (weighted) ----
# # H = weighted share agency_poor; 
# # A = weighted mean c_score among the poor; 
# # M0 = H * A
# design_all <- svydesign(ids = ~1, weights = ~wt, data = agency_scored)
# 
# H <- as.numeric(svymean(~I(agency_poor==1), design_all, na.rm=TRUE))
# # Intensity among poor:
# design_poor <- subset(design_all, agency_poor == 1)
# A <- if (sum(weights(design_poor)) > 0) as.numeric(svymean(~c_score, design_poor, na.rm=TRUE)) else NA_real_
# M0 <- H * A
# 
# af_summary <- tibble(
#   k_cutoff = k_cutoff,
#   domain_wA = domain_wA,
#   domain_wB = domain_wB,
#   H = H,
#   A = A,
#   M0 = M0
# )
# 
# # ---- 8) Indicator analysis: censored headcounts & contributions ----
# # Censored deprivation for indicator j: d_ij * 1(agency_poor)
# 
# make_censored <- function(df, var) {
#   as.integer(df[[var]] == 1 & df$agency_poor == 1)
# }
# 
# all_inds <- c(A_vars, B_vars)
# 
# # Create censored indicators
# cens_df <- agency_scored %>%
#   mutate(across(all_of(all_inds),
#                 ~ make_censored(agency_scored, cur_column()),
#                 .names = "{.col}_cens"))
# 
# 
# # Build survey designs for:
# #  (a) censored headcounts (needs *_cens), and
# #  (b) average indicator weights (uses *_w)
# design_cens <- svydesign(ids = ~1, weights = ~wt, data = cens_df)
# design_all  <- svydesign(ids = ~1, weights = ~wt, data = agency_scored)
# 
# # Weighted censored headcount for each indicator (H_cens,j)
# cens_H <- map_dbl(all_inds, function(v) {
#   svymean(as.formula(paste0("~", v, "_cens")), design_cens, na.rm = TRUE) %>% as.numeric()
# })
# 
# # Average indicator weights (across the whole population) for contribution accounting
# avg_w <- map_dbl(all_inds, function(v) {
#   svymean(as.formula(paste0("~", v, "_w")), design_all, na.rm = TRUE) %>% as.numeric()
# })
# 
# 
# M0_scalar <- as.numeric(M0)[1]  # guard against accidental length > 1
# 
# contrib_tbl <- tibble(
#   indicator = all_inds,
#   censored_headcount = cens_H,                # length = number of indicators
#   avg_indicator_weight = avg_w                # length = number of indicators
# ) %>%
#   mutate(
#     pct_contribution = if (is.finite(M0_scalar) && M0_scalar > 0) {
#       (censored_headcount * avg_indicator_weight) / M0_scalar
#     } else {
#       NA_real_
#     }
#   ) %>%
#   arrange(desc(pct_contribution))
# 
# # ---- 9) Save outputs ----
# out_individual <- agency_scored %>%
#   select(caseid, v001, v002, v003, region, wt, c_score, agency_poor)
# 
# # write_csv(out_individual, "ethiopia2016_agency_index_individual.csv")
# # write_csv(af_summary,    "ethiopia2016_agency_index_AFsummary.csv")
# # write_csv(contrib_tbl,   "ethiopia2016_agency_index_indicator_contributions.csv")
# 
# # ---- 10) Sensitivity checks (optional) ----
# # Explore alternative A/B domain weights and k values; return H, A, M0
# compute_AF <- function(domain_wA, k_cutoff, df = agency_raw) {
#   domain_wB <- 1 - domain_wA
#   A_w <- mk_domain_weights(df, A_vars, domain_wA)
#   B_w <- mk_domain_weights(df, B_vars, domain_wB)
#   dat <- bind_cols(df, setNames(A_w, paste0(names(A_w), "_w")), setNames(B_w, paste0(names(B_w), "_w"))) %>%
#     rowwise() %>%
#     mutate(c_score = sum(c_across(all_of(c(A_vars, B_vars))) * 
#                            c_across(all_of(paste0(c(A_vars, B_vars), "_w"))), na.rm = TRUE),
#            agency_poor = as.integer(c_score >= k_cutoff)) %>%
#     ungroup() %>%
#     left_join(ir %>% select(caseid, wt), by = "caseid")
#   des <- svydesign(ids = ~1, weights = ~wt, data = dat)
#   H <- as.numeric(svymean(~I(agency_poor==1), des, na.rm=TRUE))
#   des_p <- subset(des, agency_poor == 1)
#   A <- if (sum(weights(des_p)) > 0) as.numeric(svymean(~c_score, des_p, na.rm=TRUE)) else NA_real_
#   tibble(domain_wA = domain_wA, domain_wB = 1 - domain_wA, k_cutoff = k_cutoff, H = H, A = A, M0 = H * A)
# }
# 
# sens_results <- cross_df(list(domain_wA = weight_grid$domain_wA, k_cutoff = k_grid)) %>%
#   pmap_dfr(~ compute_AF(..1, ..2))
# 
# # write_csv(sens_results, "ethiopia2016_agency_index_sensitivity.csv")
# 
# # ---- 11) Finished ----
# af_summary