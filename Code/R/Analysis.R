# ================================================================
# Perform the analysis
# Author: Arthur Martin 
# Last updated: Sys.Date(20/02)

# # -- Notes:
# run both of the empowerment scripts for 2000 and 2005
# 
# then use the constructed tables and flag for regions affected, 
# 
# important that the regions are correctly identified 
# 
# then we can run a simple DiD and add extra complexity of varaibles if needed 

# ================================================================


user <- Sys.getenv("USERNAME")

if (user == "Arthur.Martin") {
  
  base_directory <- paste0(
    "C:/Users/", user,
    "/OneDrive - Department of Health and Social Care/Documents/LSE/",
    "DV495_DISSERTATION/DV495_EFLR_Agency"
  )
}

output_directory <- paste0(base_directory, "/Outputs/figures")

source("Code/R/packages and functions.R")

## run 2000 and 2005 
source("Code/R/Empowerment_index_2000.R")


source("Code/R/Empowerment_index_05.R")


## region codes 


region_code <- tribble(
  ~region_code, ~region_name,
  1,  "tigray",
  2,  "affar",
  3,  "amhara",
  4,  "oromiya",
  5,  "somali",
  6,  "ben-gumz",
  7,  "snnp",
  12, "gambela",
  13, "harari",
  14, "addis",
  15, "dire dawa"
)

treated_region <- c("addis","amhara","dire dawa","oromiya", "tigray" )

## we need to add in survey weighting 

# baseline <-agency_scored_2000 |> 
#   select(c("region", "c_score"))
# 
# post <- agency_scored_2005 |> 
#   select(c("region", "c_score"))



# --- Keep region, outcome, weights, age band ---
baseline <- agency_scored_2000 %>%
  select(region, c_score, wt, age_band) %>%
  mutate(year = 2000, post = 0L)

post <- agency_scored_2005 %>%
  select(region, c_score, wt, age_band) %>%
  mutate(year = 2005, post = 1L)


# Standardise treated names (lowercase, squish)
treated_region_std <- treated_region %>%
  str_to_lower() %>%
  str_squish()

# Standardise lookup names
region_lu <- region_code %>%
  mutate(region_name_std = region_name %>% str_to_lower() %>% str_squish()) %>%
  select(region_code, region_name_std)

# Stack baseline + post into one long panel (2 periods)

did_df <- bind_rows(baseline, post) %>%
  mutate(region = as.integer(region)) %>%
  left_join(region_lu, by = c("region" = "region_code")) %>%
  mutate(
    treated = if_else(region_name_std %in% treated_region_std, 1L, 0L),
    did = treated * post,
    # ensure age_band is a factor with a clear reference group
    age_band = factor(age_band, levels = age_labels)
  )

# Quick checks
did_df %>% count(year, treated)
did_df %>% filter(is.na(region_name_std)) %>% distinct(region)

## run the most basic DiD mainly to observe comparison 

# did_lm <- lm(c_score ~ treated + post + treated:post, data = did_df)
# summary(did_lm)
# 
# ## run a DiD with clustered Standard Errors 
# 
# m1 <- feols(
#   c_score ~ treated * post,
#   data = did_df,
#   cluster = ~ region
# )
# 
# summary(m1)

### Age band DiD 

# Function to run DiD within an age band
# ---------------------------------------------
# 1. Split data into age-band subsets manually
# ---------------------------------------------

df_15_24 <- did_df %>% filter(age_band == "15-24")
df_25_34 <- did_df %>% filter(age_band == "25-34")
df_35_49 <- did_df %>% filter(age_band == "35-49")

# ---------------------------------------------
# 2. Run DiD separately for each age band
# ---------------------------------------------

mod_15_24 <- feols(c_score ~ treated * post,
                   data = df_15_24,
                   weights = ~ wt,
                   cluster = ~ region)

mod_25_34 <- feols(c_score ~ treated * post,
                   data = df_25_34,
                   weights = ~ wt,
                   cluster = ~ region)

mod_35_49 <- feols(c_score ~ treated * post,
                   data = df_35_49,
                   weights = ~ wt,
                   cluster = ~ region)

# ---------------------------------------------
# 3. Extract the 'treated:post' coefficient
# ---------------------------------------------

did_15_24 <- coef(mod_15_24)["treated:post"]
se_15_24  <- se(mod_15_24)["treated:post"]

did_25_34 <- coef(mod_25_34)["treated:post"]
se_25_34  <- se(mod_25_34)["treated:post"]

did_35_49 <- coef(mod_35_49)["treated:post"]
se_35_49  <- se(mod_35_49)["treated:post"]

# ---------------------------------------------
# 4. Store results in a clean data frame
# ---------------------------------------------

did_age_results <- tibble::tibble(
  age_band  = c("15-24", "25-34", "35-49"),
  estimate  = c(did_15_24, did_25_34, did_35_49),
  std_error = c(se_15_24, se_25_34, se_35_49)
)

did_age_results

######################################################
### Exploratory data work ############################


##visualisation - bit tricky as the regional values are factots so have to make them characters first 

plot_df_00 <- region_means_00 %>%
  mutate(region = as.integer(as.character(region))) %>%
  left_join(region_lu, by = c("region" = "region_code")) %>%
  mutate(
    # treated group identifier (EARLY vs LATE)
    treat_group = if_else(region_name_std %in% treated_region_std,
                          "Early treated", "Late treated"),
    # nicer labels
    region_name = str_to_title(region_name_std)
  ) %>%
  arrange(mean_c_score) %>%
  mutate(
    region_name = factor(region_name, levels = region_name),
    treat_group = factor(treat_group, levels = c("Late treated", "Early treated"))
  )
plot00_comp <- ggplot(plot_df_00, aes(x = region_name, y = mean_c_score, fill = treat_group)) +
  geom_col(alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
  scale_fill_manual(values = c("Early treated" = "#D7191C",  # red
                               "Late treated"  = "#2C7FB8"  # blue
  )) +
  labs(
    title = "Average women's agency deprivation score (c_score) by region, 2000",
    subtitle = "Weighted using DHS sampling weights (v005/1e6)\nEarly treated regions highlighted in red; late treated in blue.",
    x = "Region",
    y = "Mean c_score (0 = none, 1 = maximum deprivation)",
    fill = ""
  ) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

plot00_comp

ggsave(
  filename = paste0(output_directory, "/Avg_agency_region_2000_treated.png"),
  plot = plot00_comp,
  width = 9, height = 5, dpi = 300
)


### DID plot 

plot_df_00 <- region_means_00 %>%
  mutate(region = as.integer(as.character(region)), year = 2000) %>%
  left_join(region_lu, by = c("region" = "region_code")) %>%
  mutate(treat_group = if_else(region_name_std %in% treated_region_std, "Early treated", "Late treated"),
         region_name = str_to_title(region_name_std))

plot_df_05 <- region_means_05%>%
  mutate(region = as.integer(as.character(region)), year = 2005) %>%
  left_join(region_lu, by = c("region" = "region_code")) %>%
  mutate(treat_group = if_else(region_name_std %in% treated_region_std, "Early treated", "Late treated"),
         region_name = str_to_title(region_name_std))

plot_both <- bind_rows(plot_df_00, plot_df_05) %>%
  mutate(region_name = factor(region_name, levels = unique(plot_df_00$region_name)))  # lock order

p_change <- ggplot(plot_both, aes(x = year, y = mean_c_score, group = region_name, color = treat_group)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 2.4) +
  facet_wrap(~ region_name, ncol = 4) +
  scale_color_manual(values = c("Early treated" = "#D7191C", "Late treated" = "#2C7FB8")) +
  labs(
    title = "Change in UNWEIGHTED mean agency deprivation by region (2000 → 2005)",
    subtitle = "Lower = less deprivation (higher agency). Each panel is a region; colour indicates early vs late treated group.",
    x = "", y = "Mean c_score", color = ""
  ) +
  theme_minimal(base_size = 11)

p_change


