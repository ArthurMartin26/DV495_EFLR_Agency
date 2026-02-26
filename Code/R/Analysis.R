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

baseline <-agency_scored_2000 |> 
  select(c("region", "c_score"))

post <- agency_scored_2005 |> 
  select(c("region", "c_score"))

# Standardise treated names (lowercase, squish)
treated_region_std <- treated_region %>%
  str_to_lower() %>%
  str_squish()

# Standardise lookup names
region_lu <- region_code %>%
  mutate(region_name_std = region_name %>% str_to_lower() %>% str_squish()) %>%
  select(region_code, region_name_std)

# Stack baseline + post into one long panel (2 periods)
did_df <- bind_rows(
  baseline %>% transmute(region = as.integer(region),
                         c_score = c_score,
                         year = 2000,
                         post = 0L),
  post %>% transmute(region = as.integer(region),
                     c_score = c_score,
                     year = 2005,
                     post = 1L)
) %>%
  left_join(region_lu, by = c("region" = "region_code")) %>%
  mutate(
    treated = if_else(region_name_std %in% treated_region_std, 1L, 0L),
    did = treated * post
  )

# Quick checks
did_df %>% count(year, treated)
did_df %>% filter(is.na(region_name_std)) %>% distinct(region)

## run the most basic DiD mainly to observe comparison 

did_lm <- lm(c_score ~ treated + post + treated:post, data = did_df)
summary(did_lm)

## run a DiD with clustered Standard Errors 

m1 <- feols(
  c_score ~ treated * post,
  data = did_df,
  cluster = ~ region
)

summary(m1)



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
