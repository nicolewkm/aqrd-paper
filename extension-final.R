# Load libraries
library(tidyverse)
library(gt)
library(modelsummary)
library(fixest)
library(haven)
library(patchwork)
library(broom)

# Load data
dat <- read_dta("data/Final_Main.dta")

### IMF data cleaning, combining with existing dataset 

debt <- readxl::read_excel("data/imfdebt.xls")

long_debt <- debt |>
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Debt")

long_debt <- long_debt |>
  mutate(Debt = replace(Debt, Debt == "no data", NA))

# Convert "Year" and "Debt" to numeric
long_debt <- long_debt |>
  mutate(Year = as.numeric(Year),
         Debt = as.numeric(Debt))
str(long_debt)

# Load in COW 
cow_codes <- read.csv("data/COW-country-codes.csv")

# Create a mapping to see which countries need to be matched to COW code
mapping <- long_debt |>
  distinct(Country) |>
  left_join(cow_codes, by = c("Country" = "StateNme"))

?filter
mapping |>
  filter(is.na(StateAbb)) |>
  print(n=28)

# Match countries to COW code 
long_debt <- long_debt |>
  mutate(Country = recode(
    Country,
    "Antigua and Barbuda" = "Antigua & Barbuda",
    "Bahamas, The" = "Bahamas",
    "Brunei Darussalam" = "Brunei",
    "Cabo Verde" = "Cape Verde",
    "Congo, Republic of" = "Congo",
    "Côte d'Ivoire" = "Ivory Coast",
    "Eswatini" = "Swaziland",
    "Gambia, The" = "Gambia",
    "Korea, Republic of" = "South Korea",
    "Kyrgyz Republic" = "Kyrgyzstan",
    "Lao P.D.R." = "Laos",
    "North Macedonia" = "Macedonia",
    "Russian Federation" = "Russia",
    "Saint Kitts and Nevis" = "St. Kitts and Nevis",
    "Saint Lucia" = "St. Lucia",
    "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
    "Slovak Republic" = "Slovakia",
    "South Sudan, Republic of" = "South Sudan",
    "São Tomé and Príncipe" = "Sao Tome and Principe",
    "Taiwan Province of China" = "Taiwan",
    "Timor-Leste" = "East Timor",
    "Türkiye, Republic of" = "Turkey",
    "United States" = "United States of America"
  ))


# Join with COW code  

cow_debt <- left_join(long_debt, cow_codes, by = c("Country" = "StateNme"), relationship = "many-to-many")

# Countries without Ccode: Hong Kong SAR, Serbia, West Bank and Gaza, NA, IMF 2023 

#Renaming ccode and year variables in cow_debt
cow_debt <- cow_debt |>
  rename(ccode = CCode,
         year = Year)

# Create final joined dataset with IMF data and Carnegie and Marinov data 
extension_dat <- left_join(dat, cow_debt, by = c("year", "ccode"))

# Saving new dataset in data folder
write_dta(extension_dat, "data/extension_dat.dta")

### Create graph plots for Indonesia (850), Argentina (160), India (750) and Nigeria (475)

selected_countries <- c(160, 750, 850, 475)

extension_filter <- extension_dat |>
  filter(ccode %in% selected_countries)|>
  mutate(country = case_when(
    ccode == 160 ~ "Argentina",
    ccode == 750 ~ "India",
    ccode == 850 ~ "Indonesia",
    ccode == 475 ~ "Nigeria",
    TRUE ~ as.character(ccode)  # Default if none of the conditions match
  ))


ext_plot <- extension_filter |>
  ggplot(aes(x = year, y = Debt, group = country)) +
  ggtitle("Figure 1: Trends in National Debt (% of GDP) for Argentina, India, Indonesia and Nigeria (1987 - 2011)") +
  geom_line() +
  geom_line(data = select(extension_filter, -country), aes(group = ccode), alpha = 0.2, size = 0.2) +
  facet_wrap(~ country, ncol = 2) +
  labs(x = "Year", y = "Debt (% of GDP)") +
  scale_x_continuous(limits = c(1987, max(extension_filter$year))) +
  theme_grey() +
  theme(
    strip.text = element_text(face = "bold", size = 11)
    )

print(ext_plot)

ggsave("figures/figure1.png", ext_plot)



### Regression exploring the relationship between foreign aid and indebtedness: feols(outcome ~ controls + factor(country) + factor(year) | treatment ~ instrument, data)
#Subset condition for extensiion (retaining the same time period >= 1987 as original analysis)
subset_condition_ext <- extension_dat$year >= 1987

# Outcome = Debt, No covariates
fit1_ext <- feols(Debt ~ factor(ccode) + factor(year)
                  | EV ~ l2CPcol2, 
                  data = extension_dat, subset = subset_condition_ext, cluster = c("ccode", "year"), robust = "HC1")

# Outcome = Debt, Yes covariates
fit2_ext <- feols(Debt ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi 
                  + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF 
                  + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion 
                  + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF 
                  + factor(ccode) + factor(year)
                  | EV ~ l2CPcol2, 
                  data = extension_dat, subset = subset_condition_ext, cluster = c("ccode", "year"), robust = "HC1")

# Summary of regression output
table_ext_sum <- modelsummary(
  list(Debt_yes = fit1_ext,
       Debt_no = fit2_ext),
  gof_map = c("nobs"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)
print(table_ext_sum)

### Table showing results of both models 
#Function to extract results from model
extract_results <- function(model) {
  coefficient <- coefficients(model)["fit_EV"]
  standard_errors <- se(model)["fit_EV"]
  n_obs <- mean(nobs(model))
  return(data.frame(Coefficient = coefficient, Standard_Error = standard_errors, N_Obs = n_obs))
}

#Function to count countries and years in each model
count_country <- function(model) {
  tidy_summary <- tidy(model)
  unique_factor_ccode_vars <- sum(grepl("^factor\\(ccode\\)", tidy_summary$term))
  return(unique_factor_ccode_vars + 1)
}

count_year <- function(model) {
  tidy_summary <- tidy(model)
  unique_factor_year_vars <- sum(grepl("^factor\\(year\\)", tidy_summary$term))
  return(unique_factor_year_vars + 1)
}

# Extract results for each model
results_ext1 <- extract_results(fit1_ext)
results_ext2 <- extract_results(fit2_ext)

# Combine results into a single data frame
combined_ext <- data.frame(
  Model = c("Debt_no", "Debt_yes"),
  Coefficient = c(results_ext1$Coefficient, results_ext2$Coefficient),
  Standard_Error = c(results_ext1$Standard_Error, results_ext2$Standard_Error),
  Countries = c(count_country(fit1_ext),count_country(fit2_ext)),
  Years = c(count_year(fit1_ext),count_year(fit2_ext)),
  Covariates = c("No", "Yes"),
  Year_FE = c("Yes", "Yes"),
  Country_FE = c("Yes", "Yes"),
  N_Obs = c(results_ext1$N_Obs, results_ext1$N_Obs)
) |>
  mutate(across(where(is.numeric), ~ round(., 3)))

# Rotate table so that each column corresponds to one model, remove model name dataframe
combined_ext_transposed <- as.data.frame(t(combined_ext)) |>
  slice(-1)

rownames(combined_ext_transposed) <- c("Effect of Aid", "Standard Error", "Countries", "Years","Covariates", "Year Fixed Effects", "Country Fixed Effects", "N" )

# Create a gt table
table_ext <- combined_ext_transposed |>
  gt(rownames_to_stub = TRUE) |>
  tab_header(
    title = md("Table 3: Two-Stage Least Squares Estimates of Effects of Logged Foreign Aid (in Year *t*-1) from the European Community on Debt (% of GDP)")
  ) |>
  tab_stubhead(label = md("**Dependent Variable**")) |>
  tab_spanner(
    label = md("**Debt (% of GDP)**"),
    columns = vars(V1, V2)
  ) |>
  cols_label(V1 = "Model 1",
             V2 = "Model 2") |>
  tab_options(
    heading.align = "left"
  ) 

#Save the table
gtsave(table_ext, filename = "figures/table3.png")

### Creating chart of lead results

#Function to extract coefficients
fig_coefs <- function(model) {
  coefficient <- coefficients(model)["fit_EV"]
  standard_errors <- se(model)["fit_EV"]
  return(data.frame(Coefficient = coefficient, Standard_Error = standard_errors))
}

#Create lead variables (on dependent, debt)
ext_lead <- extension_dat |>
  group_by(ccode) |>
  arrange(year) |>
  mutate(
    debt_lead1 = lead(Debt, 1),
    debt_lead2 = lead(Debt, 2),
    debt_lead3 = lead(Debt, 3), 
    debt_lead4 = lead(Debt, 4), 
    debt_lead5 = lead(Debt, 5),
    debt_lead6 = lead(Debt, 6),
    debt_lead7 = lead(Debt, 7),
    debt_lead8 = lead(Debt, 8),
    debt_lead9 = lead(Debt, 9),
    debt_lead10 = lead(Debt, 10)
  )

#Generate regressions to see impact on lead variables (Debt)

fitdebt <-
  feols(
    Debt ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead1 <-
  feols(
    debt_lead1 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


fitdebt_lead2 <-
  feols(
    debt_lead2 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead3 <-
  feols(
    debt_lead3 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead4 <-
  feols(
    debt_lead4 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead5 <-
  feols(
    debt_lead5 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


fitdebt_lead6 <-
  feols(
    debt_lead6 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead7 <-
  feols(
    debt_lead7 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead8 <-
  feols(
    debt_lead8 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


fitdebt_lead9 <-
  feols(
    debt_lead9 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitdebt_lead10 <-
  feols(
    debt_lead10 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = ext_lead,
    subset = subset_condition_ext,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

#Extract coefficients
results_debtl0 <- fig_coefs(fitdebt)
results_debtl1 <- fig_coefs(fitdebt_lead1)
results_debtl2 <- fig_coefs(fitdebt_lead2)
results_debtl3 <- fig_coefs(fitdebt_lead3)
results_debtl4 <- fig_coefs(fitdebt_lead4)
results_debtl5 <- fig_coefs(fitdebt_lead5)
results_debtl6 <- fig_coefs(fitdebt_lead6)
results_debtl7 <- fig_coefs(fitdebt_lead7)
results_debtl8 <- fig_coefs(fitdebt_lead8)
results_debtl9 <- fig_coefs(fitdebt_lead9)
results_debtl10 <- fig_coefs(fitdebt_lead10)

combined_debtlead <- bind_rows(
  mutate(results_debtl0),
  mutate(results_debtl1),
  mutate(results_debtl2),
  mutate(results_debtl3),
  mutate(results_debtl4),
  mutate(results_debtl5),
  mutate(results_debtl6),
  mutate(results_debtl7),
  mutate(results_debtl8),
  mutate(results_debtl9),
  mutate(results_debtl10)
) |>
  mutate(term = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Plot for HEI outcome, slight differences between this and the figure in the main paper. Overall trends remains accurate.
plot_debt <- combined_debtlead |>
  ggplot(aes(x = term, y = Coefficient)) +
  ggtitle("Figure 2: Estimated Effects of Logged Foreign Aid in Year t-1 on Debt (% of GDP)") +
  labs(subtitle = "(in years t through t + 10)") +
  geom_point(color = "darkblue") +
  geom_errorbar(
    aes(
      ymin = Coefficient - 1.96 * Standard_Error,
      ymax = Coefficient + 1.96 * Standard_Error
    ),
    color = "darkblue",
    width = 0.1
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "darkblue") +
  labs(x = "Years Forward", y = "Effect of Foreign Aid") +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  ) 

ggsave("figures/figure2.png", plot_debt)











