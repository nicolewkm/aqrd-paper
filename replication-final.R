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

### IV Regression: feols(outcome ~ controls + factor(country) + factor(year) | treatment ~ instrument, data)

# Subset: Paper says they only use post Cold War data, and Do file also has year >= 1987
subset_condition <- dat$year >= 1987

# Cluster: Table 1 footnote says robust std error accounting for multi-way clustering at country and year;
# Do file clusters robust / heteroskedastic by ccode and year, I've added this below

# Outcome = CIRI HEI, No covariates
fit1 <- feols(
  new_empinxavg ~ factor(ccode) + factor(year)
  | EV ~ l2CPcol2,
  data = dat,
  subset = subset_condition,
  cluster = ~ ccode + year,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  robust = "HC1"
)

# Outcome = CIRI HEI, Yes covariates
fit2 <-
  feols(
    new_empinxavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

# Outcome = Polity IV, No covariates
fit3 <- feols(
  polity2avg ~ factor(ccode) + factor(year)
  | EV ~ l2CPcol2,
  data = dat,
  subset = subset_condition,
  cluster = ~ ccode + year,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  robust = "HC1"
)

# Outcome = Polity IV, Yes covariates
fit4 <-
  feols(
    polity2avg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


# Regression coefficients, coefficient on EV is right, std. errors are slightly off, number of obs is right
table <- modelsummary(
  list(
    CIRI_no = fit1,
    CIRI_yes = fit2,
    Polity_no = fit3,
    Polity_yes = fit4
  ),
  gof_map = c("nobs"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)
print(table)

### Table 1: Formatting table to resemble paper (Used as Table 2 in paper submission)

#Creating function to extract necessary values from regressions
extract_results <- function(model) {
  coefficient <- coefficients(model)["fit_EV"]
  standard_errors <- se(model)["fit_EV"]
  n_obs <- mean(nobs(model))
  return(
    data.frame(
      Coefficient = coefficient,
      Standard_Error = standard_errors,
      N_Obs = n_obs
    )
  )
}

# Extract results for each model
results_fit1 <- extract_results(fit1)
results_fit2 <- extract_results(fit2)
results_fit3 <- extract_results(fit3)
results_fit4 <- extract_results(fit4)

# Combine results into a single data frame
combined_results <- data.frame(
  Model = c("CIRI_no", "CIRI_yes", "Polity_no", "Polity_yes"),
  Coefficient = c(
    results_fit1$Coefficient,
    results_fit2$Coefficient,
    results_fit3$Coefficient,
    results_fit4$Coefficient
  ),
  Standard_Error = c(
    results_fit1$Standard_Error,
    results_fit2$Standard_Error,
    results_fit3$Standard_Error,
    results_fit4$Standard_Error
  ),
  Countries = c(115, 115, 95, 95),
  Years = c(20, 20, 20, 20),
  Covariates = c("No", "Yes", "No", "Yes"),
  Year_FE = c("Yes", "Yes", "Yes", "Yes"),
  Country_FE = c("Yes", "Yes", "Yes", "Yes"),
  N_Obs = c(
    results_fit1$N_Obs,
    results_fit2$N_Obs,
    results_fit3$N_Obs,
    results_fit4$N_Obs
  )
) |>
  mutate(across(where(is.numeric), ~ round(., 3)))

# Rotate table so that each column corresponds to one model, remove model name dataframe
combined_results_transposed <- as.data.frame(t(combined_results)) |>
  slice(-1)

rownames(combined_results_transposed) <-
  c(
    "Effect of Aid",
    "Standard Error",
    "Countries",
    "Years",
    "Covariates",
    "Year Fixed Effects",
    "Country Fixed Effects",
    "N"
  )

# Create a gt table
gt_table <-
  gt(combined_results_transposed, rownames_to_stub = TRUE) |>
  tab_header(
    title = md(
      "Table 2: Two-Stage Least Squares Estimates of Effects of Logged Foreign Aid (in Year *t*-1) from the European Community on Dependent Variables Averaged over Years *t* through *t*+3"
    )
  ) |>
  tab_stubhead(label = md("**Dependent Variable (4-Year Average)**")) |>
  cols_label(V1 = "Model 1",
             V2 = "Model 2",
             V3 = "Model 3",
             V4 = "Model 4") |>
  tab_spanner(label = md("**CIRI Human Empowerment Index**"),
              columns = vars(V1, V2)) |>
  tab_spanner(label = md("**Polity IV Combined Score**"),
              columns = vars(V3, V4)) |>
  tab_options(heading.align = "left",
              heading.title.font.size = "medium",
  )

# Save the table
gtsave(gt_table, filename = "figures/table2.png")


### Figure 1

#Function to extract coefficients
fig_coefs <- function(model) {
  coefficient <- coefficients(model)["fit_EV"]
  standard_errors <- se(model)["fit_EV"]
  return(data.frame(Coefficient = coefficient, Standard_Error = standard_errors))
}

#Create lead variables (on dependent, CIRI HEI & polity)
dat_lead <- dat |>
  group_by(ccode) |>
  arrange(year) |>
  mutate(
    new_empinx_lead1 = lead(new_empinx, 1),
    new_empinx_lead2 = lead(new_empinx, 2),
    new_empinx_lead3 = lead(new_empinx, 3),
    new_empinx_lead4 = lead(new_empinx, 4),
    new_empinx_lead5 = lead(new_empinx, 5),
    polity_lead1 = lead(polity2, 1),
    polity_lead2 = lead(polity2, 2),
    polity_lead3 = lead(polity2, 3),
    polity_lead4 = lead(polity2, 4),
    polity_lead5 = lead(polity2, 5)
  )

#Generate regressions to see impact on lead variables (CIRI HEI), paper uses unaveraged scores

fitHEIC <-
  feols(
    new_empinx ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitHEIC_lead1 <-
  feols(
    new_empinx_lead1 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


fitHEIC_lead2 <-
  feols(
    new_empinx_lead2 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitHEIC_lead3 <-
  feols(
    new_empinx_lead3 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitHEIC_lead4 <-
  feols(
    new_empinx_lead4 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitHEIC_lead5 <-
  feols(
    new_empinx_lead5 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

#Extract coefficients
results_f2l0 <- fig_coefs(fitHEIC)
results_f2l1 <- fig_coefs(fitHEIC_lead1)
results_f2l2 <- fig_coefs(fitHEIC_lead2)
results_f2l3 <- fig_coefs(fitHEIC_lead3)
results_f2l4 <- fig_coefs(fitHEIC_lead4)
results_f2l5 <- fig_coefs(fitHEIC_lead5)

combined_f2lead <- bind_rows(
  mutate(results_f2l0),
  mutate(results_f2l1),
  mutate(results_f2l2),
  mutate(results_f2l3),
  mutate(results_f2l4),
  mutate(results_f2l5),
) |>
  mutate(term = c(0, 1, 2, 3, 4, 5))

# Plot for HEI outcome, slight differences between this and the figure in the main paper. Overall trends remains accurate.
plot_HEI <- combined_f2lead |>
  ggplot(aes(x = term, y = Coefficient)) +
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
  ggtitle("CIRI Human Empowerment Index") +
  theme_grey() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

#Generate regressions to see impact on lead variables (polity), paper uses unaveraged scores

fitpol <-
  feols(
    polity2 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitpol_lead1 <-
  feols(
    polity_lead1 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


fitpol_lead2 <-
  feols(
    polity_lead2 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitpol_lead3 <-
  feols(
    polity_lead3 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitpol_lead4 <-
  feols(
    polity_lead4 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )

fitpol_lead5 <-
  feols(
    polity_lead5 ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi
    + covwdi_fdiF + covwdi_imp + covwdi_impF + covwvs_rel + covwvs_relF
    + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion
    + covdemregionF + covloggdpC + covloggdpCF + covloggdp + covloggdpF
    + factor(ccode) + factor(year)
    | EV ~ l2CPcol2,
    data = dat_lead,
    subset = subset_condition,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    robust = "HC1"
  )


#Extract coefficients
results_fpoll0 <- fig_coefs(fitpol)
results_fpoll1 <- fig_coefs(fitpol_lead1)
results_fpoll2 <- fig_coefs(fitpol_lead2)
results_fpoll3 <- fig_coefs(fitpol_lead3)
results_fpoll4 <- fig_coefs(fitpol_lead4)
results_fpoll5 <- fig_coefs(fitpol_lead5)

combined_fpollead <- bind_rows(
  mutate(results_fpoll0),
  mutate(results_fpoll1),
  mutate(results_fpoll2),
  mutate(results_fpoll3),
  mutate(results_fpoll4),
  mutate(results_fpoll5),
) |>
  mutate(term = c(0, 1, 2, 3, 4, 5))

# Plot for polity outcome, slight differences between this and the figure in the main paper. Overall trends remains accurate.
plot_pol <- combined_fpollead |>
  ggplot(aes(x = term, y = Coefficient)) +
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
  ggtitle("Polity IV Score") +
  theme_grey() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

combined_plot <-
  plot_HEI + plot_pol + plot_layout(ncol = 2) +  plot_annotation(
    title = (
      'Append A: Estimated Effects of Logged Foreign Aid in Year t-1 on CIRI Human Empowerment Index and Polity IV Combined Score'
    ),
    subtitle = ('(in years t through t + 5)'),
    caption = (
      'Note: Two-stage least squares point estimates presented with 95% confidence intervals as black error bars'
    )
  )

ggsave("figures/appendixA.png", combined_plot)

        
        