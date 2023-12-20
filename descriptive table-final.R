library(tidyverse)
library(gt)
library(haven)
library(webshot2)

# Load data
extension_dat <- read_dta("data/extension_dat.dta")

summary_orig <- extension_dat |>
  mutate(
    educ = ifelse(covihme_ayemF == 1, NA, covihme_ayem),
    export = ifelse(covwdi_expF == 1, NA, covwdi_exp),
    fdi = ifelse(covwdi_fdiF == 1, NA, covwdi_fdi),
    import = ifelse(covwdi_impF == 1, NA, covwdi_imp),
    relig = ifelse(covwvs_relF == 1, NA, covwvs_rel),
    oilrev = ifelse(coviNY_GDP_PETR_RT_ZSF == 1, NA, coviNY_GDP_PETR_RT_ZS),
    dem = ifelse(covdemregionF == 1, NA, covdemregion),
    loggdppC = ifelse(covloggdpCF == 1, NA, covloggdpC),
    loggdp = ifelse(covloggdpF == 1, NA, covloggdp)
  ) |>
  select(
    educ,
    export,
    fdi,
    import,
    relig,
    oilrev,
    dem,
    loggdppC,
    loggdp,
    new_empinx,
    new_empinxavg,
    polity2,
    polity2avg,
    EV,
    Debt
  ) |>
  rename(
    "Years Education - Male" = educ,
    "Log Exports" = export,
    "FDI" = fdi,
    "Log Imports" = import,
    "Regiosity" = relig,
    "Petroleum Revenue" = oilrev,
    "Democracies in Region" = dem,
    "Log GDP per capita" = loggdppC,
    "Log GDP" = loggdp,
    "CIRI Human Empowerment Index Score" = new_empinx,
    "CIRI Human Empowerment Index Score (4-Year Average)" = new_empinxavg,
    "Polity IV Score" = polity2,
    "Polity IV Score (4-Year Average)" = polity2avg,
    "Aid (EU)" = EV,
    "Total Debt (% of GDP)" = Debt
  ) |>
  gather() |>
  group_by(key) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    obs = sum(!is.na(value))
  )

gt_summary <- summary_orig |>
  gt() |>
  tab_header(title = "Table 1: Descriptive Statistics") |>
  cols_label(
    mean = md("**Mean**"),
    sd = md("**Standard Deviation**"),
    obs = md("**Observations**"),
    key = "",
  ) |>
  tab_row_group(
    label = md("**A. Impact of Aid Measures**"),
    rows = key %in% c(
      "Aid (EU)",
      "Total Debt (% of GDP)",
      "Polity IV Score",
      "Polity IV Score (4-Year Average)",
      "CIRI Human Empowerment Index Score",
      "CIRI Human Empowerment Index Score (4-Year Average)"
    ),
    id = "aidimpact"
  ) |>
  tab_row_group(
    label = md("**B. Economic Measures**"),
    rows = key %in% c(
      "Log Exports",
      "Log Imports",
      "Log GDP",
      "Log GDP per capita",
      "Petroleum Revenue"
    ),
    id = "econ"
  ) |>
  tab_row_group(
    label = md("**C. Country Characteristics**"),
    rows = key %in% c(
      "Democracies in Region",
      "Years Education - Male",
      "FDI",
      "Regiosity"
    ),
    id = "country"
  ) |>
  row_group_order(groups = c("aidimpact",
                             "econ",
                             "country")) |>
  fmt_number(columns = c(mean, sd),
             decimals = 2)  |>
  tab_options(
    heading.align = "left"
  )

gtsave(gt_summary, filename = "figures/table1.png")
