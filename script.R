library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)


SLB_s26_bonds <- read_excel(
  "./data/SLB sep 26 v2.xlsx", 
  sheet = "Cp - Bonds"
)
colnames(SLB_s26_bonds) <- c(
  "issuer", "cusip", "ticker", "issue_date", "kpis", "selection_of_kpi",
  "moody_init_rating", "coupon", "maturity", "series", "bbg_composite",
  "mty_type", "currency", "desc_notes"
)
SLB_s26_bonds <- SLB_s26_bonds |>
  mutate(
    issue_date = as.POSIXct(issue_date, tz = "UTC"),
    selection_of_kpi = if_else(selection_of_kpi %in% c("N", "Y"), as.logical(selection_of_kpi == "Y"), NA),
    moody_init_rating = if_else(moody_init_rating == "#N/A N/A", NA_character_, moody_init_rating),
    maturity = if_else(maturity == "#N/A Field Not Applicable", NA, maturity),
    maturity = as.POSIXct(maturity, tz = "UTC"),
    bbg_composite = if_else(bbg_composite == "#N/A N/A", NA_character_, bbg_composite),
    cusip_id = paste0(cusip, " Corp")
  )
View(SLB_s26_bonds)


SLB_s26_slb <- read_excel(
  "./data/SLB sep 26 v2.xlsx", 
  sheet = "Cp - SLB"
)
SLB_s26_slb <- SLB_s26_slb |>
  mutate(
    cusip_id = ID,
    sselection_of_kpi = selection_of_kpi,
    kpi_association = if_else(kpi_association == "#N/A", NA_character_, kpi_association),
    sustnabilty_linkd_start_dt = as.POSIXct(sustnabilty_linkd_start_dt, tz = "UTC", format = "%m/%d/%Y"),
    sustnabilty_linkd_end_dt = as.POSIXct(sustnabilty_linkd_end_dt, tz = "UTC", format = "%m/%d/%Y"),
    first_sustain_perf_tgt_obs_dt = as.POSIXct(first_sustain_perf_tgt_obs_dt, tz = "UTC", format = "%m/%d/%Y"),
    bond_modification_mechanism = if_else(bond_modification_mechanism == "#N/A", NA_character_, bond_modification_mechanism),
    msci_esg_rating = if_else(msci_esg_rating == "N.S.", NA_character_, msci_esg_rating),
    esg_previous_rating = if_else(esg_previous_rating == "N.S.", NA_character_, esg_previous_rating),
    sa_esg_risk_category = if_else(sa_esg_risk_category == "#N/A", NA_character_, sa_esg_risk_category),
    sustainable_theme = if_else(sustainable_theme == "#N/A", NA_character_, sustainable_theme),
    sustainable_security_type = if_else(sustainable_security_type == "#N/A", NA_character_, sustainable_security_type),
    esg_risk_pct_subindustry = as.numeric(esg_risk_pct_subindustry),
    esg_risk_pct_universe = as.numeric(esg_risk_pct_universe),
    sustainability_linked_kpi = if_else(sustainability_linked_kpi == "#N/A", NA_character_, sustainability_linked_kpi),
    sustainability_lnk_rpt_componnts = if_else(sustainability_lnk_rpt_componnts == "#N/A", NA_character_, sustainability_lnk_rpt_componnts),
    sustainability_linked_metrc_scre = if_else(sustainability_linked_metrc_scre == "#N/A", NA_character_, sustainability_linked_metrc_scre),
    sustainable_development_goals = if_else(sustainable_development_goals == "#N/A", NA_character_, sustainable_development_goals),
    stepup_dt = as.POSIXct(stepup_dt, tz = "UTC", format = "%m/%d/%Y")
  ) |>
  select(-selection_of_kpi, -ID)
View(SLB_s26_slb)


SLB_s26_slbcomp <- read_excel(
  "./data/SLB sep 26 v2.xlsx", 
  sheet = "Cp - SLB Comp"
)
colnames(SLB_s26_slbcomp) <- c(
  "cusip_id", "bp_increase", "observation_date", "baseline_unit", "coupon_step_payment_dt",
  "spt_achieved", "kpi_theme", "spt_observed_val", "baseline_value", "sustainability_lnk_rpt_item",
  "spt_unit", "coupon_step_effective_dt", "bp_decrease", "spt_observed_unit", "certification_dt",
  "benchmark_approach", "baseline_date", "spt_value", "spt_description"
)
SLB_s26_slbcomp <- SLB_s26_slbcomp[
  , c(
    "cusip_id", "bp_increase", "bp_decrease", 
    "baseline_value", "baseline_unit", "baseline_date", 
    "spt_value", "spt_unit", "observation_date", 
    "spt_observed_val", "spt_observed_unit", "spt_achieved", 
    "coupon_step_effective_dt", "coupon_step_payment_dt",
    "certification_dt", "sustainability_lnk_rpt_item", "benchmark_approach",
    "kpi_theme", "spt_description"
  )
]
SLB_s26_slbcomp <- SLB_s26_slbcomp |>
  mutate(
    baseline_unit = if_else(baseline_unit == "#N/A", NA_character_, baseline_unit),
    baseline_date = as.POSIXct(baseline_date, tz = "UTC", format = "%m/%d/%Y"),
    spt_unit = if_else(spt_unit == "#N/A", NA_character_, spt_unit),
    observation_date = as.POSIXct(observation_date, tz = "UTC", format = "%m/%d/%Y"),
    spt_observed_unit = if_else(spt_observed_unit == "#N/A", NA_character_, spt_observed_unit),
    coupon_step_effective_dt = as.POSIXct(coupon_step_effective_dt, tz = "UTC", format = "%m/%d/%Y"),
    coupon_step_payment_dt = as.POSIXct(coupon_step_payment_dt, tz = "UTC", format = "%m/%d/%Y"),
    kpi_theme = if_else(kpi_theme == "#N/A", NA_character_, kpi_theme),
    spt_description = if_else(spt_description == "#N/A", NA_character_, spt_description),
    certification_dt = as.POSIXct(certification_dt, tz = "UTC", format = "%m/%d/%Y"),
    sustainability_lnk_rpt_item = as.numeric(sustainability_lnk_rpt_item),
    benchmark_approach = if_else(benchmark_approach == "#N/A", NA_character_, benchmark_approach)
  )
SLB_s26_slbcomp <- SLB_s26_slbcomp |>
  filter(!is.na(spt_achieved) & (!is.na(bp_increase) | !is.na(bp_decrease))) |>
  filter(!is.na(observation_date) & (!is.na(coupon_step_effective_dt) & !is.na(coupon_step_payment_dt)))
cusip_list <- SLB_s26_slbcomp$cusip_id


SLB_s26_mprices <- read_excel(
  "./data/SLB sep 26 v2.xlsx", 
  sheet = "Cp - SLB Ts"
)
SLB_s26_mprices <- SLB_s26_mprices[,!grepl("\\.\\.\\.", colnames(SLB_s26_mprices))] |>
  filter(!is.na(Dates))
SLB_s26_mprices <- SLB_s26_mprices |>
  pivot_longer(
    cols = -Dates,
    names_to = "cusip_id",
    values_to = "price"
  ) |>
  rename(date = Dates) |>
  mutate(
    date = as.POSIXct(date, tz = "UTC", format = "%m/%d/%Y"),
    price = as.numeric(price)
  )

SLB_s26_mprices <- SLB_s26_mprices |>
  filter(cusip_id %in% stringr::str_to_upper(cusip_list)) |>
  group_by(cusip_id) |>
  filter(any(!is.na(price))) |>
  ungroup()
SLB_s26_mprices <- SLB_s26_mprices |>
  arrange(cusip_id, date)



