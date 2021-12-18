# Check PrEP_NEW achievement
# Date: 2021-11-24
# Purpose: Verify a statement
# Details: Proposed decomposition for achievement into three parts
# Author Tim Essam


# LIBRARIES ----

  library(glitr)
  library(glamr)
  #library(gisr)
  #library(sf)
  library(tidyverse)
  library(gophr)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(glue)

# GLOBALS ----

  source("./Scripts/00_Setup.R")
  source("./Scripts/00_Utils.R")

# LOAD DATA ----

  ou_im <- read_msd(file_ou_im)

  psnu_im <- read_msd(file_psnu_im)

  curr_fy = ou_im %>% identifypd(pd_type = "year")
  curr_pd = ou_im %>% identifypd(pd_type = "full")

# MUNGE JUST PREP_NEW ----

  # Clean indicators & cntry names
  ou_im <- ou_im %>%
    clean_agency() %>%
    clean_countries(colname = "operatingunit") %>%
    clean_countries(colname = "countryname")

  # Indicators & Disaggs
  ou_im %>%
    filter(fiscal_year == curr_fy, indicator == "PrEP_NEW") %>%
    distinct(fundingagency, indicator, standardizeddisaggregate)

  ou_im %>%
    filter(fiscal_year == curr_fy, indicator == "PrEP_NEW") %>%
    distinct(indicator, standardizeddisaggregate)

  # Filter to just PrEP_NEW
  df_ou_prep <- ou_im %>%
    extract_data(df_msd = .,
                 operatingunit,
                 fy = curr_fy,
                 agencies = "USAID",
                 ind = "PrEP_NEW",
                 disagg = "Total Numerator")

  # Global level
  df_ou_prep <- ou_im %>%
    extract_data(df_msd = .,
                 operatingunit,
                 fy = curr_fy,
                 agencies = "USAID") %>%
    decompose_results(operatingunit) %>%
    calculate_gapshare(operatingunit) %>%
    rollup_achv()

  # PSNU level
  df_psnu_prep <- psnu_im %>%
    filter(operatingunit == "Nigeria") %>%
    extract_data(df_msd = .,
                 operatingunit, psnu,
                 fy = curr_fy,
                 agencies = "USAID") %>%
    decompose_results(operatingunit, psnu) %>%
    calculate_gapshare(operatingunit, psnu) %>%
    rollup_achv(operatingunit)

  df_psnu_prep <- psnu_im %>%
    filter(operatingunit == "Zambia") %>%
    clean_psnu() %>%
    extract_data(df_msd = .,
                 operatingunit, psnu,
                 fy = curr_fy,
                 agencies = "USAID") %>%
    decompose_results(operatingunit, psnu) %>%
    calculate_gapshare(operatingunit, psnu) %>%
    rollup_achv(operatingunit)

# VIZ ----

  df_ou_prep %>% distinct(fundingagency)

  # Global decomposition
  df_ou_prep %>%
    mutate(grp_order = fct_reorder(operatingunit, results)) %>%
    viz_decomposition("USAID")

  # OU/PSNU Decomposition
  df_psnu_prep %>%
    filter(!str_detect(psnu, "_Mil")) %>%
    mutate(grp_order = fct_reorder(psnu, results)) %>%
    viz_decomposition("Nigeria")
