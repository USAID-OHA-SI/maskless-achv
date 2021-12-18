# Check PrEP_NEW achievement
# Date: 2021-11-24
# Purpose: Verify a statement
# Details: Proposed decomposition for achievement into three parts
# Author Tim Essam


# LIBRARIES ----

  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
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

  ou_im %>%
    filter(fiscal_year == curr_fy, indicator == "PrEP_NEW") %>%
    head()

  # Filter to just PrEP_NEW
  ou_im %>% extract_data()

  ou_im %>% extract_data(countryname)

  ou_im %>%
    extract_data(df_msd = .,
                 countryname, ageasentered, sex,
                 fy = curr_fy,
                 agencies = NULL,
                 ind = "PrEP_NEW",
                 disagg = "Age/Sex")

  prep_df <- ou_im %>%
    extract_data(df_msd = .,
                 operatingunit,
                 fy = curr_fy,
                 agencies = "USAID",
                 ind = "PrEP_NEW",
                 disagg = "Total Numerator")

  prep_df <- ou_im %>%
    filter(indicator == "PrEP_NEW",
           fiscal_year == 2021,
           fundingagency == "USAID",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(operatingunit, indicator, fiscal_year, fundingagency) %>%
    summarize(across(matches("targets|cumu"), sum, na.rm = T)) %>%
    ungroup()

  # Create target capped results, spillover results, and results deficit
  # TODO: Do we need a separate deficit/surplus variable or
  # should we use binaries to flag? May make plotting easier w/ separate vars.
  # TODO: Check edge case when results == targets (is this covered?)
  prep_adj <-
    prep_df %>%
    group_by(operatingunit, indicator) %>%
    mutate(rslt_capped = ifelse(cumulative > targets, targets, cumulative),
           rslt_gap = cumulative - targets,
           rslt_deficit = ifelse(cumulative < targets, cumulative - targets, NA_real_),
           rslt_surplus = ifelse(cumulative >= targets, cumulative - targets, NA_real_),
           achv = cumulative / targets,
           achv_adj = rslt_capped / targets,
           ou_over_achv = cumulative > targets) %>%
    ungroup() %>%
    arrange(rslt_gap)

  # Create deficit share associated with each OU (or level of interest)
  # ou_gap_sh allows us to say of the 96K deficity, 65% belongs to Tanzania
  # And on the other side, of the 94K surplus, 43% belongs to Nigeria

  prep_adj <-
    prep_adj %>%
    group_by(ou_over_achv) %>%
    mutate(rslt_gap_tot = sum(rslt_gap)) %>%
    ungroup()%>%
    mutate(ou_gap_sh = rslt_gap / rslt_gap_tot,
           gap_running_sh = cumsum(ou_gap_sh),
           gap_running_target = cumsum(rslt_gap)
    )


  # Create Global rollups for achv and adjusted achv
  prep_adj <-
    prep_adj %>%
    group_by(fundingagency) %>%
    mutate(across(c(targets:rslt_surplus), sum, na.rm = T, .names = "{.col}_glbl")) %>%
    ungroup() %>%
    mutate(achv_glbl = cumulative_glbl / targets_glbl,
           achv_adj_glbl = rslt_capped_glbl / targets_glbl,
           deficit_sh = abs(rslt_deficit_glbl)/targets_glbl,
           surplus_sh = rslt_surplus_glbl / targets_glbl) %>%
    mutate(ou_order = fct_reorder(operatingunit, cumulative),
           facet_label = ifelse(ou_over_achv == TRUE, "Achieved Targets", "Unachieved"))

  #
  df_tst <- ou_im %>%
    extract_data(operatingunit) %>%
    decompose_results(operatingunit) %>%
    calculate_gapshare(operatingunit) %>%
    rollup_achv()

  df_tst2 <- psnu_im %>%
    filter(operatingunit == "Nigeria") %>%
    extract_data(operatingunit, psnu) %>%
    decompose_results(operatingunit, psnu) %>%
    calculate_gapshare(operatingunit, psnu) %>%
    rollup_achv(operatingunit)

  df_tst %>%
    filter(fundingagency == agency) %>%
    mutate(ou_order = fct_reorder(operatingunit, results)) %>%
    slice(1) %>%
    viz_group()

  df_tst %>%
    filter(fundingagency == agency) %>%
    mutate(ou_order = fct_reorder(operatingunit, results)) %>%
    viz_orgunits()

  df_tst %>% viz_decomposition()

  df_tst2 %>%
    filter(!str_detect(psnu, "_Mil")) %>%
    mutate(grp_order = fct_reorder(psnu, results)) %>%
    viz_decomposition("Nigeria")

# VIZ ----

  # What colors to use?
  si_rampr("scooters")[4] %>% show_col()

  # Pull global achv and global ach adj
  achv_lab     <- prep_adj %>% slice(1) %>% pull(achv_glbl)
  achv_adj_lab <- prep_adj %>% slice(1) %>% pull(achv_adj_glbl)

  # Plots
  glbl_plot <-
    prep_adj %>%
    slice(1) %>% #glimpse()
    ggplot(aes(y = "Global")) +
    # What is benchmark?
    geom_col(aes(x = targets_glbl), fill = trolley_grey_light) +
    # What is total rollup?
    geom_col(aes(x = cumulative_glbl), fill = scooter_med) +
    # What is target capped rollup
    geom_col(aes(x = rslt_capped_glbl), fill = scooter) +
    # What is target capped deficit?
    geom_col(aes(x = rslt_deficit_glbl), fill = old_rose_light) +
    geom_text(aes(x = rslt_capped_glbl, label = percent(achv_adj_glbl)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_text(aes(x = cumulative_glbl, label = percent(achv_glbl)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_text(aes(x = rslt_deficit_glbl, label = percent(deficit_sh)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid() +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL,
         title = glue("GLOBAL ACHIEVEMENT IS {scales::percent({achv_lab})}
                       ADJUSTED ACHIEVEMENT IS {percent({achv_adj_lab})}"))


  ou_count <- prep_adj %>%
    summarise(ou_achv = sum(rslt_surplus > 0, na.rm = T)) %>%
    pull()

  ou_plot <-
    prep_adj %>%
    ggplot(aes(y = ou_order)) +
    # What is benchmark?
    geom_col(aes(x = targets), fill = trolley_grey_light) +
    # What is total rollup?
    geom_col(aes(x = cumulative), fill = scooter_med) +
    # What is target capped rollup
    geom_col(aes(x = rslt_capped), fill = scooter) +
    # Show target threshold in white to indicate surplus
    geom_errorbar(aes(xmin = targets, xmax = targets, color = ifelse(!is.na(rslt_surplus), "white", NA_character_)), size = 0.5) +
    # What is target capped deficit?
    geom_col(aes(x = rslt_deficit), fill = old_rose_light) +
    geom_text(aes(x = cumulative, label = percent(achv, 1)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
    facet_wrap(~facet_label, scales = "free_y", ncol = 1) +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid() +
    scale_color_identity()  +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL, title = glue("{ou_count} OUs ACHIEVED TARGETS AS OF {curr_pd}"))

  # TODO: Align plots so 0 axes are aligned across top and bottom
  max_dims <- get_max_dim(glbl_plot, ou_plot)
  set_dim(ou_plot, max_dims)

  cowplot::plot_grid(glbl_plot, ou_plot,
                     ncol = 1, align = "hv",
                     axis = "bt", rel_heights = c(1, 5))

  glbl_plot / ou_plot + plot_layout(heights = c(1, 7))

  si_save(file.path(dir_images, "adj_achievement.png"), scale = 1.45)

