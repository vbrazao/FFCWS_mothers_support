

# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(missForest)

# set seed for reproducible imputation

set.seed(30360836)


# get data from .rds ------------------------------------------------------

dat <- readRDS(
  file = here::here(
    "01_data-processing", 
    "data_private",
    "data_raw.rds"
  )
)

baseline_raw <- dat[[1]]

year_one_raw <- dat[[2]]


# filter based on our delineation -----------------------------------------

included_ids <- readRDS(
  here::here(
    "01_data-processing", 
    "data_public",
    "included_IDs.RDS"
  )
) |> 
  # make it a vector
  dplyr::pull()

baseline_included <- baseline_raw |> 
  dplyr::filter(
    idnum %in% included_ids
  ) |> 
  dplyr::mutate(
    idnum = as.numeric(idnum)
  )

year_one_included <- year_one_raw |> 
  dplyr::filter(
    idnum %in% included_ids
  ) |> 
  dplyr::mutate(
    idnum = as.numeric(idnum)
  )


# CREATE ONE VARIABLE AT A TIME -------------------------------------------
# IPV ---------------------------------------------------------------------

# which relationship to take IPV from
dat_ipv_source <- year_one_included |> 
  dplyr::select(
    idnum,
    # relationship status
    # still with father
    m_romantically_father_now = m2d5,
    
    # with father before
    m_romantically_father_before = m2d7,
    
    # with another partner at year 1
    m_romantically_other_partner = m2e2
  ) |> 
  dplyr::mutate(
    source_ipv = dplyr::case_when(
      m_romantically_father_now == 1 ~ "Father now",
      m_romantically_other_partner == 1 ~ "Other partner now",
      m_romantically_father_before == 1 ~ "Father past"
    ) |> 
      forcats::as_factor(),
    .keep = "unused"
  )

# ipv variables

dat_ipv <- year_one_included |> 
  dplyr::select(
    idnum,
    
    # IPV - intimate partner violence scales #
    # _r indicates a positive item that will need to be reverse scored so it's 
    # negative
    # behavior of father now
    ipv_01_r = m2d6a,
    ipv_02_r = m2d6b,
    ipv_03 = m2d6c,
    ipv_04_r = m2d6d,
    ipv_05 = m2d6e,
    ipv_06 = m2d6f,
    ipv_07 = m2d6g,
    ipv_08 = m2d6h,
    ipv_09 = m2d6i,
    ipv_10 = m2d6j,
    ipv_11_r = m2d6k,
    ipv_12_r = m2d6l,
    
    # behavior of current partner, not father
    ipv_01_other_r = m2e8a,
    ipv_02_other_r = m2e8b,
    ipv_03_other = m2e8c,
    ipv_04_other_r = m2e8d,
    ipv_05_other = m2e8e,
    ipv_06_other = m2e8f,
    ipv_07_other = m2e8g,
    ipv_08_other = m2e8h,
    ipv_09_other = m2e8i,
    ipv_10_other = m2e8j,
    ipv_11_other_r = m2e8k,
    ipv_12_other_r = m2e8l,
    
    # behavior in last month of relationship with father (no longer together)
    ipv_01_past_r = m2d8a,
    ipv_02_past_r = m2d8b,
    ipv_03_past = m2d8c,
    ipv_04_past_r = m2d8d,
    ipv_05_past = m2d8e,
    ipv_06_past = m2d8f,
    ipv_07_past = m2d8g,
    ipv_08_past = m2d8h,
    ipv_09_past = m2d8i,
    ipv_10_past = m2d8j,
    ipv_11_past_r = m2d8k,
    ipv_12_past_r = m2d8l
  ) |> 
  # recode ipv values
  dplyr::mutate(
    # create ipv variables with "_recoded" suffix
    # if they are negative (no "_r" suffix), then 3 becomes 0, 2 becomes 1,
    # and 1 becomes 2,and NA signifiers are NA. 
    dplyr::across(
      .cols = dplyr::starts_with("ipv_") & -dplyr::contains("_r"),
      .fns = ~ dplyr::case_when(
        .x == 3 ~ 0,
        .x == 2 ~ 1,
        .x == 1 ~ 2,
        .default = NA
      ),
      .names = "recoded_{.col}"
    ),
    # if they are positive (with "_r" suffix), then 3 becomes 2, 2 becomes 1,
    # 1 becomes 0, and NA signifiers are NA.
    dplyr::across(
      .cols = dplyr::starts_with("ipv_") & dplyr::contains("_r"),
      .fns = ~ dplyr::case_when(
        .x == 3 ~ 2,
        .x == 2 ~ 1,
        .x == 1 ~ 0,
        .default = NA
      ),
      .names = "recoded_{.col}"
    ),
    # turn all ipv variables into factors
    dplyr::across(
      .cols = dplyr::starts_with("recoded_ipv_"),
      .fns = ~ forcats::as_factor(.x) |> 
        forcats::fct_expand("0", "1", "2") |> 
        forcats::fct_relevel("0", "1", "2")
    ),
    .keep = "unused"
  ) 

var_ipv_and_source <- dat_ipv_source |> 
  dplyr::left_join(dat_ipv, by = "idnum") |> 
  #pivot so that all ipv values are in one column
  tidyr::pivot_longer(
    cols = dplyr::starts_with("recoded_ipv"),
    names_to = "ipv_type",
    values_to = "ipv_value"
  ) |> 
  #keep only the ipv values that match the ipv source
  dplyr::filter(
    (source_ipv == "Father now" & 
      stringr::str_detect(ipv_type, "other", negate = TRUE) &
      stringr::str_detect(ipv_type, "past", negate = TRUE)) |
      (source_ipv == "Other partner now" &
      stringr::str_detect(ipv_type, "other")) |
      (source_ipv == "Father past" &
      stringr::str_detect(ipv_type, "past"))
  ) |> 
  #clean up the names of ipv types
  dplyr::mutate(
    ipv_type = ipv_type |> 
      stringr::str_remove("recoded_") |> 
      stringr::str_remove("_other") |> 
      stringr::str_remove("_past") |> 
      stringr::str_remove("_r")
  ) |> 
  # reorder so that when we pivot back ipv items are in the right order
  dplyr::arrange(
    idnum, ipv_type
  ) |> 
  # get all the variables their own column
  tidyr::pivot_wider(
    names_from = ipv_type,
    values_from = ipv_value
  )


# Race --------------------------------------------------------------------

var_race <- baseline_included |> 
  dplyr::select(
    idnum,
    m_race = m1h3
  ) |> 
  dplyr::mutate(
    m_race = dplyr::case_when(
      m_race == 1 ~ "White",
      m_race == 2 ~ "Black"
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("White", "Black")
  )


# Age ---------------------------------------------------------------------

var_age <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # mother's age at interview (constructed)
    m_age = cm1age
  ) |> 
  dplyr::mutate(
    m_age = dplyr::case_when(
      m_age > 0 ~ m_age,
      .default = NA
    )
  )



# Education ---------------------------------------------------------------

var_education <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # mother's level of education
    m_education = m1i1
  ) |> 
  dplyr::mutate(
    # create education categories, turn into factor, make the reference factor
    # be "Below HS" 
    m_education = dplyr::case_when(
      m_education >= 4 ~ "HS and above",
      m_education >= 1 ~ "Below HS",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Below HS", "HS and above")
  )


# Alcohol use -------------------------------------------------------------

var_alcohol <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # mother's alcohol use
    m_alcohol = m1g2
  ) |> 
  dplyr::mutate(
    # create factor for alcohol use, make the reference factor be "Never"
    m_alcohol = dplyr::case_when(
      m_alcohol == 5 ~ "Never",
      m_alcohol == 4 ~ "<1 / month",
      m_alcohol %in% c(3, 2, 1) ~ ">1 / month",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Never", "<1 / month", ">1 / month")
  )


# Drug use ----------------------------------------------------------------

var_drugs <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # mother's drug use
    m_drugs = m1g3
  ) |> 
  dplyr::mutate(
    # create factor for drug use, make the reference factor be "Never"
    m_drugs = dplyr::case_when(
      m_drugs == 5 ~ "Never",
      m_drugs == 4 ~ "<1 / month",
      m_drugs %in% c(3, 2, 1) ~ ">1 / month",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Never", "<1 / month", ">1 / month"),
  )


# Employment --------------------------------------------------------------

var_employment <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # time of mother's interview
    m_month = cm1intmon,
    m_year = cm1intyr,
    
    # mother's employment
    m_employed_month = m1i2i,
    m_employed_year = m1i2ii,
    m_never_employed = m1i2iii
  ) |> 
  dplyr::mutate(
    # replace negative values with NA for month and year
    dplyr::across(
      .cols = c(m_year, m_month, m_employed_month, m_employed_year),
      .fns = ~ dplyr::case_when(
        .x < 0 ~ NA,
        .default = .x
      )
    ), 
    
    # find the difference, in months, between time of interview and last 
    # employment, then construct the employment variable
    employment_difference = (m_month + 12*m_year) - (m_employed_month + 12*m_employed_year),
    m_employment = dplyr::case_when(
      m_never_employed == 1 ~ "Unemployed",
      employment_difference >= 2 ~ "Unemployed",
      employment_difference < 2 ~ "Employed",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Employed", "Unemployed")
  ) |> 
  dplyr::select(
    idnum, m_employment
  )


# Number of children ------------------------------------------------------

var_children <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # number of other biological children
    m_children_binary = m1a12, # 1 = yes, 2 = no
    m_children_number = m1a12a
  ) |> 
  dplyr::mutate(
    # join the two variables for number of children so that mothers without
    # other children get assigned a 0, and values are taken from the 
    # m_children_number variable
    m_children = dplyr::case_when(
      m_children_binary == 2 ~ 0,
      m_children_number < 0 ~ NA,
      .default = m_children_number
    )
  ) |> 
  dplyr::select(
    idnum, m_children
  )


# Household income --------------------------------------------------------

var_income <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # household income,
    m_household_income = m1j3
  ) |> 
  dplyr::mutate(
    m_household_income = dplyr::case_when(
      m_household_income %in% c(1, 2, 3) ~ "Under $15,000",
      m_household_income %in% c(4, 5, 6) ~ "$15,000 to $34,999",
      m_household_income %in% c(7, 8, 9) ~ "$35,000 or more",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Under $15,000", "$15,000 to $34,999", 
                           "$35,000 or more")
  )


# Welfare -----------------------------------------------------------------

var_welfare_last_year <- year_one_included |> 
  dplyr::select(
    idnum,
    
    # welfare 
    m_welfare = m2h9a1,
    m_food_stamps = m2h9a2,
    m_other_assistance = m2h9a3
  ) |> 
  dplyr::mutate(
    # relabel welfare over year 1 variable
    m_welfare_last_year = dplyr::case_when(
      m_welfare == 1 ~ "Yes",
      m_food_stamps == 1 ~ "Yes",
      m_other_assistance == 1 ~ "Yes",
      m_welfare == 2 &
        m_food_stamps == 2 &
        m_other_assistance == 2 ~ "No",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("No", "Yes")
  ) |> 
  dplyr::select(
    idnum, m_welfare_last_year
  )


# Home ownership ----------------------------------------------------------

var_home <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # home owned or rented
    m_home = m1f2
  ) |> 
  dplyr::mutate(
    m_home = dplyr::case_when(
      m_home == 2 ~ "Rented",
      m_home == 1 ~ "Owned",
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("Owned", "Rented")
  )



# Religious services ------------------------------------------------------

var_religious <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # how often religious services,
    m_religious = m1f6
  ) |> 
  dplyr::mutate(
    m_religious = dplyr::case_when(
      # reverse score, so 1 = not at all and 5 = once a week or more
      m_religious > 0 ~ 6 - m_religious,
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("1", "2", "3", "4", "5")
  )


# Health ------------------------------------------------------------------

var_health <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # how would you say your health is
    m_health = m1g1
  ) |> 
  dplyr::mutate(
    m_health = dplyr::case_when(
      # reverse score, so 1 = not at all and 5 = once a week or more
      m_health > 0 ~ 6 - m_health,
      .default = NA
    ) |> 
      forcats::as_factor() |> 
      forcats::fct_relevel("1", "2", "3", "4", "5")
  )



# Social support ----------------------------------------------------------

var_support <- baseline_included |> 
  dplyr::select(
    idnum,
    
    # informal support (prospective)
    m_support_loan = m1e4a,
    m_support_place = m1e4b,
    m_support_care = m1e4c
  ) |> 
  dplyr::mutate(
    # correct scores for prospective support
    dplyr::across(
      .cols = dplyr::starts_with("m_support"),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .default = NA
      ) |> 
        forcats::as_factor()
    )
  ) 



# Weights -----------------------------------------------------------------

var_weights <- baseline_included |> 
  dplyr::select(
    idnum,
    
    m1natwt,
    dplyr::starts_with("m1natwt_")
  )


# National sample flag ----------------------------------------------------

var_flag <- baseline_included |> 
  dplyr::select(
    idnum,
    
    m_national_sample = cm1innatsm
  )

# Put it all together -----------------------------------------------------

dat_full <- var_race |> 
  left_join(var_ipv_and_source, by = "idnum") |> 
  left_join(var_age, by = "idnum") |> 
  left_join(var_education, by = "idnum") |> 
  left_join(var_alcohol, by = "idnum") |> 
  left_join(var_drugs, by = "idnum") |> 
  left_join(var_employment, by = "idnum") |> 
  left_join(var_children, by = "idnum") |> 
  left_join(var_income, by = "idnum") |> 
  left_join(var_welfare_last_year, by = "idnum") |> 
  left_join(var_religious, by = "idnum") |> 
  left_join(var_health, by = "idnum") |> 
  left_join(var_home, by = "idnum") |> 
  left_join(var_support, by = "idnum")




# Imputing missing values -------------------------------------------------

dat_full_ids <- dat_full |> dplyr::select(idnum)

dat_full_imputed_obj <- dat_full |>
  dplyr::select(-idnum) |>
  as.data.frame() |>
  missForest::missForest()

dat_full_imputed <- dat_full_imputed_obj$ximp |> 
  dplyr::mutate(
    idnum = dat_full_ids$idnum
  ) |> 
  dplyr::relocate(
    idnum
  )

dat_full_cc <- dat_full|> 
  dplyr::filter(
    complete.cases(dat_full)
  )

# recode outcomes, add weights, and add sample flag
recode_outcomes <- function(data){
  df <- data |> 
    dplyr::mutate(
      across(
        .cols = c(
          dplyr::starts_with("ipv_"),
          dplyr::starts_with("m_support"),
          m_health, m_religious
        ), 
        .fns = ~ as.character(.x) |> as.numeric()
      ),
      ipv_sum = (ipv_01 + ipv_02 + ipv_03 + ipv_04 + ipv_05 + ipv_06 + ipv_07 +
        ipv_08 + ipv_09 + ipv_10 + ipv_11 + ipv_12),
      ipv_prop = ipv_sum/24,
      ipv_max = 24,
      ipv_physical = ipv_08 + ipv_09 + ipv_10,
      ipv_physical_prop = ipv_physical / 6,
      ipv_physical_max = 6,
      ipv_emotional = ipv_01 + ipv_02 + ipv_03 + ipv_04 + ipv_11 + ipv_12,
      ipv_emotional_prop = ipv_emotional / 12,
      ipv_emotional_max = 12,
      ipv_controlling = ipv_05 + ipv_06 + ipv_07,
      ipv_controlling_prop = ipv_controlling / 6,
      ipv_controlling_max = 6,
      informal_support_sum = m_support_loan + m_support_place + m_support_care,
      informal_support_prop = informal_support_sum/3
    ) |> 
    left_join(var_weights, by = "idnum") |> 
    left_join(var_flag, by = "idnum")
  
  return(df)
}

data_sets_recoded <- list(dat_full, dat_full_cc, dat_full_imputed) |> 
  purrr::map(.f = recode_outcomes)



# save the datasets -------------------------------------------------------

saveRDS(data_sets_recoded[[1]], file = here::here("01_data-processing", "data_private", "data_final.RDS"))
saveRDS(data_sets_recoded[[2]], file = here::here("01_data-processing", "data_private", "data_final_complete_cases.RDS"))
saveRDS(data_sets_recoded[[3]], file = here::here("01_data-processing", "data_private", "data_final_imputed_cases.RDS"))


rm(list = ls())
