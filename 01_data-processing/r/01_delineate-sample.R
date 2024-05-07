
# packages ----------------------------------------------------------------
packages <- c("tidyverse", "here")
groundhog_day <- "2024-01-11"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)

# get data from .rds ------------------------------------------------------

dat <- readRDS(file = here::here("01_data-processing", "data_private", "data_raw.rds"))

baseline_raw <- dat[[1]]

year_one_raw <- dat[[2]]


baseline_renamed <- baseline_raw |> 
  dplyr::select(
    ID = idnum,
    
    # national sample flag
    m_national_sample = cm1innatsm,
    
    # two cities flag
    m_two_cities = cm1twoc,
    
    # mother's race 1 is white, 2 is black
    m_race = m1h3
  )

year_one_renamed <- year_one_raw |> 
  dplyr::select(
    ID = idnum,
    
    # was mother interviewed at year-one (according to mother's records)
    m_interviewed_one = cm2mint,
    
    # relationship status
    # still with father
    m_romantically_father_now = m2d5,
    
    # with father before
    m_romantically_father_before = m2d7,
    
    # with another partner at year 1
    m_romantically_other_partner = m2e2
  )

dat_zero_one <- baseline_renamed |> 
  dplyr::left_join(year_one_renamed, by = "ID") 

dat_zero_one_include <- dat_zero_one |> 
  dplyr::mutate(
    m_two_cities = case_when(
      m_two_cities == 0 ~ 1, 
      .default = 0
    ),
    m_in_relationship = case_when(
      m_romantically_father_now == 1 | m_romantically_father_before == 1 | m_romantically_other_partner == 1 ~ 1,
      .default = 0
    ),
    m_race = case_when(
      m_race %in% c(1,2) ~ 1,
      .default = 0
    ),
    
    fit_criterion_1 = m_national_sample == 1,
    fit_criterion_2 = fit_criterion_1 & m_two_cities == 1,
    fit_criterion_3 = fit_criterion_2 & m_interviewed_one == 1,
    fit_criterion_4 = fit_criterion_3 & m_in_relationship == 1,
    fit_criterion_5 = fit_criterion_4 & m_race == 1
  )

included_ids <- dat_zero_one_include |> 
  dplyr::filter(fit_criterion_5 == 1) |> 
  dplyr::select(ID)

# save data with sample delineation

saveRDS(
  dat_zero_one_include, 
  file = here::here(
    "01_data-processing", "data_public",
    "delineation.RDS"
  )
)

# save included IDs

saveRDS(
  included_ids, 
  file = here::here(
    "01_data-processing", "data_public",
    "included_IDs.RDS"
  )
)

rm(list = ls())