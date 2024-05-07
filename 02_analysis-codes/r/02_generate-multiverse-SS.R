

# code that generates the multiverse analysis and saves the results

# packages ----------------------------------------------------------------
packages <- c("tidyverse", "broom", "multiverse", "srvyr", "marginaleffects", "survey", "future", "beepr")
groundhog_day <- "2024-01-11"

# (install and) load package versions available on the specified day to try
# to ensure reproducibility

library(groundhog)

groundhog::meta.groundhog(groundhog_day)

groundhog::groundhog.library(pkg = packages, date = groundhog_day)

# allow parallelization (depending on available cores in machine)
future::plan(multisession)

# create multiverse object
M <- multiverse::multiverse()

# generate multiverse
# ignore warnings about non-integer #successes in binomial glm

multiverse::inside(
  M,
  {
    
    dat_cc <- readRDS(here::here("01_data-processing", "data_private", "data_final_complete_cases.RDS"))
    dat_imputed <- readRDS(here::here("01_data-processing", "data_private", "data_final_imputed_cases.RDS"))
    
    
    dat <- branch(missing,
                  "imputed" ~ dat_imputed,
                  "complete_cases" ~ dat_cc
    ) |> dplyr::filter(m_national_sample == 1)
    
    dat <- 
      branch(
        proportion,
        "continuous" ~ dat,
        "binary" ~ dat |> dplyr::mutate(
          informal_support_prop = ifelse(informal_support_prop == 1, 1, 0),
          ipv_prop = ifelse(ipv_prop == 0, 0, 1),
          ipv_physical_prop = ifelse(ipv_physical_prop == 0, 0, 1),
          ipv_emotional_prop = ifelse(ipv_emotional_prop == 0, 0, 1),
          ipv_controlling_prop = ifelse(ipv_controlling_prop == 0, 0, 1)
        )
      )
    
    dat_design <- srvyr::as_survey_rep(
      .data = dat,
        repweights = dplyr::contains("m1natwt_rep"),
        weights = m1natwt,
        combined_weights = TRUE,
        # why: https://stats.stackexchange.com/questions/409463/duplicating-stata-survey-design-using-svrepdesign-from-survey-package-in-r
        type = "JKn",
        scales = 1,
        rscales = 1,
        mse = TRUE
      )
    
    model.m <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = informal_support_prop ~ branch(
          covariates,
          "adjusted" ~ m_race + 
            m_age + m_education + m_alcohol + m_drugs +
            m_employment + m_children + m_household_income +
            m_home + m_welfare_last_year + m_health + m_religious,
          "unadjusted" ~ m_race
        ),
        design = dat_design,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "binomial"
        )
      ),
      "unweighted" ~ glm(
        formula = informal_support_prop ~ branch(
          covariates,
          "adjusted" ~ m_race + 
            m_age + m_education + m_alcohol + m_drugs +
            m_employment + m_children + m_household_income +
            m_home + m_welfare_last_year + m_health + m_religious,
          "unadjusted" ~ m_race
        ),
        data = dat,
        family = branch(
          proportion,
          "continuous" ~ "gaussian",
          "binary" ~ "binomial"
        )
      )
    )
    
    comp_race <- marginaleffects::avg_comparisons(
      model.m,
      variables = "m_race",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binary" ~ NULL
      )
    ) |> broom::tidy() |>
      dplyr::mutate(comp = "race")

  }
)

# run ALL the analyses
# ignore warnings about weights being taken as sampling weights
# (that is what we wanted and expected)
multiverse::execute_multiverse(M, parallel = TRUE)

# store all the analyses
multi_results <- multiverse::expand(M)

# # to retrieve just the relevant results
# comp_results <- multi_results |>
#   dplyr::mutate(comp_race = purrr::map(.results, "comp_race")) |>
#   tidyr::unnest(cols = comp_race)

# store the parameters
multi_parameters <- multiverse::parameters(M)

# save all the results
# takes a little bit
saveRDS(
  object = multi_results,
  file = here::here("02_analysis-codes", "outputs", "multiverse_results_SS.RDS")
)

# save the parameters
saveRDS(
  object = multi_parameters,
  file = here::here("02_analysis-codes", "outputs", "multiverse_parameters_SS.RDS")
)

# stop parallelization
future::plan(sequential)

# "notify" me when it's finished computing
beepr::beep()

rm(list = ls())
