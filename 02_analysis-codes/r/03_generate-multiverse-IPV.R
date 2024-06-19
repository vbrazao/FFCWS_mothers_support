

# code that generates the multiverse analysis and saves the results

# packages ----------------------------------------------------------------
packages <- c("tidyverse", "broom", "multiverse", "srvyr", "marginaleffects", "survey", "future", "beepr")
groundhog_day <- "2024-04-22"

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
        "binomial" ~ dat,
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
    
    model.ipv.race <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
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
          "binomial" ~ "binomial",
          "binary" ~ "binomial"
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          ),
          "binary" ~ NULL
        )
      ),
      "unweighted" ~ glm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ branch(
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
          "binomial" ~ "binomial",
          "binary" ~ "binomial"
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          ),
          "binary" ~ NULL
        )
      )
    )
    
    comp_ipv_race <- marginaleffects::avg_comparisons(
      model.ipv.race,
      variables = "m_race",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binomial" ~ NULL,
        "binary" ~ NULL
      )
    ) |> broom::tidy() |>
      dplyr::mutate(comp = "ipv_race")
    
    model.ipv.race.sup <- branch(
      weights,
      "weighted" ~ survey::svyglm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ informal_support_prop + branch(
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
          "binomial" ~ "binomial",
          "binary" ~ "binomial"
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          ),
          "binary" ~ NULL
        )
      ),
      "unweighted" ~ glm(
        formula = branch(
          outcome,
          "total" ~ ipv_prop,
          "physical" ~ ipv_physical_prop,
          "emotional" ~ ipv_emotional_prop,
          "controlling" ~ ipv_controlling_prop
        ) ~ informal_support_prop + branch(
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
          "binomial" ~ "binomial",
          "binary" ~ "binomial"
        ),
        weights = branch(
          proportion,
          "continuous" ~ NULL,
          "binomial" ~ branch(
            outcome,
            "total" ~ ipv_max,
            "physical" ~ ipv_physical_max,
            "emotional" ~ ipv_emotional_max,
            "controlling" ~ ipv_controlling_max
          ),
          "binary" ~ NULL
        )
      )
    )
    
    comp_ipv_sup_race <- marginaleffects::avg_comparisons(
      model.ipv.race.sup,
      variables = "informal_support_prop",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binomial" ~ NULL,
        "binary" ~ NULL
      )
    ) |> broom::tidy() |>
      dplyr::mutate(comp = "ipv_sup_race")
    
    comp_ipv_race_sup <- marginaleffects::avg_comparisons(
      model.ipv.race.sup,
      variables = "m_race",
      vcov = branch(
        proportion,
        "continuous" ~ branch(
          weights,
          "weighted" ~ NULL,
          "unweighted" ~ "HC"
        ),
        "binomial" ~ NULL,
        "binary" ~ NULL
      )
    ) |> broom::tidy() |>
      dplyr::mutate(comp = "ipv_race_sup")

    comps <- dplyr::bind_rows(comp_ipv_race, comp_ipv_sup_race, comp_ipv_race_sup)
  }
)

# run ALL the analyses
# ignore warnings about weights being taken as sampling weights
# (that is what we wanted and expected)
multiverse::execute_multiverse(M, parallel = TRUE)

# store all the analyses
multi_results <- multiverse::expand(M)

# to retrieve just the relevant results
comp_results <- multi_results |>
  dplyr::mutate(comp = purrr::map(.results, "comps")) |>
  tidyr::unnest(cols = comp) |> 
  # remove columns that take too much space and are saved with the full results
  dplyr::select(
    -c(.parameter_assignment, .code, .results, .errors)
  )


# store the parameters
multi_parameters <- multiverse::parameters(M)

# save all the results
# takes a little bit
saveRDS(
  object = multi_results,
  file = here::here("02_analysis-codes", "outputs", "multiverse_results_IPV.RDS")
)

saveRDS(
  object = comp_results,
  file = here::here("02_analysis-codes", "outputs", "multi_ipv_results.RDS")
)

# save the parameters
saveRDS(
  object = multi_parameters,
  file = here::here("02_analysis-codes", "outputs", "multiverse_parameters_IPV.RDS")
)

# stop parallelization
future::plan(sequential)

# "notify" me when it's finished computing
beepr::beep()

rm(list = ls())
