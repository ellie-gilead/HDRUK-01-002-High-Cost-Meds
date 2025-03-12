results <- list()

# analysis settings -----
study_period <- c(as.Date("2022-01-01"), as.Date(NA))
study_age_groups <- list(c(0, 0),
                         c(1, 17), 
                         c(18, 65),
                         c(66, 150))

# cdm summary -----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# instantiate cohorts ----
source(here("Cohorts", "instantiate_cohorts.R"))

# add age groups ----
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  addAge(ageGroup = study_age_groups, 
         name = "high_cost_meds")

# cohort counts ----
results[["counts"]] <- summariseCohortCount(cdm$high_cost_meds, 
                                            strata = "age_group")
results[["attrition"]] <- summariseCohortAttrition(cdm$high_cost_meds)
# index codes ----
for(i in seq_along(high_cost_meds_with_count)){
  results[[paste0("code_diag_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
    x = omopgenerics::cohortCodelist(cdm$high_cost_meds, 
                                     high_cost_meds_with_count[[i]]),
    cdm = cdm,
    cohortTable = "high_cost_meds",
    cohortId = high_cost_meds_with_count[[i]],
    timing = "entry",
    countBy = c("record", "person"),
    byConcept = TRUE
  )
}

# patient characteristics ----
results[["chars"]] <- summariseCharacteristics(cdm$high_cost_meds, 
                                               strata = "age_group",
                                               cohortIntersectFlag = list(targetCohortTable = "icd",
                                                                          window = c(-Inf, 0)))  

# large scale characteristics ----
results[["lsc"]] <- summariseLargeScaleCharacteristics(cdm$high_cost_meds,
                             strata = "age_group",
                             eventInWindow = c("condition_occurrence",
                                               "observation",
                                               "procedure_occurrence"),
                             window = list(c(0, 0),
                                           c(-30, 0),
                                           c(-Inf, 0)))

# export results ----
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results, 
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")
