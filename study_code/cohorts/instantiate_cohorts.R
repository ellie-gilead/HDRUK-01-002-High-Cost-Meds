# high cost drug cohorts -----
cli::cli_inform("Creating high cost medicines cohorts")
drug_codes <- importCodelist(path = here("cohorts", "drug_codelists"), type = "csv")
cdm$high_cost_meds <- conceptCohort(cdm, 
                                    conceptSet = drug_codes, 
                                    name = "high_cost_meds", 
                                    exit = "event_end_date")
# all records in study period
cdm$high_cost_meds_all <- cdm$high_cost_meds |> 
  requireInDateRange(dateRange = study_period, 
                     name = "high_cost_meds_all")
attr(cdm$high_cost_meds_all, "cohort_set") <- attr(cdm$high_cost_meds_all, "cohort_set") |> 
  mutate(cohort_name = paste0(cohort_name, "_all"))

# keep first ever and in study period
cdm$high_cost_meds_first <- cdm$high_cost_meds |> 
  requireIsFirstEntry(name = "high_cost_meds_first") |> 
  requireInDateRange(dateRange = study_period,
                     name = "high_cost_meds_first")
attr(cdm$high_cost_meds_first, "cohort_set") <- attr(cdm$high_cost_meds_first, "cohort_set") |> 
  mutate(cohort_name = paste0(cohort_name, "_first"))

cdm <- bind(cdm$high_cost_meds_all, cdm$high_cost_meds_first,
     name = "high_cost_meds")
dropSourceTable(cdm,
                c("high_cost_meds_all", "high_cost_meds_first"))

# remove cohorts with zero counts
high_cost_meds_with_count <- cohortCount(cdm$high_cost_meds) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(high_cost_meds_with_count) > 0){
  cdm$high_cost_meds <- subsetCohorts(cdm$high_cost_meds,
                                      cohortId = high_cost_meds_with_count, 
                                      name = "high_cost_meds")
} 
if(length(high_cost_meds_with_count) == 0){
  cli::cli_abort("All high cost drugs cohorts are empty")
}
