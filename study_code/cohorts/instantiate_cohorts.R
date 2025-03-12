# high cost drug cohorts -----
drug_name <- readr::read_csv(here("cohorts",
                                  "high_cost_ingredients.csv"), 
                             col_types = list(col_character()))
drug_name <- drug_name |>
  filter(is.na(exclude_reason)) |> 
  mutate(concept_id = as.integer(concept_id))

drug_codes <- getDrugIngredientCodes(cdm, 
                                     drug_name$concept_id, 
                                     nameStyle = "{concept_name}")
cdm$high_cost_meds <- conceptCohort(cdm, 
                                    conceptSet = drug_codes, 
                                    name = "high_cost_meds", 
                                    exit = "event_end_date")

# keep only records in study period
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  requireInDateRange(study_period)

# remove cohorts with zero counts
high_cost_meds_with_count <- cohortCount(cdm$high_cost_meds) |>
  filter(number_subjects > 0) |> 
  dplyr::pull("cohort_definition_id")
if(length(high_cost_meds_with_count) > 0){
cdm$high_cost_meds <- subsetCohorts(cdm$high_cost_meds,
                                 cohortId = high_cost_meds_with_count, 
                                 name = "high_cost_meds")
}

# add age and sex matched cohorts -----
cdm$high_cost_meds <- cdm$high_cost_meds |> 
  matchCohorts(ratio = 1, 
               keepOriginalCohorts = TRUE)

# icd chapter cohorts -----
# as not all dps will have icd codes in their vocab, pre compute these
# icd_subchapter <- readr::read_csv(here("cohorts",
#                                   "icd_subchapter.csv"), col_types = "c") |>
#   filter(include == "yes")
# for(i in seq_along(icd_subchapter$icd_subchapter)){
# codes <- getICD10StandardCodes(cdm = cdm,
#                       name = icd_subchapter$icd_subchapter[i],
#                       level = "ICD10 SubChapter")
# if(inherits(codes, "codelist") && length(codes[[1]]) > 0){
# names(codes) <- icd_subchapter$cohort_name[i]
# codes |>
#   subsetOnDomain(cdm = cdm,
#                  domain = c("condition")) |>
#   exportCodelist(type = "csv",
#                  path = here("cohorts", "icd"))
# }
# }

icd_chapters <- importCodelist(type = "csv",
               path = here("cohorts", "icd"))
cdm$icd <- conceptCohort(cdm,
                         conceptSet = icd_chapters,
                         name = "icd",
                         exit = "event_end_date",
                         subsetCohort = "high_cost_meds")
