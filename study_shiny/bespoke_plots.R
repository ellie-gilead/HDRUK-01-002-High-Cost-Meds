library(here)
library(stringr)
library(dplyr)
library(ggplot2)
library(omopgenerics)
library(CohortCharacteristics)

atc_ref <- readr::read_csv("atc_ref.csv")
atc_ref <- atc_ref |> 
  mutate(cohort_name_short = stringr::str_replace(name, "^[^_]*_", ""))


res <- importSummarisedResult(here::here("data"))

tableCharacteristics(res)


# plot cohort counts
counts <- tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))



drug_counts_atc_1 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 1st"))
drug_counts_atc_2 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 2nd"))
drug_counts_atc_3 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 3rd"))
drug_counts_atc_4 <- counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 4th"))




counts |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name, scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_1 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cdm_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_2 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_3 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")

drug_counts_atc_4 |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group == "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_wrap(vars(atc_name, cdm_name), scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")


counts |> 
  filter(str_detect(cohort_name, "_first")) |> 
  filter(age_group != "overall",
         variable_name == "Number subjects") |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name, scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = age_group)) +
  theme_bw() +
  theme(legend.position = "top")



# counts by age group
counts |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name + cohort_name) +
  geom_col(aes(age_group, count, 
               fill = age_group))
