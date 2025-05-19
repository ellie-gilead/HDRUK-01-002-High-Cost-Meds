library(here)
library(stringr)
library(dplyr)
library(ggplot2)
library(omopgenerics)
library(CohortCharacteristics)
library(tidytext)

atc_ref <- readr::read_csv("atc_ref.csv")
atc_ref <- atc_ref |> 
  mutate(cohort_name_short = stringr::str_replace(name, "^[^_]*_", ""))


res <- importSummarisedResult(here::here("data"))


counts <- tidy(res |> 
                 filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))|> 
  filter(variable_name == "Number subjects") |> 
  filter(str_detect(cohort_name, "_first")) |> 
  select("cdm_name", "cohort_name_short", "count") |> 
  arrange(desc(cohort_name_short))
counts %>%
  filter(count > 500) %>%
  left_join(
    atc_ref %>% 
      filter(concept_class_id == "ATC 1st")
  ) %>%
  mutate(cohort_name_short= CodelistGenerator:::tidyWords(cohort_name_short)) |>
  mutate(cohort_name_short= str_to_sentence(cohort_name_short)) |> 
  ggplot() +
  facet_grid(concept_code ~ cdm_name, 
             scales = 'free_y', 
             space = 'free') +
  geom_col(aes(
    x = count, 
    y = reorder_within(cohort_name_short, count, cdm_name),
    fill = atc_name
  ), width = 1, colour = 'grey30') +
  scale_y_reordered() +  
  theme_bw() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "lines"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Number of individuals", y = "")

ggsave("counts.png", height = 15, width = 9)



# plot cohort counts
counts <- tidy(res |> 
       filterSettings(result_type == "summarise_characteristics")) |> 
  filter(variable_name %in% c("Number records", "Number subjects")) |> 
  mutate(cohort_name_short = cohort_name) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_all", "")) |> 
  mutate(cohort_name_short = str_replace(cohort_name_short, "_first", ""))

counts |> 
  filter(str_detect(cohort_name, "_all")) |> 
  filter(age_group == "overall"
         ,
         variable_name == "Number subjects"
         ) |> 
  ggplot() +
  facet_grid(variable_name ~ cdm_name, scales = "free") +
  geom_col(aes(count, cohort_name, 
               fill = cohort_name)) +
  theme_bw() +
  theme(legend.position = "none")



variable_name


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
