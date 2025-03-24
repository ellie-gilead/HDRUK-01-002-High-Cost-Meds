
# packages -----
library(omopgenerics)
library(CDMConnector)
library(here)
library(DBI)
library(CodelistGenerator)
library(dplyr)
library(ggplot2)

# connect to a db with vocabularies -----
db <- dbConnect(duckdb::duckdb(), 
                dbdir = eunomiaDir(datasetName = "synthea-covid19-10k"))
cdm <- cdmFromCon(db, "main", "main") 

# import characterisation results -----
results <- importSummarisedResult(here("characterisation_results"))
concept_counts <- results |> 
  filterSettings(result_type == "summarise_concept_id_counts") |> 
  # filter(strata_level == "overall") |> 
  suppress(5)

# get codes for all drugs from the high cost medicines  -----
hc_meds <- readr::read_csv(here("hc_meds.csv")) |> 
  filter(is.na(exclusion_reason))

drug_codes <- getDrugIngredientCodes(cdm, 
                                     hc_meds$concept_id, 
                                     nameStyle = "{concept_name}")

# get record counts from feasibility results -----
drug_counts <- list()
for(i in seq_along(drug_codes)){
  cli::cli_inform("Getting counts for {drug_name$ingredient_concept_name[i]}") 
  drug_counts[[i]] <- concept_counts |> 
  # filter(strata_level == "overall") |> 
   filter(variable_level %in% as.character(drug_codes[[i]])) |> 
   mutate(count = as.numeric(estimate_value)) |> 
   summarise(count = sum(count), 
             .by = c(cdm_name, strata_name, strata_level)) |> 
   mutate(ingredient_concept_name = 
            drug_name$ingredient_concept_name[i])
}
drug_counts <- bind_rows(drug_counts) |>
  arrange(desc(count))

drug_counts <- drug_counts |> 
  splitStrata()

drug_counts_details <- drug_counts |> 
  left_join(drug_name,
            by = "ingredient_concept_name") |> 
  arrange(desc(count))

# plot overall results ------

plot_data <- drug_counts_details |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "2024")
top_ten <- plot_data |> 
  group_by(cdm_name) |> 
  slice_max(count, n = 10) |> 
  pull(ingredient_concept_name) |> 
  unique()

plot_data |>
  ggplot() + 
  geom_col(aes(reorder(ingredient_concept_name, count), 
               count, 
               fill = cdm_name, 
               colour = "atc_1")) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs in 2024")
ggsave("high_cost_meds_overall_2024.png", 
       width = 12, height = 18.5)

plot_data |>
  filter(ingredient_concept_name %in% top_ten) |> 
  ggplot() + 
  geom_col(aes(reorder(ingredient_concept_name, count), 
               count, 
               fill = atc_3)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs in 2024")
ggsave("high_cost_meds_overall_2024_top_ten.png", 
       width = 12, height = 18.5)

# 0 to 18 -----
plot_data <- drug_counts_details |> 
  filter(age_group == "0 to 19",
         sex == "overall",
         year == "2024")
top_ten <- plot_data |> 
  group_by(cdm_name) |> 
  slice_max(count, n = 10) |> 
  pull(ingredient_concept_name) |> 
  unique()
plot_data |>
  filter(ingredient_concept_name %in% top_ten) |> 
  ggplot() + 
  geom_col(aes(reorder(ingredient_concept_name, count), 
               count, 
               fill = atc_3)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs in 2024 - age 0 to 19")
ggsave("high_cost_meds_0_19_2024_top_ten.png", 
       width = 12, height = 18.5)





# 80 and above -----
plot_data <- drug_counts_details |> 
  filter(age_group == "80 or above",
         sex == "overall",
         year == "2024")
top_ten <- plot_data |> 
  group_by(cdm_name) |> 
  slice_max(count, n = 10) |> 
  pull(ingredient_concept_name) |> 
  unique()
plot_data |>
  filter(ingredient_concept_name %in% top_ten) |> 
  ggplot() + 
  geom_col(aes(reorder(ingredient_concept_name, count), 
               count, 
               fill = atc_3)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs in 2024 - age 80 or above")
ggsave("high_cost_meds_80_up_2024_top_ten.png", 
       width = 12, height = 18.5)




