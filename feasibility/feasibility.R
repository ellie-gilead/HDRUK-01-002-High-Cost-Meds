# packages -----
library(omopgenerics)
library(CDMConnector)
library(here)
library(DBI)
library(CodelistGenerator)
library(dplyr)
library(ggplot2)

# import codelists and atc equivalence ----
drug_codes <- importCodelist(here("codelists"), 
               type = "csv")

atc_ref <- readr::read_csv("atc_ref.csv")

# import characterisation results -----
# note - need to add characterisation csv to directory
results <- importSummarisedResult(here("characterisation_results"))
concept_counts <- results |> 
  filterSettings(result_type == "summarise_concept_id_counts")

# get record counts from feasibility results -----
drug_counts <- list()
for(i in seq_along(drug_codes)){
  cli::cli_inform("Getting counts for {names(drug_codes)[i]} ({i} of {length(drug_codes)})") 
  drug_counts[[i]] <- concept_counts |> 
   filter(variable_level %in% as.character(drug_codes[[i]])) |> 
   mutate(count = as.numeric(estimate_value)) |> 
   summarise(count = sum(count), 
             .by = c(cdm_name, strata_name, strata_level)) |> 
   mutate(name =  names(drug_codes)[i])
}
drug_counts <- bind_rows(drug_counts) |>
  arrange(desc(count))

drug_counts <- drug_counts |> 
  splitStrata()

drug_counts <- drug_counts |> 
  mutate(cdm_name = if_else(cdm_name == "IDRIL_1",
                            "Lancashire", cdm_name)) |> 
  mutate(cdm_name = if_else(cdm_name == "UCLH-2years",
                            "UCLH", cdm_name))

drug_counts$nice_name <- stringr::str_replace(drug_counts$name, "^[^_]*_", "")
drug_counts$nice_name <- stringr::str_to_sentence(drug_counts$nice_name)

# export ----
readr::write_csv(drug_counts, 
          here::here("drug_counts.csv"))

# atc counts -----
# important - some ingredients are in multiple atc categories!
drug_counts_atc_1 <- drug_counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 1st"))
drug_counts_atc_2 <- drug_counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 2nd"))
drug_counts_atc_3 <- drug_counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 3rd"))
drug_counts_atc_4 <- drug_counts |> 
  left_join(atc_ref |> 
              filter(concept_class_id == "ATC 4th"))


# summary ---- 
# total number of high cost medicines across dps
drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(name) |> 
  tally() |> 
  nrow()

# total by dp
drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(cdm_name) |> 
  tally() 

# top for lancs
drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall", 
         cdm_name == "Lancashire") |> 
  arrange(desc(count)) |> 
  select(name, count)

# top for barts
drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall", 
         cdm_name == "Barts") |> 
  arrange(desc(count)) |> 
  select(name, count)

# top for uclh
drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall", 
         cdm_name == "UCLH") |> 
  arrange(desc(count)) |> 
  select(name, count)

drug_counts_atc_1 |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(atc_name) |> 
  tally() |> 
  arrange(desc(n))

drug_counts_atc_2 |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(atc_name) |> 
  tally() |> 
  arrange(desc(n))

drug_counts_atc_3 |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(atc_name) |> 
  tally() |> 
  arrange(desc(n))

drug_counts_atc_4 |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall") |> 
  group_by(atc_name) |> 
  tally() |> 
  arrange(desc(n))



drug_counts_atc_4 |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall", 
         cdm_name == "IDRIL_1") |> 
  arrange(desc(count)) |> 
  select(name, count) |> 
  distinct()

# plot overall results ------
plot_data <- drug_counts |> 
  filter(age_group == "overall",
         sex == "overall",
         year == "overall")
top_ten <- plot_data |> 
  group_by(cdm_name) |> 
  slice_max(count, n = 10) |> 
  pull(name) |> 
  unique()

plot_data |>
  ggplot() + 
  geom_col(aes(reorder(name, count), 
               count, 
               fill = cdm_name)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs")
ggsave("high_cost_meds_overall_all.png", 
       width = 12, height = 18.5)

plot_data |>
  filter(name %in% top_ten) |> 
  ggplot() + 
  geom_col(aes(reorder(name, count), 
               count)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) +
  ggtitle("Use of high-cost drugs - top ten")
ggsave("high_cost_meds_overall_overall_top_ten.png", 
       width = 12, height = 18.5)

# protein kinase inhibitors ------
plot_data <- drug_counts_atc_3 |> 
  filter(age_group != "overall",
         sex == "overall",
         year == "overall",
         atc_name == "PROTEIN KINASE INHIBITORS")
plot_data |>
  # mutate(ingredient_concept_name == tolower(ingredient_concept_name)) |> 
  ggplot() + 
  geom_col(aes(reorder(nice_name, count), 
               count,
               fill = forcats::fct_rev(age_group))) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) + 
  guides(fill = guide_legend(reverse=T)) +
  ggtitle("Use of protein kinase inhibitors drugs by age group")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())
ggsave("protein_kinase_inhibitors_by_age.png", 
       width = 12, height = 18.5)

plot_data <- drug_counts_atc_3 |> 
  filter(age_group == "overall",
         sex != "overall",
         year == "overall",
         atc_name == "PROTEIN KINASE INHIBITORS")
plot_data |>
  # mutate(ingredient_concept_name == tolower(ingredient_concept_name)) |> 
  ggplot() + 
  geom_col(aes(reorder(name, count), 
               count,
               fill = sex)) +
  ggplot2::coord_flip() + 
  theme_bw() +
  xlab("") +
  ylab("Record count") +
  facet_grid(. ~ cdm_name, scales = "free_x") +
  scale_y_continuous(labels = scales::label_comma()) + 
  guides(fill = guide_legend(reverse=T)) +
  ggtitle("Use of protein kinase inhibitors drugs by age group")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())
ggsave("protein_kinase_inhibitors_by_sex.png", 
       width = 12, height = 18.5)


