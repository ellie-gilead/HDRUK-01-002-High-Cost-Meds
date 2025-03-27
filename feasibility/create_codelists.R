

# packages -----
library(omopgenerics)
library(CDMConnector)
library(here)
library(DBI)
library(CodelistGenerator)
library(dplyr)
library(here)

# create codelists
# for each high cost ingredient we will get it and all its descendants
hc_meds <- readr::read_csv(here("hc_meds.csv")) |> 
  filter(is.na(exclusion_reason))

# connect to a db with vocabularies -----
vocab.folder <- "C:/Users/eburn/Documents/vocab_2025"
db <- DBI::dbConnect(duckdb::duckdb(), 
                  paste0(vocab.folder,"/vocab.duckdb"))
cdm <- cdmFromCon(db, "main", "main", cdmName = "vocab") 
# getVocabVersion(cdm) # "v5.0 27-FEB-25"

drug_codes <- getDrugIngredientCodes(cdm, 
                                     hc_meds$concept_id, 
                                     nameStyle = "{concept_code}_{concept_name}")
# note we are using the ohdsi concept names from now on
exportCodelist(drug_codes, 
               path = here("codelists"),
               type = "csv")

# export table with concept code for each med ----- 
readr::write_csv(
  tibble(
    med_names = names(drug_codes)) |> 
    mutate(
      concept_name = stringr::str_replace(names(drug_codes), "^[^_]*_", ""),
      concept_code = stringr::str_extract(med_names, "^[^_]*")) |> 
    mutate(concept_name = stringr::str_to_sentence(concept_name)) |> 
    select(!med_names),   
  here::here("hc_meds_concept_code.csv")
)


# get atc equivalence ------
atc_ref <- list()
for(i in seq_along(names(drug_codes))){
  working_name <- names(drug_codes)[i]
  working_concept_code <- stringr::str_extract(working_name, "^[^_]+")
  cli::cli_inform("Getting atc grouping for {working_name} ({i} of {length(names(drug_codes))})") 
atc_ref[[i]] <- cdm$concept |>
  filter(concept_code == working_concept_code) |>
  select(descendant_concept_id = concept_id) |> 
  left_join(cdm$concept_ancestor, by = "descendant_concept_id") |> 
  inner_join(cdm$concept |> 
               filter(concept_class_id %in%  
                        c("ATC 1st", "ATC 2nd",
                          "ATC 3rd", "ATC 4th",
                          "ATC 5th")) |>
               select(ancestor_concept_id = concept_id),
             by = "ancestor_concept_id") |> 
  select(concept_id = ancestor_concept_id) |> 
  left_join(cdm$concept, by = "concept_id") |> 
  select("atc_name" = "concept_name",
         "concept_class_id",
         "concept_code") |> 
  collect() |> 
  mutate(name = working_name)
}
atc_ref <- bind_rows(atc_ref)

readr::write_csv(atc_ref, "atc_ref.csv")





