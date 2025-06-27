

# packages -----
library(omopgenerics)
library(CDMConnector)
library(here)
library(DBI)
library(CodelistGenerator)
library(dplyr)
library(here)

# create codelists -----
# for each high cost ingredient we will get it and all its descendants
hc_meds <- readr::read_csv(here("hc_meds.csv")) |> 
  filter(is.na(exclusion_reason))

# connect to a db with vocabularies -----
db <- DBI::dbConnect(odbc::odbc(),
                     Driver = "ODBC Driver 18 for SQL Server",
                     Server = "...",
                     Database = "...",
                     UID = "...",
                     PWD = "...!",
                     TrustServerCertificate = "yes",
                     Port = "...")
cdm <- CDMConnector::cdmFromCon(con = db,
                                cdmSchema = c("vocabularies", "vocab_2025_02"),
                                writeSchema = c("vocabularies", "results"),
                                cdmName = "vocab_2025_02")
# getVocabVersion(cdm) # "v5.0 27-FEB-25"

drug_codes <- getDrugIngredientCodes(cdm, 
                                     hc_meds$concept_id, 
                                     nameStyle = "{concept_id}_{concept_name}")
drug_codes <- subsetOnDomain(drug_codes, cdm = cdm, domain = "drug")

# export table with concept code for each med ----- 
readr::write_csv(
  tibble(
    med_names = names(drug_codes)) |> 
    mutate(
      concept_name = stringr::str_replace(names(drug_codes), "^[^_]*_", ""),
      concept_id = stringr::str_extract(med_names, "^[^_]*"))  |> 
    mutate(concept_name = stringr:::str_replace_all(concept_name, "_", " ")) |> 
    mutate(concept_name = CodelistGenerator:::tidyWords(concept_name)) |> 
    mutate(concept_name = stringr::str_to_sentence(concept_name)) |> 
    select(!med_names),   
  here::here("hc_meds_concept_code.csv")
)

# note we are using the ohdsi concept names from now on
names(drug_codes) <- stringr::str_replace(names(drug_codes), "^[^_]*_", "")
exportCodelist(drug_codes, 
               path = here("drug_codelists"),
               type = "csv")


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






# icd codes ----

icd_subchapter <- getICD10StandardCodes(cdm,
                      level = c("ICD10 SubChapter"),
                      nameStyle = "{concept_code}_{concept_name}")
icd_subchapter <- icd_subchapter[stringr::str_starts(names(icd_subchapter), 
                                   "a15|b20|c15|c64|c73|d55|d65|n17|q20")]

icd_hierarchy <- getICD10StandardCodes(cdm,
                                        level = c("ICD10 Hierarchy"),
                                        nameStyle = "{concept_code}_{concept_name}")
icd_hierarchy <- icd_hierarchy[stringr::str_starts(names(icd_hierarchy), 
                                    "a41|b15|b16|b25|c34|c40|c45|c49|c50|c53|c54|c56|c61|c71|c81|c82|c83|c91|c92|d70|e21|e22|e83|e84|g20|g35|i63|j12|j18|j22|j45|j47|k21|k50|k51|l03|l97|m06|m32|m81|O00|q21|r11|r50|r54|t86")]

icd_code <- getICD10StandardCodes(cdm,
                                  level = c("ICD10 Code"),
                                  nameStyle = "{concept_code}_{concept_name}")
icd_code <- icd_code[stringr::str_starts(names(icd_code), 
                                      "a047|a099|c900|d570|e870|e871|e872|e873|e875|e876|e877|g710|g936|j440|j960|j981|n390|i270|i517|q899|u071|z940")]

# add specific
icd_subchapter$b20_b24_human_immunodeficiency_virus_hiv_disease <- getDescendants(cdm, 439727) |> 
  select(concept_id) |> distinct() |> pull()

icd_code$u071_emergency_use_of_u071__covid_19_virus_identified
names(icd_code) <- stringr::str_replace(names(icd_code),
            "u071_emergency_use_of_u071__covid_19_virus_identified",
            "u071_covid_19_virus_identified")
icd_code$u071_covid_19_virus_identified <- c(icd_code$u071_covid_19_virus_identified,
  getDescendants(cdm, 4100065) |> 
  select(concept_id) |> pull()) |> unique()

icd <- bind(icd_subchapter,
            icd_hierarchy,
            icd_code)
icd <- subsetOnDomain(icd, cdm = cdm, domain = "condition")
omopgenerics::exportCodelist(icd, 
                             path = here::here("icd"), 
                             type = "csv")

# procedures ----
procedures <- list(chemotherapy = getDescendants(cdm, 4273629) |> 
  select(concept_id) |> distinct() |> pull(),
  blood_transfusion = getDescendants(cdm, 4024656) |> 
    select(concept_id) |> distinct() |> pull(),
  hemodialysis = getDescendants(cdm, 4120120) |> 
    select(concept_id) |> distinct() |> pull(),
  pacemaker_implantation = getDescendants(cdm, 4144921) |> 
    select(concept_id) |> distinct() |> pull(),
  stem_cell_transplant = getDescendants(cdm, 37154552) |> 
    select(concept_id) |> distinct() |> pull(),
  transplant_of_kidney = getDescendants(cdm, 4322471) |> 
    select(concept_id) |> distinct() |> pull())
procedures <- subsetOnDomain(procedures, cdm = cdm, domain = "procedure")
omopgenerics::exportCodelist(procedures, 
                             path = here::here("procedures"), 
                             type = "csv")








