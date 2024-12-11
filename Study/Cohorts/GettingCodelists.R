library(readxl)
library(dplyr)
library(CohortConstructor)
library(DBI)
library(RPostgres)
library(CodelistGenerator)
library(here)
library(readxl)

# postgres database connection details
serverDbi <- Sys.getenv("DB_SERVER_cdm_gold_202307_dbi")
user <- Sys.getenv("USER")
password <- Sys.getenv("PASSWORD")
port <- Sys.getenv("PORT")
host <- Sys.getenv("HOST")

db <- dbConnect(RPostgres::Postgres(),
                dbname = serverDbi,
                port = port,
                host = host,
                user = user,
                password = password
)

# name of vocabulary schema
vocabularyDatabaseSchema <- "vocabulary"

cdm_schema <- "public_100k"
write_schema <- "results"
study_prefix <- "amr_hdruk"

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_schema,
  write_schema = c(schema = write_schema,
                   prefix = study_prefix),
  cdm_name = serverDbi)

hcm_summary <- read_excel("Cohorts/high_cost_meds_summary.xlsx", 
                                            sheet = "high cost meds", skip = 5)

colnames(hcm_summary) <- sapply(colnames(hcm_summary), function(x) {
  new_name <- gsub(" ", "_", x)  # Replace spaces with underscores
  tolower(new_name)
})

drug_name <- hcm_summary %>%
  pull(drug_name) %>%
  tolower() %>%
  unique()

 hcm_codes <- getDrugIngredientCodes(
   cdm = cdm,
   name = drug_name,
   type = "codelist"
 )
 
codes_list <- list()

for (name in names(hcm_codes)) {
  codes <- hcm_codes[[name]]
  codes_list[[name]] <- data.frame(
    Codelist = name,         
    Code = codes                      
  )
}

# Combine the list of data frames into one large data frame
all_codes_df <- do.call(rbind, codes_list)

rownames(all_codes_df) <- NULL

all_codes_df_grouped <- all_codes_df %>%
  group_by(Codelist) %>%
  summarise(Codes = list(Code),
            code_count = n(),
            .groups = "drop")

# Convert Codes to a comma-separated string
all_codes_df_grouped$Codes <- sapply(all_codes_df_grouped$Codes, function(x) paste(x, collapse = ", "))

# Now write to CSV
write.csv(all_codes_df_grouped, file = here("Cohorts", "codelist_summary.csv"), row.names = FALSE)


write.csv(all_codes_df_grouped, file = here("Cohorts", "codelist_summary.csv"))

#######

cdm$hcm <- conceptCohort(cdm = cdm,
                         conceptSet = hcm_codes$concept_id,
                         name = "hcm")

hcm_counts <- merge(cohortCount(cdm$hcm), settings(cdm$hcm), by = "cohort_definition_id")

write.csv(hcm_counts, here("Cohorts", "hcm_counts.csv"))
