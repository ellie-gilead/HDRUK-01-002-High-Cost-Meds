# renv ----
renv::restore()

# libraries -----
library(omopgenerics)
library(CDMConnector)
library(OmopSketch)
library(PatientProfiles)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(visOmopResults)
library(DBI)
library(dplyr)
library(here)
library(readr)
library(RPostgres)
library(odbc)

# database details -----
db <- dbConnect(
  drv = Redshift(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("HOST"),
  port = Sys.getenv("PORT"),
  password = Sys.getenv("PASSWORD"),
  user = Sys.getenv("USERNAME")
)

database <- "AMB"
cdmSchema <- Sys.getenv(paste0(database,"_SCHEMA"))
writeSchema <- Sys.getenv("WRITE_SCHEMA")
writePrefix <- paste0("high_cost_meds_",database)

cdm <- CDMConnector::cdmFromCon(con = db,
                                cdmSchema = cdmSchema,
                                writeSchema = writeSchema,
                                writePrefix = writePrefix)

# run study -----
# minimum counts that can be displayed according to data governance
min_cell_count <- 5

# Run the study
source(here("run_study.R"))
# after the study is run you should have a single csv with all your results to share
