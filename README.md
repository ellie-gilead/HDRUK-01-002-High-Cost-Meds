## Instructions to run the study code
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 
2) Open the project <i>study.Rproj</i> from the study_code directory in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3) Open the code_to_run.R file - this is the only file you should need to interact with. 
- Install the required packages using renv::restore() and then load these libraries
- Add your database specific parameters (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema).
- Create a cdm using CDMConnector (see https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html for connection examples for different dbms). 
- Run the last line of code_to_run.R to run the analysis.

## Instructions to review your study results
1) After running the study code, you should CSV with your results created in the study_code/results directory. Copy this csv to the study_shiny/data directory
2) Open the project <i>shiny.Rproj</i> from the shiny directory in RStudio
3) Open global.R and click the "Run App" button
4) A shiny app with your results should then be launched
