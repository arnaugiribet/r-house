# R-House

Repo for a real estate investing platform.


See it in action here:

[ShinyApp - Rentabilidad](https://agiribet.shinyapps.io/rentabilidad/)

## Getting Started

### Dependencies

* R version 4.2.2
* Python version 3.8.12

### Files

* **01-data_download/**
* * **00.api_get_data.py** Python code that downloads data and stores in **output** folders
* * **01.process_data.py** Python code that merges downloaded data and exports it in .csv
* * **02.keepActiveUrls.R** R code that checks which urls in the dataset are still active
* * **03.ROIcalculations.R** R code that calculates ROI of sale properties given rent and sale parameters
* * **dades.RData** R file that contains sale and rent data (from both the .csv files)
* * **idealista_properties.csv** Exported sale data
* * **idealista_properties.csv** Exported rent data
* * **oauth2-documentation.pdf** Info on how to query the Idealista API
* * **property-search-api-v3_5.pdf** Examples of queries to the Idealista API
* **rsconnect/** Contains info about the deployed shiny app
* **readme_shiny** Contains extra info to be deployed on the shiny app
* **server.r** R file with server data
* **ui.r** R file with ui data
* **r-house.Rproj** R project file
* **.gitignore** list of files not to be tracked by Git when performing changes
* **.idea/** contains PyCharms project configuration files


## Authors

Arnau Giribet March

[LinkedIn](https://www.linkedin.com/in/arnau-giribet/)

## Version History

* 0.1
    * Initial Release
