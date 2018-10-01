# ECA&D Stations Browser
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This application gives the possibility to explore the impressive amount of weather stations data available through the [ECAD (European Climate Assessment & Dataset) project](https://www.ecad.eu/).

The application, based on [Shiny](https://shiny.rstudio.com/), is accessible at the following URL: [https://mdefelice.shinyapps.io/ECAD-data-browser/](https://mdefelice.shinyapps.io/ECAD-data-browser/).

This repository includes also a R script (`create_database.R`) that creates a data frame with all the stations metadata starting from the Source and Stations files available on the [project's website](https://www.ecad.eu//dailydata/predefinedseries.php). To create the data frame, follow these steps:

1. Put all the needed txt files in the folder `data` (the directory contains a `README` describing where to download the files)
2. Run the script (you need R with `tidyverse` and `lubridate` packages)

At the end, you will have the data structure stored in a serialised R object that can be visualised through the Shiny application (the files can be loaded with the `base::readRDS` and `readr::read_rds` functions).

### License
All the code here is under license [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). 

### Author
Matteo De Felice ([web page](http://matteodefelice.name), [twitter](https://twitter.com/matteodefelice))


