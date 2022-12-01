The data for this shiny app was downloaded from the County Health Rankings https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation.  The downloaded data was then modified to make it more suitable for the report.  For example, fractions where changed to percentages and remove extra columns not used by the shiny app.  Missing values have also been replaced using MICE

The shiny app can be run using R studio and the shiny library.  After loading the library(shiny), please run runGitHub("data824_final","dghysels") from the R console. 

Note that the dataset is large and the app may take a few moments to load.
