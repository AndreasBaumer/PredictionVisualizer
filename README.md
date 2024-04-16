You need the following 5 libraries.

-MASS
-pROC
-ggpubr
-shiny
-shinydashboard

You launch the App with this code (you need to call "library(shiny)" first:
library(shiny)
runApp("path/to/the/app/Prediction Metrics.R")


Warning: The libraries take a long time to install on linux, because they contain a bunch of Fortran code which takes a while to compile.

Should this not work for you, there is an online (slightly less nice) version available at: https://abaumer.shinyapps.io/PredictionPerformanceVisualizer/
