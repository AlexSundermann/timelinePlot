# install.packages("devtools")
library("devtools")
# devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("/Users/Nick/dev/timelineViz")
document()

setwd("..")
install("timelineViz")



library(timelineViz)
data <- read.csv("/Users/Nick/dev/timelineViz/data/GA_Data.csv", stringsAsFactors = F)

timelinePlot(data, event_vars = c("gestage", "congestftlmp", "congest6wklmp", "SABlmp","lblmp"),time_interval = "day")

devtools::install_github("nstrayer/timelinePlot")
