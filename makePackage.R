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


#Full data with still births
time_data <- data %>%
  select(id, enroll = gestage, `6wk` = congest6wklmp, ft = congestftlmp, SAB = SABlmp, `Live Birth` = lblmp, Stillbirth = sblmp) %>%
  melt(id = "id") %>%
  na.omit()

timelinePlot(time_data)

devtools::install_github("nstrayer/timelinePlot")
