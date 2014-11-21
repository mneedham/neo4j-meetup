# Setup commands for Big Data Exchange Talk

library(RNeo4j)
library(ggplot2)
library(seriation)
library(reshape)
library(dplyr)
library(zoo)
library(igraph)
library(geosphere)
library(cluster)
library(gridExtra)

graph = startGraph("http://localhost:7474/db/data/")
timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")

# member-heatmaps
# when-do-people-go
# venues-spatial
# social-network