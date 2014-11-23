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
library(ggmap)
library(stats)

graph = startGraph("http://localhost:7474/db/data/")
timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")

# when-do-people-go
# member-heatmaps
# venues-spatial
# social-network