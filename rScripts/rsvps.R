library(RNeo4j)
library(ggplot2)
library(gridExtra)

query = "MATCH (e:Event)<-[:TO]-(response {response: 'yes'})
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
allYesRSVPs = cypher(graph, query)
allYesRSVPs$time = timestampToDate(allYesRSVPs$time)
allYesRSVPs$eventTime = timestampToDate(allYesRSVPs$eventTime)

allYesRSVPs$difference = allYesRSVPs$eventTime - allYesRSVPs$time
allYesRSVPs$difference = as.numeric(allYesRSVPs$eventTime - allYesRSVPs$time, units="days")
allYesRSVPs$answer = "yes"

query = "MATCH (e:Event)<-[:TO]-(response {response: 'no'})<-[:NEXT]-()
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
allNoRSVPs = cypher(graph, query)
allNoRSVPs$time = timestampToDate(allNoRSVPs$time)
allNoRSVPs$eventTime = timestampToDate(allNoRSVPs$eventTime)
allNoRSVPs$difference = as.numeric(allNoRSVPs$eventTime - allNoRSVPs$time, units="days")
allNoRSVPs$answer = "no"

yes = ggplot(allYesRSVPs, aes(x=difference)) + 
  geom_histogram(binwidth=1, fill="green") +
  xlim(0,75) + 
  ylim(0, 400)
  
no = ggplot(allNoRSVPs, aes(x=difference)) +
  geom_histogram(binwidth=1, fill="red") +
  xlim(0,75) + 
  ylim(0, 400) +
  scale_y_reverse()

grid.arrange(yes,no,ncol=1,widths=c(1,1))