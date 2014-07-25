library(RNeo4j)
library(ggplot2)

timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")

query = "MATCH (e:Event)<-[:TO]-(response {response: 'yes'})
         WITH e, COLLECT(response) AS yeses
         MATCH (e)<-[:TO]-(response {response: 'no'})<-[:NEXT]-()
         WITH e, COLLECT(response) + yeses AS responses
         UNWIND responses AS response
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime, response.response AS response"

allRSVPs = cypher(graph, query)
allRSVPs$time = timestampToDate(allRSVPs$time)
allRSVPs$eventTime = timestampToDate(allRSVPs$eventTime)
allRSVPs$difference = as.numeric(allRSVPs$eventTime - allRSVPs$time, units="days")
allRSVPs$eventTime = format(allRSVPs$eventTime, "%A")

ggplot(allRSVPs, aes(x = difference, fill=response)) + 
  geom_bar(binwidth=1) + 
  facet_wrap(~ response, nrow=2, ncol=1)

ggplot(allRSVPs, aes(x = difference, fill=response)) + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  geom_bar(binwidth=1) +
  facet_wrap(~ response, nrow=2, ncol=1, as.table = FALSE)

ggplot(allRSVPs, aes(x = difference, fill=response)) + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  geom_bar(binwidth=1) +
  facet_wrap(~ response + dayOfWeek, as.table = FALSE)