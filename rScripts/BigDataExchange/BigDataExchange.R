# Exploring London's NoSQL meetups using R 

library(RNeo4j)
library(ggplot2)
library(seriation)
library(dplyr)
library(zoo)
library(reshape)
library(igraph)

timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")
graph = startGraph("http://localhost:7474/db/data/")



# When did people join the group?
query = "MATCH (:Person)-[:HAS_MEETUP_PROFILE]->()-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(g:Group {name: \"Neo4j - London User Group\"})
         RETURN membership.joined AS joinTimestamp"

meetupMembers = cypher(graph, query)
meetupMembers$joinDate <- timestampToDate(meetupMembers$joinTimestamp)
meetupMembers$dayMonthYear <- as.Date(meetupMembers$joinDate)
meetupMembers$monthYear <- as.Date(as.yearmon(meetupMembers$joinDate))
meetupMembers$quarterYear <- as.Date(as.yearqtr(meetupMembers$joinDate))
meetupMembers$week <- as.Date("1970-01-01")+7*trunc(as.numeric(meetupMembers$joinDate)/(3600*24*7))

groupBy = function(members, field) {
  members %>% group_by_(field) %>% dplyr::summarise(n = n())
}

meetupMembers %>% group_by(week) %>% dplyr::summarise(n = n())

byWeek = groupBy(meetupMembers, 'week')
joinsByWeek = data.frame(actual = byWeek$n, 
                         week = byWeek$week,
                         rolling = rollmean(byWeek$n, 4, fill = NA, align=c("right")))

head(joinsByWeek, 10)
meltedJoinsByWeek = melt(joinsByWeek, id = 'week')
head(meltedJoinsByWeek, 10)

# Show the rolling number of people joining the meetup group
ggplot(meltedJoinsByWeek, aes(x = week, y = value, colour = variable)) + 
  geom_line() + 
  ylab(label="Number of new members") + 
  xlab("Week Number") + 
  scale_colour_manual(values=c("grey", "blue"))

# show the cumulative members over time
ggplot(data = meetupMembers %>% group_by(dayMonthYear) %>% dplyr::summarise(n = n()) %>% mutate(n = cumsum(n)), 
       aes(x = dayMonthYear, y = n)) + 
  ylab("Number of members") +
  xlab("Date") +
  geom_line()


byVenue[1,]
byVenue %>% filter(row_number() == 1)

distToVenues = function(venue, otherVenues) {  
  dist = by(otherVenues, 1:nrow(otherVenues), function(row) {
    distHaversine(c(venue$lon,venue$lat),c(row$lon,row$lat))
  }) %>% cbind() %>% as.vector()
  otherVenues %>% mutate(dist = dist)
}

distToVenues(byVenue %>% slice(1), byVenue) %>% filter(dist < 100)
distToVenues(byVenue %>% slice(2), byVenue) %>% filter(dist < 100)

distHaversine(c(51.52482,-0.099109),c(51.52451,-0.099152))
?distHaversine


events %>% 
  group_by(venue) %>%
  dplyr::summarise(events = n()) %>%
  arrange(desc(events))

events %>% 
  group_by(venue) %>%
  dplyr::summarise(events = n()) %>%
  arrange(desc(events))


# chart showing how many people attend by month


# histogram showing the number of attendees - like what Martin did with goals scored

# RSVPS
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

head(allRSVPs, 10)

ggplot(allRSVPs, aes(x = difference, fill=response)) + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  geom_bar(binwidth=1) +
  facet_wrap(~ response, nrow=2, ncol=1)

# regression model - simple prediction of attendees based on meetup /day?

# who's at the centre of the London NoSQL meetup scene?