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

# Show member overlap between the different groups
query = "MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
         OPTIONAL MATCH p = (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)
         WITH group1, group2, COLLECT(p) AS paths
         RETURN group1.name, group2.name, LENGTH(paths) as commonMembers
         ORDER BY group1.name, group2.name"

group_overlap = cypher(graph, query)

ggplot(group_overlap, aes(x=group1.name, y=group2.name, fill=commonMembers)) + 
  geom_bin2d() +
  geom_text(aes(label = commonMembers)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Show member overlap as a percentage

query = "MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
         OPTIONAL MATCH (group1)<-[:MEMBER_OF]-(member)
 
         WITH group1, group2, COLLECT(member) AS group1Members
         WITH group1, group2, group1Members, LENGTH(group1Members) AS numberOfGroup1Members

         UNWIND group1Members AS member
         OPTIONAL MATCH path =  (member)-[:MEMBER_OF]->(group2) 
         WITH group1, group2, COLLECT(path) AS paths, numberOfGroup1Members
         WITH group1, group2, LENGTH(paths) as commonMembers, numberOfGroup1Members
  
         RETURN group1.name, group2.name, toInt(round(100.0 * commonMembers / numberOfGroup1Members)) AS percentage
         ORDER BY  group1.name, group1.name"

group_overlap_percentage = cypher(graph, query)

ggplot(group_overlap_percentage, aes(x=group2.name, y=group1.name, fill=percentage)) + 
  geom_bin2d() +
  geom_text(aes(label = percentage)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

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

# events
query = "MATCH (g:Group)-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-(),
               (event)-[:HELD_AT]->(venue)
         WHERE (event.time + event.utc_offset) < timestamp()
         RETURN g.name, 
                event.time + event.utc_offset AS eventTime,
                event.announced_at AS announcedAt, 
                event.name, 
                COUNT(*) AS rsvps, 
                venue.name AS venue, venue.lat AS lat, venue.lon AS lon"

events = cypher(graph, query)

events$eventTime <- timestampToDate(events$eventTime)
events$time = format(events$eventTime, "%H:%M")
events$day <- format(events$eventTime, "%A")
events$monthYear <- format(events$eventTime, "%m-%Y")
events$month <- format(events$eventTime, "%m")
events$year <- format(events$eventTime, "%Y")
events$announcedAt<- timestampToDate(events$announcedAt)
events$timeDiff = as.numeric(events$eventTime - events$announcedAt, units = "days")

byVenue = events %>% 
  count(lat, lon, venue) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  dplyr::rename(count = n)

map = get_map(location = 'London', zoom = 12)
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = byVenue,
             col = "red",
             alpha = 0.8)

library(geosphere)
library(cluster)

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

clusteramounts = 40
distance.matrix = (distm(byVenue[,c("lon","lat")]))
clustersx <- as.hclust(agnes(distance.matrix, diss = T))
byVenue$group <- cutree(clustersx, k=clusteramounts)
byVenue %>% arrange(group) %>% head(50)


?rename

?select

events %>% 
  group_by(venue) %>%
  dplyr::summarise(events = n()) %>%
  arrange(desc(events))

events %>% 
  group_by(venue) %>%
  dplyr::summarise(events = n()) %>%
  arrange(desc(events))


events %>% 
  group_by(month) %>%
  dplyr::summarise(events = n(), 
                   count = sum(rsvps), 
                   max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))

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

query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event),
               (event)<-[:TO]-({response:'yes'})<-[:RSVPD]-(other)
         WHERE ID(p) < ID(other)
         RETURN p.name, other.name, COUNT(*) AS times"

data = cypher(graph, query)

data %>% arrange(desc(times)) %>% head(5)

# betweenness centrality
options("scipen"=100, "digits"=4)
g = graph.data.frame(data, directed = F)

sort(betweenness(g), decreasing = T)[1:5]
sort(betweenness.estimate(g, cutoff=2), decreasing = T) %>% head(5)

# page rank
?page.rank
pr = page.rank(g)$vector
prDf = data.frame(name = names(pr), rank = pr)
head(prDf)

data.frame(prDf) %>%
  arrange(desc(pr)) %>%
  head(10)