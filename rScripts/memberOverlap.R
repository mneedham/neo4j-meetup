install.packages("devtools")
devtools::install_github("nicolewhite/Rneo4j")
install.packages("ggplot2")
install.packages("seriation")
install.packages('plyr')

library(Rneo4j)
library(ggplot2)
library(seriation)
library(plyr)

graph = startGraph("http://localhost:7474/db/data/")

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

// as percentage

query = "MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
	       OPTIONAL MATCH path = (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)

         WITH group1, group2, COLLECT(path) AS paths

		     WITH group1, group2, LENGTH(paths) as commonMembers
  	     MATCH (group1)<-[:MEMBER_OF]-(group1Member)

  	     WITH group1, group2, commonMembers, COLLECT(id(group1Member)) AS group1Members
  	     MATCH (group2)<-[:MEMBER_OF]-(group2Member)

  	     WITH group1, group2, commonMembers, group1Members, COLLECT(id(group2Member)) AS group2Members
  	     WITH group1, group2, commonMembers, group1Members, group2Members

  	     UNWIND(group1Members + group2Members) AS combinedMember
  	     WITH DISTINCT group1, group2, commonMembers, combinedMember

  	     WITH group1, group2, commonMembers, COUNT(combinedMember) AS combinedMembers

  	     RETURN group1.name, group2.name, toInt(round(100.0 * commonMembers / combinedMembers)) AS percentage

  	     ORDER BY group1.name, group1.name"

group_overlap = cypher(graph, query)

ggplot(group_overlap, aes(x=group1.name, y=group2.name, fill=percentage)) + 
  geom_bin2d() +
  geom_text(aes(label = percentage)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

query = "match (:Person)-[:HAS_MEETUP_PROFILE]->()-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(g:Group {name: \"Neo4j - London User Group\"})
RETURN membership.joined AS joinDate"

timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01")

meetupMembers = cypher(graph, query)
meetupMembers$joined <- timestampToDate(meetupMembers$joinDate)
meetupMembers$monthYear <- format(meetupMembers$joined, "%m-%Y")

groupBy = function(dates, format) {
  dd = aggregate(dates, by= list(format(dates, format)), function(x) length(x))
  colnames(dd) = c("key", "count")
  dd
}

groupBy(meetupMembers$joined, "%m-%Y")
groupBy(meetupMembers$joined, "%Y")
groupBy(meetupMembers$joined, "%A")

byDayTime = groupBy(meetupMembers$joined, "%A %H:00")
byDayTime[order(-byDayTime$count),][1:10,]

# all events
query = "MATCH (g:Group {name: \"Neo4j - London User Group\"})-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         WHERE (event.time + event.utc_offset) < timestamp()
         RETURN event.time + event.utc_offset AS eventTime, event.name, COUNT(*) AS rsvps"
events = cypher(graph, query)
events$datetime <- timestampToDate(events$eventTime)
events$day <- format(events$datetime, "%A")
events$monthYear <- format(events$datetime, "%m-%Y")

# events with most rsvps
ddply(events, .(rsvps = rsvps* -1))[1:10, ]

# events held on a Thursday 
thursdayEvents = events[events$day == "Thursday", ]
handsOnCypher = events[events["event.name"] == "Hands On Intro to Cypher - Neo4j's Query Language", ]
introToGraphs = events[events["event.name"] == "Intro to Graphs", ]
handsOnApp = events[events["event.name"] == "Hands on build your first Neo4j app for Java developers",]

# number of events + rsvps by day
aggregateEvents <- function(events) {
  ddply(ddply(events, .(dayOfWeek=format(datetime, "%A")), function(x) {
    events <- length(x$datetime)
    rsvps <- sum(x$rsvps)
    rsvpsPerEvent <- rsvps / events
    data.frame(events = events, rsvps = rsvps, rsvpsPerEvent = rsvpsPerEvent)
  }), .(events = events* -1))
}

aggregateEvents(events)
aggregateEvents(introToGraphs)
aggregateEvents(handsOnCypher)
aggregateEvents(handsOnApp)

allTheEvents = rbind(introToGraphs, handsOnCypher, handsOnApp)

ddply(allTheEvents, "event.name", function(x) {
  count <- length(x$datetime)
  rsvps <- sum(x$rsvps)
  rsvpsPerEvent <- rsvps / count
  data.frame(count = count, rsvps = rsvps, rsvpsPerEvent = rsvpsPerEvent)  
})

ddply(allTheEvents, "event.name", summarise, 
     count = length(datetime), 
     rsvps = sum(rsvps), 
     days = paste(unique(format(datetime, "%A")), collapse = ","),
     rsvpsPerEvent = rsvps / count)

allTheEvents$day = format(allTheEvents$datetime, "%A")
allTheEvents$year = format(allTheEvents$datetime, "%Y")

ddply(ddply(allTheEvents, c("event.name", "day"), summarise, 
      count = length(datetime), 
      rsvps = sum(rsvps), 
      days = paste(unique(format(datetime, "%A")), collapse = ","),
      rsvpsPerEvent = rsvps / count), .(rsvpsPerEvent = rsvpsPerEvent * -1))

ddply(allTheEvents[format(allTheEvents$datetime, "%Y") == 2014,], "event.name", summarise, 
      days = paste(unique(format(datetime, "%A")), collapse = ","),
      count = length(datetime), 
      rsvps = sum(rsvps), 
      rsvpsPerEvent = rsvps / count)

ddply(allTheEvents[format(allTheEvents$datetime, "%Y") == 2014,], .(day=format(datetime, "%A")), summarise, 
      count=length(datetime),
      rsvps=sum(rsvps),
      rsvpsPerEvent = rsvps / count)

eventsOf2014 = allTheEvents[format(allTheEvents$datetime, "%Y") == 2014,]

ddply(eventsOf2014, .(day=format(datetime, "%A")), summarise, 
      count=length(datetime),
      rsvps=sum(rsvps),
      rsvpsPerEvent = rsvps / count)

ddply(eventsOf2014, .(day=format(datetime, "%A")), summarise, 
      events = paste(unique(event.name), collapse = ","),
      count=length(datetime),
      rsvps=sum(rsvps),
      rsvpsPerEvent = rsvps / count)

ddply(allTheEvents, 
      c("day=format(datetime, \"%A\")", 
        "event.name"), 
      summarise, 
      count = length(day),
      rsvps = sum(rsvps))

# events by month with average RSVPs
ddply(ddply(eventsOf2014, 
      c("event.name", month="format(datetime, \"%B\")",
      monthNumber="format(datetime, \"%m\")"),
      summarise, 
      numberOfEvents = length(day),
      rsvps = sum(rsvps),
      rsvpsPerEvent = rsvps / numberOfEvents), "monthNumber")[,c("event.name","month","numberOfEvents", "rsvps", "rsvpsPerEvent")]

ddply(ddply(allTheEvents, c("event.name", year="format(datetime, \"%Y\")"), summarise, 
      count = length(datetime), rsvps = sum(rsvps), rsvpsPerEvent = rsvps / count), c("event.name", "year"))

ddply(ddply(allTheEvents, .(event.name, year=format(datetime, "%Y")), summarise, 
            count = length(datetime), rsvps = sum(rsvps), rsvpsPerEvent = rsvps / count), c("event.name", "year"))

ddply(allTheEvents, c("event.name", "year"), summarise, 
            count = length(datetime), rsvps = sum(rsvps), rsvpsPerEvent = rsvps / count)

#hopeless error message for this
ddply(allTheEvents, "event.name", function(x) {
  countr <- length(x$datetime)
  data.frame(count = count)  
})

# events + members

eventsByMonthYear = ddply(events, c("monthYear"), summarise, events = length(datetime), rsvps = sum(rsvps))
View(eventsByMonthYear)

memberJoinDatesByMonthYear = ddply(meetupMembers, c("monthYear"), summarise, members = length(joined))
View(memberJoinDatesByMonthYear)

membersEventsByMonthYear = merge(eventsByMonthYear, memberJoinDatesByMonthYear, by = "monthYear", all = TRUE)
membersEventsByMonthYear[is.na(membersEventsByMonthYear)] <- 0
View(membersEventsByMonthYear)

ggplot(membersEventsByMonthYear, aes(x=events, y=members)) + geom_point(shape=1)
ggplot(membersEventsByMonthYear, aes(x=events, y=rsvps)) + geom_point(shape=1)
ggplot(membersEventsByMonthYear, aes(x=rsvps, y=members)) + geom_point(shape=1)

ggplot(membersEventsByMonthYear, aes(x=events, y=members)) + geom_point(shape=1) + geom_smooth()

ggplot(membersEventsByMonthYear, aes(x=events, y=members)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region

library(HMisc)
rcorr(as.matrix(membersEventsByMonthYear), type="pearson") 
rcorr(as.matrix(membersEventsByMonthYear[, 2:4]), type="pearson")

cor.test(membersEventsByMonthYear$events, membersEventsByMonthYear$members, method="pearson", conf.level=0.99)

#groupBy(events$datetime, "%A")
#dd = aggregate(events$rsvps, by= list(format(events$datetime, "%A")), FUN=sum)
#colnames(dd) = c("key", "count")
#dd = dd[order(-dd$count),]
#x = merge(groupBy(events$datetime, "%A"), dd, by = "key")
#colnames(x) = c("day", "events", "rsvps")

# getting my plyr on

set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 3), count = round(runif(9, 0, 20)))

ddply(d, "year", function(x) {
  sum <- sum(x$count)
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count/mean.count
  data.frame(cv.count = cv, mean = mean.count, sd = sd.count, sum = sum)
})

# now with our data

ddply(ddply(meetupMembers, .(dayOfWeek=format(joined, "%A")), function(x) {
  count <- length(x$joined)
  data.frame(count = count)
}), .(count = count* -1))


# analyse RSVPs

query = "MATCH (e:Event {id: {eventId}}) 
         RETURN e.name, e.time + e.utc_offset AS eventTime"
rsvps = cypher(graph, query, eventId = "187178902")

rsvps$date = timestampToDate(rsvps$eventTime)