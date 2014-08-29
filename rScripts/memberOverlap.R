install.packages("devtools")
devtools::install_github("nicolewhite/Rneo4j")
install.packages("ggplot2")
install.packages("seriation")
install.packages('plyr')

library(RNeo4j)
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
RETURN membership.joined AS joinTimestamp"

timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")

meetupMembers = cypher(graph, query)
meetupMembers$joinDate <- timestampToDate(meetupMembers$joinTimestamp)
meetupMembers$dayMonthYear <- as.Date(meetupMembers$joinDate)

library(zoo)

# format(meetupMembers$joinDate, "%m-%Y")

meetupMembers$monthYear <- as.Date(as.yearmon(meetupMembers$joinDate))
meetupMembers$quarterYear <- as.Date(as.yearqtr(meetupMembers$joinDate))
meetupMembers$week <- as.Date("1970-01-01")+7*trunc(as.numeric(meetupMembers$joinDate)/(3600*24*7))

groupBy = function(dates, format) {
  dd = aggregate(dates, by= list(format(dates, format)), function(x) length(x))
  colnames(dd) = c("key", "count")
  dd
}

groupBy(meetupMembers$joinDate, "%m-%Y")
groupBy(meetupMembers$joinDate, "%Y")
groupBy(meetupMembers$joinDate, "%A")

byDayTime = groupBy(meetupMembers$joinDate, "%A %H:00")
byDayTime[order(-byDayTime$count),][1:10,]

library(dplyr)

byDay = meetupMembers %.% 
  group_by(dayMonthYear) %.%
  summarise(n = n())

plotByDayMonthYear = ggplot(data = meetupMembers %.% 
                              group_by(dayMonthYear) %.% 
                              summarise(n = n()), 
                            aes(x = dayMonthYear, y = n)) + 
  ylab("Number of members") +
  xlab("Date") +
  geom_line(aes(y = cumsum(n)))

groupMembersBy = function(field) {
  meetupMembers %.% regroup(list(field)) %.% summarise(n = n())
}

groupBy = function(field) {
  meetupMembers %.% group_by(field) %.% summarise(n = n())
}

ggplot(data = meetupMembers %.% 
         group_by(monthYear) %.%
         summarise(n = n()), 
       aes(x = monthYear, y = n)) + 
  ylab("Number of members") +
  xlab("Date") +
  geom_line(aes(y = cumsum(n))) +
  geom_line(color="blue", type = 'closed')

plotByMonthYear = ggplot(data = meetupMembers %.% 
         group_by(monthYear) %.%
         summarise(n = n()), 
       aes(x = monthYear, y = n)) + 
  ylab("Number of members") +
  xlab("Date") +
  geom_line(aes(y = cumsum(n)))

ggplot(data = meetupMembers %.% 
         group_by(quarterYear) %.%
         summarise(n = n()), 
       aes(x = quarterYear, y = n)) + 
  ylab("Number of new members") +
  xlab("Date") +
  geom_bar(stat="identity", fill = "blue")

ggplot(data = meetupMembers %.% 
         group_by(monthYear) %.%
         summarise(n = n()), 
       aes(x = monthYear, y = n)) + 
  ylab("Number of new members") +
  xlab("Date") +
  geom_bar(stat="identity")

            
ggplot(data = meetupMembers %.% 
         group_by(week) %.%
         summarise(n = n()), 
       aes(x = week, y = n)) + 
  ylab("Number of new members") +
  xlab("Date") +
  geom_bar(stat="identity")

ggplot(data = meetupMembers %.% 
         group_by(dayMonthYear) %.%
         summarise(n = n()), 
       aes(x = dayMonthYear, y = n)) + 
  ylab("Number of new members") +
  xlab("Date") +
  geom_bar(stat="identity")

summary(meetupMembers[format(meetupMembers$dayMonthYear, "%Y") == 2014,] %.% 
        group_by(dayMonthYear) %.%
        summarise(n = n()))

summary(meetupMembers[format(meetupMembers$dayMonthYear, "%Y") == 2013,] %.% 
          group_by(dayMonthYear) %.%
          summarise(n = n()))

summary(meetupMembers[format(meetupMembers$dayMonthYear, "%Y") == 2012,] %.% 
          group_by(dayMonthYear) %.%
          summarise(n = n()))

summary(meetupMembers[format(meetupMembers$dayMonthYear, "%Y") == 2011,] %.% 
          group_by(dayMonthYear) %.%
          summarise(n = n()))


meetupMembers %.% 
  group_by(quarterYear) %.%
  summarise(n = n())

grid.arrange(plotByDayMonthYear, plotByMonthYear, ncol=1, widths=c(1,1))

# rolling functions
smoothIndex = rollmean(x = ftseIndex,  # original series
         k = 30,  # width of the rolling window
         fill = NA) 
lines(smoothIndex, col = "RED")

ftseIndex <- EuStockMarkets[, 4]
plot(ftseIndex, col = "GRAY")

byDayCumSum = meetupMembers %.% 
  group_by(dayMonthYear) %.% 
  summarise(n = n()) %.%
  mutate(n = cumsum(n))


byDay = meetupMembers %.% 
  group_by(dayMonthYear) %.% 
  summarise(n = n())

smoothByDay = rollmean(x = byDay$n, k = 30, fill = NA)

plot(smoothByDay, col = "GRAY")
            

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

ddply(introToGraphs[format(introToGraphs$datetime, "%Y") == 2014,], .(day=format(datetime, "%A")), summarise, 
      count=length(datetime),
      rsvps=sum(rsvps),
      rsvpsPerEvent = rsvps / count)

ddply(introToGraphs, .(day=format(datetime, "%Y")), summarise, 
      count=length(datetime),
      rsvps=sum(rsvps),
      rsvpsPerEvent = rsvps / count)

ddply(events, .(day=format(datetime, "%Y")), summarise, 
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

ddply(eventsOf2014, .(day=format(datetime, "%A"), event.name), summarise, 
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

ddply(ddply(allTheEvents, .(event.name, year= format(datetime, "%Y")), summarise, 
            count = length(datetime), rsvps = sum(rsvps), rsvpsPerEvent = rsvps / count), c("event.name", "year"))



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
events = cypher(graph, query, eventId = "187178902")

events$eventTime = timestampToDate(events$eventTime)

query = "MATCH (e:Event {id: {eventId}})<-[:TO]-(response {response: 'yes'})
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
yesRSVPs = cypher(graph, query, eventId = "187178902")
yesRSVPs$time = timestampToDate(yesRSVPs$time)
yesRSVPs$eventTime = timestampToDate(yesRSVPs$eventTime)

yesRSVPs$difference = yesRSVPs$eventTime - yesRSVPs$time
yesRSVPs$difference = as.numeric(yesRSVPs$eventTime - yesRSVPs$time, units="days")

ggplot(yesRSVPs, aes(x=difference)) + geom_histogram(binwidth=1, colour="grey", fill="green")

#ggplot(yesRSVPs, aes(x=difference)) + geom_histogram(binwidth=.5)

query = "MATCH (e:Event)<-[:TO]-(response {response: 'yes'})
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
allYesRSVPs = cypher(graph, query)
allYesRSVPs$time = timestampToDate(allYesRSVPs$time)
allYesRSVPs$eventTime = timestampToDate(allYesRSVPs$eventTime)

allYesRSVPs$difference = allYesRSVPs$eventTime - allYesRSVPs$time
allYesRSVPs$difference = as.numeric(allYesRSVPs$eventTime - allYesRSVPs$time, units="days")

allYesRSVPs$answer = "yes"

yes = ggplot(allYesRSVPs, aes(x=difference)) + 
  geom_histogram(binwidth=1, fill="green") +
  xlim(0,120) + 
  ylim(0, 400)

# When did people initially sign up
query = "MATCH (e:Event)<-[:TO]-({response: 'no'})<-[:NEXT]-(response)
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
allNoRSVPs = cypher(graph, query)
allNoRSVPs$time = timestampToDate(allNoRSVPs$time)
allNoRSVPs$eventTime = timestampToDate(allNoRSVPs$eventTime)
allNoRSVPs$difference = as.numeric(allNoRSVPs$eventTime - allNoRSVPs$time, units="days")
allNoRSVPs$answer = "no"

# When did people drop out
query = "MATCH (e:Event)<-[:TO]-(response {response: 'no'})<-[:NEXT]-()
         RETURN response.time AS time, e.time + e.utc_offset AS eventTime"
allNoRSVPs = cypher(graph, query)
allNoRSVPs$time = timestampToDate(allNoRSVPs$time)
allNoRSVPs$eventTime = timestampToDate(allNoRSVPs$eventTime)
allNoRSVPs$difference = as.numeric(allNoRSVPs$eventTime - allNoRSVPs$time, units="days")
allNoRSVPs$answer = "no"

no = ggplot(allNoRSVPs, aes(x=difference)) +
  geom_histogram(binwidth=1, fill="red") +
  xlim(0,120) + ylim(0, 400) + scale_y_reverse()

allRSVPs = rbind(allNoRSVPs, allYesRSVPs)

ggplot(allRSVPs, aes(x = difference, fill=answer)) + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) + 
  geom_bar(binwidth=1) +
  facet_wrap(~ answer, nrow=2, ncol=1)
  
  
library(gridExtra)
grid.arrange(yes,no,ncol=1,widths=c(1,1))

# attempt 2 

allRSVPs = rbind(allNoRSVPs, allYesRSVPs)

ggplot(allRSVPs, aes(x=difference, fill=answer)) + 
  geom_bar(binwidth=1) +
  geom_bar(position="fill") +
  geom_bar(position="dodge")


distance <- function(time1, time2)  {
  sapply(time1, function(x) {
    if((time2 - x) <= 2) {
      "within 2 days"
    } else if((time2 -x ) <= 7) {
      "within a week"
    } else if((time2 -x ) <= 10) {
      "within 10 days"
    } else {
      "more than 10 days ago"
    }
  })  
}

distance2 <- function(time1, time2)  {
  mapply(function(t1, t2) {
    if((t2 - t1) <= 2) {
      "within 2 days"
    } else if((t2 - t1 ) <= 7) {
      "within a week"
    } else if((t2 -t1 ) <= 10) {
      "within 10 days"
    } else {
      "more than 10 days ago"
    }
  }, time1, time2)
} 

ddply(yesRSVPs, .(distance=distance(time, events$eventTime[1])), summarise, count=length(time))

ddply(allYesRSVPs, .(distance=distance2(time, eventTime)), summarise, count=length(time))

summarisedDifference <- function(one, two) {
  mapply(function(x, y) { 
    if((x-y) >= 5) {
      "5 or more"
    } else if((x-y) >= 3) {
      "3 to 5"
    } else {
      "less than 5"
    }    
  }, one, two)
}

df = data.frame(x=c(10,9,8,7,6,5,4,3,2,1), y=c(5,4,3,4,3,2,2,1,2,1))
ddply(df, .(difference=summarisedDifference(x,y)), summarise, count=length(x))

query = "match (e:Event {name: 'Intro to Graphs'})<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         where e.time + e.utc_offset < timestamp()
         RETURN e.time + e.utc_offset AS time, COUNT(*) AS rsvps"
rsvpsIntro = cypher(graph, query)

rsvpsIntro$date <- timestampToDate(rsvpsIntro$time)
rsvpsIntro$date2 = as.Date(rsvpsIntro$date)
head(rsvpsIntro)

library(dplyr)
rsvpsIntro %.% arrange(time)

library(scales)

ggplot(data = rsvpsIntro %.% arrange(time), aes(x = date, y = rsvps)) + geom_point()
ggplot(data = rsvpsIntro %.% arrange(time), aes(x = date, y = rsvps)) + geom_line()

query = "match (e:Event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         where e.name <> 'Intro to Graphs' 
         AND e.name <> \"Hands On Intro to Cypher - Neo4j's Query Language\"
         AND NOT(e.name =~ \".*Hackathon.*\")
         AND NOT(e.name =~ \".*training.*\")
         AND NOT(e.name =~ \".*Training.*\")
         AND NOT(e.name =~ \".*Tutorial.*\")         
         AND NOT(e.name =~ \".*Hands on.*\")
         AND NOT(e.name =~ \".*Lunch.*\")
         AND NOT(e.name =~ \".*Graph Clinic.*\")
         AND (e.time + e.utc_offset < timestamp())
         RETURN e.time + e.utc_offset AS time, COUNT(*) AS rsvps, e.name"
rsvpsAll = cypher(graph, query)

rsvpsAll$date <- timestampToDate(rsvpsAll$time)
rsvpsAll$date2 = as.Date(rsvpsAll$date)

head(rsvpsAll)

ggplot(data = rsvpsAll %.% arrange(time), aes(x = date, y = rsvps)) + geom_point() 

ggplot(data = rsvpsAll %.% arrange(time), aes(x = date2, y = rsvps)) + geom_point() + 
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", labels=date_format("%b %y"))
  
ggplot(data = rsvpsAll %.% arrange(time), aes(x = date2, y = rsvps)) +   
  geom_point(shape=1) + 
  scale_x_date(breaks = "6 month", minor_breaks = "1 month", labels=date_format("%b %y")) + 
  geom_smooth(fill=NA) + ylim(0,110)

ggplot(data = rsvpsIntro %.% arrange(time), aes(x = date2, y = rsvps)) +   
  geom_point(shape=1) + 
  scale_x_date(breaks = "6 month", minor_breaks = "1 month", labels=date_format("%b %y")) + 
  geom_smooth(fill=NA) + ylim(0,65)

query = "match (e:Event {name: \"Hands On Intro to Cypher - Neo4j's Query Language\"})<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         where e.time + e.utc_offset < timestamp()
         RETURN e.time + e.utc_offset AS time, COUNT(*) AS rsvps"
rsvpsCypher = cypher(graph, query)

rsvpsCypher$date <- timestampToDate(rsvpsCypher$time)
rsvpsCypher$date2 = as.Date(rsvpsCypher$date)

ggplot(data = rsvpsCypher %.% arrange(time), aes(x = date2, y = rsvps)) +   
  geom_point(shape=1) +   
  geom_smooth(fill=NA) + ylim(0,65)

query = "match (e:Event {name: \"Intro to Graphs\"})<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         where e.time + e.utc_offset < timestamp()
         RETURN e.time + e.utc_offset AS time, COUNT(*) AS rsvps"
rsvpsIntro = cypher(graph, query)

rsvpsIntro$date <- timestampToDate(rsvpsIntro$time)
rsvpsIntro$date2 = as.Date(rsvpsIntro$date)

ggplot(data = rsvpsIntro %.% arrange(time), aes(x = date2, y = rsvps)) +   
  geom_point(shape=1) +   
  geom_smooth(fill=NA) + ylim(0,65)


query = "match (e:Event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         where (e.time + e.utc_offset < timestamp())
         RETURN e.time + e.utc_offset AS time, COUNT(*) AS rsvps, e.name"
rsvpsEverything = cypher(graph, query)

rsvpsEverything$date <- timestampToDate(rsvpsEverything$time)
rsvpsEverything$date2 = as.Date(rsvpsEverything$date)
rsvpsEverything$monthYear = format(rsvpsEverything$date, "%Y-%m")


rsvpsEverything %.% 
  group_by(format(date, "%b-%Y")) %.% 
  summarise(n = sum(rsvps))

rsvpsByMonth = rsvpsEverything %.% 
  group_by(monthYear) %.% 
  summarise(n = sum(rsvps))

ggplot(data = rsvpsByMonth, aes(x = monthYear, y = n)) + geom_point()

ggplot(data = rsvpsEverything %.% arrange(time), aes(x = date2, y = rsvps)) +   
  geom_point(shape=1) +   
  geom_smooth(fill=NA)


query = "MATCH (e:Event {name: 'Intro to Graphs'})<-[:TO]-(rsvp{response: 'yes'})<-[:RSVPD]-()
WITH e.name AS name, e.time + e.utc_offset AS time, e.announced_at AS announcedAt, COUNT(*) as yesRSVPs, COLLECT(rsvp.time) AS rsvps
WITH name, time, announcedAt, (time - announcedAt) / 1000 / 60 / 60 / 24 AS days, yesRSVPs, rsvps
RETURN name, time, announcedAt, days, yesRSVPs, REDUCE(total=0, rsvp IN rsvps |  CASE WHEN rsvp > announcedAt THEN total + 1 ELSE total END) AS after,
REDUCE(total=0, rsvp IN rsvps |  CASE WHEN rsvp <= announcedAt THEN total + 1 ELSE total END) AS before
ORDER BY yesRSVPs DESC"

events = subset(cypher(graph, query), !is.na(announcedAt))

events$announcedAt = timestampToDate(events$announcedAt)
events$time = timestampToDate(events$time)

head(events)

events$timeDiff = as.numeric(events$time - events$announcedAt, units = "days")

all = ggplot(data = events, aes(x = timeDiff, y = yesRSVPs)) + 
  geom_point() +
  geom_smooth(method=lm, fill = NA)

after = ggplot(data = events, aes(x = timeDiff, y = after)) + 
  geom_point() + 
  geom_smooth(method=lm, fill = NA)  

before = ggplot(data = events, aes(x = timeDiff, y = before)) + 
  geom_point() +
  geom_smooth(method=lm, fill = NA)  

with(events, cor.test(timeDiff, yesRSVPs, method = "pearson"))
with(events, cor.test(timeDiff, after, method = "pearson"))
with(events, cor.test(timeDiff, before, method = "pearson"))

library(gridExtra)

grid.arrange(all, before, after)

?geom_smooth

