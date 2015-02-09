library(RNeo4j)
library(dplyr)
library(car)
library(geosphere)

graph = startGraph("http://localhost:7574/db/data/")

# london nosql events
query = "MATCH (g:Group)-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-(),
               (event)-[:HELD_AT]->(venue)
         WHERE (event.time + event.utc_offset) < timestamp()
         RETURN g.name, 
                event.time + event.utc_offset AS eventTime,
                event.name, 
                COUNT(*) AS rsvps, 
                venue.name AS venue,
                venue.lat AS lat,
                venue.lon AS lon"

events = cypher(graph, query)

events %>% head()

events$eventTime <- timestampToDate(events$eventTime)
events$time = format(events$eventTime, "%H:%M")
events$day <- format(events$eventTime, "%A")
events$monthYear <- format(events$eventTime, "%m-%Y")
events$month <- factor(format(events$eventTime, "%B"), levels = month.name)
events$year <- format(events$eventTime, "%Y")

#centre = c(-0.129581, 51.516578)
centre = c(-0.0825, 51.5265)

events$distanceFromCentre = distHaversine(c(events$lat, events$lon), centre)
events %>% mutate(distanceFromCentre = distHaversine(c(lat, lon), centre))

write.csv(events, "/tmp/events.csv", row.names = FALSE)
write.csv(withDistNoOutliers, "/tmp/events.csv", row.names = FALSE)

eventOne = events %>% slice(1)
distHaversine(c(eventOne$lat, eventOne$lon), centre)

options("scipen"=100, "digits"=4)

withDist = events %>% 
  mutate(
    fromCentre = by(events, 1:nrow(events), function(row) { distHaversine(c(row$lon,row$lat),centre)}) %>% 
      cbind() %>% 
      as.vector()) 

ggplot(aes(x = fromCentre, y = rsvps), data = withDist) + geom_point()
withDist %>% arrange(desc(fromCentre)) %>% head()

quantile(withDist$fromCentre)
sd(withDist$fromCentre)

withDistNoOutliers = withDist %>% filter(fromCentre < quantile(withDist$fromCentre, 0.98))
closeToCentre = withDist %>% filter(fromCentre < 10000)

ggplot(aes(x = fromCentre, y = rsvps), data = withDistNoOutliers) + geom_point()
ggplot(aes(x = fromCentre, y = rsvps), data = closeToCentre) + geom_point()

ggplot(aes(x = fromCentre, y = rsvps), data = withDist %>% filter(fromCentre < 6000)) + 
  geom_point() +
  geom_smooth(fill = NA)

ggplot(aes(x = fromCentre, y = attendees), data = data) + 
  geom_point()

ggplot(aes(x = fromCentre, weight = attendees), data = data) + 
  geom_histogram(binwidth=250)

ggplot(aes(x = fromCentre), data = withDist %>%  filter(fromCentre < 6000)) + 
  geom_histogram(binwidth=250)

ggplot(aes(x = fromCentre, weight = rsvps), data = withDist %>%  filter(fromCentre < 6000)) + 
  geom_histogram(binwidth=250)


with(withDist %>% filter(fromCentre < 6000), cor(fromCentre, rsvps))
cor(closeToCentre$fromCentre, closeToCentre$rsvps)

# get member sign up times
query = "match (:Person)-[:HAS_MEETUP_PROFILE]->()-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(g:Group)
RETURN g.name, membership.joined AS joinTimestamp"

timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")
meetupMembers = cypher(graph, query)
meetupMembers$joinDate <- timestampToDate(meetupMembers$joinTimestamp)
meetupMembers$dayMonthYear <- as.Date(meetupMembers$joinDate)

cumulativeMeetupMembers = meetupMembers %>% 
  group_by(g.name, dayMonthYear) %>% 
  summarise(n = n()) %>% 
  mutate(n = cumsum(n)) %>% 
  group_by(g.name) %>%
  mutate(total = max(n))

ggplot(aes(x = dayMonthYear, y = n, color = g.name), data = cumulativeMeetupMembers) + geom_line()

write.csv(meetupMembers, "/tmp/meetupMembers.csv")

query = "MATCH (g:Group)<-[:MEMBER_OF]-()
WITH g, COUNT(*) AS max
ORDER BY max DESC
match (:Person)-[:HAS_MEETUP_PROFILE]->()-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(g:Group)
RETURN g.name, membership.joined AS joinTimestamp, max"
meetupMembers = cypher(graph, query)
meetupMembers$joinDate <- timestampToDate(meetupMembers$joinTimestamp)
meetupMembers$dayMonthYear <- as.Date(meetupMembers$joinDate)

cumulativeMeetupMembers = meetupMembers %>% 
  group_by(g.name, dayMonthYear) %>% 
  summarise(n = n()) %>%
  mutate(n = cumsum(n))

ggplot(aes(x = dayMonthYear, y = n, color = g.name), 
       data = merge(meetupMembers, cumulativeMeetupMembers, by = c("g.name", "dayMonthYear"))) +
  geom_line()

write.csv(meetupMembers, "/tmp/meetupMembers2.csv")

# Look up member count when the meetup happened
memberCount = function(meetupMembers) {
  function(groupName, date) {
    (meetupMembers %>% 
       filter(g.name == groupName & dayMonthYear < date) %>% do(tail(., 1)))$n    
  }  
} 

findMemberCount = memberCount(cumulativeMeetupMembers)

membersWithGroupCounts= withDistNoOutliers %>%
  mutate(
    groupMembers = by(withDistNoOutliers, 1:nrow(withDistNoOutliers), function(row) { 
        findMemberCount(row$g.name, as.character(row$eventTime))
      }) %>% 
      cbind() %>% 
      as.vector()    
    )

membersWithGroupCounts %>% arrange(desc(groupMembers)) %>% head()

ggplot(aes(x = groupMembers, y = rsvps), data = membersWithGroupCounts) + geom_point() + geom_smooth(fill = NA)
ggplot(aes(x = fromCentre, y = rsvps), data = membersWithGroupCounts) + geom_point() + geom_smooth(fill = NA)

fit = lm(rsvps ~ groupMembers, data = membersWithGroupCounts)
membersWithGroupCounts$predictedRSVPS = predict(fit, membersWithGroupCounts)
ggplot(aes(x = rsvps, y = predictedRSVPS), data = membersWithGroupCounts) + geom_point() + geom_abline(intercept=0, slope=1)

write.csv(membersWithGroupCounts, "/tmp/membersWithGroupCounts.csv")

top5 = (cumulativeMeetupMembers %>%
  select(g.name, total) %>%                         # keep only columns of interest
  ungroup() %>%                              # forget about the grouping
  distinct() %>%                                 # get distinct rows
  arrange(desc(total)) %>%                          # order by "total" descending
  slice(1:5))$g.name# get top 5 rows

ggplot(aes(x = dayMonthYear, y = n, color = g.name), data = cumulativeMeetupMembers %>% filter(g.name %in% top5)) + geom_line()

cumulativeMeetupMembers %>% select(g.name) %>% distinct()

databases = c("London MongoDB User Group", "Neo4j - London User Group", "London Riak Meetup", "Hadoop Users Group UK", "Cassandra London", "Couchbase London", "London ElasticSearch User Group")

ggplot(aes(x = dayMonthYear, y = n, color = g.name), data = cumulativeMeetupMembers %>% filter(g.name %in% databases)) + geom_line(size = 1)

filteredCumulativeMeetupMembers = meetupMembers %>%
  group_by(g.name, dayMonthYear) %>%
  summarise(n = n()) %>%
  mutate(n = cumsum(n)) %>%
  group_by(g.name) %>%
  mutate(total = max(n)) %>%
  ungroup() %>%
  mutate(min.total.of.interest = sort(unique(total), decreasing=T)[5]) %>%
  filter(total >= min.total.of.interest)

ggplot(aes(x = dayMonthYear, y = n, color = g.name), data = filteredCumulativeMeetupMembers) + geom_line(size = 1)

?geom_line


events = read.csv("/tmp/events.csv")
summary(lm(rsvps ~ timeDiff + time + month + day, data = events))
summary(lm(rsvps ~ timeDiff + time + month + day + event.name, data = events))

names(events)
events %>% head()

summary(lm(rsvps ~ venue, data = events))

events %>% 
  group_by(venue) %>%
  dplyr::summarise(events = n()) %>%
  arrange(desc(events))

summary(lm(rsvps ~ month, data = events))

events
# 81% of variation explained
summary(lm(rsvps ~., data = events))

# 53% without the event name
summary(lm(rsvps ~., data = subset(events, select = -c(event.name, eventTime, monthYear))))

# 60% variation explained
summary(lm(rsvps ~., data = subset(events, select = c(rsvps, event.name, day, timeDiff, month, year))))

summary(lm(rsvps ~., data = subset(events, select = c(rsvps, event.name, day, timeDiff))))

# Try simplifying the name down to practical vs theory sessions
events$practical = grepl("Hackathon|Hands on|Hands On", events$event.name)

summary(lm(rsvps ~., data = subset(events, select = -c(event.name))))
summary(lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear))))
summary(lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear, month, year, day))))
summary(lm(rsvps ~.+0, data = subset(events, select = -c(event.name, monthYear, month, year, day))))

summary(lm(rsvps ~.+0, data = subset(events, select = -c(event.name, monthYear, month, year, day, announcedAt))))

events$rsvps

fit  = lm(rsvps ~.+, data = subset(events, select = -c(event.name, monthYear, month, year, day, announcedAt, eventTime)))

summary(
  lm(rsvps ~., data = subset(events, 
                             select = -c(event.name, monthYear, year, announcedAt, eventTime))))

summary(
  lm(rsvps ~., data = subset(events, 
                             select = c(rsvps, day, practical, time))))
fit = lm(rsvps ~ day + practical + venue , data = events)
summary(fit)
sqrt(vif(fit))
alias(fit)

par(mfrow = c(2, 2))
plot(fit)
dfbetas(fit)

ggplot(aes(x = day, y = rsvps), data = events) + geom_point()
fit = lm(rsvps ~ day  , data = events)
summary(fit)

par(mfrow = c(2, 2))
plot(fit)


par(mfrow = c(2, 2))
plot(predict(fit), resid(fit))
plot(predict(fit), rstandard(fit))

head(events)
head(predict(fit))

dfPredict = data.frame(day = c("Wednesday"))

predict(fit, dfPredict)

options("scipen" = 20)
foo = events %>% dplyr::select(day, rsvps)
foo$prediction = predict(fit)
foo$residuals = foo$rsvps - foo$prediction
foo$standardized = foo$residuals / sd(foo$residuals)
head(foo)
head(events)

ggplot(aes(x = timeDiff, y = rsvps), data = events) + geom_point()
fit = lm(rsvps ~ timeDiff, data = events)
x = events %>% dplyr::select(timeDiff, rsvps)
x$prediction = predict(fit)
x$residuals = x$rsvps - x$prediction
x$standardized = x$residuals / sd(x$residuals)
plot(fit)
summary(fit)

par(mfrow=c(1,1))
ggplot(aes(x = prediction, y = standardized), data = x) + geom_point()

par(mfrow=c(2,2))
plot(fit)
hatvalues(fit)
?hatvalues

qqnorm(x$standardized, ylab="Standardized Residuals", xlab="Normal Scores", main="Old Faithful Eruptions") 
qqline(x$standardized)

?qqnorm

qplot(rsvps, data=events, geom="histogram", binwidth = 5)

par(mfrow = c(2, 2))
?par
ggplot(aes(x = prediction, y = standardized), data = foo) + geom_point()
ggplot(aes(x = day, y = rsvps), data = foo) + geom_point()

resid(fit)
rstandard(fit)
?rstandard

?rstandard

# has collinearity
vif(lm(rsvps ~ day + practical + venue + time , data = events))
vif(lm(rsvps ~ day + practical + time , data = events))

head(events)
simpleFit = lm(I(rsvps) ~ I(timeDiff), data = events)
summary(simpleFit)

ggplot(aes(x = venue, y = rsvps), data = events) + geom_point()
ggplot(aes(x = day, y = rsvps), data = events) + geom_point()

simpleFit = lm(I(rsvps) ~ I(venue), data = events)
summary(simpleFit)

summary(lm(I(rsvps) ~ I(day) + I(venue), data = events))
vif(lm(I(rsvps) ~ I(day) + I(venue) + I(time), data = events))

summary(lm(I(rsvps) ~ I(day) + I(venue) + I(time), data = events))
alias(lm(I(rsvps) ~ I(day) + I(venue) + I(time), data = events))

fit = lm(rsvps ~ day + practical + time , data = events)

fit = lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear, month, year, day)))

plot(predict(fit), residuals(fit))
which.max(hatvalues (fit))
vif(fit)

op <- options(contrasts = c("contr.helmert", "contr.poly"))
alias(fit)
options(op)  # reset

fit = lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear, month, year, day, time, timeDiff)))
vif(fit)

fit1 = lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear, month, year, day)))
fit2 = lm(rsvps ~., data = subset(events, select = -c(event.name, monthYear, month, year, day, time, timeDiff)))

summary(fit1)
summary(fit2)
anova(fit1, fit2)

vif(lm(rsvps ~ day + practical + venue + time , data = events))
alias(lm(rsvps ~ day + practical + venue + time , data = events))
summary(lm(rsvps ~ day + practical + venue + time , data = events))

op <- options(contrasts = c("contr.helmert", "contr.poly"))
options(op)

alias(lm(rsvps ~ day + practical + venue:time , data = events))
vif(lm(rsvps ~ day + practical + venue:time , data = events))
vif(lm(rsvps ~ day + practical + venue , data = events))

vif(lm(rsvps ~ day + practical + venue , data = events))
vif(lm(rsvps ~ practical + venue , data = events))
summary(lm(rsvps ~ practical + venue , data = events))

ggplot(aes(x = day, y = time), data = events) + geom_point()
head(events)

events %>% 
  group_by(month) %>%
  dplyr::summarise(events = n(), years = paste(unique(year), collapse = ","), count = sum(rsvps), max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))

?max

dfPredict = data.frame(practical=c(FALSE, FALSE, FALSE, FALSE), 
                       day = c("Tuesday", "Tuesday", "Wednesday", "Wednesday"),
                       venue = c("Neo Technology","Neo Technology","Neo Technology","Neo Technology"),
                       time = c("18:00", "18:30", "18:00", "18:30"))

pred = predict(fit, dfPredict, interval = "confidence")

pred
dfPredict

class(pred)