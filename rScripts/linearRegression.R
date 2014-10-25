library(RNeo4j)
library(ggplot2)
library(dplyr)
library(car)

# all events
query = "MATCH (g:Group {name: \"Neo4j - London User Group\"})-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-(),
         (event)-[:HELD_AT]->(venue)
WHERE (event.time + event.utc_offset) < timestamp()
RETURN event.time + event.utc_offset AS eventTime,event.announced_at AS announcedAt, event.name, COUNT(*) AS rsvps, venue.name AS venue"

officeEventsQuery = "MATCH (g:Group {name: \"Neo4j - London User Group\"})-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-(),
                           (event)-[:HELD_AT]->(venue)
                     WHERE (event.time + event.utc_offset) < timestamp() AND venue.name IN [\"Neo Technology\", \"OpenCredo\"]
                     RETURN event.time + event.utc_offset AS eventTime,event.announced_at AS announcedAt, event.name, COUNT(*) AS rsvps"

events = subset(cypher(graph, query), !is.na(announcedAt))
events = subset(cypher(graph, officeEventsQuery), !is.na(announcedAt))

events$eventTime <- timestampToDate(events$eventTime)
events$time = format(events$eventTime, "%H:%M")
events$day <- format(events$eventTime, "%A")
events$monthYear <- format(events$eventTime, "%m-%Y")
events$month <- format(events$eventTime, "%m")
events$year <- format(events$eventTime, "%Y")
events$announcedAt<- timestampToDate(events$announcedAt)
events$timeDiff = as.numeric(events$eventTime - events$announcedAt, units = "days")

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
  summarise(events = n(), years = paste(unique(year), collapse = ","), count = sum(rsvps), max = max(rsvps)) %>%
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