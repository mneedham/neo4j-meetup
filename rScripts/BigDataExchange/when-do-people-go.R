# when do people typically attend meetups

query = "MATCH (g:Group)-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         WHERE (event.time + event.utc_offset) < timestamp()
         RETURN g.name, 
                event.time + event.utc_offset AS eventTime,
                event.announced_at AS announcedAt, 
                event.name, 
                COUNT(*) AS rsvps"

events = cypher(graph, query)

events$eventTime <- timestampToDate(events$eventTime)
events$time = format(events$eventTime, "%H:%M")
events$day <- factor(format(events$eventTime, "%A"), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
events$monthYear <- format(events$eventTime, "%m-%Y")
events$month <- factor(format(events$eventTime, "%B"), levels = month.name)
events$year <- format(events$eventTime, "%Y")
events$announcedAt<- timestampToDate(events$announcedAt)
events$timeDiff = as.numeric(events$eventTime - events$announcedAt, units = "days")

# group by month
byMonth = events %>% 
  group_by(month) %>%
  summarise(events = n(), 
            count = sum(rsvps), 
            max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))

byMonth 

# group by day
byDay = events %>% 
  group_by(day) %>%
  summarise(events = n(), 
            count = sum(rsvps), 
            max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(day)

g1 = ggplot(aes(x = day, y = ave), data = byDay) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Average attendees by day")
g2 = ggplot(aes(x = day, y = count), data = byDay) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Total attendees by day")

grid.arrange(g1,g2, ncol = 1)

g3 = ggplot(aes(x = month, y = ave), data = byMonth) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Average attendees by month")
g4 = ggplot(aes(x = month, y = count), data = byMonth) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Total attendees by month")

grid.arrange(g3,g4, ncol = 1)

grid.arrange(g1,g3, g2, g4, ncol = 2)

# group 