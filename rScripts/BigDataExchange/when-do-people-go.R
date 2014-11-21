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
events$month <- format(events$eventTime, "%m")
events$year <- format(events$eventTime, "%Y")
events$announcedAt<- timestampToDate(events$announcedAt)
events$timeDiff = as.numeric(events$eventTime - events$announcedAt, units = "days")

# group by month
events %>% 
  group_by(month) %>%
  dplyr::summarise(events = n(), 
                   count = sum(rsvps), 
                   max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))

# group by day
events %>% 
  group_by(day) %>%
  dplyr::summarise(events = n(), 
                   count = sum(rsvps), 
                   max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))


byDay = events %>% 
  group_by(day) %>%
  dplyr::summarise(events = n(), 
                   count = sum(rsvps), 
                   max = max(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(day)

g1 = ggplot(aes(x = day, y = ave), data = byDay) + geom_bar(stat="identity", fill="dark blue")
g2 = ggplot(aes(x = day, y = count), data = byDay) + geom_bar(stat="identity", fill="dark blue")

grid.arrange(g1,g2,nrow = 1)



# group 