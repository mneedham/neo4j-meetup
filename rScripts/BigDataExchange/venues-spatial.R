# where do NoSQL meetups happen in London?

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
events$day <- factor(format(events$eventTime, "%A"), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
events$monthYear <- format(events$eventTime, "%m-%Y")
events$month <- factor(format(events$eventTime, "%B"), levels = month.name)
events$year <- format(events$eventTime, "%Y")
events$announcedAt<- timestampToDate(events$announcedAt)
events$timeDiff = as.numeric(events$eventTime - events$announcedAt, units = "days")

byVenue = events %>% 
  count(lat, lon, venue) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  dplyr::rename(count = n)

byVenue %>% head(10)

# map showing where the meetups happen
map = get_map(location = 'London', zoom = 12)

m1 = ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = byVenue,
             col = "red",
             alpha = 0.8)

# cluster the venues so we can aggregate close by venues

clusteramounts = 40
distance.matrix = byVenue %>% select(lon, lat) %>% distm
clustersx <- agnes(distance.matrix, diss = T) %>% as.hclust
byVenue$group <- cutree(clustersx, k=clusteramounts)
byVenue %>% arrange(group) %>% head(20)

# hclust
d = dist(distance.matrix)
hc = hclust(d, method = "ward.D")
byVenue$hclustGroup = cutree(hc, k = 40)

byVenue %>% group_by(group) %>% summarise(events = sum(count)) %>% arrange(desc(events)) %>% head(10)
byVenue %>% group_by(hclustGroup) %>% summarise(events = sum(count)) %>% arrange(desc(events)) %>% head(10)

byVenueClustered = byVenue %>% 
  group_by(group) %>% 
  summarise(meanLat = mean(lat),
            meanLon = mean(lon),
            total = sum(count),
            venues = paste(venue, collapse = ",")) %>%
  arrange(desc(total))   

m2 = ggmap(map) +
  geom_point(aes(x = meanLon, y = meanLat, size = total), 
             data = byVenueClustered,
             col = "red",
             alpha = 0.8)

grid.arrange(m1, m2, ncol = 2)

# What's going on in Shoreditch?
byVenue %>% filter(group == byVenueClustered$group[1]) %>% select(-hclustGroup)