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


# map showing where the meetups happen
map = get_map(location = 'London', zoom = 12)
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = byVenue,
             col = "red",
             alpha = 0.8)

# cluster the venues so we can aggregate close by venues

clusteramounts = 40
distance.matrix = (distm(byVenue[,c("lon","lat")]))
clustersx <- as.hclust(agnes(distance.matrix, diss = T))
byVenue$group <- cutree(clustersx, k=clusteramounts)
byVenue %>% arrange(group) %>% head(50)