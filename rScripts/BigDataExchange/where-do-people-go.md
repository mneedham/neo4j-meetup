Where do people go to meetups?
========================================================

So we already know which days of the week and months of the year people attend meetups but where abouts in London are those meetups being held?

First things first, let's load in the required libraries:


```r
library(RNeo4j)
library(dplyr)
library(ggplot2)
library(ggmap)
library(grid)
library(geosphere)
library(cluster)
```



```r
graph = startGraph("http://localhost:7474/db/data/")
options(width = 200)
```


```r
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

byVenue = events %>% 
  count(lat, lon, venue) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  rename(count = n)

byVenue %>% head(10)
```

```
## Source: local data frame [10 x 4]
## 
##         lat       lon                                          venue count
## 1  51.52482 -0.099109                                  Skills Matter    59
## 2  51.50530 -0.100214                                 Neo Technology    36
## 3  51.52336 -0.083790                            10gen London Office    25
## 4  51.50790 -0.131782                            The Hub Westminster    23
## 5  51.52655 -0.084219                             Look Mum No Hands!    22
## 6  51.52362 -0.083438 MongoDB London 5 - 25 Scrutton Street, Unit 1E    18
## 7  51.50524 -0.101121                                      OpenCredo    13
## 8  51.52451 -0.099152                                  Skills Matter    13
## 9  51.52452 -0.099231                     The Skills Matter eXchange    13
## 10 51.50734 -0.127683                                         ONLINE    10
```

Skillsmatter is first as you'd probably expect - there are a lot of meetups going on here. Let's have a look what's happening in the rest of London:


```r
map = get_map(location = 'London', zoom = 12)
```

```
## Error in download.file(url, destfile = destfile, quiet = !messaging, mode = "wb"): cannot open URL 'http://maps.googleapis.com/maps/api/staticmap?center=London&zoom=12&size=%20640x640&scale=%202&maptype=terrain&sensor=false'
```

```r
ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = byVenue,
             col = "red",
             alpha = 0.8) + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
```

```
## Error in ggmap(map): object 'map' not found
```

Clustering venues
-------------


```r
clusteramounts = 40
distance.matrix = byVenue %>% select(lon, lat) %>% distm
clustersx <- agnes(distance.matrix, diss = T) %>% as.hclust
byVenue$group <- cutree(clustersx, k=clusteramounts)
```



```r
byVenue %>% group_by(group) %>% summarise(events = sum(count)) %>% arrange(desc(events)) %>% head(10)
```

```
## Source: local data frame [10 x 2]
## 
##    group events
## 1      3    123
## 2      1     87
## 3      2     55
## 4      4     54
## 5      6     18
## 6      7     18
## 7      8     18
## 8      5     13
## 9     13     12
## 10    15      9
```



```r
byVenueClustered = byVenue %>% 
  group_by(group) %>% 
  summarise(meanLat = mean(lat),
            meanLon = mean(lon),
            total = sum(count)) %>%
  arrange(desc(total))   
byVenueClustered %>% head(10)
```

```
## Source: local data frame [10 x 4]
## 
##    group  meanLat     meanLon total
## 1      3 51.52349 -0.08506461   123
## 2      1 51.52459 -0.09916100    87
## 3      2 51.50575 -0.09998700    55
## 4      4 51.50794 -0.12714600    54
## 5      6 51.53655 -0.13798514    18
## 6      7 51.52159 -0.10934720    18
## 7      8 51.51679 -0.10048167    18
## 8      5 51.51155 -0.07004417    13
## 9     13 51.51459 -0.12314650    12
## 10    15 51.52129 -0.07588867     9
```


```r
ggmap(map) +
  geom_point(aes(x = meanLon, y = meanLat, size = total), 
             data = byVenueClustered,
             col = "red",
             alpha = 0.8)
```

```
## Error in ggmap(map): object 'map' not found
```

### The meetup quadrangle

We can now see 4 big clusters - one over in Westminster; Skillsmatter as we identified before but also one over in Shoreditch. 

First let's grab those groups:


```r
topGroups = (byVenueClustered %>% head(4))$group
topGroups
```

```
## [1] 3 1 2 4
```



```r
merge(byVenue, byVenueClustered, by.id = "group") %>% 
  filter(group %in% topGroups) %>% 
  select(group, venue, count, total) %>% 
  arrange(desc(total))
```

```
##    group                                                  venue count total
## 1      3                                     Look Mum No Hands!    22   123
## 2      3         MongoDB London 5 - 25 Scrutton Street, Unit 1E    18   123
## 3      3                                    10gen London Office    25   123
## 4      3                                    Oracle City Office      6   123
## 5      3                                           10gen London     2   123
## 6      3                                Shoreditch Village Hall     6   123
## 7      3                                           makeshift.io     1   123
## 8      3                                  Bloomberg European HQ     1   123
## 9      3                                                   Juno     1   123
## 10     3                                  Electricity Showrooms     1   123
## 11     3                                             Red Badger     1   123
## 12     3                                             JustAddRed     2   123
## 13     3                                 Basho Technologies Ltd     3   123
## 14     3                                       Techhub @ Campus     4   123
## 15     3                                        The Jugged Hare     1   123
## 16     3                                     Oracle Corporation     1   123
## 17     3                        Google Campus - Central Working     2   123
## 18     3                                          Google Campus     6   123
## 19     3                                       The Village Hall     2   123
## 20     3                       3rd Floor Space, Google Campus,      3   123
## 21     3                               81 Leonard Street Events     1   123
## 22     3                                          Mind Candy HQ     2   123
## 23     3 Garden Room and Conservatory, Level 3, Barbican Centre     1   123
## 24     3                                             The Bakery     1   123
## 25     3                                Google Campus 3rd Floor     2   123
## 26     3 Garden Room and Conservatory, Level 3, Barbican Centre     1   123
## 27     3                                             The Bakery     2   123
## 28     3                                              Bloomberg     1   123
## 29     3                                         Bar Music Hall     1   123
## 30     3                               Shoreditch Village Hall      2   123
## 31     3                                        Google Campus,      1   123
## 32     1                                          Skills Matter    59    87
## 33     1                                  SkillsMatter eXchange     2    87
## 34     1                                          Skills Matter    13    87
## 35     1                             The Skills Matter eXchange    13    87
## 36     2                                         Neo Technology    36    55
## 37     2         Financial Times - FT Offices, Southwark Bridge     1    55
## 38     2                                              OpenCredo    13    55
## 39     2                                     BAE Systems Detica     1    55
## 40     2                             LMG Training & Development     4    55
## 41     4                                    10gen London Office     1    54
## 42     4                                    The Hub Westminster    23    54
## 43     4                                  The Theodore Bullfrog     8    54
## 44     4                                Online - web conference     5    54
## 45     4                                The Open Data Institute     3    54
## 46     4                                      One Canada Square     2    54
## 47     4                                                Mozilla     1    54
## 48     4                                      Theodore Bullfrog     1    54
## 49     4                                                 ONLINE    10    54
```


