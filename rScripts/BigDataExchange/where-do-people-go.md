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

This is the part of the graph that we're interested in this time:

<img src="images/where.png" width="500" />

The only change from the previous set of queries is that we're now interested in the `HELD_AT` relationship and the `venue` the meetup is being held at.



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

byVenue %>% sample_n(10)
```

```
## Source: local data frame [10 x 4]
## 
##         lat       lon                          venue count
## 1  51.50256 -0.019379 Skyline Bar at CCT Venues Plus     1
## 2  51.53373 -0.122340                   The Guardian     1
## 3  51.51289 -0.067163               Erlang Solutions     3
## 4  51.49146 -0.219424               Novotel - W6 8DR     1
## 5  51.49311 -0.146531                    Google HQ       1
## 6  51.52655 -0.084219             Look Mum No Hands!    22
## 7  51.51976 -0.097270       Vibrant Media, 3rd Floor     1
## 8  51.52303 -0.085178                  Mind Candy HQ     2
## 9  51.51786 -0.109260         ThoughtWorks UK Office     2
## 10 51.51575 -0.097978                      BT Centre     1
```

Skillsmatter is first as you'd probably expect - there are a lot of meetups going on here. Let's have a look what's happening in the rest of London:


```r
map = get_map(location = 'London', zoom = 12)

ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = count), 
             data = byVenue,
             col = "red",
             alpha = 0.8) + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

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
## 2      1     89
## 3      2     62
## 4      4     55
## 5      8     19
## 6      6     18
## 7      7     18
## 8      5     13
## 9     12     13
## 10    14     10
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
## 2      1 51.52443 -0.09919280    89
## 3      2 51.50547 -0.10325925    62
## 4      4 51.50794 -0.12714600    55
## 5      8 51.51671 -0.10028908    19
## 6      6 51.53655 -0.13798514    18
## 7      7 51.52159 -0.10934720    18
## 8      5 51.51155 -0.07004417    13
## 9     12 51.51459 -0.12314650    13
## 10    14 51.52129 -0.07588867    10
```


```r
ggmap(map) +
  geom_point(aes(x = meanLon, y = meanLat, size = total), 
             data = byVenueClustered,
             col = "red",
             alpha = 0.8)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

### The meetup quadrangle

We can now see 4 big clusters - Skillsmatter as we identified before, one south of the River, one over in Westminster and the biggest cluster over in Shoreditch / Liverpool Street.

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
##    group                                                                 venue count total
## 1      3                        MongoDB London 5 - 25 Scrutton Street, Unit 1E    18   123
## 2      3                                                    Look Mum No Hands!    22   123
## 3      3                                                         Google Campus     6   123
## 4      3                                                          10gen London     2   123
## 5      3                                                   Oracle City Office      6   123
## 6      3                                              81 Leonard Street Events     1   123
## 7      3                                                            The Bakery     2   123
## 8      3                                                   10gen London Office    25   123
## 9      3                                                 Bloomberg European HQ     1   123
## 10     3                                                 Electricity Showrooms     1   123
## 11     3                                                       The Jugged Hare     1   123
## 12     3                                                                  Juno     1   123
## 13     3                                                            The Bakery     1   123
## 14     3                                       Google Campus - Central Working     2   123
## 15     3                                                          makeshift.io     1   123
## 16     3                                                      The Village Hall     2   123
## 17     3                                                        Bar Music Hall     1   123
## 18     3                                               Google Campus 3rd Floor     2   123
## 19     3                                                      Techhub @ Campus     4   123
## 20     3                Garden Room and Conservatory, Level 3, Barbican Centre     1   123
## 21     3                Garden Room and Conservatory, Level 3, Barbican Centre     1   123
## 22     3                                                       Google Campus,      1   123
## 23     3                                                             Bloomberg     1   123
## 24     3                                               Shoreditch Village Hall     6   123
## 25     3                                                            Red Badger     1   123
## 26     3                                                Basho Technologies Ltd     3   123
## 27     3                                      3rd Floor Space, Google Campus,      3   123
## 28     3                                                         Mind Candy HQ     2   123
## 29     3                                              Shoreditch Village Hall      2   123
## 30     3                                                            JustAddRed     2   123
## 31     3                                                    Oracle Corporation     1   123
## 32     1                                                         Skills Matter    60    89
## 33     1                                                  The Slaughtered Lamb     1    89
## 34     1                                            The Skills Matter eXchange    13    89
## 35     1                                                         Skills Matter    13    89
## 36     1                                                 SkillsMatter eXchange     2    89
## 37     2                                                        Neo Technology    38    62
## 38     2                        Financial Times - FT Offices, Southwark Bridge     1    62
## 39     2                                                 IT Training Room Hire     1    62
## 40     2                                                             OpenCredo    13    62
## 41     2 The Union Jack Club, (I have to sign you in, call me when you arrive)     1    62
## 42     2                                               IBM Southbank (SE1 9PZ)     3    62
## 43     2                                            LMG Training & Development     4    62
## 44     2                                                    BAE Systems Detica     1    62
## 45     4                                                 The Theodore Bullfrog     8    55
## 46     4                                                   10gen London Office     1    55
## 47     4                                                   The Hub Westminster    24    55
## 48     4                                                     Theodore Bullfrog     1    55
## 49     4                                               Online - web conference     5    55
## 50     4                                               The Open Data Institute     3    55
## 51     4                                                                ONLINE    10    55
## 52     4                                                     One Canada Square     2    55
## 53     4                                                               Mozilla     1    55
```
