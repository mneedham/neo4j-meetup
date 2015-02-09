# Data setup


```r
library(lubridate)
library(dplyr)
```


```r
set.seed(1)
group1Counts = as.integer(cumsum(runif(10,1,10)))
group1Dates = ymd_hms("2011-01-01 17:00:00") + c(1) * days(group1Counts)

set.seed(3)
group2Counts = as.integer(cumsum(runif(10,1,10)))
group2Dates = ymd_hms("2011-01-10 17:00:00") + c(1) * days(group2Counts)

g1 = merge(c("Group1"), group1Counts) %>% mutate(date = group1Dates)
g2 = merge(c("Group2"), group2Counts) %>% mutate(date = group2Dates)

g = rbind(g1, g2)
names(g) = c("name", "count", "date")
g
```

```
##      name count                date
## 1  Group1     3 2011-01-04 17:00:00
## 2  Group1     7 2011-01-08 17:00:00
## 3  Group1    13 2011-01-14 17:00:00
## 4  Group1    23 2011-01-24 17:00:00
## 5  Group1    25 2011-01-26 17:00:00
## 6  Group1    34 2011-02-04 17:00:00
## 7  Group1    44 2011-02-14 17:00:00
## 8  Group1    51 2011-02-21 17:00:00
## 9  Group1    58 2011-02-28 17:00:00
## 10 Group1    59 2011-03-01 17:00:00
## 11 Group2     2 2011-01-12 17:00:00
## 12 Group2    10 2011-01-20 17:00:00
## 13 Group2    15 2011-01-25 17:00:00
## 14 Group2    19 2011-01-29 17:00:00
## 15 Group2    25 2011-02-04 17:00:00
## 16 Group2    32 2011-02-11 17:00:00
## 17 Group2    34 2011-02-13 17:00:00
## 18 Group2    37 2011-02-16 17:00:00
## 19 Group2    44 2011-02-23 17:00:00
## 20 Group2    50 2011-03-01 17:00:00
```

## How many members in Group 1 on 24th February 2011 and Group 2 on 18th February 2011?

We should get 51 for Group 1 and 37 for Group 2

### Non vectorised


```r
events = data.frame(eventId = c(1,2), group = c("Group1", "Group2"), eventDate = c(ymd("2011-02-24"), ymd("2011-02-18")))
events
```

```
##   eventId  group  eventDate
## 1       1 Group1 2011-02-24
## 2       2 Group2 2011-02-18
```


```r
getCount =  function(g, group, eventDate) {
  (g %>% filter(name == group & eventDate > date) %>% do(tail(., 1)))$count
}

events %>% mutate(ind = row_number()) %>% group_by(ind) %>%
  mutate(count = getCount(g, group, eventDate))
```

```
## Source: local data frame [2 x 5]
## Groups: ind
## 
##   eventId  group  eventDate ind count
## 1       1 Group1 2011-02-24   1    51
## 2       2 Group2 2011-02-18   2    37
```

### Vectorised


```r
getCountV =  function(g, group, eventDate) {
  (g %>% filter(name == group & eventDate > date) %>% do(tail(., 1)))$count
}

events %>% mutate(count = getCountV(g, group, eventDate))
```

```
##   eventId  group  eventDate count
## 1       1 Group1 2011-02-24    37
## 2       2 Group2 2011-02-18    37
```

That result isn't correct...effectively it's doing this:


```r
g %>% 
  filter(name == c("Group1", "Group2") & c(ymd("2011-02-24"), ymd("2011-02-18")) > date)
```

```
##     name count                date
## 1 Group1     3 2011-01-04 17:00:00
## 2 Group1    13 2011-01-14 17:00:00
## 3 Group1    25 2011-01-26 17:00:00
## 4 Group1    44 2011-02-14 17:00:00
## 5 Group2    10 2011-01-20 17:00:00
## 6 Group2    19 2011-01-29 17:00:00
## 7 Group2    32 2011-02-11 17:00:00
## 8 Group2    37 2011-02-16 17:00:00
```

but it isn't coming back with the correct rows for Group1, it's like it only applied the second date?


```r
g %>% 
  filter(name == c("Group1") & c(ymd("2011-02-24")) > date)
```

```
##     name count                date
## 1 Group1     3 2011-01-04 17:00:00
## 2 Group1     7 2011-01-08 17:00:00
## 3 Group1    13 2011-01-14 17:00:00
## 4 Group1    23 2011-01-24 17:00:00
## 5 Group1    25 2011-01-26 17:00:00
## 6 Group1    34 2011-02-04 17:00:00
## 7 Group1    44 2011-02-14 17:00:00
## 8 Group1    51 2011-02-21 17:00:00
```

```r
g %>% 
  filter(name == c("Group1") & c(ymd("2011-02-18")) > date)
```

```
##     name count                date
## 1 Group1     3 2011-01-04 17:00:00
## 2 Group1     7 2011-01-08 17:00:00
## 3 Group1    13 2011-01-14 17:00:00
## 4 Group1    23 2011-01-24 17:00:00
## 5 Group1    25 2011-01-26 17:00:00
## 6 Group1    34 2011-02-04 17:00:00
## 7 Group1    44 2011-02-14 17:00:00
```

but even then it seems to be skipping off half of the rows on the data frame which comes back...
