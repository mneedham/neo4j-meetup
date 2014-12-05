When do people go to meetups?
========================================================

The first thing we'll explore is when people go to meetups by day of week and month of year - starting simple!

Let's import the libraries we'll need:


```r
library(RNeo4j)
library(dplyr)
library(ggplot2)
library(gridExtra)
```

And setup our connection to Neo4j and define a function which we'll use to translate the timestamps we get from Neo4j into R Dates:


```r
timestampToDate <- function(x) as.POSIXct(x / 1000, origin="1970-01-01", tz = "GMT")
graph = startGraph("http://localhost:7474/db/data/")
```

```
## Error in function (type, msg, asError = TRUE) : Failed connect to localhost:7474; Connection refused
```

```r
options(width = 200)
```

This is the part of the graph that we're interested in:

<img src="images/when.png" width="500" />

Now we'll execute a query to get back the names of events, the date they were hosted and the number of people who RSVP'd 'yes' which we'll be using as a proxy for 'attended':



```r
query = "MATCH (g:Group)-[:HOSTED_EVENT]->(event)<-[:TO]-({response: 'yes'})<-[:RSVPD]-()
         WHERE (event.time + event.utc_offset) < timestamp()
         RETURN g.name, 
                event.time + event.utc_offset AS eventTime,
                event.name, 
                COUNT(*) AS rsvps"

events = cypher(graph, query)
```

```
## Error in cypher(graph, query): object 'graph' not found
```

```r
events$eventTime <- timestampToDate(events$eventTime)
```

```
## Error in as.POSIXct(x/1000, origin = "1970-01-01", tz = "GMT"): object 'events' not found
```

```r
events$day <- factor(format(events$eventTime, "%A"), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

```
## Error in format(events$eventTime, "%A"): object 'events' not found
```

```r
events$month <- factor(format(events$eventTime, "%B"), levels = month.name)
```

```
## Error in format(events$eventTime, "%B"): object 'events' not found
```

```r
pickRandomRows = function(df, numberOfRows = 10) {
  df %>% slice(as.integer(runif(numberOfRows,0, length(df[,1]))))
}
```

Let's have a look at the most attended events to get a feel for the data:


```r
events %>% select(event.name, g.name, rsvps) %>% arrange(desc(rsvps)) %>% pickRandomRows()
```

```
## Error in eval(expr, envir, enclos): object 'events' not found
```

Now we'll get back to checking which day of the week people go to meetups on:



```r
byDay = events %>% 
  group_by(day) %>%
  summarise(events = n(), 
            count = sum(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(day)
```

```
## Error in eval(expr, envir, enclos): object 'events' not found
```

```r
byDay
```

```
## Error in eval(expr, envir, enclos): object 'byDay' not found
```

We first group by day and then use the `summarise` function which allows us to calculate the number of events on that day and then sum up the RSVPs as well. We then use the `mutate` function to add an average column before sorting the data frame by day.

ggplot gives us a nice way of visualising this data:


```r
g1 = ggplot(aes(x = day, y = ave), data = byDay) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Average attendees by day")
```

```
## Error in ggplot(aes(x = day, y = ave), data = byDay): object 'byDay' not found
```

```r
g2 = ggplot(aes(x = day, y = count), data = byDay) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Total attendees by day")
```

```
## Error in ggplot(aes(x = day, y = count), data = byDay): object 'byDay' not found
```

```r
grid.arrange(g1,g2, ncol = 1)
```

```
## Error in arrangeGrob(..., as.table = as.table, clip = clip, main = main, : object 'g1' not found
```

We can do the same thing to see which months of the year are most popular for meetups:


```r
byMonth = events %>% 
  group_by(month) %>%
  summarise(events = n(), 
            count = sum(rsvps)) %>%
  mutate(ave = count / events) %>%
  arrange(desc(ave))
```

```
## Error in eval(expr, envir, enclos): object 'events' not found
```


```r
g3 = ggplot(aes(x = month, y = ave), data = byMonth) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Average attendees by month")
```

```
## Error in ggplot(aes(x = month, y = ave), data = byMonth): object 'byMonth' not found
```

```r
g4 = ggplot(aes(x = month, y = count), data = byMonth) + 
  geom_bar(stat="identity", fill="dark blue") + 
  ggtitle("Total attendees by month")
```

```
## Error in ggplot(aes(x = month, y = count), data = byMonth): object 'byMonth' not found
```

```r
grid.arrange(g3,g4, ncol = 1)
```

```
## Error in arrangeGrob(..., as.table = as.table, clip = clip, main = main, : object 'g3' not found
```

