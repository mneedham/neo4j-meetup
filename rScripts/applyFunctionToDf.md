



```r
# https://gist.github.com/mneedham/7e926a213bf76febf5ed
venues = read.csv("/tmp/venues.csv")

venues %>% head()
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

We want to calculate the distance from each venue to a centre point in London. I've chosen the (lat, long) coordinates of the Centre Point building in Tottenham Court Road. The distHaversine function in the geosphere library allows us to calculate this:


```r
options("scipen"=100, "digits"=4)
library(geosphere)

centre = c(-0.129581, 51.516578)
aVenue = venues %>% slice(1)
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

```r
aVenue
```

```
## Error in eval(expr, envir, enclos): object 'aVenue' not found
```

Now we can calculate the distance from Skillsmatter to our centre point:


```r
distHaversine(c(aVenue$lon, aVenue$lat), centre)
```

```
## Error in inherits(p, "SpatialPoints"): object 'aVenue' not found
```

That works pretty well so now we want to apply it to every row in the `venues` data frame and add an extra column containing that value. 

This was my first attempt...


```r
venues %>% mutate(distHaversine(c(lon,lat),centre))
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

...which didn't work quite as I'd imagined! 

I eventually found my way to the <a href="https://stat.ethz.ch/R-manual/R-devel/library/base/html/by.html">by</a> function which allows you to 'apply a function to a data frame split by factors'. In this case I wouldn't be grouping rows by a factor - I'd apply the function to each row separately.

I wired everything up like so:


```r
distanceFromCentre = by(venues, 1:nrow(venues), function(row) { distHaversine(c(row$lon, row$lat), centre)  })
distanceFromCentre %>% head()
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

We can now add the distaces to our `venues` data frame:


```r
venuesWithCentre = venues %>% 
  mutate(distanceFromCentre = by(venues, 1:nrow(venues), function(row) { distHaversine(c(row$lon, row$lat), centre)  }))
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

```r
venuesWithCentre %>% head()
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```
