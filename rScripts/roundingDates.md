# Rounding dates


```r
library(lubridate)

april20 <- ymd_hms("2010-04-20 11:33:29")

as.numeric(april20 - trunc(april20, "day"), units="hours")
```

```
## [1] 11.55806
```

```r
(april20 - round_date(april20, "day")) / dhours(1)
```

```
## [1] 11.55806
```
