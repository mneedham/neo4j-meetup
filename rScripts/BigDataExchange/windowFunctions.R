library(Lahman)
library(dplyr)

batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H) 
batting <- arrange(batting, playerID, yearID, teamID)
players <- group_by(batting, playerID)

players %>% head()

# For each player, find the two years with most hits
filter(players, min_rank(desc(H)) <= 2 & H > 0)

players %>% mutate(rank = min_rank(desc(H))) %>% head(20)
