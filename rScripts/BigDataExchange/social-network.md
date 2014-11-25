The meetup social network
========================================================


```r
library(RNeo4j)
library(dplyr)
library(igraph)
```


```r
graph = startGraph("http://localhost:7474/db/data/")
options(width = 200)
options("scipen"=100, "digits"=4)
```

### Who's at the centre of London's NoSQL meetup scene?

We want to find out who's the most influential person in the London NoSQL meetup scene which we'll calculate based on assuming relationships between a pair of people if they've both attended the same meetup.



```r
nodes_query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event)
               RETURN DISTINCT ID(p) AS id, p.id AS name, p.name AS fullName"

nodes = cypher(graph, nodes_query)

edges_query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event),
                     (event)<-[:TO]-({response:'yes'})<-[:RSVPD]-(other)
               WHERE ID(p) < ID(other)
               RETURN ID(p) AS source, ID(other) AS target, COUNT(*) AS weight"

edges = cypher(graph, edges_query)
```

### Betweenness Centrality

We can use igraph's `betweenness` function to find out which person has the most shortest paths running through them:


```r
g = graph.data.frame(edges, directed = F, nodes)

bw = betweenness(g)
bwDf = data.frame(id = names(bw), score = bw)
bwDf %>% arrange(desc(score)) %>%  head()
```

```
##         id  score
## 1 13483828 488569
## 2  1797906 419598
## 3  6580245 395273
## 4 14327689 386134
## 5 41493372 385061
## 6 48230152 294807
```

```r
merge(nodes, bwDf, by.x = "name", by.y = "id") %>% 
  arrange(desc(score)) %>% 
  head(10)
```

```
##        name    id              fullName  score
## 1  13483828 11198          Peter Morgan 488569
## 2   1797906 10840 Kannappan Sirchabesan 419598
## 3   6580245 12121          Paul McGrath 395273
## 4  14327689 11167            Ricki Long 386134
## 5  41493372 10803                 louis 385061
## 6  48230152 11111                 Rohit 294807
## 7  38680722  9646            Amit Nandi 282983
## 8  42900792 11149            Raja Kolli 272351
## 9   7837110 14268       Paddy Gallagher 270614
## 10 38546232 14891        Enzo Martoglio 264940
```

Now that  we've got those values let's write them into the graph:


```r
query = "MATCH (p:MeetupProfile {id: {id}}) SET p.betweenness = {score}"

tx = newTransaction(graph)

for(i in 1:nrow(bwDf)) {
  if(i %% 1000 == 0) {
    commit(tx)
    print(paste("Batch", i / 1000, "committed."))
    tx = newTransaction(graph)
  }
  id = bwDf[i, "id"]
  score = bwDf[i, "score"]    
  appendCypher(tx,
               query,
               id = id,
               score = as.double(score))
}

commit(tx)
```

### Page Rank

Calculating page rank is as simple as calling the `page.rank` function:


```r
pr = page.rank(g)$vector
prDf = data.frame(name = names(pr), rank = pr) %>% arrange(desc(rank))

merge(nodes, prDf, by.x = "name", by.y = "name") %>%
  arrange(desc(rank)) %>%
  head(10)
```

```
##        name    id              fullName     rank
## 1  41493372 10803                 louis 0.003217
## 2   1797906 10840 Kannappan Sirchabesan 0.002845
## 3  44517222 11056             zheng zhu 0.002684
## 4  38680722  9646            Amit Nandi 0.002505
## 5  38546232 14891        Enzo Martoglio 0.002446
## 6  42900792 11149            Raja Kolli 0.002336
## 7  12569718 10327        Anish Mohammed 0.002298
## 8  13483828 11198          Peter Morgan 0.002232
## 9  35662872 10996   Deepak Subhramanian 0.002125
## 10 14327689 11167            Ricki Long 0.001976
```

We can now write the page rank back into the graph for use in future queries.


```r
query = "MATCH (p:MeetupProfile {id: toInt({id})}) SET p.pageRank = toFloat({score})"

tx = newTransaction(graph)

for(i in 1:nrow(prDf)) {
  if(i %% 1000 == 0) {
    commit(tx)
    print(paste("Batch", i / 1000, "committed."))
    tx = newTransaction(graph)
  }
  name = prDf[i, "name"]
  rank = prDf[i, "rank"]    
  appendCypher(tx,
               query,
               id = name,
               score = as.double(rank))
}

commit(tx)
```
