# who's at the centre of London's NoSQL meetup scene

query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event),
               (event)<-[:TO]-({response:'yes'})<-[:RSVPD]-(other)
         WHERE ID(p) < ID(other)
         RETURN p.name, other.name, COUNT(*) AS times"

data = cypher(graph, query)

data %>% arrange(desc(times)) %>% head(5)

# betweenness centrality
options("scipen"=100, "digits"=4)
g = graph.data.frame(data, directed = F)

sort(betweenness(g), decreasing = T)[1:5]
sort(betweenness.estimate(g, cutoff=2), decreasing = T) %>% head(5)

# page rank
?page.rank
pr = page.rank(g)$vector
prDf = data.frame(name = names(pr), rank = pr)
head(prDf)

data.frame(prDf) %>%
  arrange(desc(pr)) %>%
  head(10)