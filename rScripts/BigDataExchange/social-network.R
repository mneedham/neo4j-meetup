options("scipen"=100, "digits"=4)

# who's at the centre of London's NoSQL meetup scene

nodes_query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event)
               RETURN DISTINCT ID(p) AS id, p.id AS name, p.name AS fullName"

nodes = cypher(graph, nodes_query)

edges_query = "MATCH (p:MeetupProfile)-[:RSVPD]->({response: 'yes'})-[:TO]->(event),
                     (event)<-[:TO]-({response:'yes'})<-[:RSVPD]-(other)
               WHERE ID(p) < ID(other)
               RETURN ID(p) AS source, ID(other) AS target, COUNT(*) AS weight"

edges = cypher(graph, edges_query)

# betweenness centrality

g = graph.data.frame(edges, directed = F, nodes)

#sort(betweenness(g), decreasing = T)[1:5]
bwGraph = betweenness.estimate(g, cutoff=2)
bwDf = data.frame(id = names(bwGraph), score = bwGraph)

bwDf %>% arrange(desc(score)) %>% head()

merge(nodes, bwDf, by.x = "name", by.y = "id") %>% 
  arrange(desc(score)) %>% 
  head(5)

for(i in 1:nrow(bwDf)) {
  id = bwDf[i, "id"]
  score = bwDf[i, "score"]    
  cypher(graph, "MATCH (p:MeetupProfile {id: {id}}) SET p.betweenness = {score}", id = id, score = score)
}

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

# page rank
pr = page.rank(g)$vector
prDf = data.frame(name = names(pr), rank = pr) %>% arrange(desc(rank))

data.frame(merge(nodes, prDf, by.x = "name", by.y = "name")) %>%
  arrange(desc(rank)) %>%
  head(10)

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


# blended

query = "MATCH (p:MeetupProfile)-[:MEMBER_OF]->(group)
         WITH p, COUNT(group) AS groups
         MATCH (p)-[:RSVPD]->({response: 'yes'})-[:TO]->()
         RETURN p.name, p.id, p.betweenness, p.pageRank, groups, COUNT(*) AS attended
         ORDER BY p.pageRank DESC"

blended_data = cypher(graph, query)

blended_data %>% head(10)