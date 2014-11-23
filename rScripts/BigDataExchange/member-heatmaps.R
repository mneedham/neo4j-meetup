# Group heatmap based on membership overlap

# Show member overlap between the different groups
query = "MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
OPTIONAL MATCH p = (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)
WITH group1, group2, COLLECT(p) AS paths
RETURN group1.name, group2.name, LENGTH(paths) as commonMembers
ORDER BY group1.name, group2.name"

group_overlap = cypher(graph, query)

ggplot(group_overlap, aes(x=group1.name, y=group2.name, fill=commonMembers)) + 
  geom_bin2d() +
  geom_text(aes(label = commonMembers)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Show member overlap as a percentage

query = "MATCH (group1:Group), (group2:Group)
WHERE group1 <> group2
OPTIONAL MATCH (group1)<-[:MEMBER_OF]-(member)

WITH group1, group2, COLLECT(member) AS group1Members
WITH group1, group2, group1Members, LENGTH(group1Members) AS numberOfGroup1Members

UNWIND group1Members AS member
OPTIONAL MATCH path =  (member)-[:MEMBER_OF]->(group2) 
WITH group1, group2, COLLECT(path) AS paths, numberOfGroup1Members
WITH group1, group2, LENGTH(paths) as commonMembers, numberOfGroup1Members

RETURN group1.name, group2.name, toInt(round(100.0 * commonMembers / numberOfGroup1Members)) AS percentage
ORDER BY  group1.name, group1.name"

group_overlap_percentage = cypher(graph, query)

ggplot(group_overlap_percentage, aes(x=group2.name, y=group1.name, fill=percentage)) + 
  geom_bin2d() +
  geom_text(aes(label = percentage)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# How many groups are people part of?

query = "match (p:MeetupProfile)-[:MEMBER_OF]->()
         return ID(p), COUNT(*) AS groups
         ORDER BY groups DESC"

group_count = cypher(graph, query)

ggplot(aes(x = groups, y = n), data = group_count %>% count(groups)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  scale_y_sqrt() +
  scale_x_continuous(breaks = round(seq(min(group_count$groups), max(group_count$groups), by = 1),1)) +
  ggtitle("Number of groups people are members of")