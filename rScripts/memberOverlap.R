install.packages("devtools")
devtools::install_github("nicolewhite/Rneo4j")
install.packages("ggplot2")
install.packages("seriation")

library(Rneo4j)
library(ggplot2)
library(seriation)

graph = startGraph("http://localhost:7474/db/data/")

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

// as percentage

query = "MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
	       OPTIONAL MATCH path = (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)

         WITH group1, group2, COLLECT(path) AS paths

		     WITH group1, group2, LENGTH(paths) as commonMembers
  	     MATCH (group1)<-[:MEMBER_OF]-(group1Member)

  	     WITH group1, group2, commonMembers, COLLECT(id(group1Member)) AS group1Members
  	     MATCH (group2)<-[:MEMBER_OF]-(group2Member)

  	     WITH group1, group2, commonMembers, group1Members, COLLECT(id(group2Member)) AS group2Members
  	     WITH group1, group2, commonMembers, group1Members, group2Members

  	     UNWIND(group1Members + group2Members) AS combinedMember
  	     WITH DISTINCT group1, group2, commonMembers, combinedMember

  	     WITH group1, group2, commonMembers, COUNT(combinedMember) AS combinedMembers

  	     RETURN group1.name, group2.name, toInt(round(100.0 * commonMembers / combinedMembers)) AS percentage

  	     ORDER BY group1.name, group1.name"

group_overlap = cypher(graph, query)

ggplot(group_overlap, aes(x=group1.name, y=group2.name, fill=percentage)) + 
  geom_bin2d() +
  geom_text(aes(label = percentage)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

