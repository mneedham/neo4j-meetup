Members and Groups
========================================================

Now we want to find out how the members of different groups overlap.


```r
library(RNeo4j)
library(dplyr)
library(ggplot2)
```


```r
graph = startGraph("http://localhost:7474/db/data/")
options(width = 200)
```

This time we're only interested in a very specific part of the graph:

<img src="images/overlap.png" width="500" />

In the following query we take every pair of groups and calculate how many common members they have:


```r
query = "CYPHER 2.2-rule MATCH (group1:Group), (group2:Group)
         WHERE group1 <> group2
         OPTIONAL MATCH p = (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)
         WITH group1, group2, COLLECT(p) AS paths
         RETURN group1.name, group2.name, LENGTH(paths) as commonMembers
         ORDER BY group1.name, group2.name"

group_overlap = cypher(graph, query)

pickRandomRows = function(df, numberOfRows = 10) {
  df %>% slice(as.integer(runif(numberOfRows,0, length(df[,1]))))
}

pickRandomRows(group_overlap, 10)
```

```
##                                                  group1.name                                               group2.name commonMembers
## 1                                           Couchbase London                                               Hive London            25
## 2                                    The Data Scientist - UK                                       Data Science London           172
## 3                     Marklogic Financial Services Community                                  Scale Warriors of London             1
## 4                                   Scale Warriors of London        The London Distributed Graph Database Meetup Group             3
## 5                   HPC & GPU Supercomputing Group of London                                  Scale Warriors of London             0
## 6                                  Neo4j - London User Group London Actionable Behavioral Analytics for Web and Mobile            24
## 7                            London ElasticSearch User Group                                   Big Data Jobs in London            28
## 8           TokuMX London - Super-charging MongoDB in London                                     Hadoop Users Group UK             5
## 9                                  London MongoDB User Group                    Marklogic Financial Services Community             6
## 10 London Actionable Behavioral Analytics for Web and Mobile                  Span: scalable and distributed computing             2
```


```r
ggplot(group_overlap, aes(x=group1.name, y=group2.name, fill=commonMembers)) + 
  geom_bin2d() +
  geom_text(aes(label = commonMembers)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

That shows absolute values but what's more interesting is what % of a groups members are members of other groups. We could then learn more about potential joint meetups or topics that the group may find interesting.



```r
query = "CYPHER 2.2-rule MATCH (group1:Group), (group2:Group)
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
pickRandomRows(group_overlap_percentage)
```

```
##                                         group1.name                              group2.name percentage
## 1                           Data Enthusiasts London              Big Data Week London Meetup          4
## 2                       Big Data Week London Meetup           London PostgreSQL Meetup Group          2
## 3                                      Redis London                          Big Data London         22
## 4                   Enterprise Search London Meetup                       London Riak Meetup          2
## 5  TokuMX London - Super-charging MongoDB in London                              Hive London          0
## 6            Marklogic Financial Services Community                          GridGain London          0
## 7                           Big Data Jobs in London              MarkLogic User Group London          2
## 8                                      Redis London Span: scalable and distributed computing          1
## 9  TokuMX London - Super-charging MongoDB in London         Oracle Big Data 4 the Enterprise          0
## 10                                      Hive London                            Women in Data          5
```


```r
ggplot(group_overlap_percentage, aes(x=group2.name, y=group1.name, fill=percentage)) + 
  geom_bin2d() +
  geom_text(aes(label = percentage)) +
  labs(x= "Group", y="Group", title="Member Group Member Overlap") +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        plot.title = element_text(size = 16, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

## How many groups are people members of?


```r
query = "match (p:MeetupProfile)-[:MEMBER_OF]->()
         return ID(p), COUNT(*) AS groups
         ORDER BY groups DESC"

group_count = cypher(graph, query)
pickRandomRows(group_count)
```

```
##   ID(p) groups
## 1    NA     NA
## 2 42339      2
## 3 38874      1
## 4 35897      1
## 5 23673      1
## 6   266      1
## 7 12878      1
## 8 55134      1
## 9  9669      9
```


```r
ggplot(aes(x = groups, y = n), data = group_count %>% count(groups)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  scale_y_sqrt() +
  scale_x_continuous(breaks = round(seq(min(group_count$groups), max(group_count$groups), by = 1),1)) +
  ggtitle("Number of groups people are members of")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

It's a typical long tail curve where there's a few people who are interested in lots of different topics but most people are only interested in 1 or 2 things.

We might then look at how many groups people belong to grouped by meetup group:


```r
query = "CYPHER 2.2-rule MATCH (group:Group)
         MATCH (group)<-[:MEMBER_OF]-(member)
         WITH group, COLLECT(member) AS members
         UNWIND members AS member
         OPTIONAL MATCH (member)-[:MEMBER_OF]->(other)
         WITH group, member, COLLECT(other) AS otherGroups
         WITH group, member, LENGTH(otherGroups) AS numberOfOtherGroups
         RETURN group.name, numberOfOtherGroups, COUNT(*) AS numberOfPeople
         ORDER BY group.name, numberOfOtherGroups "
members_of_other_groups = cypher(graph, query)

summariseNumberOfMembers = function(numberOfGroups) {
  mapply(function(number) {
    if(number == 1) {
       "1";
    } else if(number >1 && number <= 5) {
       "Up to 5";
    } else if(number > 5 && number <= 10) {
       "Up to 10";
    } else {
      "More than 10";
    }
  }, numberOfGroups) 
}

members_of_other_groups$summarisedNumberOfOtherGroups = factor(summariseNumberOfMembers(members_of_other_groups$numberOfOtherGroups), 
                                                               levels = c("1", "Up to 5", "Up to 10", "More than 10"))

pickRandomRows(members_of_other_groups)
```

```
##                                       group.name numberOfOtherGroups numberOfPeople summarisedNumberOfOtherGroups
## 1             Hazelcast User Group London (HUGL)                   2             31                       Up to 5
## 2  Big Data / Data Science / Data Analytics Jobs                   1             12                             1
## 3                        Big Data Jobs in London                  17              1                  More than 10
## 4                                    Hive London                  11              7                  More than 10
## 5                                     MEAN Stack                   2             80                       Up to 5
## 6               Oracle Big Data 4 the Enterprise                  13              2                  More than 10
## 7                 London Cloud Computing / NoSQL                   6             16                      Up to 10
## 8       Span: scalable and distributed computing                   5              7                       Up to 5
## 9                                     MEAN Stack                   6              6                      Up to 10
## 10                              Couchbase London                   4             25                       Up to 5
```


```r
ggplot(data=members_of_other_groups, 
       aes(x = group.name, 
           y = numberOfPeople, 
           fill=members_of_other_groups$summarisedNumberOfOtherGroups)) + 
  geom_bar(stat="identity") +
  coord_flip() +  ylab("") + xlab("") +
  guides(fill=guide_legend(title="Number of Groups"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
