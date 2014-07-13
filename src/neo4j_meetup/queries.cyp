// find the number of members / guests coming to each meetup
MATCH (e:Event)
OPTIONAL MATCH (e)<-[:TO]-(rsvp {response: "yes"})
return e.name, e.time, count(rsvp) as members, sum(rsvp.guests) AS guests
ORDER BY (members + guests) DESC

// events attended 
MATCH (e:Event)<-[:TO]-(rsvp {response: "yes"})<-[:RSVPD]-(person)
return person.name, count(rsvp) as timesAttended
ORDER BY timesAttended DESC

// what was the last meetup they attended before this one
match (e:Event {id: "167264852"})<-[:TO]-({response: "yes"})-[:RSVPD]-(person)
match (person)-[:RSVPD]->({response: "yes"})-[:TO]->(otherEvent)
WHERE otherEvent <> e AND otherEvent.time < timestamp()
WITH person, otherEvent
ORDER BY person.name, otherEvent.time
RETURN person.name, COUNT(otherEvent) AS count, COLLECT(otherEvent.name)[-1]
ORDER BY count DESC 

// events held at each venue
MATCH (v:Venue)<-[:HELD_AT]-(event)<-[:TO]-(rsvp {response: "yes"})
WHERE event.time < timestamp()
WITH v, COUNT(DISTINCT event) as events, COUNT(rsvp) AS rsvps
RETURN v.name AS venue, events, rsvps, rsvps / events AS rsvpsPerEvent
ORDER BY events DESC

// visualise events at each venue
MATCH (v:Venue)<-[:HELD_AT]-(event)<-[:HOSTED_EVENT]-(group)
OPTIONAL MATCH v-[:ALIAS_OF]->(v2)
RETURN v, event, group

// create time tree

WITH 2011 as startYear
WITH startYear, range(startYear, 2014) AS years, range(1,12) as months
FOREACH(year IN years | 
  MERGE (y:Year {year: year})
  FOREACH(month IN months | 
    CREATE (m:Month {month: month})
    MERGE (m)-[:PART_OF]->(y)
    FOREACH(day IN (CASE 
                      WHEN month IN [1,3,5,7,8,10,12] THEN range(1,31) 
                      WHEN month = 2 THEN 
                        CASE
                          WHEN year % 4 <> 0 THEN range(1,28)
                          WHEN year % 100 <> 0 THEN range(1,29)
                          WHEN year % 400 THEN range(1,29)
                          ELSE range(1,28)
                        END
                      ELSE range(1,30)
                    END) |      
      CREATE (d:Day {day: day})
      MERGE (d)-[:PART_OF]->(m)
    )
  )
)

WITH range(2011, 2014) AS years, range(1,12) as months
FOREACH(year IN years | 
  MERGE (y:Year {year: year})
  FOREACH(month IN months | 
    CREATE (m:Month {month: month})
    MERGE (y)-[:HAS_MONTH]->(m)
    FOREACH(day IN (CASE 
                      WHEN month IN [1,3,5,7,8,10,12] THEN range(1,31) 
                      WHEN month = 2 THEN 
                        CASE
                          WHEN year % 4 <> 0 THEN range(1,28)
                          WHEN year % 100 <> 0 THEN range(1,29)
                          WHEN year % 400 <> 0 THEN range(1,29)
                          ELSE range(1,28)
                        END
                      ELSE range(1,30)
                    END) |      
      CREATE (d:Day {day: day})
      MERGE (m)-[:HAS_DAY]->(d))))

WITH *

MATCH (year:Year)-[:HAS_MONTH]->(month)-[:HAS_DAY]->(day)
WITH year,month,day
ORDER BY year.year, month.month, day.day
WITH collect(day) as days
FOREACH(i in RANGE(0, length(days)-2) | 
    FOREACH(day1 in [days[i]] | 
        FOREACH(day2 in [days[i+1]] | 
            CREATE UNIQUE (day1)-[:NEXT]->(day2))))

// get the previous 5 days 
MATCH (y:Year {year: 2014})-[:HAS_MONTH]->(m:Month {month: 2})-[:HAS_DAY]->(:Day {day: 1})<-[:NEXT*0..5]-(day)
RETURN y,m,day

// which events have the most drop outs
MATCH ()-[:INITIALLY_RSVPD]->(rsvp {response: "yes"})-[:TO]->(event)
MATCH (rsvp)-[:NEXT]->(rsvp2 {response: "no"})
RETURN event.id, event.name, COUNT(rsvp) AS rsvps
ORDER BY rsvps DESC

// events with the most dropouts

MATCH ()-[:INITIALLY_RSVPD]->(rsvp {response: "yes"})-[:TO]->(event)
MATCH (rsvp)-[:NEXT]->({response: "no"})
WITH event, COUNT(rsvp) AS dropouts
MATCH (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-()
RETURN event.id, event.name, COUNT(event) as finalRsvps, dropouts
ORDER BY dropouts DESC

// dropouts in intro to graphs
MATCH ()-[:INITIALLY_RSVPD]->(rsvp {response: "yes"})-[:TO]->(event {name: "Intro to Graphs"})
MATCH (rsvp)-[:NEXT]->({response: "no"})
WITH event, COUNT(rsvp) AS dropouts
MATCH (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-()
RETURN event.id, event.name, COUNT(event) as finalRsvps, dropouts
ORDER BY dropouts DESC

// attendees at events
MATCH ()-[:INITIALLY_RSVPD]->(rsvp {response: "yes"})-[:TO]->(event)
MATCH (rsvp)-[:NEXT]->({response: "no"})
WITH event, COUNT(rsvp) AS dropouts
MATCH (event)<-[:TO]-(rsvp {response: "yes"})<-[:RSVPD]-()
WITH event, COUNT(event) as finalRsvps, SUM(rsvp.guests) AS guests, dropouts
RETURN event.id, event.name, event.time, finalRsvps + guests AS potentialAttendees, dropouts
ORDER BY dropouts DESC

// when do people change their response
MATCH (meetup)-[:INITIALLY_RSVPD]->(rsvp1 {response: "yes"})-[:NEXT]->(rsvp2 {response: "no"})-[:TO]->(event)
MATCH (event)-[:HAPPENED_ON]->(ed1)<-[:HAS_DAY]-(em1)<-[:HAS_MONTH]-(ey1)
MATCH (rsvp1)-[:HAPPENED_ON]->(d1)<-[:HAS_DAY]-(m1)<-[:HAS_MONTH]-(y1)
MATCH (rsvp2)-[:HAPPENED_ON]->(d2)<-[:HAS_DAY]-(m2)<-[:HAS_MONTH]-(y2)
MATCH path = (d2)<-[:NEXT*]-(d1)
RETURN meetup.name, 
       event.name, 
       ed1.day + "/" +  em1.month + "/" + ey1.year AS eventDate,
       d1.day + "/" +  m1.month + "/" + y1.year AS initialYes,
       d2.day + "/" +  m2.month + "/" + y2.year AS changedToNo,
       LENGTH(path) AS days
ORDER BY days DESC

// changed response within 100 days of initial response
MATCH (meetup:MeetupProfile)-[:INITIALLY_RSVPD]->(rsvp1 {response: "yes"})-[:NEXT]->(rsvp2 {response: "no"})-[:TO]->(event)
MATCH (event)-[:HAPPENED_ON]->(ed1)<-[:HAS_DAY]-(em1)<-[:HAS_MONTH]-(ey1)
MATCH (rsvp1)-[:HAPPENED_ON]->(d1)<-[:HAS_DAY]-(m1)<-[:HAS_MONTH]-(y1)
MATCH (rsvp2)-[:HAPPENED_ON]->(d2)<-[:HAS_DAY]-(m2)<-[:HAS_MONTH]-(y2)
WITH extract(p in (d2)<-[:NEXT*..100]-(d1) | length(p))[0] AS days,
     meetup, 
     event, 
     ed1.day + "/" +  em1.month + "/" + ey1.year AS eventDate,
     d1.day + "/" +  m1.month + "/" + y1.year AS initialYes,
     d2.day + "/" +  m2.month + "/" + y2.year AS changedToNo
WHERE NOT days is null
RETURN meetup.name, event.name, eventDate, initialYes, changedToNo, days       
ORDER BY days DESC

// using shortest path
MATCH (meetup:MeetupProfile)-[:INITIALLY_RSVPD]->(rsvp1 {response: "yes"})-[:NEXT]->(rsvp2 {response: "no"})-[:TO]->(event)
MATCH (event)-[:HAPPENED_ON]->(ed1)<-[:HAS_DAY]-(em1)<-[:HAS_MONTH]-(ey1)
MATCH (rsvp1)-[:HAPPENED_ON]->(d1)<-[:HAS_DAY]-(m1)<-[:HAS_MONTH]-(y1)
MATCH (rsvp2)-[:HAPPENED_ON]->(d2)<-[:HAS_DAY]-(m2)<-[:HAS_MONTH]-(y2)
WITH length(shortestpath((d2)<-[:NEXT*..100]-(d1))) AS days,
meetup, 
event, 
ed1.day + "/" + em1.month + "/" + ey1.year AS eventDate,
d1.day + "/" + m1.month + "/" + y1.year AS initialYes,
d2.day + "/" + m2.month + "/" + y2.year AS changedToNo
WHERE NOT days is null
RETURN meetup.name, event.name, eventDate, initialYes, changedToNo, days
ORDER BY days DESC

// topics in common 

MATCH (n:Group {name: "Neo4j - London User Group"} ), (m:Group)
MATCH path = (n)-[:HAS_TOPIC]->(topic)<-[:HAS_TOPIC]-(m)
RETURN n.name, m.name, COLLECT(topic.name)
 
// popular topics of other meetups
MATCH (neo:Group {name: "Neo4j - London User Group"} ), (other:Group)
MATCH (other)-[:HAS_TOPIC]->(topic)
WHERE NOT ((neo)-[:HAS_TOPIC]->(topic))
RETURN topic.name, COUNT(*) AS appearances
ORDER BY appearances DESC

// show which groups have which
MATCH (neo:Group {name: "Neo4j - London User Group"} ), (other:Group)
MATCH (other)-[:HAS_TOPIC]->(topic)
WHERE NOT ((neo)-[:HAS_TOPIC]->(topic))
RETURN topic.name, COLLECT(other.name),  COUNT(*) AS appearances
ORDER BY appearances DESC

// nosql london groups
MATCH (m:MeetupProfile)
WITH COUNT(m) AS allMembers
MATCH (m:MeetupProfile)-[:MEMBER_OF]->(group)
WITH group, count(m) as members, allMembers
RETURN group.name, members, round(members * 10000.0 / allMembers) / 100 as percentage
ORDER BY members DESC
LIMIT 10

// neo only
MATCH (m:MeetupProfile)-[:MEMBER_OF]->(group {name: "Neo4j - London User Group"})
OPTIONAL MATCH (m)-[:MEMBER_OF]->(other)
WITH m, COUNT(m) as members
WITH COLLECT({member:m, groups: members}) AS memberGroups
RETURN LENGTH([x in memberGroups WHERE x.groups = 1]) AS neoOnly,
       LENGTH([x in memberGroups  WHERE x.groups > 1]) AS others

// overlap between the groups
MATCH (group1:Group), (group2:Group)
MATCH (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)
WHERE id(group1) > id(group2)
RETURN group1.name, group2.name, COUNT(*) as commonMembers
ORDER BY commonMembers DESC
LIMIT 10       

// adjacency matrix of overlap
MATCH (g1:Group), (g2:Group)
OPTIONAL MATCH path = (g1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(g2)

WITH g1, g2, CASE WHEN path is null THEN 0 ELSE COUNT(path) END AS overlap
ORDER BY g1.name, g2.name

RETURN g1.name, COLLECT(overlap) AS overlap
ORDER BY g1.nameMATCH (g1:Group), (g2:Group)
OPTIONAL MATCH path = (g1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(g2)

WITH g1, g2, CASE WHEN path is null THEN 0 ELSE COUNT(path) END AS overlap
ORDER BY g1.name, g2.name

RETURN g1.name, COLLECT(overlap) AS overlap
ORDER BY g1.name

// adjacency matrix of top 5
MATCH (g:Group)<-[:MEMBER_OF]-()
WITH g, COUNT(*) as members
ORDER BY members DESC
LIMIT 5

WITH COLLECT(g) AS groups
UNWIND groups AS g1
UNWIND groups AS g2

OPTIONAL MATCH path = (g1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(g2)

WITH g1, g2, CASE WHEN path is null THEN 0 ELSE COUNT(path) END AS overlap
ORDER BY g1.name, g2.name

RETURN g1.name, COLLECT(overlap)
ORDER BY g1.name

// overlap with the neo4j group 
MATCH (group1:Group {name: "Neo4j - London User Group"}), (group2:Group)
MATCH (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)
RETURN group1.name, group2.name, COUNT(*) as commonMembers
ORDER BY commonMembers DESC


// overlap + % of total joins
MATCH (group1:Group {name: "Neo4j - London User Group"}), (group2:Group)
MATCH (group1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(group2)

WITH group1, group2, COUNT(*) as commonMembers
MATCH (group1)<-[:MEMBER_OF]-(group1Member)

WITH group1, group2, commonMembers, COLLECT(id(group1Member)) AS group1Members
MATCH (group2)<-[:MEMBER_OF]-(group2Member)

WITH group1, group2, commonMembers, group1Members, COLLECT(id(group2Member)) AS group2Members
WITH group1, group2, commonMembers, group1Members, group2Members

UNWIND(group1Members + group2Members) AS combinedMember
WITH DISTINCT group1, group2, commonMembers, combinedMember

WITH group1, group2, commonMembers, COUNT(combinedMember) AS combinedMembers

RETURN group1.name, group2.name, commonMembers, combinedMembers, round(10000.0 * commonMembers / combinedMembers) / 100 as percentage

ORDER BY percentage DESC

// other groups with our keywords
MATCH (:Group {name: "Neo4j - London User Group"})-[:HAS_TOPIC]->(topic)<-[:HAS_TOPIC]-(other)

WITH  other, COUNT(*) AS freq, COLLECT(topic.name) AS topics
MATCH (other)<-[:MEMBER_OF]-()

RETURN other.name, freq, topics, COUNT(*) AS members
ORDER BY freq DESC, members DESC

// create relationship between adjacent memberships
MATCH (membership:Membership)<-[:HAS_MEMBERSHIP]-(meetupProfile)

WITH meetupProfile, membership
ORDER BY id(meetupProfile), membership.joined

WITH meetupProfile, COLLECT(membership) AS memberships

UNWIND reduce(acc=[], x in range(1,length(memberships) -1) | acc+ { one: memberships[x-1], two: memberships[x]}) AS pair 
WITH pair.one AS p1, pair.two AS p2
CREATE (p1)-[:NEXT]->(p2)

// what groups do people join consecutively
MATCH (group1:Group), (group2:Group)
MATCH path = (group1)<-[:OF_GROUP]-(membership)-[:NEXT]->(nextMembership)-[:OF_GROUP]->(group2)
RETURN group1.name, group2.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC
LIMIT 20

// what was after neo?

MATCH (group1:Group {name: "Neo4j - London User Group"}), (group2:Group)
OPTIONAL MATCH path = (group1)<-[:OF_GROUP]-(membership)-[:NEXT]->(nextMembership)-[:OF_GROUP]->(group2)
RETURN group2.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC
LIMIT 20

// what was before neo?

MATCH (group1:Group), (group2:Group {name: "Neo4j - London User Group"})
OPTIONAL MATCH path = (group1)<-[:OF_GROUP]-(membership)-[:NEXT]->(nextMembership)-[:OF_GROUP]->(group2)
RETURN group1.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC
LIMIT 20

// create relationship between adjacent memberships
MATCH (membership:Membership)<-[:HAS_MEMBERSHIP]-(meetupProfile)

WITH meetupProfile, membership
ORDER BY id(meetupProfile), membership.joined

WITH meetupProfile, COLLECT(membership) AS memberships

UNWIND reduce(acc=[], x in range(1,length(memberships) -1) | acc+ { one: memberships[x-1], two: memberships[x]}) AS pair 
WITH pair.one AS p1, pair.two AS p2
CREATE (p1)-[:NEXT]->(p2)

// what groups do people join consecutively

MATCH (group1:Group {name: "Neo4j - London User Group"}), (group2:Group)
OPTIONAL MATCH path = (group1)<-[:OF_GROUP]-(membership)-[:NEXT]->(nextMembership)-[:OF_GROUP]->(group2)
RETURN group1.name, group2.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC
LIMIT 20

// people joining neo first

MATCH (group1:Group {name: "Neo4j - London User Group"}), (group2:Group)
OPTIONAL MATCH path = (group1)<-[:OF_GROUP]-(membership)
OPTIONAL MATCH shortestpath((membership)-[:NEXT*..5]->(nextMembership))
OPTIONAL MATCH (nextMembership)-[:OF_GROUP]->(group2)
RETURN group2.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC
LIMIT 20

// people joining neo second

MATCH (group1:Group), (group2:Group {name: "Neo4j - London User Group"})
OPTIONAL MATCH path = (group1)<-[:OF_GROUP]-(membership)-[:NEXT]->(nextMembership)-[:OF_GROUP]->(group2)
RETURN group1.name, COUNT(path) AS count, AVG(nextMembership.joined - membership.joined) / 1000 / 60 / 60 / 24 AS timeDiff
ORDER BY count DESC

// 

MATCH (m)-[:MEMBER_OF]->(group:Group {id: {groupId}}),
      (m)-[:INTERESTED_IN]-(topic)
RETURN topic, COUNT(*) as count
ORDER BY count DESC
LIMIT 50


// 

MATCH (g:Group)
WHERE g.id IN {ids}

WITH COLLECT(g) AS groups
UNWIND groups AS g1
UNWIND groups AS g2

WITH g1, g2 WHERE g1 <> g2

OPTIONAL MATCH path = (g1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(g2) WHERE id(g1) < id(g2)

WITH g1, g2, COUNT(path) AS overlap
ORDER BY id(g1)


WITH g1, [o IN COLLECT({group: g2.name, overlap: overlap }) WHERE o.overlap > 0] AS others

MATCH (g1)<-[:MEMBER_OF]-()

RETURN g1.name, id(g1), COUNT(*) AS members, others 
ORDER BY id(g1)

