match (p:MeetupProfile)-[:RSVPD]->({response: "yes"})-[:TO]->(event),
      (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-(other)
RETURN p.name, other.name, event.name;

// the social network of NoSQL London attendees
match (p:MeetupProfile)-[:RSVPD]->({response: "yes"})-[:TO]->(event),
      (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-(other)
WHERE ID(p) < ID(other)
RETURN p.name, other.name, COUNT(*) AS times

// create paths of joining groups
match (person:MeetupProfile)
match (group)<-[:OF_GROUP]-(membership)<-[:HAS_MEMBERSHIP]-(person)
WITH person, membership ORDER BY membership.joined
WITH person, collect(membership) AS memberships
FOREACH(i in RANGE(0, length(memberships)-2) |
    FOREACH(si in [memberships[i]] |
        FOREACH(si2 in [memberships[i+1]] |
            MERGE (si)-[:NEXT]->(si2))))

// find Mark's joining order

match (m:MeetupProfile {name: "Mark Needham"})-[:HAS_MEMBERSHIP]->(membership)
WITH membership LIMIT 1
MATCH path = (membership)<-[:NEXT*]-(previous)
WHERE NOT (previous)<-[:NEXT]-()
WITH NODES(path) AS memberships
UNWIND memberships AS membership
MATCH (membership)-[:OF_GROUP]->(group)
RETURN COLLECT(group.name)

// what did people join before Neo4j

match (group:Group {name: "Neo4j - London User Group"})<-[:OF_GROUP]-(membership)
WHERE (membership)<-[:NEXT]-()
WITH membership
MATCH path = (other)-[:NEXT*]->(membership) WHERE NOT (other)<-[:NEXT]-()
WITH membership AS initialMembership, NODES(path) AS memberships
UNWIND memberships AS membership
MATCH (membership)-[:OF_GROUP]->(group)
WITH initialMembership, COLLECT(group.name) AS groups
RETURN groups, COUNT(*) AS times
ORDER BY LENGTH(groups) DESC
