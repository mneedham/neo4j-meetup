(ns neo4j-meetup.meetup
  (:require [neo4j-meetup.db :as db]
            [neo4j-meetup.timestamp :as timestamp]))

(defn extract-date-time [timestamp]
  { :formatted-time (timestamp/as-time timestamp)
    :formatted-date (timestamp/as-date timestamp) })

(defn group [group-id]
  (let [query "
                MATCH (m)-[:MEMBER_OF]->(group:Group {id: {groupId}})
                OPTIONAL MATCH (m)-[:MEMBER_OF]->(other)
                WITH m, COUNT(m) as members, group
                WITH group, COLLECT({member:m, groups: members}) AS memberGroups
                WITH group,
                       LENGTH([x in memberGroups WHERE x.groups = 1]) AS thisGroupOnly,
                       LENGTH([x in memberGroups  WHERE x.groups > 1]) AS otherGroups
                RETURN group, thisGroupOnly, otherGroups,
                       (thisGroupOnly + otherGroups) AS members       
"
        params {:groupId (read-string group-id)}]
    (->>
     (db/cypher query params)
     first)))

(defn group-topics [group-id]
  (let [query "                
                MATCH (m)-[:MEMBER_OF]->(group:Group {id: {groupId}})

                WITH COLLECT(m) AS members, group
                UNWIND members AS m
                MATCH (m)-[:INTERESTED_IN]-(topic)

                WITH topic,
                     COUNT(*) as count,
                     LENGTH(members) AS numberOfMembers,
                     CASE
                       WHEN LENGTH((group)-[:HAS_TOPIC]->(topic)) = 0 THEN false
                       ELSE true
                       END AS groupHasTopic,
                     10^2 AS factor
                RETURN topic,
                       count,
                       round((count * 100.0 / numberOfMembers) * factor) / factor AS percentage,
                       groupHasTopic
                ORDER BY count DESC
                LIMIT 50

"
        params {:groupId (read-string group-id)}]
    (->>
     (db/cypher query params)
     )))

(defn all-events [meetup-id]
  (let [query "MATCH (event:Event)-[:HELD_AT]->(venue)
               OPTIONAL MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD]-(person)
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH event, venue, COLLECT({rsvp: rsvp, initial:initial, person:person}) AS responses
               WITH event,
                    venue,
                    [response in responses WHERE response.initial is null AND response.rsvp.response = 'yes'] AS a,
                    [response in responses WHERE NOT response.initial is null] AS d
               RETURN event,
                      venue,
                      LENGTH(a) + REDUCE(acc=0, count IN [value IN a | value.rsvp.guests] | acc + count) as attendees,
                      LENGTH(d) + REDUCE(acc=0, count IN [value IN d | value.rsvp.guests] | acc + count) as dropouts

                      "]
    (->>
     (db/cypher query)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset))))))))

(defn event [event-id]
  (let [query "MATCH (event:Event {id: {eventId}})-[:HELD_AT]->(venue)
               OPTIONAL MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD]-(person)
               OPTIONAL MATCH (person)-[:INTERESTED_IN]->(topic)
               WHERE ()-[:HAS_TOPIC]->(topic)
               WITH event, venue, rsvp, person, COLLECT(topic) as topics
               ORDER BY rsvp.time
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH event,
                    venue, 
                    COLLECT({rsvp: rsvp, initial: initial, person: person, topics: topics}) AS responses
               WITH event,
                    venue,
                    [response in responses WHERE response.initial is null
                                           AND response.rsvp.response = 'yes'] as attendees,
                    [response in responses WHERE not response.initial is null] as dropouts,
                    responses
               UNWIND([response in attendees | response.topics]) AS topics
               UNWIND(topics) AS topic
               WITH event, venue, attendees, dropouts, {id: topic.id, name:topic.name, freq:COUNT(*)} AS t
               RETURN event, venue, attendees, dropouts, COLLECT(t) AS topics
"
        params {:eventId event-id}]
    (->>
     (db/cypher query params)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset)))))
     first)))

(defn all-groups []
  (let [ query "
                MATCH (m:MeetupProfile)
                WITH COUNT(m) AS allMembers
                MATCH (m)-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(group:Group)
                WITH group, count(m) as members, collect(membership.joined) as memberships, allMembers
                RETURN group,
                       members,
                       round(members * 10000.0 / allMembers) / 100 as percentage,
                       memberships
                ORDER BY group.name
                "]
    (->> (db/cypher query {}))))


(defn group-overlap []
  (let [ query "
                MATCH (g1:Group), (g2:Group)
                OPTIONAL MATCH path = (g1)<-[:MEMBER_OF]-()-[:MEMBER_OF]->(g2)

                WITH g1, g2, CASE WHEN path is null THEN 0 ELSE COUNT(path) END AS overlap
                ORDER BY g1.name, g2.name

                RETURN g1, COLLECT(overlap) AS overlap
                ORDER BY g1.name
                "]
    (->> (db/cypher query {}))))


(defn all-members []
  (let [ query "MATCH (profile:MeetupProfile)-[:MEMBER_OF]->(g:Group {name: 'Neo4j - London User Group'})
                OPTIONAL MATCH (profile)-[r:RSVPD]->(rsvp {response: 'yes'})-[:TO]->(e)
                WHERE e.time < timestamp()
                WITH profile, rsvp, e
                ORDER BY profile.name, e.time DESC
                RETURN profile,
                       COUNT(rsvp) as rsvps,
                       COLLECT({event: e, rsvp: rsvp})[0] AS recent
                ORDER BY profile.name
                "]
    (->> (db/cypher query {}))))

(defn member [member-id]
  (let [query "

               MATCH (member:MeetupProfile {id: {memberId}})
               OPTIONAL MATCH (member)-[:HAS_MEMBERSHIP]->(membership)-[:OF_GROUP]->(group)
               WITH member, group, membership
               ORDER BY membership.joined

               WITH member, COLLECT({group: group, membership: membership}) AS groups
               OPTIONAL MATCH (member)-[:INTERESTED_IN]->(topic)
               WITH member, COLLECT(topic) as topics, groups
               OPTIONAL MATCH (member)-[:RSVPD]->(rsvp)-[:TO]-(event)
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH member, rsvp, event, initial, topics, groups           
               ORDER BY event.time
               
               RETURN member, 
                      COLLECT({rsvp: rsvp, initial:initial, event:event}) AS rsvps, topics, groups
"
        params {:memberId (read-string member-id)}]
    (->>
     (db/cypher query params)
     first)))

(defn venue [venue-id]
  (let [query "MATCH (venue:Venue {id: {venueId}})<-[:HELD_AT]-(meetup)
               WITH venue, meetup
               ORDER BY meetup.time
               RETURN venue, COLLECT(meetup) AS meetups"
        params {:venueId (read-string venue-id)}]
    (->>
     (db/cypher query params)
     first)))
