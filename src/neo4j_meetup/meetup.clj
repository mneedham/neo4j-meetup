(ns neo4j-meetup.meetup
  (:require [neo4j-meetup.db :as db]
            [neo4j-meetup.timestamp :as timestamp]))

(defn extract-date-time [timestamp]
  { :formatted-time (timestamp/as-time timestamp)
    :formatted-date (timestamp/as-date timestamp) })

(defn all-events [meetup-name]
  (let [query "MATCH (event:Event)-[:HELD_AT]->(venue)
               OPTIONAL MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD]-(person)
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH event, venue, COLLECT({rsvp: rsvp, initial:initial, person:person}) AS responses
               RETURN event, 
                      venue, 
                      LENGTH([response in responses
                                WHERE response.initial is null
                                AND response.rsvp.response = 'yes']) AS attendees,
                      LENGTH([response in responses
                                WHERE NOT response.initial is null]) AS dropouts"]
    (->>
     (db/cypher query)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset))))))))

(defn event [event-id]
  (let [query "MATCH (event:Event {id: {eventId}})-[:HELD_AT]->(venue)
               OPTIONAL MATCH (event)<-[:TO]-(rsvp)<-[:RSVPD]-(person)
               WITH event, venue, rsvp, person
               ORDER BY rsvp.time
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH event,
                    venue,
                    COLLECT({rsvp: rsvp, initial: initial, person: person}) AS responses
               RETURN event,
                      venue,
                      [response in responses WHERE response.initial is null
                                             AND response.rsvp.response = 'yes'] as attendees,
                      [response in responses WHERE not response.initial is null] as dropouts
"
        params {:eventId event-id}]
    (->>
     (db/cypher query params)
     (map #(merge %  (extract-date-time
                      (+ (-> % :event :data :time) (-> % :event :data :utc_offset)))))
     first)))

(defn all-members []
  (let [ query "MATCH (profile:MeetupProfile)
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
  (let [query "MATCH (member:MeetupProfile {id: {memberId}})
               OPTIONAL MATCH (member)-[:INTERESTED_IN_TOPIC]->(topic)
               WITH member, COLLECT(topic) as topics
               OPTIONAL MATCH (member)-[:RSVPD]->(rsvp)-[:TO]-(event)
               OPTIONAL MATCH (rsvp)<-[:NEXT]-(initial)
               WITH member, rsvp, event, initial, topics
               ORDER BY event.time
               RETURN member, COLLECT({rsvp: rsvp, initial:initial, event:event}) AS rsvps, topics"
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
