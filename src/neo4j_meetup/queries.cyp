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