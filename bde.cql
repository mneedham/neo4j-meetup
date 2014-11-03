match (p:MeetupProfile)-[:RSVPD]->({response: "yes"})-[:TO]->(event),
      (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-(other)
RETURN p.name, other.name, event.name;

// the social network of NoSQL London attendees
match (p:MeetupProfile)-[:RSVPD]->({response: "yes"})-[:TO]->(event),
      (event)<-[:TO]-({response: "yes"})<-[:RSVPD]-(other)
WHERE ID(p) < ID(other)
RETURN p.name, other.name, COUNT(*) AS times
