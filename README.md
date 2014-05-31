# Analysing meetups using Neo4j

This is a Clojure library + app for analysing meetup data.

## Usage

Create a directory for the data:

````
mkdir data
````

Download all the data:

````
lein run -m neo4j-meetup.core
````

Import the data into Neo4j:

````
lein run -m neo4j-meetup.import 2014-05-31
````

You need to pass in the date when you downloaded the data as a parameter. In this case my most recent data set is on 31st May 2014 so I'll import that.

## License

Copyright © 2014 Mark Neehdham

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
