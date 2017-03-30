# travel_optimizer
Solution to the classic traveling salesman problem using clustering and the Google Maps API

This script came out of a request to analyze the possibility of having four employees based in
different locations across North Carolina attend meetings in the same place at the same time.
Instead of just writing the code once, I decided to make it reproducible. This is the result.
The code does the following:
* Downloads necessary information from Google APIs for declared origins and destinations
* Maps the data
* Generates a plan with optmized routes
The resulting code could easily be adapted to other similar situations or applied at a larger scale.
(Google APIs limit the number of elements per call on the API.)

This is one of the first R scripts I pulled together, and one that I am still very proud of. 
