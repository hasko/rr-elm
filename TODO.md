* Train location is maybe. Make "off map" a valid location.
* Find and fix TODO comments
* Model has only one train now
* Check layout correctness: 
    * Switches should not have common edges
    * all edges have data
    * all edges referred to in switches exits
    * all tracks lead somewhere, even if it's a track end or map exit
    * Map exit end nodes should only have one incoming edge (which is infinitely far out)
* Implement partially visible train cars.
* Stop simulation if all trains are invisible and none is approaching from a map exit.
* Fix drawing of reverse curves and straights