* Find and fix TODO comments
* Check layout correctness: 
    * Switches should not have common edges
    * all edges have data
    * all edges referred to in switches exits
    * all tracks lead somewhere, even if it's a track end or map exit
    * Map exit end nodes should only have one incoming edge (which is infinitely far out)
* Implement partially visible train cars.
* Don't stop simulation if no trains is visible but at least one is approaching from a map exit.
* Fix drawing of reverse curves and straights
* Train location is maybe. Make "off map" a valid location.