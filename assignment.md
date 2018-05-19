
## MODULE: SMART CITIES AND IoT

In the excel file you can find information related to mobility of different zones in one study area.  
In particular you have info on:
- network nodes and arcs
- coordinates of nodes and centroids
- OD Matrix
- speed-flow  

### Notes
- The cost function is defined exclusively by the trip time
- The graph of the matrix is made only by 17 centroids

### Output
- Build a complete network in R using the package Igraph; 
- Build the minimum spanning tree (for example using Dijkstra's algorithm)
- Assign incrementally the distribution matrix according to the following rule: 50% - 30% - 20% of the matrix; 
- Update  the link travel times with flow/capacity 
