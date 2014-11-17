**network_diameter.f90** accepts an input data file containing undirected node to node pairs, in the format:

<NODE1>,<NODE2>
...
...

EXAMPLES: **testdata.dat** or **networkdata.dat**

And outputs:
1) Number of node-node pairs found
2) Number of unique nodes found
3) An initial adjacency matrix
4) The final adjacency matrix
5) Diameter of the network

EXAMPLES: testdata_output and networkdata_output

The adjacency matrix is multiplied with itself repeatedly until all of its elements have reached a positive value AT LEAST ONCE. The number of times that self-multiplication must occur for this to happen is the **diameter**. 

