# CeConn-ColorMotifs
The aim of this tool is to evaluate directed and signed 3-node subgraphs of the Caenorhabditis elegans connectome (and in random networks for comparison).

- motifdef.csv : definition of motifs. 
  - If we name the nodes ‘A’, ‘B’ and ‘C’, the 6-digit code representes edges in the following order: A->B, B->A, B->C, C->B, C->A, A->C
  - Different number correspond to different edge ‘colours’
    - ‘1’ is a virtual colour, it means that there is no connection in that direction
    - ‘2’ means that the synapse is excitatory
    - ‘3’ means that the synapse is inhibitory
    - ‘4’ means that the polarity of the synapse is unknown
  - The name of the motifs consists a letter and a number
    - The letter shows the uncolored structure of the motif
    - The number shows the exact coloring of the motif
  - Rotational and reflectional symmetries were considered as the same motif
- triplets.csv : triplets in the C. elegans connectome
 -  3 node of the connectome is considered as a triplet, if none of them is isolated from the other two
 - Neurons are represented by 3-digit number from 101 to 402
   - Therefore triplets are represented by 9-digit numbers (the codes of the 3 neurons combined)
- motifnames.csv : names of motifs defined in motifdef.csv
- edges.csv : list of signed edges in the connectome and 1 000 random networks
  - Neurons are represented as 3-digit numbers (see above), therefore edges are coded as 6-digit numbers (the combination of the pre- and postsynaptic neuron code)
  - Edge-colours are coded from 2 to 4 (see above)
  - Connectome polarity data from Fenyves et al. 2020
- inducedfinder.py: this algorithm enumerates induced subgraphs in the connectome and random networks using the data described above
- inducedresults.csv: Induced motifs in the connectome and 1 000 random networks
  - Data from inducedfinder.py
- translator.csv: list of partial subgraphs in each induced motif
- partialtranslator.py: this algorithm translates the induced results to partial motif counts (using translator.csv)
- partialresults.csv: Partial motifs in the connectome and 1 000 random networks
  - Data from partialtranslator.py
