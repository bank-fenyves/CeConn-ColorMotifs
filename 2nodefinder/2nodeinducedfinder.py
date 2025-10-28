# -*- coding: utf-8 -*-
"""
Created on Sun Sep  7 10:31:30 2025

@author: Szilágyi Gábor S
"""
import pandas as pd
finalresultsdf = pd.DataFrame()
from pathlib import Path
p = Path(__file__).with_name('modef2.csv')
with p.open("r") as f:
  content = f.read()
motifcodes = content.split ("\n")
motifcodes.pop()
motif_dict = {}
for motifcode in motifcodes:
    barcode, motif = motifcode.split(';')
    motif_dict[barcode] = motif
motifsdf = pd.read_csv (Path(__file__).with_name('motifnames.csv'), "r")
p = Path(__file__).with_name('neurons.csv')
with p.open("r") as f:
  content = f.read()
neuroncodes = content.split ("\n")
neuroncodes.pop()
neuron_dict = {}
for neuroncode in neuroncodes:
    neuronid, neurontype = neuroncode.split(';')
    neuron_dict[neuronid] = neurontype
motiflist = motifsdf["Motif name"].tolist()
finalresultsdf['Motif names'] = motiflist
edgesdf = pd.read_csv (Path(__file__).with_name('edges.csv'), sep=';')
edgesdf = edgesdf.astype(str)
for connumber in range (0, 1001) :
   print(connumber) 
   y = str(connumber)
   name = 'Pred_' + y
   sign_dict = edgesdf.set_index('Edgecode').to_dict()[name]
   results = []
   for a in range (101, 403):
    for b in range (101, 403):
        if a<b:
           typea = neuron_dict[str(a)]
           typeb = neuron_dict[str(b)]
           edge1 = str(a)+str(b)
           edge2 = str(b)+str(a)
           if edge1 in sign_dict.keys() :
               sign1 = sign_dict[edge1]
           else:
               sign1 = "1"
           if edge2 in sign_dict.keys() :
               sign2 = sign_dict[edge2]
           else:
               sign2 = "1"
           samplemotif = typea+typeb+sign1+sign2
           if samplemotif in motif_dict.keys() :
               foundmotif = motif_dict[samplemotif]
               results.append(foundmotif)              
   resultslist = []
   for index in range (0, 136) :
       resultslist.append(results.count(motiflist[index]))
   finalresultsdf[str(connumber)] = resultslist
finalresultsdf.to_csv(Path(__file__).with_name('inducedresults.csv') , sep='\t')
