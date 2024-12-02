# -*- coding: utf-8 -*-
"""
@author: Gábor S Szilágyi
"""

import pandas as pd
finalresultsdf = pd.DataFrame()
from pathlib import Path
p = Path(__file__).with_name('motifdef.csv')
with p.open("r") as f:
  content = f.read()
motifcodes = content.split ("\n")
motifcodes.pop()
motif_dict = {}
for motifcode in motifcodes:
    barcode, motif = motifcode.split(';')
    motif_dict[barcode] = motif
motifsdf = pd.read_csv (Path(__file__).with_name('motifnames.csv'), "r")
motiflist = motifsdf[" Motif name"].tolist()
finalresultsdf['Motif names'] = motiflist
tripdf = pd.read_csv (Path(__file__).with_name('triplets.csv'))
tripletcount = len(tripdf.index)
edgesdf = pd.read_csv (Path(__file__).with_name('edges.csv'), sep=';')
edgesdf = edgesdf.astype(str)
for connumber in range (0, 1001) :
   y = str(connumber)
   name = 'Pred_' + y
   sign_dict = edgesdf.set_index('Edgecode').to_dict()[name]
   results = []
   for x in range (0, 64962):
        sampletriplet = tripdf.iat[x, 0]
        a = str(sampletriplet)[:3]
        b = str(sampletriplet)[3:6]
        c = str(sampletriplet)[6:9]
        edge1 = a+b
        edge2 = b+a
        edge3 = b+c
        edge4 = c+b
        edge5 = c+a
        edge6 = a+c
        if edge1 in sign_dict.keys() :
            sign1 = sign_dict[edge1]
        else:
            sign1 = "1"
        if edge2 in sign_dict.keys() :
            sign2 = sign_dict[edge2]
        else:
            sign2 = "1"
        if edge3 in sign_dict.keys() :
            sign3 = sign_dict[edge3]
        else:
            sign3 = "1"
        if edge4 in sign_dict.keys() :
            sign4 = sign_dict[edge4]
        else:
            sign4 = "1"
        if edge5 in sign_dict.keys() :
            sign5 = sign_dict[edge5]
        else:
            sign5 = "1"
        if edge6 in sign_dict.keys() :
            sign6 = sign_dict[edge6]
        else:
            sign6 = "1"
        samplemotif = sign1+sign2+sign3+sign4+sign5+sign6
        if samplemotif in motif_dict.keys() :
            foundmotif = motif_dict[samplemotif]
            results.append(foundmotif)
        else:
            print ("error")
   resultslist = []
   for index in range (0, 710) :
       resultslist.append(results.count(motiflist[index]))
   finalresultsdf[str(connumber)] = resultslist
finalresultsdf.to_csv(Path(__file__).with_name('inducedresults.csv') , sep='\t')
