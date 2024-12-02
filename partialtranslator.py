# -*- coding: utf-8 -*-
"""
@author: Szilágyi Gábor S
"""
import pandas as pd
from pathlib import Path

inducedresultsdf = pd.read_csv (Path(__file__).with_name('inducedresults.csv'), sep='\t')
inducedresultsdf.drop(columns=inducedresultsdf.columns[0], axis=1, inplace=True)
translatordf =  pd.read_csv (Path(__file__).with_name('translator.csv'), sep=';')
partialresultsdf = pd.DataFrame()
partialresultsdf.loc[:, 'Motif names'] = translatordf.loc[:, 'Motif names']
for networkcounter in range (0, 1001):
    sampledf = pd.read_csv (Path(__file__).with_name('translator.csv'), sep=';')
    for x in range (1, 710):
        for y in range (1,711):
            sampledf.iloc[x,y] = translatordf.iloc[x,y] *inducedresultsdf.iloc[y-1,networkcounter+1]
    sampledf['Sum'] = sampledf.sum(axis=1) + inducedresultsdf[str(networkcounter)]
    partialresultsdf = pd.concat([partialresultsdf, sampledf["Sum"].rename(networkcounter)], axis=1) 
    print (networkcounter)
partialresultsdf.to_csv(Path(__file__).with_name('partialresults.csv'), sep='\t')  