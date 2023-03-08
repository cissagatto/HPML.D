##############################################################################
# HYBRID PARTITIONS FOR MULTI-LABEL CLASSIFICATION (HPML)                    #
# COMPLETE CHAINS OF HPML - EXTERNAL AND INTERNAL CHAINS                     #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# PhD Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri       #
# Ferrandin | Prof. Dr. Celine Vens | PhD Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
##############################################################################


import numpy as np
import pandas as pd
from ecc import ECC
from sklearn.base import clone
from sklearn.multioutput import ClassifierChain

class ECCExin:
    random_seed = 1234
    rng = np.random.default_rng(random_seed)

    def __init__(self,
                 model,
                 n_chains = 10,
                 ):
       self.model = model
       self.n_chains = n_chains
       self.chains = None ## one ECC per cluster
        
    def fit(self,
            x, ## dataframe
            y,
            clusters,
            ):
        self.clusters = self.__preprocessClustersName(clusters,y)
        self.chains = []
        self.orderLabelsDataset = y.columns

        chain_x = x.copy()       
        for c in self.clusters:
            ecc = ECC(self.model,self.n_chains)
            chain_y = y[y.columns[c]]    
            ecc.fit(chain_x,chain_y)
            ecc.labelName_ = y.columns[c]
            self.chains.append(ecc)
            chain_x = pd.concat([chain_x, chain_y],axis=1)
                
    def predict(self,
                x):
        predictions = pd.concat([self.__predictChain(x) for i in range(self.n_chains)],axis=0)
        return predictions.groupby(predictions.index).apply(np.mean)

    def __predictChain(self,
                x
                ):
        if self.chains is None:
            raise Exception('Oh no no no no!', 'Model has not been fitted yet.')
        
        chain_x = x.copy()
        predictions = pd.DataFrame([])
       #print(self.chains)
        for model in self.chains:
            predictionsChain = pd.DataFrame(model.predict(chain_x), columns = model.labelName_ )
            predictions[model.labelName_] = predictionsChain
            chain_x = pd.concat([chain_x, predictionsChain], axis=1)
        predictions = predictions[self.orderLabelsDataset]
        return predictions            

    def __preprocessClustersName(self,
            clusters,
            y):       ### transform clusters names to integers
        clustersIndexes = [[y.columns.get_loc(l) for l in labels] for labels in clusters ]
        return clustersIndexes