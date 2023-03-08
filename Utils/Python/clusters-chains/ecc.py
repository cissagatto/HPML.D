##############################################################################
# HYBRID PARTITIONS FOR MULTI-LABEL CLASSIFICATION (HPML)                    #
# CLUSTER CHAINS OF HPML - ONLY EXTERNAL CHAINS                              #
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
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
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
from sklearn.base import clone

class ECC:
    random_seed = 1234
    rng = np.random.default_rng(random_seed)

    def __init__(self,
                 model,
                 n_chains = 10,
                 ):
       self.model = model

       self.n_chains = n_chains
       self.orders = None
       self.chains = []
       
    def fit(self,
            x, ## dataframe
            y,
            clusters
            ):
        self.__generateOrders(len(clusters))
        self.clusters = self.__preprocessClustersName(clusters,
                                    y)
        for i in range(self.n_chains):
            self.__fitChain(self.orders[i], 
                           x, 
                           y)
    def predict(self,
                x):
        predictions = pd.concat([self.__predictChain(x, i) for i in range(self.n_chains)],axis=0)
        return predictions.groupby(predictions.index).apply(np.mean)

    def predict_probabilities(self,
                              x):
        predictions = pd.concat([self.__predictChain(x, i) for i in range(self.n_chains)],axis=0)
        return predictions.groupby(predictions.index).apply(np.mean)    

    def __generateOrders(self,
                        n_labels
                        ):
        self.orders = [self.rng.permutation(n_labels) for _ in range(self.n_chains)]
    
    def __fitChain(self,
                  order,
                  x,
                  y,
                  ):
        chain_x = x.copy()
        chain = []
        self.orderLabelsDataset = y.columns
        for i in order:
            chainModel = self.__getModel()            
            chain_y = pd.DataFrame(y[y.columns[self.clusters[i]]])
            chainModel.labelName_ = y.columns[self.clusters[i]]
            chainModel.fit(chain_x, chain_y)
            chain_x = pd.concat([chain_x, chain_y],axis=1)
            chain.append(chainModel)
        self.chains.append(chain)
        
    def __predictChain(self,
                       x,
                       chainIndex):
        chain_x = x.copy()
        predictions = pd.DataFrame([])
        for model in self.chains[chainIndex]:
            predictionsChain = pd.DataFrame(model.predict(chain_x), columns = model.labelName_ )
            predictions[model.labelName_] = predictionsChain
            chain_x = pd.concat([chain_x, predictionsChain], axis=1)
        predictions = predictions[self.orderLabelsDataset]
        return predictions  

    def __predictChainProba(self,
                            x,
                            chainIndex):
        chain_x = x.copy()
        predictions = pd.DataFrame([])
        for model in self.chains[chainIndex]:
            predictionsChainProba = pd.DataFrame(model.predict_proba(chain_x), columns = model.labelName_ )
            predictions[model.labelName_] = predictionsChainProba
            chain_x = pd.concat([chain_x, predictionsChainProba], axis=1)
        predictions = predictions[self.orderLabelsDataset]
        return predictions  

    def __getModel(self):
        return clone(self.model)   
             
    def __preprocessClustersName(self,
            clusters,
            y):       ### transform clusters names to integers
        clustersIndexes = [[y.columns.get_loc(l) for l in labels] for labels in clusters ]
        return clustersIndexes