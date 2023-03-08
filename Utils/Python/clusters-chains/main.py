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


import sys
import pandas as pd
from ecc import ECC
from sklearn.tree import DecisionTreeRegressor

if __name__ == '__main__':

    n_chains = 10
    random_state = 0
    n_estimators = 200
    baseModel = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    partitions = pd.read_csv(sys.argv[4])    
    directory = sys.argv[5]  
    
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    clusters = partitions.groupby("group")["label"].apply(list)   
    allLabels = partitions["label"].unique()
    
    x_train = train.drop(allLabels, axis=1)
    y_train = train[allLabels]
    x_test = test.drop(allLabels, axis=1)

    ecc = ECC(baseModel,
            n_chains)

    ecc.fit(x_train,
            y_train,
            clusters,
            )

    test_predictions = pd.DataFrame(ecc.predict(x_test))
    train_predictions = pd.DataFrame(ecc.predict(x_train))
    
    true = (directory + "/y_true.csv")
    pred = (directory + "/y_pred.csv")    
    
    test_predictions.to_csv(pred, index=False)
    test[allLabels].to_csv(true, index=False)
    
