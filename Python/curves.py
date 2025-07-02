##############################################################################
# STANDARD HYBRID PARTITIONS FOR MULTI-LABEL CLASSIFICATION                  #
# Copyright (C) 2025                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - Prof Elaine Cecilia Gatto                                              #
# 2 - Prof PhD Ricardo Cerri                                                 #
# 3 - Prof PhD Mauri Ferrandin                                               #
# 4 - Prof PhD Celine Vens                                                   #
# 5 - PhD Felipe Nakano Kenji                                                #
# 6 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            # 
# 1 = Federal University of Lavras - UFLA                                    #
#                                                                            # 
# 2 = State University of São Paulo - USP                                    #
#                                                                            # 
# 3 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 4 and 5 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium     #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 6 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################


import sys
import io
import platform
import os
import time
import pickle
import pandas as pd
import numpy as np
import importlib
import evaluation as eval
importlib.reload(eval)
import measures as ms
importlib.reload(ms)



if __name__ == '__main__':   

    # obtendo argumentos da linha de comando
    train = pd.read_csv(sys.argv[1]) # conjunto de treino
    valid = pd.read_csv(sys.argv[2]) # conjunto de validação
    test = pd.read_csv(sys.argv[3])  # conjunto de teste
    start = int(sys.argv[4])         # inicio do espaço de rótulos    
    directory = sys.argv[5]          # diretório para salvar as predições 
    fold = int(sys.argv[6])   
        
    #train = pd.read_csv("/tmp/d-emotions/Datasets/emotions/CrossValidation/Tr/emotions-Split-Tr-1.csv")
    #valid = pd.read_csv("/tmp/d-emotions/Datasets/emotions/CrossValidation/Vl/emotions-Split-Vl-1.csv")
    #test = pd.read_csv("/tmp/d-emotions/Datasets/emotions/CrossValidation/Ts/emotions-Split-Ts-1.csv")
    #start = 72
    # directory = "/tmp/d-emotions/Tested/Split-1"


    #print("\n\n%==============================================%")
    #print("SINGLE-LABEL ")
    #print("train: ", sys.argv[1])
    #print("valid: ", sys.argv[2])
    #print("test: ", sys.argv[3])
    #print("label start: ", sys.argv[4])
    #print("directory: ", sys.argv[5])
    #print("fold: ", sys.argv[6])
    #print("%==============================================%\n\n")
    
    # juntando treino com validação
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train = train.iloc[:, :start]    # atributos 
    Y_train  = train.iloc[:, start:] # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test = test.iloc[:, :start]     # atributos
    Y_test = test.iloc[:, start:] # rótulos verdadeiros
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)

    # setando nome do diretorio e arquivo para salvar
    true = (directory + "/y_true.csv")     
    proba = (directory + "/y_pred_proba.csv")  

    y_true = pd.read_csv(true)
    y_proba = pd.read_csv(proba)
    
    res_curves = eval.multilabel_curve_metrics(y_true, y_proba)    
    name = (directory + "/results-python.csv") 
    res_curves.to_csv(name, index=False)    
 
