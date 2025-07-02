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
from sklearn.ensemble import RandomForestClassifier  
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
        
    #train = pd.read_csv("/tmp/d-emotions/Tested/Split-2/Group-1/emotions-split-tr-2-group-1.csv")
    #valid = pd.read_csv("/tmp/d-emotions/Tested/Split-2/Group-1/emotions-split-vl-2-group-1.csv")
    #test = pd.read_csv("/tmp/d-emotions/Tested/Split-2/Group-1/emotions-split-ts-2-group-1.csv")
    #start = 72
    #directory = "/tmp/d-emotions/Tested/Split-2/Group-1"

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
    pred = (directory + "/y_pred_bin.csv") 
    proba = (directory + "/y_pred_proba.csv")  
    proba_original = (directory + "/proba_original.csv")  
    
    # parametros do classificador base
    random_state = 1234
    n_estimators = 200
    
    # inicializa o classificador base
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    
    start_time_train = time.time()
    #rf.fit(X_train.values, Y_train.values)
    #rf.fit(X_train, Y_train)
    rf.fit(X_train, Y_train.values.ravel())
    end_time_train = time.time()
    train_duration = end_time_train - start_time_train
    
    # predições binárias
    start_time_test_bin = time.time()
    y_pred_bin = pd.DataFrame(rf.predict(X_test))
    end_time_test_bin = time.time()
    test_duration_bin = end_time_test_bin - start_time_test_bin

    y_pred_bin.columns = labels_y_test    
    y_pred_bin.to_csv(pred, index=False)

    Y_test.to_csv(true, index=False)    

    # predições probabilísticas
    start_time_test_proba = time.time()
    probabilities = rf.predict_proba(X_test)
    end_time_test_proba = time.time()
    test_duration_proba = end_time_test_proba - start_time_test_proba

    probabilities_2 = pd.DataFrame(probabilities)
    probabilities_2.columns = [f'prob_0', f'prob_1']         
    probabilities_2.to_csv(proba_original, index=False)

    proba_1 = pd.DataFrame(probabilities_2.iloc[:,1])
    proba_1.columns = labels_y_test
    proba_1.to_csv(proba, index=False)

    times_df = pd.DataFrame({
        'train_duration': [train_duration],
        'test_duration_proba': [test_duration_proba],
        'test_duration_bin': [test_duration_bin]
    })
    times_path = os.path.join(directory, "runtime-python.csv")
    times_df.to_csv(times_path, index=False)

    # ========== SAVE MEASURES ===========   
    res_curves = eval.multilabel_curve_metrics(Y_test, proba_1)    
    name = (directory + "/results-python.csv") 
    res_curves.to_csv(name, index=False)    
 

    # =========== SAVE MODEL SIZE EM BYTES ===========
    model_buffer = io.BytesIO()
    pickle.dump(rf, model_buffer)
    model_size_bytes = model_buffer.tell()
    model_size_df = pd.DataFrame({
        'model_size_bytes': [model_size_bytes]
    })
    model_size_df.to_csv(os.path.join(directory, "model-size.csv"), index=False)
