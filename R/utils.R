##############################################################################
# STANDARD HPML                                                              #
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



##################################################
# SET WORK SPACE
##################################################
FolderRoot = "~/Standard-HPML"
FolderScripts = "~/Standard-HPML/R"




#########################################################################
# FUNCTION DIRECTORIES                                   
#   Objective:                                           
#      Creates all the necessary folders for the project.
#   Parameters:                                          
#      dataset_name: name of the dataset                 
#      folderResults: path to save process the algorithm. 
#               Example: "/dev/shm/birds", "/scratch/birds", 
#            "/home/usuario/birds", "/C:/Users/usuario/birds"
#   Return:                                                              
#      All path directories                                              
#########################################################################
directories <- function(parameters){
  
  FolderRoot = "~/Standard-HPML"
  FolderScripts = "~/Standard-HPML/R"
  
  retorno = list()
  
  folderResults = parameters$Config$Folder.Results
  
  #############################################################################
  # RESULTS FOLDER:                                                           #
  # Parameter from command line. This folder will be delete at the end of the #
  # execution. Other folder is used to store definitely the results.          #
  #############################################################################
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }
  retorno$folderResults = folderResults
  
  
  #############################################################################
  # UTILS FOLDER:                                                             #
  # Get information about the files within folder utils that already exists   #
  # in the project.                                                           #
  #############################################################################
  folderUtils = paste(FolderRoot, "/Utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  }
  retorno$folderUtils = folderUtils
  
  
  ############################################################################
  # PYTHON FOLDER:                                                           #
  # Folder that contains the python scripts                                  #
  #############################################################################
  folderPython = paste(folderUtils, "/Python", sep="")
  if(dir.exists(folderPython) == TRUE){
    setwd(folderPython)
    dir_folderPython = dir(folderPython)
    n_folderPython = length(dir_folderPython)
  } else {
    dir.create(folderPython)
    setwd(folderPython)
    dir_folderPython = dir(folderPython)
    n_folderPython = length(dir_folderPython)
  }
  retorno$folderPython = folderPython
  
  
  #############################################################################
  #
  #############################################################################
  # folderReports = paste(FolderRoot, "/Reports", sep="")
  # if(dir.exists(folderReports) == TRUE){
  #   setwd(folderReports)
  #   dir_folderReports = dir(folderReports)
  #   n_folderReports = length(dir_folderReports)
  # } else {
  #   dir.create(folderReports)
  #   setwd(folderReports)
  #   dir_folderReports = dir(folderReports)
  #   n_folderReports = length(dir_folderReports)
  # }
  
  #############################################################################
  # TESTED FOLDER                                                             #
  #############################################################################
  folderTested = paste(folderResults, "/Tested", sep="")
  if(dir.exists(folderTested) == TRUE){
    setwd(folderTested)
    dir_folderTested = dir(folderTested)
    n_folderTested = length(dir_folderTested)
  } else {
    dir.create(folderTested)
    setwd(folderTested)
    dir_folderTested = dir(folderTested)
    n_folderTested = length(dir_folderTested)
  }
  retorno$folderTested = folderTested
  
  
  
  #############################################################################
  # DATASETS FOLDER:                                                          #
  # Get the information within DATASETS folder that already exists in the     #
  # project. This folder store the files from cross-validation and will be    #
  # use to get the label space to modeling the label correlations and         #
  # compute silhouete to choose the best hybrid partition.                    #
  # "/home/[user]/Partitions-Kohonen/datasets"                                #
  #############################################################################
  folderDatasets = paste(folderResults, "/Datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  }
  retorno$folderDatasets = folderDatasets
  
  
  #############################################################################
  # SPECIFIC DATASET FOLDER:                                                  #
  # Path to the specific dataset that is runing. Example: with you are        # 
  # running this code for EMOTIONS dataset, then this get the path from it    #
  # "/home/[user]/Partitions-Kohonen/datasets/birds"                          #
  #############################################################################
  folderSpecificDataset = paste(folderDatasets, "/", dataset_name, sep="")
  if(dir.exists(folderSpecificDataset) == TRUE){
    setwd(folderSpecificDataset)
    dir_folderSpecificDataset = dir(folderSpecificDataset)
    n_folderSpecificDataset = length(dir_folderSpecificDataset)
  } else {
    dir.create(folderSpecificDataset)
    setwd(folderSpecificDataset)
    dir_folderSpecificDataset = dir(folderSpecificDataset)
    n_folderSpecificDataset = length(dir_folderSpecificDataset)
  }
  retorno$folderSpecificDataset = folderSpecificDataset
  
  
  #############################################################################
  # LABEL SPACE FOLDER:                                                       #
  # Path to the specific label space from the dataset that is runing.         #
  # This folder store the label space for each FOLD from the cross-validation #
  # which was computed in the Cross-Validation Multi-Label code.              #
  # In this way, we don't need to load the entire dataset into the running    #
  # "/home/elaine/Partitions-Kohonen/datasets/birds/LabelSpace"               #
  #############################################################################
  folderLabelSpace = paste(folderSpecificDataset, "/LabelSpace", sep="")
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  retorno$folderLabelSpace = folderLabelSpace
  
  
  #############################################################################
  # NAMES LABELS FOLDER:                                                      #
  # Get the names of the labels from this dataset. This will be used in the   #
  # code to create the groups for each partition. Is a way to guarantee the   #
  # use of the correct names labels.                                          #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/NamesLabels"              #
  #############################################################################
  folderNamesLabels = paste(folderSpecificDataset, "/NamesLabels", sep="")
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } 
  retorno$folderNamesLabels = folderNamesLabels
  
  
  #############################################################################
  #
  #############################################################################
  folderProperties = paste(folderSpecificDataset, "/Properties", sep="")
  if(dir.exists(folderProperties) == TRUE){
    setwd(folderProperties)
    dir_folderProperties = dir(folderProperties)
    n_folderProperties = length(dir_folderProperties)
  } else {
    dir.create(folderProperties)
    setwd(folderProperties)
    dir_folderProperties = dir(folderProperties)
    n_folderProperties = length(dir_folderProperties)
  }
  retorno$folderProperties = folderProperties
  
  
  #############################################################################
  # CROSS VALIDATION FOLDER:                                                  #
  # Path to the folders and files from cross-validation for the specific      # 
  # dataset                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation"          #
  #############################################################################
  folderCV = paste(folderSpecificDataset, "/CrossValidation", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }
  retorno$folderCV = folderCV
  
  
  #############################################################################
  # TRAIN CROSS VALIDATION FOLDER:                                            #
  # Path to the train files from cross-validation for the specific dataset    #                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Tr"       #
  #############################################################################
  folderCVTR = paste(folderCV, "/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }
  retorno$folderCVTR = folderCVTR
  
  
  #############################################################################
  # TEST CROSS VALIDATION FOLDER:                                             #
  # Path to the test files from cross-validation for the specific dataset     #                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Ts"       #
  #############################################################################
  folderCVTS = paste(folderCV, "/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }
  retorno$folderCVTS = folderCVTS
  
  
  #############################################################################
  # VALIDATION CROSS VALIDATION FOLDER:                                       #
  # Path to the validation files from cross-validation for the specific       #
  # dataset                                                                   #                                                           
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Vl"       #
  #############################################################################
  folderCVVL = paste(folderCV, "/Vl", sep="")
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }
  retorno$folderCVVL = folderCVVL
  
  
  #############################################################################
  # RESULTS DATASET FOLDER:                                                   #
  # Path to the results for the specific dataset that is running              #                                                           
  # "/dev/shm/res/birds"                                                      #
  #############################################################################
  folderResultsDataset = paste(folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderResultsDataset) == TRUE){
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  } else {
    dir.create(folderResultsDataset)
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  }
  retorno$folderResultsDataset = folderResultsDataset
  
  
  #############################################################################
  # RESULTS PARTITIONS FOLDER:                                                #
  # Folder to store the results from partitioning the label correlations      #
  # "/dev/shm/res/birds/Partitions"                                           #
  #############################################################################
  folderPartitions = paste(folderResults, "/Partitions", sep="")
  if(dir.exists(folderPartitions) == TRUE){
    setwd(folderPartitions)
    dir_folderPartitions = dir(folderPartitions)
    n_folderPartitions = length(dir_folderPartitions)
  } else {
    dir.create(folderPartitions)
    setwd(folderPartitions)
    dir_folderPartitions = dir(folderPartitions)
    n_folderPartitions = length(dir_folderPartitions)
  }
  retorno$folderPartitions = folderPartitions
  
  return(retorno)
  gc()
}



###########################################################################
# FUNCTION LABEL SPACE                                                  
#   Objective                                                           
#       Separates the label space from the rest of the data to be used
#      as input for calculating correlations                          
#   Parameters                                                        
#       ds: specific dataset information
#       dataset_name: dataset name. It is used to save files.
#       number_folds: number of folds created                
#       folderResults: folder where to save results          
#   Return:                                                  
#       Training set labels space                            
#######################################################################
labelSpace <- function(parameters){
  
  retorno = list()
  
  # return all fold label space
  classes = list()
  
  # from the first FOLD to the last
  k = 1
  while(k<=parameters$Config$Number.Folds){
    
    # get the correct fold cross-validation
    nome_arquivo = paste(parameters$Folders$folderCVTR,
                         "/", dataset_name, 
                         "-Split-Tr-", k, ".csv", sep="")
    
    # open the file
    arquivo = data.frame(read.csv(nome_arquivo))
    
    # split label space from input space
    classes[[k]] = arquivo[,ds$LabelStart:ds$LabelEnd]
    
    # get the names labels
    namesLabels = c(colnames(classes[[k]]))
    
    # increment FOLD
    k = k + 1 
    
    # garbage collection
    gc() 
    
  } # End While of the 10-folds
  
  # return results
  retorno$NamesLabels = namesLabels
  retorno$Classes = classes
  return(retorno)
  
  gc()
  cat("\n##########################################################")
  cat("\n# FUNCTION LABEL SPACE: END                              #") 
  cat("\n##########################################################")
  cat("\n\n\n\n")
}


#######################################################################
# FUNCTION INFO DATA SET                                               
#  Objective                                                           
#     Gets the information that is in the "datasets-hpmlk.csv" file.   
#  Parameters                                                          
#     dataset: the specific dataset                                    
#  Return                                                              
#     Everything in the "datasets-hpmlk.csv" file.                     
#######################################################################
infoDataSet <- function(dataset){
  
  retorno = list()
  
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  
  return(retorno)
  
  gc()
}



#########################################################################
# Function to correctly convert CSV in ARFF
converteArff <- function(arg1, arg2, arg3){
  str = paste("java -jar ", parameters$Folders$folderUtils,
              "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n")
}



##############################################################################
#
##############################################################################
get.all.partitions <- function(parameters){
  
  retorno = list()
  
  pasta.best = paste(parameters$Folders$folderPartitions, 
                     "/", parameters$Config$Dataset.Name, 
                     "/", parameters$Config$Dataset.Name, 
                     "-Best-Silhouete.csv", sep="")
  best = data.frame(read.csv(pasta.best))
  
  num.fold = c(0)
  num.part = c(0)
  num.group = c(0)
  best.part.info = data.frame(num.fold, num.part, num.group)
  
  all.partitions.info = data.frame()
  all.total.labels = data.frame()
  
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    best.fold = best[f,]
    num.fold = best.fold$fold
    num.part = best.fold$part
    
    Pasta = paste(parameters$Folders$folderPartitions, 
                  "/", parameters$Config$Dataset.Name, "/Split-", 
                  f, sep="")
    pasta.groups = paste(Pasta, "/fold-", f, 
                         "-groups-per-partition.csv", sep="")
    clusters = data.frame(read.csv(pasta.groups))
    groups.fold = data.frame(filter(clusters, partition == num.part))
    
    num.group = groups.fold$num.groups
    best.part.info = rbind(best.part.info, 
                           data.frame(num.fold, num.part, num.group))
    
    nome = paste(Pasta, "/Partition-", num.part, 
                 "/partition-", num.part, ".csv", sep="")
    partitions = data.frame(read.csv(nome))
    partitions = data.frame(num.fold, num.part, partitions)
    partitions = arrange(partitions, group)
    
    all.partitions.info = rbind(all.partitions.info, partitions)
    
    nome.2 = paste(Pasta, "/Partition-", num.part,
                   "/fold-", f, "-labels-per-group-partition-", 
                   num.group, ".csv", sep="")
    labels = data.frame(read.csv(nome.2))
    labels = data.frame(num.fold, labels)
    all.total.labels = rbind(all.total.labels , labels)
    
    f = f + 1
    gc()
  } # fim do fold
  
  setwd(parameters$Folders$folderTested)
  write.csv(best.part.info, "best-part-info.csv", row.names = FALSE)
  write.csv(all.partitions.info, "all.partitions.info.csv", row.names = FALSE)
  write.csv(all.total.labels, "all.total.labels.csv", row.names = FALSE)
  
  retorno$best.part.info = best.part.info[-1,]
  retorno$all.partitions.info = all.partitions.info
  retorno$all.total.labels = all.total.labels
  return(retorno)
  
}



properties.clusters <- function(nomes.labels.clusters,
                                fold,
                                cluster,
                                folderSave, 
                                labels.indices, 
                                train, 
                                test, 
                                val, 
                                tv){
  
  ##################################################################
  treino.labels = data.frame(train[,labels.indices])
  colnames(treino.labels) = nomes.labels.clusters
  
  teste.labels = data.frame(test[,labels.indices])
  colnames(teste.labels) = nomes.labels.clusters
  
  val.labels = data.frame(val[,labels.indices])
  colnames(val.labels) = nomes.labels.clusters
  
  tv.labels = data.frame(tv[,labels.indices])
  colnames(tv.labels) = nomes.labels.clusters
  
  
  ##########################################################################
  treino.sd = apply(treino.labels , 2, sd)
  treino.mean = apply(treino.labels , 2, mean)
  treino.median = apply(treino.labels , 2, median)
  treino.sum = apply(treino.labels , 2, sum)
  treino.max = apply(treino.labels , 2, max)
  treino.min = apply(treino.labels , 2, min)
  treino.quartis = apply(treino.labels, 2, quantile, 
                         probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  treino.summary = rbind(sd = treino.sd, mean = treino.mean, 
                         median = treino.median,
                         sum = treino.sum, max = treino.max, 
                         min = treino.min, treino.quartis)
  name = paste(folderSave, "/summary-train.csv", sep="")
  write.csv(treino.summary, name)
  
  
  ##########################################################################
  teste.sd = apply(teste.labels , 2, sd)
  teste.mean = apply(teste.labels , 2, mean)
  teste.median = apply(teste.labels , 2, median)
  teste.sum = apply(teste.labels , 2, sum)
  teste.max = apply(teste.labels , 2, max)
  teste.min = apply(teste.labels , 2, min)
  teste.quartis = apply(teste.labels, 2, quantile,
                        probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  teste.summary = rbind(sd = teste.sd, mean = teste.mean, 
                        median = teste.median,
                        sum = teste.sum, max = teste.max, 
                        min = teste.min, teste.quartis)
  name = paste(folderSave, "/summary-test.csv", sep="")
  write.csv(teste.summary, name)
  
  
  ##########################################################################
  val.sd = apply(val.labels , 2, sd)
  val.mean = apply(val.labels , 2, mean)
  val.median = apply(val.labels , 2, median)
  val.sum = apply(val.labels , 2, sum)
  val.max = apply(val.labels , 2, max)
  val.min = apply(val.labels , 2, min)
  val.quartis = apply(val.labels, 2, quantile, 
                      probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  val.summary = rbind(sd = val.sd, mean = val.mean, 
                      median = val.median,
                      sum = val.sum, max = val.max, 
                      min = val.min, val.quartis)
  name = paste(folderSave, "/summary-val.csv", sep="")
  write.csv(val.summary, name)
  
  
  ##########################################################################
  tv.sd = apply(tv.labels , 2, sd)
  tv.mean = apply(tv.labels , 2, mean)
  tv.median = apply(tv.labels , 2, median)
  tv.sum = apply(tv.labels , 2, sum)
  tv.max = apply(tv.labels , 2, max)
  tv.min = apply(tv.labels , 2, min)
  tv.quartis = apply(tv.labels, 2, quantile, 
                     probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  tv.summary = rbind(sd = tv.sd, mean = tv.mean, 
                     median = tv.median,
                     sum = tv.sum, max = tv.max, 
                     min = tv.min, tv.quartis)
  name = paste(folderSave, "/summary-tv.csv", sep="")
  write.csv(tv.summary, name)
  
  
  ##################################################################
  treino.num.positive.instances = apply(treino.labels , 2, sum)
  teste.num.positive.instances = apply(teste.labels , 2, sum)
  val.num.positive.instances = apply(val.labels , 2, sum)
  tv.num.positive.instances = apply(tv.labels , 2, sum)
  
  
  ##################################################################
  treino.num.instancias = nrow(train)
  treino.num.negative.instances = treino.num.instancias - treino.num.positive.instances 
  
  teste.num.instancias = nrow(test)
  teste.num.negative.instances = teste.num.instancias - teste.num.positive.instances 
  
  val.num.instancias = nrow(val)
  val.num.negative.instances = val.num.instancias - val.num.positive.instances 
  
  tv.num.instancias = nrow(tv)
  tv.num.negative.instances = tv.num.instancias - treino.num.positive.instances 
  
  todos = rbind(treino.num.positive.instances, treino.num.negative.instances,
                teste.num.positive.instances, teste.num.negative.instances,
                val.num.positive.instances, val.num.negative.instances,
                tv.num.positive.instances, tv.num.negative.instances)
  
  name = paste(folderSave, "/instances-pos-neg.csv", sep="")
  write.csv(todos, name)
  
  
  ##########################################################################
  treino.num.positive.instances = data.frame(treino.num.positive.instances)
  treino.num.negative.instances = data.frame(treino.num.negative.instances)
  
  teste.num.positive.instances = data.frame(teste.num.positive.instances)
  teste.num.negative.instances = data.frame(teste.num.negative.instances)
  
  val.num.positive.instances = data.frame(val.num.positive.instances)
  val.num.negative.instances = data.frame(val.num.negative.instances)
  
  tv.num.positive.instances = data.frame(tv.num.positive.instances)
  tv.num.negative.instances = data.frame(tv.num.negative.instances)
  
  
  ##################################################################
  label = rownames(treino.num.positive.instances)
  
  ##################################################################
  treino.num.positive.instances = data.frame(label , frequency = treino.num.positive.instances$treino.num.positive.instances)
  treino.num.negative.instances = data.frame(label , frequency = treino.num.negative.instances$treino.num.negative.instances)
  
  teste.num.positive.instances = data.frame(label , frequency = teste.num.positive.instances$teste.num.positive.instances)
  teste.num.negative.instances = data.frame(label , frequency = teste.num.negative.instances$teste.num.negative.instances)
  
  val.num.positive.instances = data.frame(label , frequency = val.num.positive.instances$val.num.positive.instances)
  val.num.negative.instances = data.frame(label , frequency = val.num.negative.instances$val.num.negative.instances)
  
  tv.num.positive.instances = data.frame(label , frequency = tv.num.positive.instances$tv.num.positive.instances)
  tv.num.negative.instances = data.frame(label , frequency = tv.num.negative.instances$tv.num.negative.instances)
  
  
  ##########################################################################
  treino.num.positive.instances = arrange(treino.num.positive.instances, desc(frequency))
  ultimo = nrow(treino.num.positive.instances)
  treino.max = data.frame(treino.num.positive.instances[1,])
  treino.min = data.frame(treino.num.positive.instances[ultimo,])
  
  teste.num.positive.instances = arrange(teste.num.positive.instances, desc(frequency))
  ultimo = nrow(teste.num.positive.instances)
  teste.max = data.frame(teste.num.positive.instances[1,])
  teste.min = data.frame(teste.num.positive.instances[ultimo,])
  
  val.num.positive.instances = arrange(val.num.positive.instances, desc(frequency))
  ultimo = nrow(val.num.positive.instances)
  val.max = data.frame(val.num.positive.instances[1,])
  val.min = data.frame(val.num.positive.instances[ultimo,])
  
  tv.num.positive.instances = arrange(tv.num.positive.instances, desc(frequency))
  ultimo = nrow(tv.num.positive.instances)
  tv.max = data.frame(tv.num.positive.instances[1,])
  tv.min = data.frame(tv.num.positive.instances[ultimo,])
  
  max.min = rbind(treino.max, treino.min,
                  teste.max, teste.min,
                  val.max, val.min,
                  tv.max, tv.min)
  
  set = c("train.max", "train.min",
          "teste.max", "teste.min",
          "val.max", "val.min",
          "tv.max", "tv.min")
  
  final = data.frame(set, max.min)
  
  name = paste(folderSave, "/labels-max-min.csv", sep="")
  write.csv(final, name, row.names = FALSE)
  
  ##########################################################################
  mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
  mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
  mldr.val = mldr_from_dataframe(val, labelIndices = labels.indices)
  mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
  
  
  ##########################################################################
  labelsets = data.frame(mldr.treino$labelsets)
  names(labelsets) = c("labelset", "frequency")
  name = paste(folderSave, "/labelsets-train.csv", sep="")
  write.csv(labelsets, name, row.names = FALSE)
  
  rm(labelsets)
  labelsets = data.frame(mldr.teste$labelsets)
  names(labelsets) = c("labelset", "frequency")
  name = paste(folderSave, "/labelsets-test.csv", sep="")
  write.csv(labelsets, name, row.names = FALSE)
  
  rm(labelsets)
  labelsets = data.frame(mldr.val$labelsets)
  names(labelsets) = c("labelset", "frequency")
  name = paste(folderSave, "/labelsets-val.csv", sep="")
  write.csv(labelsets, name, row.names = FALSE)
  
  rm(labelsets)
  labelsets = data.frame(mldr.tv$labelsets)
  names(labelsets) = c("labelset", "frequency")
  name = paste(folderSave, "/labelsets-tv.csv", sep="")
  write.csv(labelsets, name, row.names = FALSE)
  
  
  ##########################################################################
  labels = data.frame(mldr.treino$labels)
  name = paste(folderSave, "/labels-train.csv", sep="")
  write.csv(labels, name)
  
  rm(labels)
  labels = data.frame(mldr.teste$labels)
  name = paste(folderSave, "/labels-test.csv", sep="")
  write.csv(labels, name)
  
  rm(labels)
  labels = data.frame(mldr.val$labels)
  name = paste(folderSave, "/labels-val.csv", sep="")
  write.csv(labels, name)
  
  rm(labels)
  labels = data.frame(mldr.tv$labels)
  name = paste(folderSave, "/labels-tv.csv", sep="")
  write.csv(labels, name)
  
  
  ##########################################################################  
  properties = data.frame(mldr.treino$measures)
  properties = cbind(fold, cluster, properties)
  name = paste(folderSave , "/properties-train.csv", sep="")
  write.csv(properties , name, row.names = FALSE)
  
  rm(properties)
  properties = data.frame(mldr.teste$measures)
  properties = cbind(fold, cluster, properties)
  name = paste(folderSave , "/properties-test.csv", sep="")
  write.csv(properties , name, row.names = FALSE)
  
  rm(properties)
  properties = data.frame(mldr.val$measures)
  properties = cbind(fold, cluster, properties)
  name = paste(folderSave , "/properties-val.csv", sep="")
  write.csv(properties , name, row.names = FALSE)
  
  rm(properties)
  properties = data.frame(mldr.tv$measures)
  properties = cbind(fold, cluster, properties)
  name = paste(folderSave , "/properties-tv.csv", sep="")
  write.csv(properties , name, row.names = FALSE)
  
  
  ##########################################################################  
  # name = paste(folderSave , "/plot-train-fold-", f, ".pdf", sep="")
  # pdf(name, width = 10, height = 8)
  # print(plot(mldr.treino))
  # dev.off()
  # cat("\n")
  # 
  # name = paste(folderSave , "/plot-test-fold-", f, ".pdf", sep="")
  # pdf(name, width = 10, height = 8)
  # print(plot(mldr.teste))
  # dev.off()
  # cat("\n")
  # 
  # name = paste(folderSave , "/plot-val-fold-", f, ".pdf", sep="")
  # pdf(name, width = 10, height = 8)
  # print(plot(mldr.val))
  # dev.off()
  # cat("\n")
  # 
  # name = paste(folderSave , "/plot-tv-fold-", f, ".pdf", sep="")
  # pdf(name, width = 10, height = 8)
  # print(plot(mldr.tv))
  # dev.off()
  # cat("\n")
  
  
}





roc.curva <- function(predictions, probabilities, test, Folder){
  
  #####################################################################
  y_pred2 = sapply(predictions, function(x) as.numeric(as.character(x)))
  res.pred = mldr_evaluate(test, y_pred2)
  
  #####################################################################
  y_proba2 = sapply(probabilities, function(x) as.numeric(as.character(x)))
  res.proba = mldr_evaluate(test, y_proba2)
  
  ###############################################################
  # PLOTANDO ROC CURVE
  name = paste(Folder, "/roc-bin.pdf", sep="")
  pdf(name, width = 10, height = 8)
  print(plot(res.pred$roc, print.thres = 'all', print.auc=TRUE, 
             print.thres.cex=0.7, grid = TRUE, identity=TRUE,
             axes = TRUE, legacy.axes = TRUE, 
             identity.col = "#a91e0e", col = "#1161d5",
             main = "binary predictions"))
  dev.off()
  cat("\n")
  
  ###############################################################
  # PLOTANDO ROC CURVE
  name = paste(Folder, "/roc-proba.pdf", sep="")
  pdf(name, width = 10, height = 8)
  print(plot(res.proba$roc, print.thres = 'best', print.auc=TRUE, 
             print.thres.cex=0.7, grid = TRUE, identity=TRUE,
             axes = TRUE, legacy.axes = TRUE, 
             identity.col = "#a91e0e", col = "#1161d5",
             main = "probabilities predictions"))
  dev.off()
  cat("\n")
  
  
  ###############################################################
  setwd(Folder)
  write.csv(as.numeric(res.pred$roc$auc), "bin-auc.csv")
  write.csv(as.numeric(res.pred$macro_auc), "bin-macro-auc.csv")
  write.csv(as.numeric(res.pred$micro_auc), "bin-micro-auc.csv")
  
  
  ###############################################################
  setwd(Folder)
  write.csv(as.numeric(res.proba$roc$auc), "proba-auc.csv")
  write.csv(as.numeric(res.proba$macro_auc), "proba-macro-auc.csv")
  write.csv(as.numeric(res.proba$micro_auc), "proba-micro-auc.csv")
  
  
  ###############################################################
  # SALVANDO AS INFORMAÇÕES DO ROC SEPARADAMENTE
  name = paste(Folder, "/roc-proba-1.txt", sep="")
  output.file <- file(name, "wb")
  
  write(" ", file = output.file, append = TRUE)
  write("percent: ", file = output.file, append = TRUE)
  write(res.proba$roc$percent, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("sensitivities: ", file = output.file, append = TRUE)
  write(res.proba$roc$sensitivities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("specificities: ", file = output.file, append = TRUE)
  write(res.proba$roc$specificities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("thresholds: ", file = output.file, append = TRUE)
  write(res.proba$roc$thresholds, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("direction: ", file = output.file, append = TRUE)
  write(res.proba$roc$direction, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("cases: ", file = output.file, append = TRUE)
  write(res.proba$roc$cases, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("controls: ", file = output.file, append = TRUE)
  write(res.proba$roc$controls, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("auc: ", file = output.file, append = TRUE)
  write(res.proba$roc$auc, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original predictor: ", file = output.file, append = TRUE)
  write(res.proba$roc$original.predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original response: ", file = output.file, append = TRUE)
  write(res.proba$roc$original.response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("predictor: ", file = output.file, append = TRUE)
  write(res.proba$roc$predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("response: ", file = output.file, append = TRUE)
  write(res.proba$roc$response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("levels: ", file = output.file, append = TRUE)
  write(res.proba$roc$levels, file = output.file, append = TRUE)
  
  close(output.file)
  
  ###############################################################
  # SALVANDO AS INFORMAÇÕES DO ROC SEPARADAMENTE
  name = paste(Folder, "/roc-bin-1.txt", sep="")
  output.file <- file(name, "wb")
  
  write(" ", file = output.file, append = TRUE)
  write("percent: ", file = output.file, append = TRUE)
  write(res.pred$roc$percent, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("sensitivities: ", file = output.file, append = TRUE)
  write(res.pred$roc$sensitivities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("specificities: ", file = output.file, append = TRUE)
  write(res.pred$roc$specificities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("thresholds: ", file = output.file, append = TRUE)
  write(res.pred$roc$thresholds, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("direction: ", file = output.file, append = TRUE)
  write(res.pred$roc$direction, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("cases: ", file = output.file, append = TRUE)
  write(res.pred$roc$cases, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("controls: ", file = output.file, append = TRUE)
  write(res.pred$roc$controls, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("auc: ", file = output.file, append = TRUE)
  write(res.pred$roc$auc, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original predictor: ", file = output.file, append = TRUE)
  write(res.pred$roc$original.predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original response: ", file = output.file, append = TRUE)
  write(res.pred$roc$original.response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("predictor: ", file = output.file, append = TRUE)
  write(res.pred$roc$predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("response: ", file = output.file, append = TRUE)
  write(res.pred$roc$response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("levels: ", file = output.file, append = TRUE)
  write(res.pred$roc$levels, file = output.file, append = TRUE)
  
  close(output.file)
  
  
  ###############################################################
  # SALVANDO AS OUTRAS INFORMAÇÕES
  name = paste(Folder, "/roc-bin-2.txt", sep="")
  sink(name, type = "output")
  print(res.pred$roc)
  cat("\n\n")
  str(res.pred)
  sink()
  
  
  ###############################################################
  # SALVANDO AS OUTRAS INFORMAÇÕES
  name = paste(Folder, "/roc-proba-2.txt", sep="")
  sink(name, type = "output")
  print(res.proba$roc)
  cat("\n\n")
  str(res.proba)
  sink()
}


predictions.information <- function(nomes.rotulos, 
                                    proba, 
                                    preds, 
                                    trues, 
                                    folder){
  
  #####################################################################
  pred.o = paste(colnames(preds), "-pred", sep="")
  names(preds) = pred.o
  
  true.labels = paste(colnames(trues), "-true", sep="")
  names(trues) = true.labels
  
  proba.n = paste(nomes.rotulos, "-proba", sep="")
  names(proba) = proba.n
  
  all.predictions = cbind(proba, preds, trues)
  setwd(folder)
  write.csv(all.predictions, "predictions.csv", row.names = FALSE)
  
  ###############################################
  bipartition = data.frame(trues, preds)
  
  # número de instâncias do conjunto
  num.instancias = nrow(bipartition)
  
  # número de rótulos do conjunto
  num.rotulos = ncol(trues)
  
  # número de instâncias positivas
  num.positive.instances = apply(bipartition, 2, sum)
  
  # número de instâncias negativas
  num.negative.instances = num.instancias - num.positive.instances 
  
  # salvando
  res = rbind(num.positive.instances, num.negative.instances)
  name = paste(folder, "/instances-pn.csv", sep="")
  write.csv(res, name)
  
  # calcular rótulo verdadeiro igual a 1
  true_1 = data.frame(ifelse(trues==1,1,0))
  total_true_1 = apply(true_1, 2, sum)
  
  # calcular rótulo verdadeiro igual a 0
  true_0 = data.frame(ifelse(trues==0,1,0))
  total_true_0 = apply(true_0, 2, sum)
  
  # calcular rótulo predito igual a 1
  pred_1 = data.frame(ifelse(preds==1,1,0))
  total_pred_1 = apply(pred_1, 2, sum)
  
  # calcular rótulo verdadeiro igual a 0
  pred_0 = data.frame(ifelse(preds==0,1,0))
  total_pred_0 = apply(pred_0, 2, sum)
  
  matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
  row.names(matriz_totais) = nomes.rotulos
  name = paste(folder, "/trues-preds.csv", sep="")
  write.csv(matriz_totais, name)
  
  # Verdadeiro Positivo: O modelo previu 1 e a resposta correta é 1
  TPi  = data.frame(ifelse((true_1 & true_1),1,0))
  tpi = paste(nomes.rotulos, "-TP", sep="")
  names(TPi) = tpi
  
  # Verdadeiro Negativo: O modelo previu 0 e a resposta correta é 0
  TNi  = data.frame(ifelse((true_0 & pred_0),1,0))
  tni = paste(nomes.rotulos, "-TN", sep="")
  names(TNi) = tni
  
  # Falso Positivo: O modelo previu 1 e a resposta correta é 0
  FPi  = data.frame(ifelse((true_0 & pred_1),1,0))
  fpi = paste(nomes.rotulos, "-FP", sep="")
  names(FPi) = fpi
  
  # Falso Negativo: O modelo previu 0 e a resposta correta é 1
  FNi  = data.frame(ifelse((true_1 & pred_0),1,0))
  fni = paste(nomes.rotulos, "-FN", sep="")
  names(FNi) = fni
  
  fpnt = data.frame(TPi, FPi, FNi, TNi)
  name = paste(folder, "/tfpn.csv", sep="")
  write.csv(fpnt, name, row.names = FALSE)
  
  # total de verdadeiros positivos
  TPl = apply(TPi, 2, sum)
  tpl = paste(nomes.rotulos, "-TP", sep="")
  names(TPl) = tpl
  
  # total de verdadeiros negativos
  TNl = apply(TNi, 2, sum)
  tnl = paste(nomes.rotulos, "-TN", sep="")
  names(TNl) = tnl
  
  # total de falsos negativos
  FNl = apply(FNi, 2, sum)
  fnl = paste(nomes.rotulos, "-FN", sep="")
  names(FNl) = fnl
  
  # total de falsos positivos
  FPl = apply(FPi, 2, sum)
  fpl = paste(nomes.rotulos, "-FP", sep="")
  names(FPl) = fpl
  
  matriz_confusao_por_rotulos = data.frame(TPl, FPl, FNl, TNl)
  colnames(matriz_confusao_por_rotulos) = c("TP","FP", "FN", "TN")
  row.names(matriz_confusao_por_rotulos) = nomes.rotulos
  name = paste(folder, "/matrix-confusion-2.csv", sep="")
  write.csv(matriz_confusao_por_rotulos, name)
  
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################