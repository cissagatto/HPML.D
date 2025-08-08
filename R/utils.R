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


# cat("\n################################")
# cat("\n# Set Work Space               #")
# cat("\n###############################\n\n")
library(here)
library(stringr)
FolderRoot <- here::here()
setwd(FolderRoot)


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
  
  retorno = list()
  
  folderResults = parameters$Config$Folder.Results
  
  folderScripts = parameters$Config$FolderScripts
  retorno$folderScripts = folderScripts
  
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
  folderPython = paste(FolderRoot, "/Python", sep="")
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
  folderReports = paste(FolderRoot, "/Reports", sep="")
   if(dir.exists(folderReports) == TRUE){
     setwd(folderReports)
     dir_folderReports = dir(folderReports)
     n_folderReports = length(dir_folderReports)
   } else {
     dir.create(folderReports)
     setwd(folderReports)
     dir_folderReports = dir(folderReports)
     n_folderReports = length(dir_folderReports)
   }
  retorno$folderReports = folderReports
  
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
    nome_arquivo = paste(parameters$Directories$folderCVTR,
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
  str = paste("java -jar ", parameters$Directories$folderUtils,
              "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  system(str)
  cat("\n")
}



##############################################################################
#
##############################################################################
get.all.partitions <- function(parameters){
  
  retorno = list()
  
  pasta.best = paste(parameters$Directories$folderPartitions, 
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
    
    Pasta = paste(parameters$Directories$folderPartitions, 
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
  
  setwd(parameters$Directories$folderTested)
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
  
  r1 <- data.frame(fold, cluster, stat = rownames(val.summary), type = "val", val.summary, row.names = NULL)
  r2 <- data.frame(fold, cluster, stat = rownames(teste.summary), type = "test", teste.summary, row.names = NULL)
  r3 <- data.frame(fold, cluster, stat = rownames(treino.summary), type = "train", treino.summary, row.names = NULL)
  r4 <- data.frame(fold, cluster, stat = rownames(tv.summary), type = "tv", tv.summary, row.names = NULL)
  
  sumario <- rbind(r1, r2, r3, r4)
  name = paste(folderSave, "/summary.csv", sep="")
  write.csv(sumario, name, row.names = FALSE)
  
  ##################################################################
  instances.tr <- data.frame(
    label = names(treino.labels),
    negative = colSums(treino.labels == 0),
    positive = colSums(treino.labels == 1)
  )
  rownames(instances.tr) = NULL
  instances.tr = data.frame(fold, cluster, type = "train", instances.tr)
  
  instances.ts <- data.frame(
    label = names(teste.labels),
    negative = colSums(teste.labels == 0),
    positive = colSums(teste.labels == 1)
  )
  rownames(instances.ts) = NULL
  instances.ts = data.frame(fold, cluster, type = "test", instances.ts)
  
  instances.vl <- data.frame(
    label = names(val.labels),
    negative = colSums(val.labels == 0),
    positive = colSums(val.labels == 1)
  )
  rownames(instances.vl) = NULL
  instances.vl = data.frame(fold, cluster, type = "val", instances.vl)
  
  instances.tv <- data.frame(
    label = names(tv.labels),
    negative = colSums(tv.labels == 0),
    positive = colSums(tv.labels == 1)
  )
  rownames(instances.tv) = NULL
  instances.tv = data.frame(fold, cluster, type = "tv", instances.tv)
  
  allposneg = rbind(instances.tr, instances.ts, instances.vl, instances.tv)
  name = paste0(folderSave, "/num-pos-neg.csv")
  write.csv(allposneg , name, row.names = FALSE)
  
  ##########################################################################
  mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
  mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
  mldr.val = mldr_from_dataframe(val, labelIndices = labels.indices)
  mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
  
  ##########################################################################
  labelsets.train = data.frame(mldr.treino$labelsets)
  names(labelsets.train) = c("labelset", "frequency")
  labelsets.train = data.frame(type = "train", labelsets.train)
  
  labelsets.test = data.frame(mldr.teste$labelsets)
  names(labelsets.test) = c("labelset", "frequency")
  labelsets.test = data.frame(type = "test", labelsets.test)
  
  labelsets.val = data.frame(mldr.val$labelsets)
  names(labelsets.val) = c("labelset", "frequency")
  labelsets.val = data.frame(type = "val", labelsets.val)
  
  labelsets.tv = data.frame(mldr.tv$labelsets)
  names(labelsets.tv) = c("labelset", "frequency")
  labelsets.tv = data.frame(type = "tv", labelsets.tv)
  
  res = rbind(labelsets.train, labelsets.test, labelsets.val, labelsets.tv)
  res = cbind(fold, cluster, res)
  
  name = paste(folderSave, "/labelsets.csv", sep="")
  write.csv(res, name, row.names = FALSE)
 
  ##########################################################################
  labels.train = data.frame(mldr.treino$labels)
  labels.test = data.frame(mldr.teste$labels)
  labels.val = data.frame(mldr.val$labels)
  labels.tv = data.frame(mldr.tv$labels)
  
  r1 <- data.frame(stat = rownames(labels.train), type = "val", labels.train, row.names = NULL)
  r2 <- data.frame(stat = rownames(labels.test), type = "test", labels.test, row.names = NULL)
  r3 <- data.frame(stat = rownames(labels.val), type = "train", labels.val, row.names = NULL)
  r4 <- data.frame(stat = rownames(labels.tv), type = "tv", labels.tv, row.names = NULL)
  
  all.labels <- rbind(r1, r2, r3, r4)
  name = paste(folderSave, "/labels-info.csv", sep="")
  write.csv(all.labels, name, row.names = FALSE)
  
  
  ##########################################################################  
  properties.train = data.frame(mldr.treino$measures)
  properties.train = cbind(fold, cluster, properties.train)
  properties.train = data.frame(type = "train", properties.train)
  
  properties.test = data.frame(mldr.teste$measures)
  properties.test = cbind(fold, cluster, properties.test)
  properties.test = data.frame(type = "test", properties.test)
  
  properties.val = data.frame(mldr.val$measures)
  properties.val = cbind(fold, cluster, properties.val)
  properties.val = data.frame(type = "val", properties.val)
  
  properties.tv = data.frame(mldr.tv$measures)
  properties.tv = cbind(fold, cluster, properties.tv)
  properties.tv = data.frame(type = "tv", properties.tv)
  
  measures = rbind(properties.train, properties.test, properties.val, properties.tv)
  measures = cbind(fold, cluster, measures)
  
  name = paste(folderSave , "/measures.csv", sep="")
  write.csv(measures , name, row.names = FALSE)
  
  ##########################################################################  
  label.space.tr = train[,labels.indices]
  ld.train = dependency(label.space.tr)
  
  label.space.ts = test[,labels.indices]
  ld.test = dependency(label.space.ts)
  
  label.space.vl = val[,labels.indices]
  ld.val = dependency(label.space.vl)
  
  label.space.tv = tv[,labels.indices]
  ld.tv = dependency(label.space.tv)
  
  ld = data.frame(fold, cluster, 
                  train = ld.train$label.dependency, 
                  test = ld.test$label.dependency, 
                  val = ld.val$label.dependency, 
                  tv = ld.tv$label.dependency)
  name = paste(folderSave , "/dependency.csv", sep="")
  write.csv(ld, name, row.names = FALSE)
  
  ##################################################################
  # name = paste(folderSave , "/contingency-tr.txt", sep="")
  # sink(name )
  # print(table(label.space.tr))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-ts.txt", sep="")
  # sink(name )
  # print(table(label.space.ts))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-vl.txt", sep="")
  # sink(name )
  # print(table(label.space.vl))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-tv.txt", sep="")
  # sink(name )
  # print(table(label.space.tv))
  # sink()
  
}





##############################################################################
# 
##############################################################################
roc.curva <- function(f, y_pred, test, Folder, nome){
  res = mldr_evaluate(test, y_pred)
  
  ###############################################################
  # PLOTANDO ROC CURVE
  # name = paste(Folder, "/", nome, "-roc.pdf", sep="")
  # pdf(name, width = 10, height = 8)
  # print(plot(res$roc, print.thres = 'best', print.auc=TRUE, 
  #            print.thres.cex=0.7, grid = TRUE, identity=TRUE,
  #            axes = TRUE, legacy.axes = TRUE, 
  #            identity.col = "#a91e0e", col = "#1161d5",
  #            main = paste("fold ", f, " ", nome, sep="")))
  # dev.off()
  # cat("\n")
  
  ###############################################################
  write.csv(as.numeric(res$roc$auc), paste(Folder, "/", nome, "-roc-auc.csv", sep=""))
  write.csv(as.numeric(res$macro_auc), paste(Folder, "/", nome, "-roc-auc-macro.csv", sep=""))
  write.csv(as.numeric(res$micro_auc), paste(Folder, "/", nome, "-roc-auc-micro.csv", sep=""))
  
  
  ###############################################################
  # SALVANDO AS INFORMAÇÕES DO ROC SEPARADAMENTE
  name = paste(Folder, "/", nome, "-roc-1.txt", sep="")
  output.file <- file(name, "wb")
  
  write(" ", file = output.file, append = TRUE)
  write("percent: ", file = output.file, append = TRUE)
  write(res$roc$percent, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("sensitivities: ", file = output.file, append = TRUE)
  write(res$roc$sensitivities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("specificities: ", file = output.file, append = TRUE)
  write(res$roc$specificities, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("thresholds: ", file = output.file, append = TRUE)
  write(res$roc$thresholds, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("direction: ", file = output.file, append = TRUE)
  write(res$roc$direction, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("cases: ", file = output.file, append = TRUE)
  write(res$roc$cases, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("controls: ", file = output.file, append = TRUE)
  write(res$roc$controls, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("auc: ", file = output.file, append = TRUE)
  write(res$roc$auc, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original predictor: ", file = output.file, append = TRUE)
  write(res$roc$original.predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("original response: ", file = output.file, append = TRUE)
  write(res$roc$original.response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("predictor: ", file = output.file, append = TRUE)
  write(res$roc$predictor, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("response: ", file = output.file, append = TRUE)
  write(res$roc$response, file = output.file, append = TRUE)
  
  write(" ", file = output.file, append = TRUE)
  write("levels: ", file = output.file, append = TRUE)
  write(res$roc$levels, file = output.file, append = TRUE)
  
  close(output.file)
  
  ###############################################################
  # SALVANDO AS OUTRAS INFORMAÇÕES
  #name = paste(Folder, "/", nome, "-roc-2.txt", sep="")
  #sink(name, type = "output")
  #print(res$roc)
  #cat("\n\n")
  #str(res)
  #sink()
}



##############################################################################
# 
##############################################################################
matrix.confusao <- function(true, pred, type, salva, nomes.rotulos){ 
  
  bipartition = data.frame(true, pred)
  
  num.instancias = nrow(bipartition)
  num.rotulos = ncol(true) # número de rótulos do conjunto
  
  num.positive.instances = apply(bipartition, 2, sum) # número de instâncias positivas
  num.negative.instances = num.instancias - num.positive.instances   # número de instâncias negativas  # salvando
  
  res = rbind(num.positive.instances, num.negative.instances)
  #name = paste(salva, "/", type, "-ins-pn.csv", sep="")
  #write.csv(res, name)
  
  true_1 = data.frame(ifelse(true==1,1,0)) # calcular rótulo verdadeiro igual a 1
  total_true_1 = apply(true_1, 2, sum)
  
  true_0 = data.frame(ifelse(true==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_true_0 = apply(true_0, 2, sum)
  
  pred_1 = data.frame(ifelse(pred==1,1,0)) # calcular rótulo predito igual a 1
  total_pred_1 = apply(pred_1, 2, sum)
  
  pred_0 = data.frame(ifelse(pred==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_pred_0 = apply(pred_0, 2, sum)
  
  matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
  row.names(matriz_totais) = nomes.rotulos
  #name = paste(salva, "/", type, "-trues-preds.csv", sep="")
  #write.csv(matriz_totais, name)
  
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
  name = paste(salva, "/", type, "-tfpn.csv", sep="")
  #write.csv(fpnt, name, row.names = FALSE)
  
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
  name = paste(salva, "/", type, "-matrix-confusion.csv", sep="")
  write.csv(matriz_confusao_por_rotulos, name)
}



avaliacao <- function(f, y_true, y_pred, salva, nome){
  
  #salva.0 = paste(salva, "/", nome, "-conf-mat.txt", sep="")
  #sink(file=salva.0, type="output")
  confmat = multilabel_confusion_matrix(y_true, y_pred)
  #print(confmat)
  #sink()
  
  resConfMat = multilabel_evaluate(confmat)
  resConfMat = data.frame(resConfMat)
  names(resConfMat) = paste("Fold-", f, sep="")
  salva.1 = paste(salva, "/", nome, "-evaluated.csv", sep="")
  write.csv(resConfMat, salva.1)
  
  conf.mat = data.frame(confmat$TPl, confmat$FPl,
                        confmat$FNl, confmat$TNl)
  names(conf.mat) = c("TP", "FP", "FN", "TN")
  conf.mat.perc = data.frame(conf.mat/nrow(y_true$dataset))
  names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
  wrong = conf.mat$FP + conf.mat$FN
  wrong.perc = wrong/nrow(y_true$dataset)
  correct = conf.mat$TP + conf.mat$TN
  correct.perc = correct/nrow(y_true$dataset)
  conf.mat.2 = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
  salva.2 = paste(salva, "/", nome, "-utiml.csv", sep="")
  write.csv(conf.mat.2, salva.2)
  
  
}



##################################################################
#' @title Compute Label Dependency for a Binary Label Matrix
#'
#' @description
#' Computes the label dependency value based on the approach from Luaces et al. (2012),
#' using the Pearson correlation of label pairs, weighted by their co-occurrence.
#' It only considers the lower triangle of the pairwise matrix to avoid redundancy.
#'
#' @param label.space A binary matrix or data frame (instances × labels), where 1 indicates label presence.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{label.dependency} - A numeric value representing the overall label dependency.
#' }
#'
#' @examples
#' label.space <- matrix(c(1,0,1, 0,1,1, 1,1,0), ncol=3, byrow=TRUE)
#' result <- dependency(label.space)
#' print(result$label.dependency)
#'
#' @references
#' Luaces, O., Díez, J., Barranquero, J., del Coz, J. J., & Bahamonde, A. (2012).
#' Binary relevance efficacy for multilabel classification. Progress in Artificial Intelligence, 1(4), 303–313.
#'
#' @import Matrix
#' @export
dependency <- function(label.space) {
  retorno <- list()
  
  library(Matrix)
  label.space <- Matrix(as.matrix(label.space), sparse = TRUE)
  label.space <- as(as.matrix(label.space), "dgCMatrix")
  
  pearson.matrix <- cor(as.matrix(label.space), method = "pearson")
  pearson.matrix[is.na(pearson.matrix)] <- 0
  
  intersection.matrix <- t(label.space) %*% label.space
  intersection.matrix <- as.matrix(intersection.matrix)
  
  pearson.abs <- abs(pearson.matrix)
  pearson.abs[upper.tri(pearson.abs)] <- 0
  intersection.matrix[upper.tri(intersection.matrix)] <- 0
  
  produto <- pearson.abs * intersection.matrix
  
  soma_produto <- sum(produto)
  soma_intersecoes <- sum(intersection.matrix)
  
  retorno$label.dependency <- if (soma_intersecoes > 0) {
    soma_produto / soma_intersecoes
  } else {
    0
  }
  
  return(retorno)
}



###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #
###############################################################################
