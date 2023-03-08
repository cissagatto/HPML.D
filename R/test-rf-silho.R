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
#
#########################################################################
build.rf.silho <- function(parameters) {
  
  f = 1
  bthpkParalel <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n\n=================================================")
    cat("\nFold: ", f)
    
    #########################################################################
    FolderRoot = "~/Standard-HPML"
    FolderScripts = "~/Standard-HPML/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    
    #########################################################################
    cat("\nGetting information about clusters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold == f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    if (dir.create(Folder.Tested.Split) == FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/",
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", best.part.info.f$num.part,
                        sep = "")
    
    # Folder.Properties = paste(Folder.Tested.Split, "/Properties", sep="")
    # if (dir.create(Folder.Properties) == FALSE){dir.create(Folder.Properties)}
    
    
    #########################################################################
    cat("\nOpen Train file")
    train.name.file = paste(parameters$Folders$folderCVTR, "/",
                            parameters$Config$Dataset.Name, "-Split-Tr-",
                            f, ".csv", sep = "")
    train.dataset.original = data.frame(read.csv(train.name.file))
    
    #########################################################################
    cat("\nOpen Test file")
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    
    #########################################################################
    cat("\nOpen Validation file")
    val.name.file = paste(parameters$Folders$folderCVVL,
                          "/", parameters$Config$Dataset.Name,
                          "-Split-Vl-", f, ".csv", sep = "")
    val.dataset.original = data.frame(read.csv(val.name.file))
    
    #########################################################################
    g = 1
    while(g <= best.part.info.f$num.group) {
      
      cat("\n\n==================")
      cat("\nCluster: ", g)
      
      #########################################################################
      cat("\nCreating folder")
      Folder.Tested.Group = paste(Folder.Tested.Split, "/Group-", g, sep="")
      if(dir.exists(Folder.Tested.Group) == FALSE){dir.create(Folder.Tested.Group)}
      
      
      #########################################################################
      cat("\nSpecific Group")
      specificGroup = data.frame(filter(partition, group == g))
      
      ########################################################################
      fim = parameters$DatasetInfo$LabelStart + (nrow(specificGroup)-1)
      labels.indices = seq(parameters$DatasetInfo$LabelStart, fim, by=1)
      
      #########################################################################
      cat("\nTrain: Mount Group")
      train.attributes = train.dataset.original[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      train.classes = select(train.dataset.original, specificGroup$label)
      train.dataset.cluster = cbind(train.attributes, train.classes)
      
      #########################################################################
      cat("\nTrain: Save Group")
      train.name.csv = paste(Folder.Tested.Group,"/",
                             parameters$Config$Dataset.Name,
                             "-split-tr-",f, "-group-",g,
                             ".csv",sep = "")
      write.csv(train.dataset.cluster, train.name.csv, row.names = FALSE)
      
      #########################################################################
      cat("\nTest: Mount Group")
      test.attributes = test.dataset.original[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      test.classes = select(test.dataset.original, specificGroup$label)
      test.dataset.cluster = cbind(test.attributes, test.classes)
      
      #########################################################################
      cat("\nTest: Save Group")
      test.name.csv = paste(Folder.Tested.Group, "/",
                            parameters$Config$Dataset.Name,
                            "-split-ts-", f, "-group-", g,
                            ".csv", sep = "" )
      write.csv(test.dataset.cluster, test.name.csv, row.names = FALSE)
      
      #########################################################################
      cat("\nVal: Mount Group")
      val.attributes = val.dataset.original[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      val.classes = select(val.dataset.original, specificGroup$label)
      val.dataset.cluster = cbind(val.attributes, val.classes)
      
      #########################################################################
      cat("\nVal: Save Group")
      val.name.csv = paste(Folder.Tested.Group, "/",
                           parameters$Config$Dataset.Name,
                           "-split-vl-", f, "-group-", g, ".csv",
                           sep = "" )
      write.csv(val.dataset.cluster, val.name.csv, row.names = FALSE)
      
      
      #########################################################################
      cat("\nJuntando treino com validação")
      tv.dataset.cluster = rbind(train.dataset.cluster, val.dataset.cluster)
      
      #################################################
      clusters.labels.names = colnames(tv.dataset.cluster)
      nomes.labels.clusters = clusters.labels.names[labels.indices]
      
      if(nrow(specificGroup)==1){
        
        cat("\n grupo com um rótulo")
        
      } else {
        
        cat("\nGrupo com mais de um rótulo")
        train.mldr = mldr_from_dataframe(train.dataset.cluster, labelIndices = labels.indices)
        test.mldr = mldr_from_dataframe(test.dataset.cluster, labelIndices = labels.indices)
        val.mldr = mldr_from_dataframe(val.dataset.cluster, labelIndices = labels.indices)
        tv.mldr = mldr_from_dataframe(tv.dataset.cluster, labelIndices = labels.indices)
        
        ##############################################
        cat("\nPropriedades dos clusters")
        properties.clusters(nomes.labels.clusters,
                            fold = f,
                            cluster = g,
                            folderSave = Folder.Tested.Group,
                            labels.indices,
                            train = train.dataset.cluster,
                            test = test.dataset.cluster,
                            val = val.dataset.cluster,
                            tv = tv.dataset.cluster)
        
      } # FIM DO IF ELSE
      
      
      #######################################################################
      cat("\nExecute ECC PYTHON")
      str.execute = paste("python3 ",
                          parameters$Folders$folderPython,
                          "/random-forests.py ",
                          train.name.csv, " ",
                          val.name.csv,  " ",
                          test.name.csv, " ",
                          parameters$DatasetInfo$AttEnd, " ",
                          Folder.Tested.Group,
                          sep="")
      
      # EXECUTA
      res = print(system(str.execute))
      
      if(res!=0){
        break
      }
      
      
      #####################################################################
      setwd(Folder.Tested.Group)
      y_preds = data.frame(read.csv("y_pred.csv"))
      y_trues = data.frame(read.csv("y_true.csv"))
      y_proba = data.frame(read.csv("y_proba_1.csv"))
      
      
      #####################################################################
      nomes.rotulos = colnames(y_trues)
      
      if(nrow(specificGroup)==1){
        
      } else {
        roc.curve(predictions = y_preds,
                  test = test.mldr,
                  Folder = Folder.Tested.Group)
      }
      
      
      ##############################################
      cat("\nInformações das predições")
      predictions.information(nomes.rotulos=nomes.rotulos, 
                              proba = y_proba, 
                              preds = y_preds, 
                              trues = y_trues, 
                              folder = Folder.Tested.Group)
      
      
      #####################################################################
      unlink(test.name.csv)
      unlink(train.name.csv)
      unlink(val.name.csv)
      
      g = g + 1
      gc()
    } # end grupos
    
    # f = f + 1
    gc()
  } # ending folds
  
  gc()
  cat("\n############################################################")
  cat("\n# RF SILHOUETTE: End build.python.silho                    #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}


######################################################################
#
######################################################################
gather.preds.rf.silho <- function(parameters) {
  
  f = 1
  gatherR <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\nFold: ", f)
    
    FolderRoot = "~/Standard-HPML"
    FolderScripts = "~/Standard-HPML/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    
    ###################################################################
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)
    y_proba = data.frame(apagar)
    
    
    ###################################################################
    Folder.Split.Test = paste(parameters$Folders$folderTested,
                              "/Split-", f, sep = "")
    
    
    #########################################################################
    cat("\nGetting information about clusters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/",
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-",
                        best.part.info.f$num.part, sep = "")
    
    #########################################################################
    g = 1
    while (g <= best.part.info.f $num.group) {
      
      cat("\n\nGroup: ", g)
      
      Folder.Group.Test = paste(Folder.Split.Test, "/Group-", g, sep = "")
      
      cat("\nGather y_true ", g)
      setwd(Folder.Group.Test)
      y_true_gr = data.frame(read.csv("y_true.csv"))
      y_true = cbind(y_true, y_true_gr)
      
      setwd(Folder.Group.Test)
      cat("\nGather y_predict ", g)
      y_pred_gr = data.frame(read.csv("y_pred.csv"))
      y_pred = cbind(y_pred, y_pred_gr)
      
      setwd(Folder.Group.Test)
      cat("\nGather y_proba ", g)
      y_proba_gr = data.frame(read.csv("y_proba_1.csv"))
      y_proba = cbind(y_proba, y_proba_gr)
      
      g = g + 1
      gc()
    }
    
    cat("\nSave files")
    setwd(Folder.Split.Test)
    
    y_pred = y_pred[, -1]
    y_true = y_true[, -1]
    y_proba = y_proba[, -1]
    names(y_proba) = colnames(y_pred)
    
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    write.csv(y_proba, "y_proba.csv", row.names = FALSE)
    
    ##############################################
    # informacoes das predições
    predictions.information(nomes.rotulos = colnames(y_pred),
                            proba = y_proba,
                            preds = y_pred,
                            trues = y_true,
                            folder = Folder.Split.Test)
    
    
    ##############################################
    # curva roc
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    labels.indices = seq(parameters$DatasetInfo$LabelStart,
                         parameters$DatasetInfo$LabelEnd, by =1)
    
    test.mldr = mldr_from_dataframe(test.dataset.original, labelIndices = labels.indices)
    
    roc.curve(predictions = y_pred,
              test = test.mldr,
              Folder = Folder.Split.Test)
    
    # f = f + 1
    gc()
  } # end do foreach
  
  gc()
  cat("\n#####################################################")
  cat("\n# RF SILHOUETTE: End gather.preds.python.silho      #")
  cat("\n######################################################")
  cat("\n\n\n\n")
  
} # end da função


#######################################################################
#
#######################################################################
evaluate.rf.silho <- function(parameters) {
  
  f = 1
  avalParal <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    cat("\nFold: ", f)
    
    FolderRoot = "~/Standard-HPML"
    FolderScripts = "~/Standard-HPML/R"
    
    # data frame
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    # get the true and predict lables
    setwd(Folder.Tested.Split)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    # compute measures multilabel
    y_true2 = data.frame(sapply(y_true, function(x)
      as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 ,
                                  labelIndices = seq(1, ncol(y_true2)),
                                  name = "y_true2")
    y_pred2 = sapply(y_pred, function(x)
      as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
    setwd(Folder.Tested.Split)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep = "")
    sink(file = salva3, type = "output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep = "")
    namae = paste("Split-", f, "-Evaluated.csv", sep = "")
    write.csv(confMatPart, namae)
    
    
    # f = f + 1
    gc()
  } # end folds
  
  gc()
  cat("\n############################################################")
  cat("\n# RF SILHOUETTE: End evaluate.python.silho                 #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}




######################################################################
#
######################################################################
gather.eval.rf.silho <- function(parameters) {
  
  
  ##########################################################################
  apagar = c(0)
  final.auc = c(0)
  avaliado.final = data.frame(apagar)
  nomes = c("")
  
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    cat("\n#======================================================")
    cat("\n# Fold: ", f)
    cat("\n#======================================================\n")
    
    # vector with names
    measures = c("accuracy","average-precision","clp","coverage","F1",
                 "hamming-loss","macro-auc", "macro-f1","macro-precision",
                 "macro-recall","margin-loss","micro-auc","micro-f1",
                 "micro-precision","micro-recall","mlp","one-error",
                 "precision","ranking-loss", "recall","subset-accuracy","wlp")
    
    ##########################################################################
    # "/dev/shm/ej3-GpositiveGO/Tested/Split-1"
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    
    
    ######################################################################
    setwd(Folder.Tested.Split)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    avaliado.final= cbind(avaliado.final, avaliado[,2])
    nomes[f] = paste("Fold-", f, sep="")
    
    auc = data.frame(read.csv("auc.csv"))
    names(auc) = c("fold", "value")
    final.auc = rbind(final.auc, auc)

        
    f = f + 1
    gc()
    
  } # end folds
  
  final.auc = final.auc[-1,]
  fold = seq(1, parameters$Config$Number.Folds, by =1)
  final.auc = data.frame(fold, auc = final.auc$value)
  setwd(parameters$Folders$folderTested)
  write.csv(final.auc, "auc.csv", row.names = FALSE)
  
  media = data.frame(apply(avaliado.final[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(avaliado.final[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(avaliado.final[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  str = "STANDARD HPML - NO CHAINS AT ALL"
  write(str, "informacoes.txt")
  
  
  gc()
  cat("\n#############################################################")
  cat("\n# RF SILHOUETTE: End gather.eval.python.silho               #")
  cat("\n#############################################################")
  cat("\n\n\n\n")
}

organize <- function(parameters){
  
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    f = f + 1
    gc()
  }
  
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
