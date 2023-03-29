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
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
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
    Folder.Best.Partition.Split = paste(parameters$Directories$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Directories$folderTested,
                                "/Split-", f, sep = "")
    if (dir.create(Folder.Tested.Split) == FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Directories$folderPartitions, "/",
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", best.part.info.f$num.part,
                        sep = "")
    
    
    #########################################################################
    cat("\nOpen Train file")
    train.name.file = paste(parameters$Directories$folderCVTR, "/",
                            parameters$Config$Dataset.Name, "-Split-Tr-",
                            f, ".csv", sep = "")
    train.dataset.original = data.frame(read.csv(train.name.file))
    
    #########################################################################
    cat("\nOpen Test file")
    test.name.file = paste(parameters$Directories$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    
    #########################################################################
    cat("\nOpen Validation file")
    val.name.file = paste(parameters$Directories$folderCVVL,
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
      
      ########################################################################
      fim = parameters$DatasetInfo$LabelStart + (nrow(specificGroup)-1)
      labels.indices = seq(parameters$DatasetInfo$LabelStart, fim, by=1)
      nomes.labels.clusters = specificGroup$label
      
      if(nrow(specificGroup)==1){
        cat("\n grupo com um rótulo")
        
        #######################################################################
        cat("\nExecute ECC PYTHON")
        num.labels = as.numeric(nrow(specificGroup))
        str.execute = paste("python3 ",
                            parameters$Directories$folderPython,
                            "/standard_single.py ",
                            train.name.csv, " ",
                            val.name.csv,  " ",
                            test.name.csv, " ",
                            parameters$DatasetInfo$AttEnd, " ",
                            Folder.Tested.Group,
                            sep="")
        
        # EXECUTA
        start <- proc.time()
        res = print(system(str.execute))
        tempo = data.matrix((proc.time() - start))
        tempo = data.frame(t(tempo))
        write.csv(tempo, paste(Folder.Tested.Group, "/runtime-cluster.csv", 
                               sep=""))
        
        if(res!=0){
          break
        }
        
        
        #####################################################################
        setwd(Folder.Tested.Group)
        y_preds_bin = data.frame(read.csv("y_pred_bin.csv"))
        y_trues = data.frame(read.csv("y_true.csv"))
        y_probas = data.frame(read.csv("y_proba.csv"))
        
        
        if(nrow(y_probas)!=nrow(test.dataset.cluster)){
          break
        }
        
        # y_proba_2 = data.frame(t(y_probas))
        # nrow(y_proba_2)
        # nomes.colunas = c("prob_0_0", "prob_0_1")
        # names(y_proba_2) = nomes.colunas
        # setwd(Folder.Tested.Group)
        # write.csv(y_proba_2, "y_proba_0_1.csv", row.names = FALSE)
        # cat("\n UM RÓTULO")
        # nomes = colnames(y_probas)
        # linhas = ncol(y_probas)
        # m = (linhas/2)+1
        # nomes.2 = c("")
        # a = 0
        # while(a<m){
        #  nomes.2[a] = paste("prob_", a-1, "_1", sep="")
        #  a = a + 1
        # }
        # nomes.linhas = row.names(y_proba_2)
        # y_proba_3 = cbind(nomes.linhas, y_proba_2)
        # y_proba_4 = y_proba_3[y_proba_3$nomes.linhas %in% nomes.2, ]
        # nrow(y_proba_4)
        # y_proba_5 = y_proba_4[,2]
        # length(y_proba_5)
        
        
        #####################################################################
        cat("\n\tUTIML Threshold\n")
        y_pred_threshold <- data.frame(as.matrix(fixed_threshold(y_probas, 
                                                                 threshold = 0.5)))
        
        y_pred_threshold_1 = y_pred_threshold[,2]
        setwd(Folder.Tested.Group)
        write.csv(y_pred_threshold_1, "y_pred_threshold.csv", row.names = FALSE)
        
        #####################################################################
        cat("\nSave original and pruned predictions")
        pred.bin = paste(nomes.labels.clusters, "-bin", sep="")
        pred.thr = paste(nomes.labels.clusters, "-thr", sep="")
        true.o = paste(nomes.labels.clusters, "-true", sep="")
        proba.o = paste(nomes.labels.clusters, "-proba", sep="")
        
        all.predictions = cbind(y_probas[,1], y_pred_threshold_1, y_preds_bin, y_trues)
        names(all.predictions) = c(proba.o, pred.thr, pred.bin, true.o)
        setwd(Folder.Tested.Group)
        write.csv(all.predictions, "clusters-predictions.csv", row.names = FALSE)
        
        setwd(Folder.Tested.Group)
        unlink(val.name.csv) 
        unlink(test.name.csv)
        unlink(train.name.csv)
        
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
        
        
        #######################################################################
        cat("\nExecute ECC PYTHON")
        num.labels = as.numeric(nrow(specificGroup))
        str.execute = paste("python3 ",
                            parameters$Directories$folderPython,
                            "/standard_multilabel.py ",
                            train.name.csv, " ",
                            val.name.csv,  " ",
                            test.name.csv, " ",
                            parameters$DatasetInfo$AttEnd, " ",
                            Folder.Tested.Group,
                            sep="")
        
        # EXECUTA
        start <- proc.time()
        res = print(system(str.execute))
        tempo = data.matrix((proc.time() - start))
        tempo = data.frame(t(tempo))
        write.csv(tempo, paste(Folder.Tested.Group, "/runtime-cluster.csv", 
                               sep=""))
        
        if(res!=0){
          break
        }
        
        #####################################################################
        setwd(Folder.Tested.Group)
        y_preds_bin = data.frame(read.csv("y_pred_bin.csv"))
        y_trues = data.frame(read.csv("y_true.csv"))
        y_probas = data.frame(read.csv("y_proba.csv"))
        
        #######################################
        nomes = colnames(y_probas)
        nomes.2 = c("")
        m = ncol(y_probas)/2
        a = 1
        while(a<=m){
          nomes.2[a] = paste("prob_", a-1, "_1", sep="")
          a = a + 1
        }
        
        probabilidades.2 = y_probas %>% select(all_of(nomes.2))
        names(probabilidades.2) = nomes.labels.clusters
        setwd(Folder.Tested.Group)
        write.csv(probabilidades.2, "y_proba_2.csv", row.names = FALSE)
        
        
        #####################################################################
        cat("\n\tUTIML Threshold\n")
        y_pred_threshold <- data.frame(as.matrix(fixed_threshold(probabilidades.2, 
                                                                 threshold = 0.5)))
        setwd(Folder.Tested.Group)
        names(y_pred_threshold) = nomes.labels.clusters
        write.csv(y_pred_threshold , "y_pred_threshold.csv", row.names = FALSE)
        
        
        #####################################################################
        cat("\nSave original and pruned predictions")
        pred.bin = paste(nomes.labels.clusters, "-bin", sep="")
        pred.thr = paste(nomes.labels.clusters, "-thr", sep="")
        true.o = paste(nomes.labels.clusters, "-true", sep="")
        proba.o = paste(nomes.labels.clusters, "-proba", sep="")
        
        all.predictions = cbind(probabilidades.2, y_pred_threshold, y_preds_bin, y_trues)
        names(all.predictions) = c(proba.o, pred.thr, pred.bin, true.o)
        setwd(Folder.Tested.Group)
        write.csv(all.predictions, "clusters-predictions.csv", row.names = FALSE)
        
        setwd(Folder.Tested.Group)
        unlink(val.name.csv) 
        unlink(test.name.csv)
        unlink(train.name.csv)
        
      } # FIM DO IF ELSE
      
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
    Folder.Split.Test = paste(parameters$Directories$folderTested,
                              "/Split-", f, sep = "")
    
    
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
    Folder.Best.Partition.Split = paste(parameters$Directories$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Directories$folderTested,
                                "/Split-", f, sep = "")
    
    Folder.BP = paste(parameters$Directories$folderPartitions, "/",
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-",
                        best.part.info.f$num.part, sep = "")
    
    
    #########################################################################
    cat("\nOpen Test file")
    test.name.file = paste(parameters$Directories$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    labels.indices = seq(parameters$DatasetInfo$LabelStart, 
                         parameters$DatasetInfo$LabelEnd, by=1)
    test.mldr = mldr_from_dataframe(test.dataset.original, 
                                    labelIndices = labels.indices)
    
    
    ###################################################################
    apagar = c(0)
    y.true.final = data.frame(apagar)
    y.pred.bin.final = data.frame(apagar)
    y.pred.thr.final = data.frame(apagar)
    y.proba.final = data.frame(apagar)
    runtime.final = data.frame(cluster=c(0), user.self=c(0), 
                               sys.self=c(0), elapsed=c(0), 
                               user.child=c(0),  sys.child=c(0))
    
    
    #########################################################################
    clusters.num = c(0)
    g = 1
    while (g <= best.part.info.f $num.group) {
      
      cat("\nGroup: ", g)
      
      Folder.Group.Test = paste(Folder.Split.Test, "/Group-", g, sep = "")
      
      setwd(Folder.Group.Test)
      predictions = data.frame(read.csv("clusters-predictions.csv"))
      runtime = data.frame(read.csv("runtime-cluster.csv"))
      
      if(nrow(predictions)!=nrow(test.mldr$dataset)){
        cat("\nNúmero incorreto de instâncias", nrow(predictions))
        break
        
      } else {
        
        partition.g = data.frame(filter(partition, group == g))
        
        nomes.proba = c(paste(partition.g$label, ".proba", sep=""))
        nomes.pred.bin = c(paste(partition.g$label, ".bin", sep=""))
        nomes.pred.thr = c(paste(partition.g$label, ".thr", sep=""))
        nomes.true = c(paste(partition.g$label, ".true", sep=""))
        
        y_proba = predictions %>% select(all_of(nomes.proba))
        y_pred_bin = predictions %>% select(all_of(nomes.pred.bin))
        y_pred_thr = predictions %>% select(all_of(nomes.pred.thr))
        y_true = predictions %>% select(all_of(nomes.true))
        
        y.pred.bin.final = cbind(y.pred.bin.final, y_pred_bin)
        y.pred.thr.final = cbind(y.pred.thr.final, y_pred_thr)
        y.proba.final = cbind(y.proba.final, y_proba)
        y.true.final = cbind(y.true.final, y_true)
        
        clusters.num[g] = g
        
        names(runtime)[1] = "cluster"
        runtime.final = rbind(runtime.final, runtime)      
        
      }
      
      g = g + 1
      gc()
    }
    
    runtime.final = runtime.final[-1,]
    runtime.final$cluster = clusters.num
    setwd(Folder.Split.Test)
    write.csv(runtime.final, "runtime-clusters.csv", row.names = FALSE)
    
    y.pred.bin.final = y.pred.bin.final[, -1]
    y.pred.thr.final = y.pred.thr.final[, -1]
    y.proba.final = y.proba.final[, -1]
    y.true.final = y.true.final[,-1]
    
    all.predictions = cbind(y.proba.final, y.pred.thr.final,
                            y.pred.bin.final, y.true.final)
    setwd(Folder.Split.Test)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    names(y.pred.bin.final) = partition$label
    names(y.pred.thr.final) = partition$label
    names(y.proba.final) = partition$label
    names(y.true.final) = partition$label
    
    cat("\nSave files")
    setwd(Folder.Split.Test)
    write.csv(y.pred.bin.final, "y_pred_bin.csv", row.names = FALSE)
    write.csv(y.pred.thr.final, "y_pred_thr.csv", row.names = FALSE)
    write.csv(y.proba.final, "y_proba.csv", row.names = FALSE)
    write.csv(y.true.final, "y_true.csv", row.names = FALSE)
    
    ##############################################
    roc.curva(pred_bin = y.pred.bin.final, 
              pred_proba = y.proba.final, 
              test = test.mldr, 
              f = f, 
              Folder = Folder.Split.Test)
    
    
    ##############################################
    predictions.information(nomes.rotulos = parameters$Config$NamesLabels, 
                            probas = y.proba.final, 
                            preds = y.pred.bin.final, 
                            trues = y.true.final, 
                            thr = y.pred.thr.final,
                            folder = Folder.Split.Test)
    
    
    #####################################################################
    # Computing AU PRC MICRO AND MACRO
    nome.true = paste(Folder.Split.Test, "/y_true.csv", sep="")
    nome.proba = paste(Folder.Split.Test, "/y_proba.csv", sep="")
    nome.pred.bin = paste(Folder.Split.Test, "/y_pred_bin.csv", sep="")
    save.proba = paste(Folder.Split.Test, "/auprc_bin.csv", sep="")
    save.bin = paste(Folder.Split.Test, "/auprc_proba.csv", sep="")
    
    str.execute = paste("python3 ",
                        parameters$Directories$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.proba, " ",
                        nome.pred.bin, " ",
                        save.proba, " ",
                        save.bin, " ",
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
    if(res!=0){
      break
    }
    
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
  avaliaParalel <- foreach (f = 1:parameters$Config$Number.Folds) %dopar%{
    # while(f<=parameters$Config.File$Number.Folds){
    
    #########################################################################
    cat("\nFold: ", f)
    
    ##########################################################################
    FolderRoot = "~/Standard-HPML"
    FolderScripts = "~/Standard-HPML/R"
    
    ##########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ###########################################################################
    FolderSplit = paste(parameters$Directories$folderTested, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    ####################################################################################
    setwd(FolderSplit)
    y_pred_bin = data.frame(read.csv("y_pred_bin.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred_threshold = data.frame(read.csv("y_pred_thr.csv"))
    
    ####################################################################################
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2, 
                                  labelIndices = seq(1,ncol(y_true2 )), 
                                  name = "y_true2")
    y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
    y_pred_threshold = sapply(y_pred_threshold, function(x) as.numeric(as.character(x)))
    
    
    ###############################################################
    salva.4 = paste("Bin-ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva.4, type="output")
    confmat.bin = multilabel_confusion_matrix(y_true3, y_pred_bin)
    print(confmat.bin)
    sink()
    
    
    ###############################################################
    salva.5 = paste("Threshdol-ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva.5, type="output")
    confmat.thr = multilabel_confusion_matrix(y_true3, y_pred_threshold)
    print(confmat.thr)
    sink()
    
    
    ###############################################################
    resConfMat = multilabel_evaluate(confmat.bin)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "Bin-ResConfMat.csv")
    
    
    ###############################################################
    rm(resConfMat)
    resConfMat = multilabel_evaluate(confmat.thr)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "Thr-ResConfMat.csv")
    
    
    ###############################################################
    conf.mat.bin = data.frame(confmat.bin$TPl, confmat.bin$FPl,
                              confmat.bin$FNl, confmat.bin$TNl)
    names(conf.mat.bin) = c("TP", "FP", "FN", "TN")
    conf.mat.perc = data.frame(conf.mat.bin/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    wrong = conf.mat.bin$FP + conf.mat.bin$FN
    wrong.perc = wrong/nrow(y_true)
    correct = conf.mat.bin$TP + conf.mat.bin$TN
    correct.perc = correct/nrow(y_true)
    conf.mat.bin.2 = data.frame(conf.mat.bin, conf.mat.perc, wrong, correct, 
                                wrong.perc, correct.perc)
    setwd(FolderSplit)
    write.csv(conf.mat.bin.2, "utiml-bin-matrix-confusion.csv")
    
    
    ###############################################################
    conf.mat.thr = data.frame(confmat.thr$TPl, confmat.thr$FPl,
                              confmat.thr$FNl, confmat.thr$TNl)    
    names(conf.mat.thr) = c("TP", "FP", "FN", "TN")
    conf.mat.perc = data.frame(conf.mat.thr/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    wrong = conf.mat.thr$FP + conf.mat.thr$FN
    wrong.perc = wrong/nrow(y_true)
    correct = conf.mat.thr$TP + conf.mat.thr$TN
    correct.perc = correct/nrow(y_true)
    conf.mat.thr.2 = data.frame(conf.mat.thr, conf.mat.perc, wrong, correct, 
                                wrong.perc, correct.perc)
    setwd(FolderSplit)
    write.csv(conf.mat.thr.2, "utiml-thr-matrix-confusion.csv")
    
    
    ###############################################################
    #f = f + 1
    gc()
  }
  
  gc()
  cat("\n####################################################################")
  cat("\n# GLOBAL RF: END OF THE EVALUATION MISCELLANEOUS FUNCTION          #")
  cat("\n####################################################################")
  cat("\n\n\n\n")
}




######################################################################
#
######################################################################
gather.eval.rf.silho <- function(parameters) {
  
  # vector with names measures
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1", 
               "macro-precision", "macro-recall", "margin-loss", 
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision", 
               "ranking-loss", "recall", "subset-accuracy", "wlp")
  
  
  # dta frame
  folds = c(0)
  
  confMatFinalTHR = data.frame(measures)
  confMatFinalBIN = data.frame(measures)
  
  final.proba.auc = data.frame()
  final.proba.micro.auc =  data.frame()
  final.proba.macro.auc =  data.frame()
  
  final.bin.auc =  data.frame()
  final.bin.micro.auc =  data.frame()
  final.bin.macro.auc =  data.frame()
  
  final.auprc = data.frame()
  
  final.runtime =  data.frame()
  
  # from fold = 1 to number_labels
  f = 1
  while(f<=parameters$Config$Number.Folds ){
    
    cat("\nFold: ", f)
    
    folderSplit = paste(parameters$Directories$folderTested, "/Split-", f, sep="")
    
    setwd(folderSplit)
    
    #########################################################################
    confMatTHR = data.frame(read.csv(paste(folderSplit, "/Thr-ResConfMat.csv", sep="")))
    names(confMatTHR) = c("Measures", "Fold")
    
    confMatBIN = data.frame(read.csv(paste(folderSplit, "/Bin-ResConfMat.csv", sep="")))
    names(confMatBIN) = c("Measures", "Fold")
    
    #########################################################################
    confMatTHR[is.na(confMatTHR)] <- 0
    confMatBIN[is.na(confMatBIN)] <- 0
    
    #########################################################################
    confMatFinalTHR = cbind(confMatFinalTHR, confMatTHR$Fold) 
    folds[f] = paste("Fold-", f, sep="")
    
    confMatFinalBIN = cbind(confMatFinalBIN, confMatBIN$Fold) 
    folds[f] = paste("Fold-", f, sep="")
    
    #########################################################################
    auprc  = data.frame(read.csv("auprc_bin.csv"))       
    final.auprc = rbind(final.auprc, auprc)
    
    ##################
    pred.auc = data.frame(read.csv("roc-bin-auc.csv"))
    names(pred.auc) = c("fold", "value")
    final.bin.auc = rbind(final.bin.auc, pred.auc)
    
    pred.micro.auc = data.frame(read.csv("roc-bin-micro-auc.csv"))
    names(pred.micro.auc) = c("fold", "value")
    final.bin.micro.auc = rbind(final.bin.micro.auc, pred.micro.auc)
    
    pred.macro.auc = data.frame(read.csv("roc-bin-macro-auc.csv"))
    names(pred.macro.auc) = c("fold", "value")
    final.bin.macro.auc = rbind(final.bin.macro.auc, pred.macro.auc)
    
    
    #################################
    proba.auc = data.frame(read.csv("roc-proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    proba.micro.auc = data.frame(read.csv("roc-proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    proba.macro.auc = data.frame(read.csv("roc-proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    #################################
    runtime = data.frame(read.csv("runtime-clusters.csv"))
    runtime = cbind(fold=f, runtime)
    final.runtime = rbind(final.runtime, runtime)
    
    #################################
    f = f + 1
    gc()
  } 
  
  
  ###########################################
  setwd(parameters$Directories$folderTested)
  names(confMatFinalTHR) = c("Measures", folds)
  confMatFinalTHR[is.na(confMatFinalTHR)] <- 0
  write.csv(confMatFinalTHR, "Thr-Test-Evaluated.csv", row.names = FALSE)
  
  
  ###########################################
  setwd(parameters$Directories$folderTested)
  names(confMatFinalBIN) = c("Measures", folds)
  confMatFinalBIN[is.na(confMatFinalBIN)] <- 0
  write.csv(confMatFinalBIN, "Bin-Test-Evaluated.csv", row.names = FALSE)
  
  # identical(confMatFinalTHR, confMatFinalBIN)
  
  ###########################################
  fold = seq(1, number_folds, by =1)
  
  ###########################################
  final.micro.auprc = data.frame(fold, Micro.AUPRC = final.auprc$Micro.AUPRC)
  setwd(parameters$Directories$folderTested)
  write.csv(final.micro.auprc, "auprc-micro.csv", row.names = FALSE)  
  
  ###########################################
  final.macro.auprc = data.frame(fold, Macro.AUPRC = final.auprc$Macro.AUPRC)
  setwd(parameters$Directories$folderTested)
  write.csv(final.macro.auprc, "auprc-macro.csv", row.names = FALSE)  
  
  ###########################################
  final.proba.auc.2 = data.frame(fold, auc = final.proba.auc$value)
  final.proba.micro.auc.2 = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  final.proba.macro.auc.2 = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  setwd(parameters$Directories$folderTested)
  write.csv(final.proba.auc.2, "roc-proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc.2, "roc-proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc.2, "roc-proba-micro-auc.csv", row.names = FALSE)
  
  
  #################
  final.bin.auc.2 = data.frame(fold, auc = final.bin.auc$value)
  final.bin.micro.auc.2 = data.frame(fold, micro.auc = final.bin.micro.auc$value)
  final.pred.macro.auc.2 = data.frame(fold, macro.auc = final.bin.macro.auc$value)
  setwd(parameters$Directories$folderTested)
  write.csv(final.bin.auc.2, "roc-bin-auc.csv", row.names = FALSE)  
  write.csv(final.bin.micro.auc.2, "roc-bin-macro-auc.csv", row.names = FALSE)  
  write.csv(final.pred.macro.auc.2, "roc-bin-micro-auc.csv", row.names = FALSE)  
  
  #################
  setwd(parameters$Directories$folderTested)
  write.csv(final.runtime , "runtime-clusters.csv", row.names = FALSE)  
  
  #######################
  media.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, mean))
  media.BIN = cbind(measures, media.BIN)
  names(media.BIN) = c("Measures", "Mean10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(media.BIN, "BIN-Mean10Folds.csv", row.names = FALSE)
  
  #######################
  mediana.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, median))
  mediana.BIN = cbind(measures, mediana.BIN)
  names(mediana.BIN) = c("Measures", "Median10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(mediana.BIN, "BIN-Median10Folds.csv", row.names = FALSE)
  
  #######################
  dp.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, sd))
  dp.BIN = cbind(measures, dp.BIN)
  names(dp.BIN) = c("Measures", "SD10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(dp.BIN, "standard-deviation-10-folds.csv", row.names = FALSE)
  
  #############################################################################
  media.THR = data.frame(apply(confMatFinalTHR[,-1], 1, mean))
  media.THR = cbind(measures, media.THR)
  names(media.THR) = c("Measures", "Mean10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(media.THR, "THR-Mean10Folds.csv", row.names = FALSE)
  
  #######################
  mediana.THR = data.frame(apply(confMatFinalTHR[,-1], 1, median))
  mediana.THR = cbind(measures, mediana.THR)
  names(mediana.THR) = c("Measures", "Median10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(mediana.THR, "THR-Median10Folds.csv", row.names = FALSE)
  
  #######################
  dp.THR = data.frame(apply(confMatFinalTHR[,-1], 1, sd))
  dp.THR = cbind(measures, dp.THR)
  names(dp.THR) = c("Measures", "SD10Folds")
  setwd(parameters$Directories$folderTested)
  write.csv(dp.THR, "THR-standard-deviation-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #") 
  cat("\n########################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
