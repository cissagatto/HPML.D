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
build.rf.silho <- function(parameters){
  
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
        y_pred_proba = data.frame(read.csv("y_pred_proba.csv"))
        y_pred_bin = data.frame(read.csv("y_pred_bin.csv"))
        y_true = data.frame(read.csv("y_true.csv"))
        
        
        if(nrow(y_pred_proba)!=nrow(test.dataset.cluster)){
          break
        }
        
        # y_proba_2 = data.frame(t(y_pred_proba))
        # nrow(y_proba_2)
        # nomes.colunas = c("prob_0_0", "prob_0_1")
        # names(y_proba_2) = nomes.colunas
        # setwd(Folder.Tested.Group)
        # write.csv(y_proba_2, "y_proba_0_1.csv", row.names = FALSE)
        # cat("\n UM RÓTULO")
        # nomes = colnames(y_pred_proba)
        # linhas = ncol(y_pred_proba)
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
        cat("\nSave original and pruned predictions")
        pred.bin = paste(nomes.labels.clusters, "-pred-bin", sep="")
        true.o = paste(nomes.labels.clusters, "-true", sep="")
        proba.o = paste(nomes.labels.clusters, "-pred-proba", sep="")
        
        all.predictions = cbind(y_pred_proba[,2], y_pred_bin, y_true)
        names(all.predictions) = c(proba.o, pred.bin, true.o)
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
        y_pred_bin = data.frame(read.csv("y_pred_bin.csv"))
        y_true = data.frame(read.csv("y_true.csv"))
        y_pred_proba = data.frame(read.csv("y_pred_proba.csv"))
        
        
        #######################################
        nomes = colnames(y_pred_proba)
        nomes.2 = c("")
        m = ncol(y_pred_proba)/2
        a = 1
        while(a<=m){
          nomes.2[a] = paste("prob_", a-1, "_1", sep="")
          a = a + 1
        }
        
        y_pred_proba = y_pred_proba %>% select(all_of(nomes.2))
        names(y_pred_proba) = nomes.labels.clusters
        setwd(Folder.Tested.Group)
        write.csv(y_pred_proba, "y_pred_proba.csv", row.names = FALSE)
        
        #####################################################################
        cat("\nSave original and pruned predictions")
        pred.bin = paste(nomes.labels.clusters, "-pred-bin", sep="")
        true.o = paste(nomes.labels.clusters, "-true", sep="")
        proba.o = paste(nomes.labels.clusters, "-pred-proba", sep="")
        
        all.predictions = cbind(y_pred_proba, y_pred_bin, y_true)
        names(all.predictions) = c(proba.o, pred.bin, true.o)
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
    
    #########################################################################
    # y_true = test.mldr$dataset[,labels.indices]
    # setwd(Folder.Split.Test)
    # write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    #########################################################################
    cat("\nOpen Train file")
    train.name.file = paste(parameters$Directories$folderCVTR, "/",
                            parameters$Config$Dataset.Name, "-Split-Tr-",
                            f, ".csv", sep = "")
    train.dataset.original = data.frame(read.csv(train.name.file))
    train.mldr = mldr_from_dataframe(train.dataset.original, 
                                    labelIndices = labels.indices)
    
    
    #########################################################################
    cat("\nOpen Validation file")
    val.name.file = paste(parameters$Directories$folderCVVL,
                          "/", parameters$Config$Dataset.Name,
                          "-Split-Vl-", f, ".csv", sep = "")
    val.dataset.original = data.frame(read.csv(val.name.file))
    val.mldr = mldr_from_dataframe(val.dataset.original, 
                                     labelIndices = labels.indices)
    
    
    #########################################################################
    tv = rbind(train.dataset.original, val.dataset.original)
    mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
    
    ###################################################################
    apagar = c(0)
    y.true.final = data.frame(apagar)
    y.pred.bin.final = data.frame(apagar)
    y.pred.proba.final = data.frame(apagar)
    runtime.final = data.frame(cluster=c(0), user.self=c(0), 
                               sys.self=c(0), elapsed=c(0), 
                               user.child=c(0),  sys.child=c(0))
    
    
    #########################################################################
    cat("\n")
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
        
        nomes.pred.proba = c(paste(partition.g$label, ".pred.proba", sep=""))
        nomes.pred.bin = c(paste(partition.g$label, ".pred.bin", sep=""))
        nomes.true = c(paste(partition.g$label, ".true", sep=""))
        
        y_pred_proba = predictions %>% select(all_of(nomes.pred.proba))
        y_pred_bin = predictions %>% select(all_of(nomes.pred.bin))
        y_true = predictions %>% select(all_of(nomes.true))
        
        y.pred.bin.final = cbind(y.pred.bin.final, y_pred_bin)
        y.pred.proba.final = cbind(y.pred.proba.final, y_pred_proba)
        y.true.final = cbind(y.true.final, y_true)
        
        clusters.num[g] = g
        
        names(runtime)[1] = "cluster"
        runtime.final = rbind(runtime.final, runtime)      
        
      }
      
      g = g + 1
      gc()
    }
    
    ########################################
    runtime.final = runtime.final[-1,]
    runtime.final$cluster = clusters.num
    setwd(Folder.Split.Test)
    write.csv(runtime.final, "runtime-clusters.csv", row.names = FALSE)
    
    ########################################
    y.pred.bin.final = y.pred.bin.final[, -1]
    y.pred.proba.final = y.pred.proba.final[, -1]
    y.true.final = y.true.final[,-1]
    
    ########################################
    names(y.true.final) = parameters$Config$NamesLabels
    names(y.pred.bin.final) = parameters$Config$NamesLabels
    names(y.pred.proba.final) = parameters$Config$NamesLabels
    
    ########################################
    nome.true = paste(Folder.Split.Test, "/y_true.csv", sep="")
    nome.pred.proba = paste(Folder.Split.Test, "/y_pred_proba.csv", sep="")
    nome.pred.bin = paste(Folder.Split.Test, "/y_pred_bin.csv", sep="")
    
    ########################################
    write.csv(y.true.final, nome.true, row.names = FALSE)
    write.csv(y.pred.proba.final, nome.pred.proba, row.names = FALSE)
    write.csv(y.pred.bin.final, nome.pred.bin, row.names = FALSE)
    
    ############################################################
    y.true.2 = data.frame(sapply(y.true.final, function(x) as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2, 
                                   labelIndices = seq(1,ncol(y.true.2)), 
                                   name = "y.true.2")
    #y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
    #y_pred_proba = sapply(y_pred_proba, function(x) as.numeric(as.character(x))))
    
    
    ########################################################################
    y_threshold_05 <- data.frame(as.matrix(fixed_threshold(y.pred.proba.final,
                                                           threshold = 0.5)))
    y_threshold_05 = data.frame(as.matrix(y_threshold_05))
    write.csv(y_threshold_05, 
              paste(Folder.Split.Test, "/y_pred_thr05.csv", sep=""),
              row.names = FALSE)
    
    
    ########################################################################
    y_threshold_card = lcard_threshold(as.matrix(y.pred.proba.final), 
                                       mldr.tv$measures$cardinality,
                                       probability = F)
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    write.csv(y_threshold_card, 
              paste(Folder.Split.Test, "/y_pred_thrLC.csv", sep=""),
              row.names = FALSE)
    
    
    #####################################################################
    nome.true = paste(Folder.Split.Test, "/y_true.csv", sep="")
    nome.pred.proba = paste(Folder.Split.Test, "/y_pred_proba.csv", sep="")
    nome.pred.bin = paste(Folder.Split.Test, "/y_pred_bin.csv", sep="")
    nome.thr.05 = paste(Folder.Split.Test, "/y_pred_thr05.csv", sep="")
    nome.thr.LC = paste(Folder.Split.Test, "/y_pred_thrLC.csv", sep="")
    
    save.pred.bin = paste(Folder.Split.Test, "/pred-bin-auprc.csv", sep="")
    save.pred.proba = paste(Folder.Split.Test, "/pred-proba-auprc.csv", sep="")
    save.thr05 = paste(Folder.Split.Test, "/thr-05-auprc.csv", sep="")
    save.thrLC = paste(Folder.Split.Test, "/thr-lc-auprc.csv", sep="")
    
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.pred.bin, " ",
                        save.pred.bin, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.pred.proba, " ",
                        save.pred.proba, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.thr.05, " ",
                        save.thr05, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.thr.LC, " ",
                        save.thrLC, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    ####################################################
    names = paste(parameters$Config$NamesLabels, "-proba", sep="")
    y.pred.proba.final = data.frame(y.pred.proba.final)
    names(y.pred.proba.final) = names
    rm(names)
    
    names = paste(parameters$Config$NamesLabels, "-bin", sep="")
    y.pred.bin.final = data.frame(y.pred.bin.final)
    names(y.pred.bin.final) = names
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-true", sep="")
    y.true.final = data.frame(y.true.final)
    names(y.true.final) = names 
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-thr-05", sep="")
    y_threshold_05 = data.frame(y_threshold_05)
    names(y_threshold_05) = names 
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-thr-lc", sep="")
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    names(y_threshold_card) = names 
    rm(names)
    
    all.predictions = cbind(y.true.final, y.pred.bin.final, y.pred.proba.final,
                            y_threshold_05, y_threshold_card)
    write.csv(all.predictions, 
              paste(Folder.Split.Test, "/folder-predictions.csv", sep=""), 
              row.names = FALSE)
    
    
    ##############################################
    names(y.true.final) = parameters$Config$NamesLabels
    names(y.pred.bin.final) = parameters$Config$NamesLabels
    names(y.pred.proba.final) = parameters$Config$NamesLabels
    names(y_threshold_card) = parameters$Config$NamesLabels
    names(y_threshold_card) = parameters$Config$NamesLabels
    
    matrix.confusao(true = y.true.final, pred = y_threshold_05, 
                    type = "thr-05", salva = Folder.Split.Test, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    matrix.confusao(true = y.true.final, pred = y_threshold_card, 
                    type = "thr-lc", salva = Folder.Split.Test, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    matrix.confusao(true = y.true.final, pred = y.pred.bin.final , 
                    type = "pred-bin", salva = Folder.Split.Test, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    
    #########################################################################    
    roc.curva(f = f, y_pred = y.pred.bin.final, test = test.mldr,
              Folder = Folder.Split.Test, nome = "pred-bin")
    
    roc.curva(f = f, y_pred = y.pred.proba.final, test = test.mldr,
              Folder = Folder.Split.Test, nome = "pred-proba")
    
    roc.curva(f = f, y_pred = y_threshold_card, test = test.mldr,
              Folder = Folder.Split.Test, nome = "thr-lc")
    
    roc.curva(f = f, y_pred = y_threshold_05, test = test.mldr,
              Folder = Folder.Split.Test, nome = "thr-05")
    
    
    #f = f + 1
    gc()
  } # end do foreach
  
  gc()
  cat("\n#####################################################")
  cat("\n# RF SILHOUETTE: End gather.preds.python.silho      #")
  cat("\n######################################################")
  cat("\n\n\n\n")
  
} # end da função


############################################################################
#
############################################################################
evaluate.rf.silho <- function(parameters){
  
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
    
    
    ###################################################################
    FolderSplit = paste(parameters$Directories$folderTested,
                              "/Split-", f, sep = "")
    
    #####################################################################
    nome.true = paste(FolderSplit, "/y_true.csv", sep="")
    nome.pred.proba = paste(FolderSplit, "/y_pred_proba.csv", sep="")
    nome.pred.bin = paste(FolderSplit, "/y_pred_bin.csv", sep="")
    nome.thr.05 = paste(FolderSplit, "/y_pred_thr05.csv", sep="")
    nome.thr.LC = paste(FolderSplit, "/y_pred_thrLC.csv", sep="")
    
    #####################################################################
    y_pred_proba = data.frame(read.csv(nome.pred.proba))
    y_pred_bin = data.frame(read.csv(nome.pred.bin))
    y_pred_thr_05 = data.frame(read.csv(nome.thr.05))
    y_pred_thr_lc = data.frame(read.csv(nome.thr.LC))
    y_true = data.frame(read.csv(nome.true))
    
    
    ##########################################################################
    y.true.2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2, 
                                   labelIndices = seq(1,ncol(y.true.2)), 
                                   name = "y.true.2")
    y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
    y_pred_proba = sapply(y_pred_proba, function(x) as.numeric(as.character(x)))
    y_pred_thr_05 = sapply(y_pred_thr_05, function(x) as.numeric(as.character(x)))
    y_pred_thr_lc = sapply(y_pred_thr_lc, function(x) as.numeric(as.character(x)))
    
    
    ##########################################################################    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_bin,
              salva = FolderSplit, nome = "pred-bin")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_proba,
              salva = FolderSplit, nome = "pred-proba")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_thr_05,
              salva = FolderSplit, nome = "thr-05")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_thr_lc,
              salva = FolderSplit, nome = "thr-lc")
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n##################################")
  cat("\n# END FUNCTION EVALUATE          #")
  cat("\n##################################")
  cat("\n\n\n\n")
}




###########################################################################
#
###########################################################################
gather.eval.rf.silho <- function(parameters){
  
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1", 
               "macro-precision", "macro-recall", "margin-loss", 
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision", 
               "ranking-loss", "recall", "subset-accuracy", "wlp")
  
  folds = c(0)
  
  nomes.preds = c("pred-bin", "pred-proba", 
                  "thr-05", "thr-lc")
  
  i = 1
  while(i<=length(nomes.preds)){
    
    cat("\n\npredicao: ", i)
    
    final.roc.auc = data.frame()
    final.roc.auc.micro = data.frame()
    final.roc.auc.macro = data.frame()
    
    final.auprc.macro = data.frame(fold = c(0), value=c(0))
    final.auprc.micro = data.frame(fold = c(0), value=c(0))
    
    final.runtime = data.frame()
    final.conf.mat = data.frame(measures)
    
    
    f = 1
    while(f<=parameters$Config$Number.Folds){
      
      cat("\nFold: ", f)
      
      ###################################################################
      folderSplit = paste(parameters$Directories$folderTested,
                          "/Split-", f, sep = "")
      
      #########################################################################
      confMat = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-evaluated.csv", sep="")))
      names(confMat) = c("Measures", "Fold")
      
      #########################################################################
      confMat[is.na(confMat)] <- 0
      
      #########################################################################
      final.conf.mat = cbind(final.conf.mat, confMat$Fold) 
      folds[f] = paste("Fold-", f, sep="")
      
      #########################################################################
      roc.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-roc-auc.csv", sep="")))       
      final.roc.auc = rbind(final.roc.auc, roc.auc)
      
      #########################################################################
      roc.micro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-micro.csv", sep="")))       
      final.roc.auc.micro = rbind(final.roc.auc.micro, roc.micro.auc)
      
      #########################################################################
      roc.macro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-macro.csv", sep="")))       
      final.roc.auc.macro = rbind(final.roc.auc.macro, roc.macro.auc)
      
      #########################################################################
      auprc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                        "-auprc.csv", sep="")))       
      final.auprc.macro = rbind(final.auprc.macro, 
                                data.frame(fold = f, value = auprc$Macro.AUPRC))
      final.auprc.micro = rbind(final.auprc.micro, 
                                data.frame(fold = f, value = auprc$Micro.AUPRC))
      
      #################################
      runtime = data.frame(read.csv(paste(folderSplit, "/runtime-clusters.csv", sep="")))
      names(runtime) = c("fold", "user.self", "sys.self",
                         "elapsed","user.child","sys.child")
      final.runtime = rbind(final.runtime, runtime)
      
      #################################
      f = f + 1
      gc()
    } 
    
    
    names(final.conf.mat) = c("Measures", folds)
    names(final.roc.auc) = c("Fold", "Value")
    names(final.roc.auc.micro) = c("Fold", "Value")
    names(final.roc.auc.macro) = c("Fold", "Value")
    names(final.auprc.micro) = c("Fold", "Value")
    names(final.auprc.macro) = c("Fold", "Value")
    final.auprc.macro = final.auprc.macro[-1,]
    final.auprc.micro = final.auprc.micro[-1,]
    
    ###########################################
    fold = seq(1, parameters$Config$Number.Folds, by =1)
    
    ###########################################
    names(final.conf.mat) = c("Measures", folds)
    final.conf.mat[is.na(final.conf.mat)] <- 0
    write.csv(final.conf.mat, 
              paste(parameters$Directories$folderTested, "/", nomes.preds[i], 
                    "-Test-Evaluated.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    media = data.frame(apply(final.conf.mat[,-1], 1, mean))
    media = cbind(measures, media)
    names(media) = c("Measures", "Mean10Folds")
    write.csv(media, 
              paste(parameters$Directories$folderTested, "/", 
                    nomes.preds[i], "-Mean10Folds.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    mediana = data.frame(apply(final.conf.mat[,-1], 1, median))
    mediana = cbind(measures, mediana)
    names(mediana) = c("Measures", "Median10Folds")
    write.csv(mediana, 
              paste(parameters$Directories$folderTested, "/", 
                    nomes.preds[i], "-Median10Folds.csv", sep=""), 
              row.names = FALSE)
    
    
    #######################
    desvio = data.frame(apply(final.conf.mat[,-1], 1, sd))
    desvio  = cbind(measures, desvio)
    names(desvio ) = c("Measures", "Deviation10Folds")
    write.csv(desvio , 
              paste(parameters$Directories$folderTested, "/", 
                    nomes.preds[i], "-Deviation10Folds.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc, 
              paste(parameters$Directories$folderTested, "/", nomes.preds[i], 
                    "-roc-auc.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.micro, 
              paste(parameters$Directories$folderTested, "/", nomes.preds[i], 
                    "-roc-auc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.macro, 
              paste(parameters$Directories$folderTested, "/", nomes.preds[i], 
                    "-roc-auc-macro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.auprc.micro, 
              paste(parameters$Directories$folderTested, "/", nomes.preds[i], 
                    "-roc-auprc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.runtime, 
              paste(parameters$Directories$folderTested, 
                    "/runtime-folds.csv", sep=""), 
              row.names = FALSE)
    
    ################
    i = i + 1
    gc()
  }
  
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
