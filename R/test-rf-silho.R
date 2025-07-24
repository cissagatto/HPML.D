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
#
#########################################################################
build.rf.silho <- function(parameters) {
  
  # f = 1
  bthpkParalel <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n##########################################################")
    cat("\n# Load R Sources                                         #")
    cat("\n##########################################################\n\n")
    source(file.path(parameters$Config$FolderScript, "libraries.R"))
    source(file.path(parameters$Config$FolderScript, "utils.R"))
    
    
    cat("\n##########################################################")
    cat("\n# Getting information about clusters                     #")
    cat("\n##########################################################\n\n")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    cat("\n##########################################################")
    cat("\n# Creating Folders from Best Partitions and Splits Tests #")
    cat("\n##########################################################\n\n")
    Folder.Best.Partition.Split = paste(parameters$Directories$folderPartitions,
                                        "/Split-",
                                        f,
                                        sep = "")
    
    Folder.Tested.Split = paste(parameters$Directories$folderTested, "/Split-", f, sep = "")
    if (dir.create(Folder.Tested.Split) == FALSE) {
      dir.create(Folder.Tested.Split)
    }
    
    Folder.BP = paste(
      parameters$Directories$folderPartitions,
      "/",
      parameters$Config$Dataset.Name,
      sep = ""
    )
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF,
                        "/Partition-",
                        best.part.info.f$num.part,
                        sep = "")
    
    
    cat("\n##########################################################")
    cat("\n# Opening TRAIN file                                     #")
    cat("\n##########################################################\n\n")
    train.name.file = paste(
      parameters$Directories$folderCVTR,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Tr-",
      f,
      ".csv",
      sep = ""
    )
    train.dataset.original = data.frame(read.csv(train.name.file))
    
    
    cat("\n##########################################################")
    cat("\n# Opening TEST file                                      #")
    cat("\n##########################################################\n\n")
    test.name.file = paste(
      parameters$Directories$folderCVTS,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Ts-",
      f,
      ".csv",
      sep = ""
    )
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    
    cat("\n##########################################################")
    cat("\n# Opening VALIDATION file                                #")
    cat("\n##########################################################\n\n")
    val.name.file = paste(
      parameters$Directories$folderCVVL,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Vl-",
      f,
      ".csv",
      sep = ""
    )
    val.dataset.original = data.frame(read.csv(val.name.file))
    
    
    cat("\n##########################################################")
    cat("\n Join Train and Validation                               #")
    cat("\n##########################################################\n\n")
    tv = rbind(train.dataset.original, val.dataset.original)
    
    
    g = 1
    while (g <= best.part.info.f$num.group) {
      #########################################################################
      cat("\nCreating folder")
      Folder.Tested.Group = paste(Folder.Tested.Split, "/Group-", g, sep =
                                    "")
      if (dir.exists(Folder.Tested.Group) == FALSE) {
        dir.create(Folder.Tested.Group)
      }
      
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
      train.name.csv = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-tr-",
        f,
        "-group-",
        g,
        ".csv",
        sep = ""
      )
      write.csv(train.dataset.cluster, train.name.csv, row.names = FALSE)
      
      #########################################################################
      cat("\nTest: Mount Group")
      test.attributes = test.dataset.original[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      test.classes = select(test.dataset.original, specificGroup$label)
      test.dataset.cluster = cbind(test.attributes, test.classes)
      
      #########################################################################
      cat("\nTest: Save Group")
      test.name.csv = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-ts-",
        f,
        "-group-",
        g,
        ".csv",
        sep = ""
      )
      write.csv(test.dataset.cluster, test.name.csv, row.names = FALSE)
      
      #########################################################################
      cat("\nVal: Mount Group")
      val.attributes = val.dataset.original[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      val.classes = select(val.dataset.original, specificGroup$label)
      val.dataset.cluster = cbind(val.attributes, val.classes)
      
      #########################################################################
      cat("\nVal: Save Group")
      val.name.csv = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-vl-",
        f,
        "-group-",
        g,
        ".csv",
        sep = ""
      )
      write.csv(val.dataset.cluster, val.name.csv, row.names = FALSE)
      
      #########################################################################
      cat("\nJuntando treino com validação")
      tv.dataset.cluster = rbind(train.dataset.cluster, val.dataset.cluster)
      
      ########################################################################
      fim = parameters$DatasetInfo$LabelStart + (nrow(specificGroup) - 1)
      labels.indices = seq(parameters$DatasetInfo$LabelStart, fim, by =
                             1)
      nomes.labels.clusters = specificGroup$label
      
      if (nrow(specificGroup) == 1) {
        cat("\n\n#==============================#\n")
        cat(sprintf("# FOLD      [%2d]               #\n", f))
        cat(sprintf("# CLUSTER   [%2d]               #\n", g))
        cat("# SINGLE-LABEL                 #\n")
        cat("#==============================#\n\n")
        
        
        #######################################################################
        # cat("\nExecute ECC PYTHON")
        num.labels = as.numeric(nrow(specificGroup))
        str.execute = paste(
          "python3 ",
          parameters$Directories$folderPython,
          "/standard_single.py ",
          train.name.csv,
          " ",
          val.name.csv,
          " ",
          test.name.csv,
          " ",
          parameters$DatasetInfo$AttEnd,
          " ",
          Folder.Tested.Group,
          " ",
          fold = f,
          sep = ""
        )
        
        # EXECUTA
        start <- proc.time()
        res = system(str.execute)
        tempo = data.matrix((proc.time() - start))
        tempo = data.frame(t(tempo))
        write.csv(tempo,
                  paste(Folder.Tested.Group, "/runtime-cluster.csv", sep = ""))
        if (res != 0) {
          break
        }
        
        setwd(Folder.Tested.Group)
        unlink(val.name.csv)
        unlink(test.name.csv)
        unlink(train.name.csv)
        
      } else {
        cat("\n\n#==============================#\n")
        cat(sprintf("# FOLD      [%2d]               #\n", f))
        cat(sprintf("# CLUSTER   [%2d]               #\n", g))
        cat("# MULTI-LABEL                  #\n")
        cat("#==============================#\n\n")
        
        train.mldr = mldr_from_dataframe(train.dataset.cluster, labelIndices = labels.indices)
        test.mldr = mldr_from_dataframe(test.dataset.cluster, labelIndices = labels.indices)
        val.mldr = mldr_from_dataframe(val.dataset.cluster, labelIndices = labels.indices)
        tv.mldr = mldr_from_dataframe(tv.dataset.cluster, labelIndices = labels.indices)
        
        ##############################################
        # cat("\nPropriedades dos clusters")
        properties.clusters(
          nomes.labels.clusters,
          fold = f,
          cluster = g,
          folderSave = Folder.Tested.Group,
          labels.indices,
          train = train.dataset.cluster,
          test = test.dataset.cluster,
          val = val.dataset.cluster,
          tv = tv.dataset.cluster
        )
        
        
        #######################################################################
        #cat("\nExecute ECC PYTHON")
        num.labels = as.numeric(nrow(specificGroup))
        str.execute = paste(
          "python3 ",
          parameters$Directories$folderPython,
          "/standard_multilabel.py ",
          train.name.csv,
          " ",
          val.name.csv,
          " ",
          test.name.csv,
          " ",
          parameters$DatasetInfo$AttEnd,
          " ",
          Folder.Tested.Group,
          " ",
          fold = f,
          sep = ""
        )
        
        # EXECUTA
        start <- proc.time()
        res = system(str.execute)
        tempo = data.matrix((proc.time() - start))
        tempo = data.frame(t(tempo))
        write.csv(tempo,
                  paste(Folder.Tested.Group, "/runtime-cluster.csv", sep = ""))
        
        if (res != 0) {
          break
        }
        
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
  
  # f = 1
  gatherR <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n##########################################################")
    cat("\n# Load R Sources                                         #")
    cat("\n##########################################################\n\n")
    source(file.path(parameters$Config$FolderScript, "libraries.R"))
    source(file.path(parameters$Config$FolderScript, "utils.R"))
    
    ###################################################################
    Folder.Split.Test = paste(parameters$Directories$folderTested, "/Split-", f, sep = "")
    
    
    cat("\n##########################################################")
    cat("\n# Getting information about clusters                     #")
    cat("\n##########################################################\n\n")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Directories$folderPartitions,
                                        "/Split-",
                                        f,
                                        sep = "")
    
    Folder.Tested.Split = paste(parameters$Directories$folderTested, "/Split-", f, sep = "")
    
    Folder.BP = paste(
      parameters$Directories$folderPartitions,
      "/",
      parameters$Config$Dataset.Name,
      sep = ""
    )
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF,
                        "/Partition-",
                        best.part.info.f$num.part,
                        sep = "")
    
    #################
    f.dependency = data.frame(
      fold = 0,
      cluster = 0,
      train = 0,
      test = 0,
      val = 0,
      tv = 0
    )
    
    f.labelsets =  data.frame(
      fold = 0,
      cluster = 0,
      type = "",
      labelset = 0,
      frequency = 0
    )
    
    f.info = data.frame(
      stat = "",
      type = "",
      index = 0,
      count = 0,
      freq = 0,
      IRLbl = 0,
      SCUMBLE = 0,
      SCUMBLE.CV = 0
    )
    
    f.properties = data.frame(
      fold = 0,
      cluster = 0,
      type = "",
      num.attributes = 0,
      num.instances = 0,
      num.inputs = 0,
      num.labels = 0,
      num.labelsets = 0,
      num.single.labelsets = 0,
      max.frequency = 0,
      cardinality = 0,
      density = 0,
      meanIR = 0,
      scumble = 0,
      scumble.cv = 0,
      tcs = 0
    )
    
    f.model.size = data.frame(fold = 0,
                              cluster = 0,
                              size = 0)
    
    f.pos.neg = data.frame(
      fold = 0,
      cluster = 0,
      type = "",
      label = "",
      negative = 0,
      positive = 0
    )
    
    f.runtime = data.frame(
      fold = 0,
      cluster = 0,
      user.self = 0,
      sys.self = 0,
      elapsed = 0,
      user.child = 0,
      sys.child = 0
    )
    
    f.runtime.python = data.frame(
      fold = 0,
      cluster = 0,
      train_duration = 0,
      test_duration_proba = 0
    )
    
    f.metrics = data.frame()
    f.summary = data.frame()
    f.proba = data.frame(apagar = c(0))
    f.true = data.frame(apagar = c(0))
    f.bin = data.frame(apagar = c(0))
    
    g = 1
    while (g <= best.part.info.f$num.group) {
      cat("\nGroup: ", g)
      
      Folder.Group.Test = paste(Folder.Split.Test, "/Group-", g, sep = "")
      
      cat("\ndependency")
      name.dep = paste0(Folder.Group.Test, "/dependency.csv")
      if (file.exists(name.dep) == TRUE) {
        dependency = data.frame(read.csv(name.dep))
        f.dependency = rbind(f.dependency, dependency)
        print(system(paste0("rm -r ", name.dep)))
        cat("\n")
      } else{
        dependency = data.frame(
          fold = f,
          cluster = g,
          train = 0,
          test = 0,
          val = 0,
          tv = 0
        )
        f.dependency = rbind(f.dependency, dependency)
      }
      
      cat("\nlabelset")
      name.ls = paste0(Folder.Group.Test, "/labelsets.csv")
      if (file.exists(name.ls) == TRUE) {
        labelsets = data.frame(read.csv(name.ls))
        f.labelsets = rbind(f.labelsets, labelsets)
        print(system(paste0("rm -r ", name.ls)))
        cat("\n")
      } else {
        labelsets =  data.frame(
          fold = f,
          cluster = g,
          type = "",
          labelset = 0,
          frequency = 0
        )
        f.labelsets = rbind(f.labelsets, labelsets)
      }
      
      cat("\ninfo")
      name.info = paste0(Folder.Group.Test, "/labels-info.csv")
      if (file.exists(name.info) == TRUE) {
        info = data.frame(read.csv(name.info))
        f.info = rbind(f.info, info)
        print(system(paste0("rm -r ", name.info)))
        cat("\n")
      } else {
        info = data.frame(
          stat = "",
          type = "",
          index = 0,
          count = 0,
          freq = 0,
          IRLbl = 0,
          SCUMBLE = 0,
          SCUMBLE.CV = 0
        )
        f.info = rbind(f.info, info)
      }
      
      cat("\nmeasures")
      name.properties = paste0(Folder.Group.Test, "/measures.csv")
      if (file.exists(name.properties) == TRUE) {
        properties = data.frame(read.csv(name.properties))
        properties <- properties %>% select(-fold.1, -cluster.1)
        f.properties = rbind(f.properties, properties)
        print(system(paste0("rm -r ", name.properties)))
        cat("\n")
      } else {
        properties = data.frame(
          fold = f,
          cluster = g,
          type = "",
          num.attributes = 0,
          num.instances = 0,
          num.inputs = 0,
          num.labels = 0,
          num.labelsets = 0,
          num.single.labelsets = 0,
          max.frequency = 0,
          cardinality = 0,
          density = 0,
          meanIR = 0,
          scumble = 0,
          scumble.cv = 0,
          tcs = 0
        )
        f.properties = rbind(f.properties, properties)
      }
      
      cat("\npos neg")
      name.pos.neg = paste0(Folder.Group.Test, "/num-pos-neg.csv")
      if (file.exists(name.pos.neg) == TRUE) {
        pos.neg = data.frame(read.csv(name.pos.neg))
        f.pos.neg = rbind(f.pos.neg, pos.neg)
        print(system(paste0("rm -r ", name.pos.neg)))
        cat("\n")
      } else {
        pos.neg = data.frame(
          fold = f,
          cluster = g,
          type = "",
          label = "",
          negative = 0,
          positive = 0
        )
        f.pos.neg = rbind(f.pos.neg, pos.neg)
      }
      
      cat("\nmodel-size")
      name.model.size = paste0(Folder.Group.Test, "/model-size.csv")
      if (file.exists(name.model.size) == TRUE) {
        model.size = data.frame(read.csv(name.model.size))
        model.size = data.frame(
          fold = f,
          cluster = g,
          size = model.size$model_size_bytes
        )
        f.model.size = rbind(f.model.size, model.size)
        print(system(paste0("rm -r ", name.model.size)))
        cat("\n")
      }
      
      cat("\nperformance")
      name.metrics = paste0(Folder.Group.Test, "/results-python.csv")
      if (file.exists(name.metrics) == TRUE) {
        metrics = data.frame(read.csv(name.metrics))
        metrics = data.frame(t(metrics))
        nomes = metrics[1, ]
        colnames(metrics) = nomes
        metrics = metrics[-1, ]
        metrics = data.frame(fold = f, cluster = g, metrics)
        rownames(metrics) = NULL
        f.metrics = rbind(f.metrics, metrics)
        print(system(paste0("rm -r ", name.metrics)))
        cat("\n")
      }
      
      cat("\nruntime")
      name.runtime = paste0(Folder.Group.Test, "/runtime-cluster.csv")
      if (file.exists(name.runtime) == TRUE) {
        runtime = data.frame(read.csv(name.runtime))
        runtime = runtime[, -1]
        runtime = data.frame(fold = f, cluster = g, runtime)
        f.runtime = rbind(f.runtime, runtime)
        print(system(paste0("rm -r ", name.runtime)))
        cat("\n")
      }
      
      cat("\nruntime")
      name.runtime.python = paste0(Folder.Group.Test, "/runtime-python.csv")
      if (file.exists(name.runtime.python) == TRUE) {
        runtime = data.frame(read.csv(name.runtime.python))
        runtime = data.frame(fold = f, cluster = g, runtime)
        f.runtime.python = rbind(f.runtime.python, runtime)
        print(system(paste0("rm -r ", name.runtime.python)))
        cat("\n")
      }
      
      # cat("\nsummary")
      #name.summary = paste0(Folder.Group.Test, "/summary.csv")
      #summary = data.frame(read.csv(name.summary))
      #f.summary = rbind(f.summary, summary)
      
      cat("\nproba")
      name.proba = paste0(Folder.Group.Test, "/y_pred_proba.csv")
      y_proba = data.frame(read.csv(name.proba))
      f.proba = data.frame(f.proba, y_proba)
      #print(system(paste0("rm -r ", name.proba)))
      cat("\n")
      
      cat("\ntrue")
      name.true = paste0(Folder.Group.Test, "/y_true.csv")
      y_true = data.frame(read.csv(name.true))
      f.true = data.frame(f.true, y_true)
      #print(system(paste0("rm -r ", name.true)))
      cat("\n")
      
      #cat("\nbin")
      #name.bin = paste0(Folder.Group.Test, "/y_pred_bin.csv")
      #y_bin = data.frame(read.csv(name.bin))
      #f.bin = data.frame(f.bin, y_bin)
      #print(system(paste0("rm -r ", name.bin)))
      #cat("\n")
      
      g = g + 1
      gc()
    }
    
    nome = paste(Folder.Split.Test, "/dependency.csv", sep = "")
    f.dependency = f.dependency[-1, ]
    write.csv(f.dependency, nome, row.names = FALSE)
    
    nome  = paste(Folder.Split.Test, "/labelsets.csv", sep = "")
    f.labelsets = f.labelsets[-1, ]
    write.csv(f.labelsets, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/info.csv", sep = "")
    f.info = f.info[-1, ]
    write.csv(f.info, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/properties.csv", sep = "")
    f.properties = f.properties[-1, ]
    write.csv(f.properties, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/model-size.csv", sep = "")
    f.model.size = f.model.size[-1, ]
    write.csv(f.model.size, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/pos-neg.csv", sep = "")
    f.pos.neg = f.pos.neg[-1, ]
    write.csv(f.pos.neg, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/runtime.csv", sep = "")
    f.runtime = f.runtime[, -1]
    write.csv(f.runtime, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/runtime-python.csv", sep = "")
    f.runtime.python = f.runtime.python[, -1]
    write.csv(f.runtime.python, nome, row.names = FALSE)
    
    nome = paste(Folder.Split.Test, "/results-clusters.csv", sep = "")
    f.metrics = f.metrics[, -1]
    write.csv(f.metrics, nome, row.names = FALSE)
    
    nome.true = paste(Folder.Split.Test, "/y_true.csv", sep = "")
    f.true = f.true[, -1]
    write.csv(f.true, nome.true, row.names = FALSE)
    
    nome.pred.proba = paste(Folder.Split.Test, "/y_pred_proba.csv", sep ="")
    f.proba = f.proba[, -1]
    write.csv(f.proba, nome.pred.proba, row.names = FALSE)
    
    #nome.pred.bin = paste(Folder.Split.Test, "/y_pred_bin.csv", sep = "")
    #f.bin = f.bin[, -1]
    #write.csv(f.bin, nome.pred.bin, row.names = FALSE)
    
    # f = f + 1
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
evaluate.rf.silho <- function(parameters) {
  
  # f = 1
  avaliaParalel <- foreach (f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n##########################################################")
    cat("\n# Load R Sources                                         #")
    cat("\n##########################################################\n\n")
    source(file.path(parameters$Config$FolderScript, "libraries.R"))
    source(file.path(parameters$Config$FolderScript, "utils.R"))
    
    ###################################################################
    Folder.Split.Test = paste(parameters$Directories$folderTested, "/Split-", f, sep = "")
    
    nome.true = paste(Folder.Split.Test, "/y_true.csv", sep = "")
    nome.pred.proba = paste(Folder.Split.Test, "/y_pred_proba.csv", sep =
                              "")
    # nome.pred.bin = paste(Folder.Split.Test, "/y_pred_bin.csv", sep = "")
    
    y_pred_proba = data.frame(read.csv(nome.pred.proba))
    # y_pred_bin = data.frame(read.csv(nome.pred.bin))
    y_true = data.frame(read.csv(nome.true))
    
    
    #########################################################################
    test.name.file = paste(
      parameters$Directories$folderCVTS,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Ts-",
      f,
      ".csv",
      sep = ""
    )
    
    test.dataset.original = data.frame(read.csv(test.name.file))
    
    labels.indices = seq(parameters$DatasetInfo$LabelStart,
                         parameters$DatasetInfo$LabelEnd,
                         by = 1)
    
    test.mldr = mldr_from_dataframe(test.dataset.original, labelIndices = labels.indices)
    
    train.name.file = paste(
      parameters$Directories$folderCVTR,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Tr-",
      f,
      ".csv",
      sep = ""
    )
    
    train.dataset.original = data.frame(read.csv(train.name.file))
    train.mldr = mldr_from_dataframe(train.dataset.original, labelIndices = labels.indices)
    
    val.name.file = paste(
      parameters$Directories$folderCVVL,
      "/",
      parameters$Config$Dataset.Name,
      "-Split-Vl-",
      f,
      ".csv",
      sep = ""
    )
    val.dataset.original = data.frame(read.csv(val.name.file))
    val.mldr = mldr_from_dataframe(val.dataset.original, labelIndices = labels.indices)
    
    tv = rbind(train.dataset.original, val.dataset.original)
    mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
    
    ##########################################################################
    y.true.2 = data.frame(sapply(y_true, function(x)
      as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2,
                                   labelIndices = seq(1, ncol(y.true.2)),
                                   name = "y.true.2")
    #y_pred_bin = sapply(y_pred_bin, function(x)
    #  as.numeric(as.character(x)))
    y_pred_proba = sapply(y_pred_proba, function(x)
      as.numeric(as.character(x)))
    
    ########################################################################
    y_threshold_05 <- data.frame(as.matrix(fixed_threshold(y_pred_proba,
                                                           threshold = 0.5)))
    write.csv(y_threshold_05, 
              paste(Folder.Split.Test, "/y_pred_thr05.csv", sep=""),
              row.names = FALSE)
    
    ########################################################################
    y_threshold_card = lcard_threshold(as.matrix(y_pred_proba),
                                       mldr.tv$measures$cardinality,
                                       probability = F)
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    write.csv(
      y_threshold_card,
      paste(Folder.Split.Test, "/y_pred_thrLC.csv", sep = ""),
      row.names = FALSE
    )
    
    #########################################################################
    matrix.confusao(
      true = y_true,
      pred = y_threshold_05,
      type = "bin",
      salva = Folder.Split.Test,
      nomes.rotulos = parameters$Names.Labels$Labels
    )
    
    #########################################################################
    avaliacao(
      f = f,
      y_true = y.true.3,
      y_pred = y_pred_proba,
      salva = Folder.Split.Test,
      nome = "pred-proba"
    )
    
    #########################################################################
    str.execute = paste(
      "python3 ",
      parameters$Directories$folderPython,
      "/curves.py ",
      train.name.file,
      " ",
      val.name.file,
      " ",
      test.name.file,
      " ",
      parameters$DatasetInfo$AttEnd,
      " ",
      Folder.Split.Test,
      " ",
      fold = f,
      sep = ""
    )
    
    res = print(system(str.execute))
    
    if (res != 0) {
      break
    }
    
    # juntar resultados python com utiml
    name1 = paste0(Folder.Split.Test,"/pred-proba-evaluated.csv")
    data.utiml = data.frame(read.csv(name1))
    colnames(data.utiml) = c("Measures", paste0("Fold", f))
    system(paste0("rm -r ", name1))
    
    name2 = paste0(Folder.Split.Test,"/results-python.csv")
    data.python = data.frame(read.csv(name2))
    colnames(data.python) = c("Measures", paste0("Fold", f))
    system(paste0("rm -r ", name2))
    
    name = paste0(Folder.Split.Test,"/performance.csv")
    all = rbind(data.python, data.utiml)
    write.csv(all, name, row.names = FALSE)
    
    #  f = f + 1
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
gather.eval.rf.silho <- function(parameters) {
  
  f.dependency = data.frame(
    fold = 0,
    cluster = 0,
    train = 0,
    test = 0,
    val = 0,
    tv = 0
  )
  
  f.labelsets =  data.frame(
    fold = 0,
    cluster = 0,
    type = "",
    labelset = 0,
    frequency = 0
  )
  
  f.info = data.frame(
    fold = 0,
    stat = "",
    type = "",
    index = 0,
    count = 0,
    freq = 0,
    IRLbl = 0,
    SCUMBLE = 0,
    SCUMBLE.CV = 0
  )
  
  f.properties = data.frame(
    fold = 0,
    cluster = 0,
    type = "",
    num.attributes = 0,
    num.instances = 0,
    num.inputs = 0,
    num.labels = 0,
    num.labelsets = 0,
    num.single.labelsets = 0,
    max.frequency = 0,
    cardinality = 0,
    density = 0,
    meanIR = 0,
    scumble = 0,
    scumble.cv = 0,
    tcs = 0
  )
  
  f.model.size = data.frame(fold = 0,
                            cluster = 0,
                            size = 0)
  
  f.pos.neg = data.frame(
    fold = 0,
    cluster = 0,
    type = "",
    label = "",
    negative = 0,
    positive = 0
  )
  
  f.runtime = data.frame(
    fold = 0,
    cluster = 0,
    user.self = 0,
    sys.self = 0,
    elapsed = 0,
    user.child = 0,
    sys.child = 0
  )
  
  f.runtime.python = data.frame(
    fold = 0,
    cluster = 0,
    train_duration = 0,
    test_duration_proba = 0
    #test_duration_bin = 0
  )
  
  f.metrics = data.frame(apagar=c(0))
  f.proba = data.frame(apagar = c(0))
  f.true = data.frame(apagar = c(0))
  f.bin = data.frame(apagar = c(0))
  
  f = 1
  while (f <= parameters$Config$Number.Folds) {
    
    cat("\n#============================#")
    cat("\n# FOLD ", f)
    cat("\n#============================#\n")
    
    folderSplit = paste(parameters$Directories$folderTested, "/Split-", f, sep = "")
    
    name.ld = paste0(folderSplit, "/dependency.csv")
    data.ld = data.frame(read.csv(name.ld))
    f.dependency = rbind(f.dependency, data.ld)
    #system(paste0("rm -r ", name.ld))
    
    name.if = paste0(folderSplit, "/info.csv")
    data.if = data.frame(read.csv(name.if))
    f.info = rbind(f.info, cbind(fold = f, data.if))
    #system(paste0("rm -r ", name.if))
    
    name.ls = paste0(folderSplit, "/labelsets.csv")
    data.ls = data.frame(read.csv(name.ls))
    f.labelsets = rbind(f.labelsets, data.ls)
    #system(paste0("rm -r ", name.ls))
    
    name.ms = paste0(folderSplit, "/model-size.csv")
    data.ms = data.frame(read.csv(name.ms))
    f.model.size = rbind(f.model.size, data.ms)
    #system(paste0("rm -r ", name.ms))
    
    name.mt = paste0(folderSplit, "/performance.csv")
    data.mt = data.frame(read.csv(name.mt))
    colnames(data.mt)[1] = c("Measures")
    f.metrics = cbind(f.metrics, data.mt)
    #system(paste0("rm -r ", name.mt))
    
    name.pn = paste0(folderSplit, "/pos-neg.csv")
    data.pn = data.frame(read.csv(name.pn))
    f.pos.neg = rbind(f.pos.neg, data.pn)
    #system(paste0("rm -r ", name.pn))
    
    name.pr = paste0(folderSplit, "/properties.csv")
    data.pr = data.frame(read.csv(name.pr))
    f.properties = rbind(f.properties, data.pr)
    #system(paste0("rm -r ", name.pr))
    
    name.rt = paste0(folderSplit, "/runtime.csv")
    data.rt = data.frame(read.csv(name.rt))
    data.rt = data.frame(fold = f, data.rt)
    f.runtime = rbind(f.runtime, data.rt)
    #system(paste0("rm -r ", name.rt))
    
    name.rtp = paste0(folderSplit, "/runtime-python.csv")
    data.rtp = data.frame(read.csv(name.rtp))
    data.rtp = data.frame(fold = f, data.rtp)
    f.runtime.python = rbind(f.runtime.python, data.rtp)
    #system(paste0("rm -r ", name.rtp))
    
    f = f + 1
    gc()
  }
  
  f.dependency = f.dependency[-1,]
  mean.dependency = data.frame(apply(f.dependency,2,mean))
  names(mean.dependency) = "mean"
  name = paste0(parameters$Directories$folderTested, "/dependency.csv")
  write.csv(mean.dependency, name)
  
  f.info = f.info[-1,]
  name = paste0(parameters$Directories$folderTested, "/info.csv")
  write.csv(f.info, name, row.names = FALSE)
  
  f.labelsets = f.labelsets[-1,]
  name = paste0(parameters$Directories$folderTested, "/labelsets.csv")
  write.csv(f.labelsets, name, row.names = FALSE)
  
  f.model.size = f.model.size[,-1]
  model.size.mean = data.frame(apply(f.model.size,2,mean))
  names(model.size.mean) = "mean"
  name = paste0(parameters$Directories$folderTested, "/model-size.csv")
  write.csv(model.size.mean, name)
  
  f.metrics = f.metrics[,-1]
  df_filtrado <- f.metrics[ , !grepl("^Measures\\.\\d+$", names(f.metrics))]
  name = paste0(parameters$Directories$folderTested, "/performance.csv")
  write.csv(df_filtrado, name, row.names = FALSE)
  
  f.pos.neg = f.pos.neg[-1,]
  name = paste0(parameters$Directories$folderTested, "/pos-neg.csv")
  write.csv(f.pos.neg, name, row.names = FALSE)
  
  f.properties = f.properties[-1,]
  name = paste0(parameters$Directories$folderTested, "/properties.csv")
  write.csv(f.properties, name, row.names = FALSE)
  
  f.runtime = f.runtime[-1,]
  df_filtrado = f.runtime[!apply(f.runtime[, 3:7] == 0, 1, all), ]
  mean.runtime = data.frame(apply(f.runtime, 2, mean))
  names(mean.runtime) = "mean"
  name = paste0(parameters$Directories$folderTested, "/runtime.csv")
  write.csv(mean.runtime, name)
  
  f.runtime.python = f.runtime.python[-1,]
  df_filtrado = f.runtime[!apply(f.runtime[, 3:5] == 0, 1, all), ]
  mean.runtimeP = data.frame(apply(f.runtime.python, 2, mean))
  names(mean.runtimeP) = "mean"
  name = paste0(parameters$Directories$folderTested, "/runtime-python.csv")
  write.csv(mean.runtimeP, name)
  
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
 