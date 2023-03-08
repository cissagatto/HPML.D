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


##############################################################################
# 
##############################################################################
execute.run.rf <- function(parameters){
  
  FolderRoot = "~/Standard-HPML"
  FolderScripts = "~/Standard-HPML/R"
  
  setwd(FolderScripts)
  source("libraries.R")
  
  setwd(FolderScripts)
  source("test-rf-silho.R")
  
  if(parameters$Config$Number.Cores == 0){
    
    cat("\n\n########################################################")
      cat("\n# Zero is a disallowed value for number_cores. Please  #")
      cat("\n# choose a value greater than or equal to 1.           #")
      cat("\n########################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(parameters$Config$Number.Cores == 0)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n######################################################")
        cat("\n# Running Sequentially!                              #")
        cat("\n######################################################\n\n")
    } else {
      cat("\n\n#############################################################################")
        cat("\n# Running in parallel with ", parameters$Config$Number.Cores == 0, " cores! #")
        cat("\n#############################################################################\n\n")
    }
  }
  cl = cl
  
  cat("\n\n#######################################################")
    cat("\n# RUN RF: Get labels                                  #")
    cat("\n#######################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  parameters$Config$NamesLabels = namesLabels
  
  
  cat("\n\n#######################################################")
    cat("\n# RUN RF: Get the label space                         #")
    cat("\n#######################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$LabelSpace = resLS
  
  
  cat("\n\n#################################################")
    cat("\n# RUN RF: Get all partitions                    #")
    cat("\n#################################################\n\n")
  timeAllPartitions = system.time(resAP <- get.all.partitions(parameters))
  parameters$All.Partitions = resAP
  
  
  if(parameters$Config$Criteria=="maf1"){
    
    cat("\n\n######################################################")
      cat("\n# RUN RF MACRO-F1: Build and Test Partitions         #")
      cat("\n######################################################\n\n")
    timeBuild = system.time(resBuild <- build.rf.maf1(parameters))
    
    
    cat("\n\n#######################################################")
      cat("\n# RUN RF MACRO-F1: Matrix Confusion                   #")
      cat("\n#######################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.rf.maf1(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN RF MACRO-F1: Evaluation                          #")
      cat("\n########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.rf.maf1(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN RF MACRO-F1: Mean 10 Folds                       #")
      cat("\n########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.rf.maf1(parameters))
    
    
    cat("\n\n#######################################################")
      cat("\n# RUN RF MACRO-F1: Save Runtime                       #")
      cat("\n#######################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-RF-Maf1.csv")
    
    
  } else if(parameters$Config$Criteria=="mif1"){
    
    cat("\n\n########################################################")
       cat("\n# RUN RF MICRO-F1: Build and Test Partitions          #")
       cat("\n#######################################################\n\n")
    timeBuild = system.time(resBuild <- build.rf.mif1(parameters))
    
    
    cat("\n\n######################################################")
      cat("\n# RUN RFR MICRO-F1: Matrix Confusion                 #")
      cat("\n######################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.rf.mif1(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN RF MICRO-F1: Evaluation                          #")
      cat("\n########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.rf.mif1(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN RF MICRO-F1: Mean 10 Folds                       #")
      cat("\n########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.rf.mif1(parameters))
    
    
    cat("\n\n#####################################################")
      cat("\n# RUN RF MICRO-F1: Save Runtime                     #")
      cat("\n#####################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-RF-Mif1.csv")
    
    
    
  } else {
    
    cat("\n\n#####################################################")
      cat("\n# RUN RF SILHOUETTE: Build and Test Partitions      #")
      cat("\n######################################################\n\n")
    timeBuild = system.time(resBuild <- build.rf.silho(parameters))
    
    
    cat("\n\n######################################################")
      cat("\n# RUN RF SILHOUETTE: Matrix Confusion                #")
      cat("\n######################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.rf.silho(parameters))
    
    
    cat("\n\n######################################################")
      cat("\n# RUN RF SILHOUETTE: Evaluation                      #")
      cat("\n######################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.rf.silho(parameters))
    
    
    cat("\n\n######################################################")
      cat("\n# RUN RF SILHOUETTE: Mean 10 Folds                   #")
      cat("\n######################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.rf.silho(parameters))
    
    
    cat("\n\n#####################################################")
      cat("\n# RUN RF SILHOUETTE: Save Runtime                   #")
      cat("\n#####################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-RF-Silho.csv")
    
  }
  
  
  cat("\n\n##########################################################")
  cat("\n# RUN: Stop Parallel                                     #")
  cat("\n##########################################################\n\n")
  parallel::stopCluster(cl) 	
  
  cat("\n\n##########################################################")
  cat("\n# RUN: END                                               #")
  cat("\n##########################################################\n\n")
  gc()
  
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
