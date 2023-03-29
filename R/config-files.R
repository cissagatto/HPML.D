rm(list = ls())

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



##################################################
# PACKAGES
##################################################
library(stringr)


##################################################
# DATASETS INFORMATION
##################################################
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


##################################################
# WHICH IMPLEMENTATION WILL BE USED?
##################################################
Implementation.1 = c("rf")
Implementation.2 = c("rf")


######################################################
# SIMILARITY MEASURE USED TO MODEL LABEL CORRELATIONS
######################################################
Similarity.1 = c("jaccard")
Similarity.2 = c("j")


##################################################
# LINKAGE METRIC USED TO BUILT THE DENDROGRAM
##################################################
Dendrogram.1 = c("ward.D2")
Dendrogram.2 = c("w")


######################################################
# CRITERIA USED TO CHOOSE THE BEST HYBRID PARTITION
######################################################
Criteria.1 = c("silho")
Criteria.2 = c("s")


######################################################
FolderCF = paste(FolderRoot, "/config-files-apptainer", sep="")
if(dir.exists(FolderCF)==FALSE){dir.create(FolderCF)}

# IMPLEMENTAÇÃO
p = 1
while(p<=length(Implementation.1)){
  
  FolderImplementation = paste(FolderCF, "/", Implementation.1[p], sep="")
  if(dir.exists(FolderImplementation)==FALSE){dir.create(FolderImplementation)}
  
  # SIMILARIDADE
  s = 1
  while(s<=length(Similarity.1)){
    
    FolderSimilarity = paste(FolderImplementation, "/", Similarity.1[s], sep="")
    if(dir.exists(FolderSimilarity)==FALSE){dir.create(FolderSimilarity)}
    
    # DENDROGRAMA
    f = 1
    while(f<=length(Dendrogram.1)){
      
      FolderDendro = paste(FolderSimilarity, "/", Dendrogram.1[f], sep="")
      if(dir.exists(FolderDendro)==FALSE){dir.create(FolderDendro)}
      
      # CRITERIA
      w = 1
      while(w<=length(Criteria.1)){
        
        FolderCriteria = paste(FolderDendro, "/", Criteria.1[w], sep="")
        if(dir.exists(FolderCriteria)==FALSE){dir.create(FolderCriteria)}
        
        # DATASET
        d = 1
        while(d<=nrow(datasets)){
          
          ds = datasets[d,]
          
          cat("\n\n=======================================")
          cat("\n", Implementation.1[p])
          cat("\n\t", Similarity.1[s])
          cat("\n\t", Dendrogram.1[f])
          cat("\n\t", Criteria.1[w])
          cat("\n\t", ds$Name)
          
          name = paste("stand-", ds$Name, sep="")  
          
          file.name = paste(FolderCriteria, "/", name, ".csv", sep="")
          
          output.file <- file(file.name, "wb")
          
          write("Config, Value",
                file = output.file, append = TRUE)
          
           write("Dataset_Path, /Datasets", 
                file = output.file, append = TRUE)
          
          # write("Dataset_Path, /home/elaine/Datasets", 
          #      file = output.file, append = TRUE)
          
          # write("Dataset_Path, /home/biomal/Datasets", 
          #      file = output.file, append = TRUE)
          
          # folder.name = paste("/scratch/", name, sep = "")
          # folder.name = paste("/dev/shm/", name, sep = "")
           folder.name = paste("/tmp/", name, sep = "")
          
          str1 = paste("Temporary_Path, ", folder.name, sep="")
          write(str1,file = output.file, append = TRUE)
          
          str.1 = paste("/2-Best-Partitions/HPML.A/", 
                        Similarity.1[s], "/",
                        Dendrogram.1[f], "/", 
                        Criteria.1[w],
                        sep="")
          str.2 = paste("Partitions_Path, ", str.1,  sep="")
          write(str.2, file = output.file, append = TRUE)
          
          str0 = paste("Implementation, ", Implementation.1[p], sep="")
          write(str0, file = output.file, append = TRUE)
          
          str3 = paste("Similarity, ", Similarity.1[s], sep="")
          write(str3, file = output.file, append = TRUE)
          
          str3 = paste("Dendrogram, ", Dendrogram.1[f], sep="")
          write(str3, file = output.file, append = TRUE)
          
          str2 = paste("Criteria, ", Criteria.1[w], sep="")
          write(str2, file = output.file, append = TRUE)
          
          str3 = paste("Dataset_Name, ", ds$Name, sep="")
          write(str3, file = output.file, append = TRUE)
          
          str4 = paste("Number_Dataset, ", ds$Id, sep="")
          write(str4, file = output.file, append = TRUE)
          
          write("Number_Folds, 10", file = output.file, append = TRUE)
          
          write("Number_Cores, 10", file = output.file, append = TRUE)
          
          close(output.file)
          
          d = d + 1
          gc()
        } # FIM DO DATASET
        
        w = w + 1
        gc()
      } # FIM DO CRITERIO
      
      f = f + 1
      gc()
      
    } # FIM DO DENDROGRAMA
    
    s = s + 1
    gc()
  } # FIM DA SIMILARIDADE
  
  p = p + 1
  gc()
} # FIM DA IMPLEMENTAÇÃO



###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################