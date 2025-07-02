# STANDARD HPML
This code is part of my PhD research at PPG-CC/DC/UFSCar in colaboration with Katholieke Universiteit Leuven Campus Kulak Kortrijk in Belgium. This code test the best hybrid partition chosen - with any of the criteria methods - using CLUS or Random Forests.

# FLOW DESIGN


## How to Cite 📑
If you use this code in your research, please cite the following:

```bibtex
@misc{Gatto2025,
  author = {Gatto, E. C.},
  title = {HPML standard},
  year = {2025},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/cissagatto/HPML.D}}
}
```

## 🗂️ Project Structure

The codebase includes R and Python scripts that must be used together.

### R Scripts (in `/R` folder):

* `config-files.R`
* `libraries.R`
* `run-clus.R`
* `run-rf.R`
* `standard.R`
* `test-clus-maf1.R`
* `test-clus-mif1.R`
* `test-clus-silho.R`
* `test-rf-silho.R`
* `utils.R`


### Python Scripts (in `/Python` folder):

* `confusion_matrix.py`
* `curves.py`
* `evaluation.py`
* `measures.py`
* `standard_multilabel.py`
* `standard_single.py`


## Preparing your experiment

### STEP 1
A file called _datasets-original.csv_ must be in the *root project directory*. This file is used to read information about the datasets and they are used in the code. We have 90 multilabel datasets in this _.csv_ file. If you want to use another dataset, please, add the following information about the dataset in the file:


| Parameter    | Status    | Description                                           |
|------------- |-----------|-------------------------------------------------------|
| Id           | mandatory | Integer number to identify the dataset                |
| Name         | mandatory | Dataset name (please follow the benchmark)            |
| Domain       | optional  | Dataset domain                                        |
| Instances    | mandatory | Total number of dataset instances                     |
| Attributes   | mandatory | Total number of dataset attributes                    |
| Labels       | mandatory | Total number of labels in the label space             |
| Inputs       | mandatory | Total number of dataset input attributes              |
| Cardinality  | optional  | **                                                    |
| Density      | optional  | **                                                    |
| Labelsets    | optional  | **                                                    |
| Single       | optional  | **                                                    |
| Max.freq     | optional  | **                                                    |
| Mean.IR      | optional  | **                                                    | 
| Scumble      | optional  | **                                                    | 
| TCS          | optional  | **                                                    | 
| AttStart     | mandatory | Column number where the attribute space begins * 1    | 
| AttEnd       | mandatory | Column number where the attribute space ends          |
| LabelStart   | mandatory | Column number where the label space begins            |
| LabelEnd     | mandatory | Column number where the label space ends              |
| Distinct     | optional  | ** 2                                                  |
| xn           | mandatory | Value for Dimension X of the Kohonen map              | 
| yn           | mandatory | Value for Dimension Y of the Kohonen map              |
| gridn        | mandatory | X times Y value. Kohonen's map must be square         |
| max.neigbors | mandatory | The maximum number of neighbors is given by LABELS -1 |


1 - Because it is the first column the number is always 1.

2 - [Click here](https://link.springer.com/book/10.1007/978-3-319-41111-8) to get explanation about each property.


### STEP 2: Cross-Validation Files
The experiment requires pre-processed cross-validation files in `.tar.gz` format. You can download the 10-fold files for multilabel datasets [here](https://1drv.ms/u/s!Aq6SGcf6js1mrZJSkZ3VEJ217rEd5A?e=IH73m3).

For new datasets, you can generate these files by following the instructions in [this repository](https://github.com/cissagatto/crossvalidationmultilabel). After generating the files, place the `.tar.gz` archive in any directory, and provide the absolute path in the configuration file for the `global.R` script.



### STEP 3
You will need the previously best chosen partitions by one of the following codes:

- https://github.com/cissagatto/Best-Hybrid-Partition (beeing implemented)

- https://github.com/cissagatto/Best-Partition-Silhouette

- https://github.com/cissagatto/Best-Partition-MaF1

- https://github.com/cissagatto/Best-Partition-MiF1


You must use here the results generated from the *OUTPUT* directory in that source code. They must be compressed into a *TAR.GZ* file and placed in a directory on your computer. The absolute path of this directory must be passed as a parameter in the configuration file. Please see the configuration file example in this source code (config-files directory).




### STEP 4
You need to have installed all the Java, Python and R packages required to execute this code on your machine or server. This code does not provide any type of automatic package installation!

You can use the [Conda Environment](https://1drv.ms/u/s!Aq6SGcf6js1mw4hbhU9Raqarl8bH8Q?e=IA2aQs) that I created to perform this experiment. Below are the links to download the files. Try to use the command below to extract the environment to your computer:

```
conda env create -file AmbienteTeste.yaml
```

See more information about Conda environments [here](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html) 

You can also run this code using the AppTainer [container](https://1drv.ms/u/s!Aq6SGcf6js1mw4hcVuz_IN8_Bh1oFQ?e=5NuyxX) that I'm using to run this code in a SLURM cluster. Please, check this [tutorial](https://rpubs.com/cissagatto/apptainer-slurm-r) (in portuguese) to see how to do that. 


### STEP 5
To run this code you will need a configuration file saved in *csv* format and with the following information:

| Config          | Value                                                                         | 
|-----------------|-------------------------------------------------------------------------------| 
| FolderScripts   | Absolute path to the R folder scripts                                         |
| Dataset_Path    | Absolute path to the directory where the dataset's tar.gz is stored           |
| Temporary_Path  | Absolute path to the directory where temporary processing will be performed*  |
| Partitions_Path | Absolute path to the directory where the best partitions are                  |
| Implementation  | Must be "clus" or "rf"                                                        |
| Dendrogram      | The linkage metric that were used to built the dendrogram:                    |
|                 | - single, complete, average, ward.D, ward.D2 or mcQuitty                      |
| Similarity      | Must be "jaccard", "rogers" or another similarity measure                     |
| Criteria        | Must be "maf1" to test the best partition chosen with Macro-F1,               |
|                 | "mif1" to test the best partition chosen with Micro-F1,                       |
|                 | or "silho" to test the best partition chosen with Silhouette Coefficient      |
| Dataset_Name    | Dataset name according to *datasets-original.csv* file                        |
| Number_Dataset  | Dataset number according to *datasets-original.csv* file                      |
| Number_Folds    | Number of folds used in cross validation                                      |
| Number_Cores    | Number of cores for parallel processing                                       |

* Use directorys like */dev/shm*, *tmp* or *scratch* here.

You can save configuration files wherever you want. The absolute path will be passed as a command line argument.



## 🛠️ Software Requirements
This code was develop in RStudio 2024.12.0+467 "Kousa Dogwood" Release (cf37a3e5488c937207f992226d255be71f5e3f41, 2024-12-11) for Ubuntu Jammy Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) rstudio/2024.12.0+467 Chrome/126.0.6478.234 Electron/31.7.6 Safari/537.36, Quarto 1.5.57

- R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six", Copyright (C) 2025 The R Foundation for Statistical Computing, Platform: x86_64-pc-linux-gnu
- Python 3.10
- Conda 24.11.3

## 💻 Hardware Recommendations
This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (number_cores). If number_cores = 1 the code will run sequentially. In our experiments, we used 10 cores. For reproducibility, we recommend that you also use ten cores. This code was tested with the emotions dataset in the following machine:

- Linux 6.11.0-26-generic #26~24.04.1-Ubuntu SMP PREEMPT_DYNAMIC x86_64 x86_64 x86_64 GNU/Linux
- Distributor ID: Ubuntu, Description: Ubuntu 24.04.2 LTS, Release: 24.04, Codename: noble
- Manufacturer: Acer, Product Name: Nitro ANV15-51, Version: V1.16, Wake-up Type: Power Switch, Family: Acer Nitro V 15

Then the experiment was executed in a cluster at UFSC (Federal University of Santa Catarina Campus Blumenau).



## Results
The results are stored in the _REPORTS_ directory.

## RUN
To run the code, open the terminal, enter the *~/HPML.DL/R* directory, and type

```
Rscript standard.R [absolute_path_to_config_file]
```

Example:

```
Rscript standard.R "~/HPML.D/config-files/d-GpositiveGO.csv"
```

## DOWNLOAD RESULTS

soon...


## Acknowledgment
- This study was financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001.
- This study was financed in part by the Conselho Nacional de Desenvolvimento Científico e Tecnológico - Brasil (CNPQ) - Process number 200371/2022-3.
- The authors also thank the Brazilian research agencies FAPESP financial support.


## 📞 Contact
Elaine Cecília Gatto
✉️ [elainececiliagatto@gmail.com](mailto:elainececiliagatto@gmail.com)


## Links

| [Site](https://sites.google.com/view/professor-cissa-gatto) | [Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br) | [Computer Department](https://site.dc.ufscar.br/) |  [Biomal](http://www.biomal.ufscar.br/) | [CNPQ](https://www.gov.br/cnpq/pt-br) | [Ku Leuven](https://kulak.kuleuven.be/) | [Embarcados](https://www.embarcados.com.br/author/cissa/) | [Read Prensa](https://prensa.li/@cissa.gatto/) | [Linkedin Company](https://www.linkedin.com/company/27241216) | [Linkedin Profile](https://www.linkedin.com/in/elainececiliagatto/) | [Instagram](https://www.instagram.com/cissagatto) | [Facebook](https://www.facebook.com/cissagatto) | [Twitter](https://twitter.com/cissagatto) | [Twitch](https://www.twitch.tv/cissagatto) | [Youtube](https://www.youtube.com/CissaGatto) |

# Thanks