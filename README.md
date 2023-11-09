## PPTree Comparison

## Introduction

This repository contains the code for the evaluation of the different classification models and extensions of PPTree.
The following models are included:
- PPTree (PDA or LDA)
- PPTree Split Modification 1
- PPTree Split Modification 2
- PPTree Split Modification 3
- CART (with implementation in rpart)
- Random Forest (with implementation in randomForest)
- SVM (with implementation in e1071)
- PPForest (with implementation in PPforest)
- hhcart (with implementation in hhcartr)

The configuration of each model can be found in the file 'metadata/metadata_config.json' where you can find the training and prediction function of each model.

Each model was evaluated on the following datasets (included in the PPTree package):

- crab
- fishcatch
- glass
- leukemia
- lymphoma
- wine
- image
- NCI60
- olive
- parkinson

For each dataset 200 models were trained with different train/test configurations and in each of them the accuracy of each model was evaluated, the configuration of the parameters of each model can be found in the file 'metadata/metadata_models.json'.

For each model a file of the trained model, the training data and its test set was generated, these files are located in the 'results' folder and are in RData format.

## Gitpod development environment

[Gitpod Ready-to-Code](https://img.shields.io/badge/Gitpod-Ready--to--Code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/mauroloprete/pptree_comparacion)

## Local configuration

### Requirements

- R (>= 3.6.0)
- RStudio (>= 1.2.1335)

### Installation of dependencies

```r
source("R/config_packages.R")
```

## Configuration in Docker

```bash
docker build -t pptree_comparison .
docker run -it pptree_comparison
```

## Running

### Generate models

```r
source("generate_models.R")
```

### Analysis

```r
source("analysis.R")
```
