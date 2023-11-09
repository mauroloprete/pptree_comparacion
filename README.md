# PPTree Comparación

## Introducción

En este repositorio se encuentran el código para la evaluación de los diferentes modelos de clasificación y las extensiones de PPTree.
Se incluyen los siguientes modelos:
- PPTree (PDA ó LDA)
- PPTree Split Modificación  1
- PPTree Split Modificación  2
- PPTree Split Modificación  3
- CART (con la implementación en rpart)
- Random Forest (con la implementación en randomForest)
- SVM (con la implementación en e1071)
- PPForest (con la implementación en PPforest)
- hhcart (con la implementación en hhcartr)

La configuración de cada modelo se encuentra en el archivo 'metadata/metadata_config.json' donde puede encontrar la función de entrenamiento y predicción de cada modelo.

Cada modelo fue evaluado en los siguientes datasets (incluidos en el paquete PPTree):

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

Para cada dataset se entrenaron 200 modelos con diferentes configuraciones de train/test y en cada uno de ellos se evaluó la precisión de cada modelo, la configuración de los parámetros de cada modelo se encuentra en el archivo 'metadata/metadata_models.json'.

Para cada modelo se generó un archivo del modelo entrenado, los datos de entrenamiento y su conjunto de test, estos archivos se encuentran en la carpeta 'results' y se encuentran en formato RData.

## Ambiente de desarrollo en Gitpod

[![Gitpod Ready-to-Code](https://img.shields.io/badge/Gitpod-Ready--to--Code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/mauroloprete/pptree_comparacion)

## Configuración local

### Requisitos

- R (>= 3.6.0)
- RStudio (>= 1.2.1335)

### Instalación de dependencias

```r
source("R/config_packages.R")
```

## Configuración en Docker

```bash
docker build -t pptree_comparacion .
docker run -it pptree_comparacion
```

## Ejecución

### Generar modelos

```r
source("generate_models.R")
```

### Análisis

```r
source("analysis.R")
```

