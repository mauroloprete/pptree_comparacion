library(reticulate)

library(PPTree)
library(PPforest)
library(microbenchmark)
library(mlflow)
library(reticulate)

# 🔹 Configurar MLflow con DAGsHub
Sys.setenv(MLFLOW_TRACKING_URI = "https://dagshub.com/mauroloprete/prueba_ppforest.mlflow")
Sys.setenv(MLFLOW_TRACKING_USERNAME = "mauroloprete")
Sys.setenv(MLFLOW_TRACKING_PASSWORD = "94a182db72eeb9eb306c101e607b9a297e48cd92")

Sys.setenv(MLFLOW_PYTHON_BIN='/home/mloprete/.local/bin/mlflow')


