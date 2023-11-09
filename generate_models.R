library(rjson)
library(PPforest)
library(PPtreeViz)
library(randomForest)
library(e1071)
library(rpart)
library(PPtreeExt)
library(here)

install_packages <- FALSE

if (install_packages) {
    cat("Instalando paquetes")
    source("R/config_packages.R")
}

metadata_model <- fromJSON(
    file = here(
        "metadata",
        "metadata_models.json"
    )
)

metadata_config <- fromJSON(
    file = here::here(
        "metadata",
        "metadata_config.json"
    )
)

source(here::here("R", "funs_comparacion.R"))


datasets <- names(metadata_model)
models <- names(metadata_config)
models <- models[models != "hhcartr"]


result <- purrr::map(
    .x = datasets,
    .f = function(x) {
        purrr::map(
            .x = models,
            .f = function(y) {
                set.seed(17)
                message(crayon::bgBlue(
                    glue::glue("Evaluando modelo {y} en dataset {x}")
                ))


                evaluate_models(
                    model = y,
                    dataset = x,
                    metadata_config = metadata_config,
                    metadata_model = metadata_model,
                    n_rep = 200,
                    n_size = 2 / 3
                )
            }
        )
    }
)

saveRDS(
    result,
    file = here::here(
        "output",
        "result.rds"
    )
)
