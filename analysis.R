library(rjson)
library(purrr)
library(data.table)
library(ggplot2)

resultados <- readRDS(
    here::here(
        "output",
        "result.rds"
    )
)

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

names(resultados) <- names(metadata_model)

source(here::here("R", "summary_models.R"))

summary <- summary_models(resultados)


