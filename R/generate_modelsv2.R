library(rjson)
library(PPforest)
library(PPtreeViz)
library(randomForest)
library(e1071)
library(rpart)
library(PPtreeExt)
library(here)
library(PPTree)
install_packages <- FALSE

if (install_packages) {
    cat("Instalando paquetes")
    source("R/config_packages.R")
}

metadata_model <- rjson::fromJSON(
    file = here(
        "metadata",
        "metadata_models.json"
    )
)

metadata_model <- rjson::fromJSON(
  file = here(
    "metadata",
    "metadata_modelsv2.json"
  )
)

metadata_config <- rjson::fromJSON(
    file = here::here(
        "metadata",
        "metadata_config.json"
    )
)

source(here::here("R", "funs_comparacion.R"))


datasets <- names(metadata_model)
models <- names(metadata_config)
models <- models[models != "hhcartr"]
models <- models[models != "ppforest_new"]
datasets <- datasets[datasets != "mnist"]


file_names <- c(
  "data12", "data13-1", "data13-2", "data20-1", "data20-2", "data20-3", "data21", "data22-1", "data22-2", "data26",
  "data27-a11", "data27-a21", "data27-a31", "data27-a32", "data27-b11", "data27-b12", "data27-b31", "data27-b32", "data27-c11", "data27-c12",
  "data27-c31", "data27-c32", "data29", "data32", "data34", "data36", "data37-1", "data37-14", "data37-2", "data37-3",
  "data37-4", "data37-5", "data37-8", "data38", "data39-1", "data39-2", "data39-3", "data39-4", "data39-5", "data39-6",
  "data39-7", "data39-8", "data39-9", "data41-1", "data41-2", "data41-3", "data42", "data44", "data49-2", "data49-3",
  "data49-4", "data50", "data52-1", "data52-2", "data53", "data55-1", "data55-2", "data56-1", "data56-2", "data57",
  "data61-1", "data62", "data63", "data64-1", "data64-2", "data65", "data66", "data67", "data68-1", "data68-2",
  "data69-1", "data69-2", "data70", "data71-1", "data71-2", "data72-1", "data72-2", "data72-3", "data72-4", "data72-5",
  "data72-6", "data72-7", "data73", "data74"
)


if (TRUE) {
  lapply(
    file_names,
    function(x) {
      df = read.csv(here::here("input","aux",paste0(x,".csv")))
      df = dplyr::rename(df, Type = Y)
      df$Type = as.factor(df$Type)
      invisible(assign(x, value = df, envir = .GlobalEnv))
      print(x)
    }
  )
}


# models <- models[models %in% c("pptree_split_mod","pptree_split_mod2","pptree_split_mod3")]

#full_dataset <- readRDS(here::here(
#    "input",
#    "full_dataset.rds"
#))

#clases_columnas <- sapply(full_dataset[, names(full_dataset) != "Type"], class)

#resultados_pca <- prcomp(full_dataset[, names(full_dataset) != "Type"])

#mnist <- data.frame(
#    resultados_pca$x[, 1:10]
#)

#mnist$Type <- as.factor(full_dataset$Type)

# datasets = datasets[grep("data", datasets)]

date_inicio = Sys.time()

print(date_inicio)

date_log = Sys.Date()
date_log = lubridate::year(date_log)*10000 + lubridate::month(date_log)*100 + lubridate::day(date_log)


file_name = glue::glue("result_{date_log}.csv")

if (file.exists(file_name)) {
  file.remove(file_name)
}



result <- purrr::map(
    .x = datasets[(which(datasets == 'data37-8'):length(datasets))],
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
                    n_size = 2/3
                )
                
            },
            .progress = FALSE
        )
    }
)

date_fin = Sys.time()

time_difference <- difftime(date_fin, date_inicio, units = "hours")

print(time_difference)

saveRDS(
    result,
    file = here::here(
        "output",
        glue::glue("result_{Sys.Date()}_ppforest_new.rds")
    )
)
