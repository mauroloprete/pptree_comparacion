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
    "metadata_modelsv3.json"
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
models <- models[models != "pptree.lda"]
datasets <- datasets[datasets != "mnist"]
datasets <- datasets[datasets != "data.28"]



if (FALSE) {
  lapply(
    datasets,
    function(x) {
      tryCatch(
        {
          df <- read.csv(
            file = here::here(
              "input",
              "paper_consistency",
              x,
              paste0(x,".csv")
            )
          )
          invisible(assign(x, value = df, envir = .GlobalEnv))
          print(x)
        },
        error = function(e) {
          message("no se pudo cargar", x)
        }
      )
      
      
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
    .x = datasets,
    .f = function(x) {
      
      df <- read.csv(
        file = here::here(
          "input",
          "paper_consistency",
          x,
          paste0(x,".csv")
        )
      )
      invisible(assign(x, value = df, envir = .GlobalEnv))
      
        
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
        
      rm(list = x, envir = .GlobalEnv)
    }
)

date_fin = Sys.time()

time_difference <- difftime(date_fin, date_inicio, units = "hours")

print(time_difference)

saveRDS(
    result,
    file = here::here(
        "output",
        file_name
    )
)
