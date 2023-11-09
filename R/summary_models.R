summary_models <- function(ll) {
    purrr::map_dfr(
        .x = names(ll),
        .f = function(x) {
            dataset <- resultados[[x]]

            # De un dataset, un modelo

            message(crayon::bgBlue(
                glue::glue("Dataset {x}")
            ))

            map_dfr(
                .x = 1:length(dataset),
                .f = function(i) {
                    modelo <- data.table::data.table(dataset[[i]])

                    

                    message(crayon::bgBlue(
                        glue::glue("Modelo {i}", i = unique(modelo$model))
                    ))
                    
                    message(crayon::bgBlue(
                        glue::glue("Dataset {x}", x = nrow(modelo))
                    ))

                    modelo = modelo[grepl("train_function", modelo$config), ]

                    message(crayon::red(
                        glue::glue("Dataset {x}", x = nrow(modelo))
                    ))

                    modelo <- cbind(
                        modelo,
                        purrr::map_dfr(
                            .x = modelo$config,
                            .f = function(x) {
                                
                                tryCatch(
                                    {
                                        jsonlite::fromJSON(x)
                                    },
                                    error = function(e) {
                                        data.frame(formula = "")
                                    }
                                )
                                
                            }
                        )
                    )

                    modelo[, config := NULL]

                    notin <- function(x, y) {
                        x[!x %in% y]
                    }

                    by_columns <- notin(names(modelo), c("index", "model_name", "err.tr", "err.te"))

                    modelo <- modelo[
                        ,
                        .(
                            total_models = max(index),
                            error_test = mean(err.te, na.rm = TRUE),
                            error_train = mean(err.tr, na.rm = TRUE)
                        ),
                        by = by_columns
                    ]

                    modelo <- modelo[, (names(modelo)) := lapply(.SD, as.character), .SDcols = names(modelo)]
                    tun <- modelo[
                        which.min(error_test)
                    ]


                    cols_to_json <- notin(names(tun), c("dataset", "model", "total_models", "error_test", "error_train"))

                    tun[
                        ,
                        parameters := toJSON(.SD),
                        .SDcols = cols_to_json
                    ]

                    tun[, (cols_to_json) := NULL]
                }
            )
        }
    )
}