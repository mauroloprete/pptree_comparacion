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

                    modelo <- cbind(
                        modelo,
                        purrr::map_dfr(
                            .x = modelo$config,
                            .f = function(x) {
                                jsonlite::fromJSON(x)
                            }
                        )
                    )

                    modelo[, config := NULL]

                    notin <- function(x, y) {
                        x[!x %in% y]
                    }

                    by_columns <- notin(names(modelo), c("index", "model_name", "err.tr", "err.te"))

                    modelo[
                        ,
                        .(
                            total_models = .N,
                            error_test = mean(err.te),
                            error_train = mean(err.tr)
                        ),
                        by = by_columns
                    ]
                }
            )
        }
    )
}