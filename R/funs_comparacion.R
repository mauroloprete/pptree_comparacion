get_fit_function <- function(metadata_config) {
    return(
        metadata_config[["train_function"]]
    )
}

get_extra_args <- function(metadata_models) {
    names <- names(metadata_models)
    names_index <- names != "train_function"
    names_extra_args <- names[names_index]

    return(
        metadata_models[names_extra_args]
    )
}

build_call <- function(fit_function, extra_args, df_train) {
    call <- paste0(
        fit_function,
        "(",
        paste(
            names(extra_args),
            "=",
            unlist(extra_args),
            collapse = ", "
        )
    )

    paste0(
        call,
        ", data = ",
        quote(df_train),
        ")"
    )
}


eval_call <- function(expr, df_train) {
    # Convierte la llamada en una cadena de texto

    expr_str <- deparse(expr)


    # Evalúa la expresión modificada
    result <- eval(parse(text = expr_str), envir = parent.frame())

    # return(expr)
    eval(parse(text = result))
}

train_model <- function(
    df_train,
    metadata_config,
    metadata_models) {
    fit_function <- get_fit_function(metadata_config)
    extra_args <- get_extra_args(metadata_models)
    # extra_args$data <- df_train
    expr <- build_call(
        fit_function,
        extra_args,
        df_train = df_train
    )

    # message(crayon::green(expr))

    eval_call(expr, df_train)
}



split_dataset <- function(
    index = 1,
    dataset,
    n_size) {
    n <- round(nrow(dataset) * n_size)

    i <- sample(
        1:nrow(dataset),
        size = n,
        replace = FALSE
    )

    return(
        list(
            index = index,
            train = dataset[i, ],
            test = dataset[-i, ]
        )
    )
}

create_samples <- function(dataset, n_size, n_rep) {
    purrr::map(
        .x = 1:n_rep,
        .f = function(x) {
            split_dataset(
                index = x,
                dataset = dataset,
                n_size = n_size
            )
        }
    )
}


save_sample <- function(sample, model, dataset) {
    output_dir <- here::here(
        dataset,
        model
    )

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    index <- sample[["index"]]
    train <- sample[["train"]]
    test <- sample[["sample"]]

    saveRDS(
        train,
        file = here::here(
            output_dir,
            paste0(
                "train_",
                index,
                ".rds"
            )
        )
    )

    saveRDS(
        train,
        file = here::here(
            output_dir,
            paste0(
                "test_",
                index,
                ".rds"
            )
        )
    )
}


pptr.err <- function(model, tr, te) {
    ppt <- model
    m.tr <- PPclassify(ppt, test.data = tr[, -1], true.class = tr[, 1], Rule = 1)
    m.te <- PPclassify(ppt, test.data = te[, -1], true.class = te[, 1], Rule = 1)
    data.frame(err.tr = m.tr[[1]] / length(m.tr[[2]]), err.te = m.te[[1]] / length(m.te[[2]]))
}


pptr_mod.err <- function(model, tr, te) {
    ppt <- model
    m.tr <- PPclassify_MOD(ppt, test.data = tr[, -1], true.class = tr[, 1], Rule = 1)
    m.te <- PPclassify_MOD(ppt, test.data = te[, -1], true.class = te[, 1], Rule = 1)
    data.frame(err.tr = m.tr[[1]] / length(m.tr[[2]]), err.te = m.te[[1]] / length(m.te[[2]]))
}


cart.err <- function(model, tr, te) {
    cart.m <- model
    m.tr <- predict(cart.m, newdata = tr, type = "class")
    m.te <- predict(cart.m, newdata = te[, -1], type = "class")
    tab.tr <- table(tr[, 1], m.tr)
    tab.te <- table(te[, 1], m.te)
    data.frame(
        err.tr = (dim(tr)[1] - sum(diag(tab.tr))) / dim(tr)[1],
        err.te = (dim(te)[1] - sum(diag(tab.te))) / dim(te)[1]
    )
}

rf.err <- function(model, tr, te) {
    rf <- model
    m.te <- predict(rf, newdata = te[, -1], type = "class")
    tab.te <- table(te[, 1], m.te)
    data.frame(
        err.tr = (dim(tr)[1] - sum(diag(rf$confusion[, -dim(rf$confusion)[2]]))) / dim(tr)[1],
        err.te = (dim(te)[1] - sum(diag(tab.te))) / dim(te)[1]
    )
}

pprf.err <- function(model, tr, te) {
    ppf.m <- model
    tab.te <- table(te[, 1], trees_pred(ppf.m, xnew = te[, -1])[[2]])
    data.frame(
        err.tr = ppf.m$training.error,
        err.te = (dim(te)[1] - sum(diag(tab.te))) / dim(te)[1]
    )
}

svm.err <- function(model, tr, te) {
    svm <- model
    m.tr <- predict(svm, newdata = tr)
    m.te <- predict(svm, newdata = te[, -1], type = "class")
    tab.te <- table(
        obs = te[, 1],
        pred = m.te
    )
    tab.te <- table(te[, 1], m.te)
    data.frame(
        err.tr = mean(tr$Type != m.tr),
        err.te = (dim(te)[1] - sum(diag(tab.te))) / dim(te)[1]
    )
}




evaluate_model <- function(
    model,
    train,
    test,
    error_function) {
    # message(crayon::bgBlue(
    #     glue::glue("Cantidad de clases train {length(unique(train$Type))} cantidad de clases test {length(unique(test$Type))}")
    # ))

    do.call(
        error_function,
        args = list(
            model = model,
            tr = train,
            te = test
        )
    )
}




evaluate_models <- function(
    model,
    dataset,
    metadata_config,
    metadata_model,
    n_rep = 200,
    n_size = 2 / 3) {
    # Splits

    splits <- create_samples(
        dataset = get(dataset),
        n_size = n_size,
        n_rep = n_rep
    )


    # Guardar splits

    purrr::walk(
        .x = 1:n_rep,
        .f = function(x) {
            save_sample(
                splits[[x]],
                model = model,
                dataset = dataset
            )
        }
    )

    # Evaluar n_rep modelos

    metadata_config <- metadata_config[[model]]



    metadata_models <- generate_combinations(
        metadata_model[[dataset]][["modelos"]][[model]]
    )




    purrr::map_dfr(
        .x = 1:length(metadata_models),
        .f = function(i) {
            purrr::map_dfr(
                .x = 1:n_rep,
                .f = function(x) {
                    tryCatch(
                        {
                            model_train <- train_model(
                                df_train = splits[[x]]$train,
                                metadata_config = metadata_config,
                                metadata_models = metadata_models[[i]]
                            )

                            model_dir <- here::here(
                                "output",
                                dataset,
                                model,
                                paste0(
                                    "model_",
                                    x,
                                    ".rds"
                                )
                            )

                            saveRDS(
                                model_train,
                                file = model_dir
                            )



                            df <- evaluate_model(
                                model = model_train,
                                train = splits[[x]]$train,
                                test = splits[[x]]$test,
                                error_function = metadata_config$error_function
                            )

                            # message(crayon::bgBlue(
                            #     glue::glue('Clases diferentes {class_n}',class_n = length(unique(splits[[x]]$train[,1])))
                            # ))

                            df$index <- x
                            df$model_name <- model_dir
                            df$dataset <- dataset
                            df$model <- model
                            df$config <- toJSON(i)

                            return(df)
                        },
                        error = function(err) {
                            message(paste("Error en la repetición", x, "del modelo", model, ":", err$message))
                            df <- data.frame(
                                err.tr = NA,
                                err.te = NA,
                                index = x,
                                model_name = NA,
                                dataset = dataset,
                                model = model,
                                config = toJSON(i)
                            )
                        }
                    )
                }
            )
        }
    )


    # return(model)
}


generate_combinations <- function(input_list) {
    
    vector_elements <- sapply(input_list, is.vector)
    
    vector_list <- input_list[vector_elements]
    scalar_list <- input_list[!vector_elements]

    
    if (length(vector_list) == 0) {
        return(list(input_list))
    }

    
    vector_names <- names(vector_list)

    
    vector_list <- lapply(vector_list, as.list)

    
    combinations <- do.call(expand.grid, vector_list)

    
    combination_list <- lapply(1:nrow(combinations), function(i) {
        combined_elements <- c(scalar_list, combinations[i, ])
    
        combined_elements <- lapply(combined_elements, function(x) {
            if (is.list(x)) unlist(x) else x
        })
        names(combined_elements) <- c(names(scalar_list), vector_names)
        return(combined_elements)
    })

    return(combination_list)
}