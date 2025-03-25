# UCI

library(ucimlrepo)

get_dataset_uci <- function(dataset_path) {
  response = fetch_ucirepo(dataset_path)
  
  y_name = response$variables[response$variables$role == "Target",]$name
  
  x_names = response$variables[response$variables$role != "Target",]$name
  
  dataset = response$data$original
  
  dataset_without_na = na.omit(dataset)
  
  if (nrow(dataset) != nrow(dataset_without_na)) {
    message("El dataset tiene NA")
  }
  
  return(
    list(
      dataset = dataset,
      x_names = x_names,
      y_name = y_name,
      dataset_path = dataset_path
    )
  )
}


# Github

get_dataset_gh <- function(dataset_path) {
  
  dataset = data.table::fread(
    dataset_path,
    header = FALSE
  )
  
  dataset_names = names(dataset)
  n_cols = ncol(dataset)
  
  x_names = dataset_names[1:(n_cols-1)]
  y_name = dataset_names[n_cols]
  
  return(
    list(
      dataset = dataset,
      x_names = x_names,
      y_name = y_name,
      dataset_path = dataset_path
    )
  )
  
  
}


# Kaggle

get_dataset_kaggle <- function(dataset_path) {
  # TODO: Ver luego
}


# Funciones para generar la grilla nueva

generate_formula <- function(x_name, y_name) {
  formula_str <- paste(y_name, "~", paste(x_name, collapse = " + "))
  return(formula_str)
}

replace_formulas <- function(model_list, x_name, y_name) {
  for (name in names(model_list)) {
    if ("formula" %in% names(model_list[[name]])) {
      model_list[[name]]$formula <- generate_formula(x_name, "Type")
    }
  }
  return(model_list)
}

save_full_dataset <- function(dataset,dataset_code, parent_folder, cache = FALSE) {
  
  file_name = here::here(
    parent_folder,
    dataset_code,
    paste0(dataset_code, ".csv")
  )
  
  
  if (!cache | !file.exists(file_name)) {
    data.table::fwrite(
      x = dataset,
      file = file_name
    )
  }
  
  
  
}

generate_metadata_model <- function(template, y_name, x_name, custom_lambda = seq(0,1,0.1)) {
  
  if (length(custom_lambda) > 1 & 0 %in% custom_lambda) {
    template$modelos$pptree.lda <- NULL
    template$modelos$pptree.pda$lambda <- custom_lambda
    
    ppmethod <- "'PDA'"
    
    # Ppforest
    template$modelos$ppforest$lambda <- custom_lambda
    template$modelos$ppforest$size.p <- c(template$modelos$ppforest$size.p, 1/3)
    
    # Mod1
    template$modelos$pptree_split_mod$PPmethod <- ppmethod
    template$modelos$pptree_split_mod$lambda <- custom_lambda
    template$modelos$pptree_split_mod$size.p <- c(template$modelos$pptree_split_mod$size.p, 1/3)
    
    # Mod2 
    template$modelos$pptree_split_mod2$PPmethod <- ppmethod
    template$modelos$pptree_split_mod2$lambda <- custom_lambda
    template$modelos$pptree_split_mod2$size.p <- c(template$modelos$pptree_split_mod2$size.p, 1/3)
    
    
    # Mod3
    template$modelos$pptree_split_mod3$PPmethod <- ppmethod
    template$modelos$pptree_split_mod3$lambda <- custom_lambda
    template$modelos$pptree_split_mod3$size.p <- c(template$modelos$pptree_split_mod3$size.p, 1/3)
    
    
  }
  
  model_list_changed <- replace_formulas(template$modelos, x_name, y_name)
  
  template$modelos <- model_list_changed
  
  return(template)
  
  
}

generate_metadata_models <- function(template, custom_lambda, dataset_grid, cache) {
  
  dataset_names <- sapply(
    X = dataset_grid,
    FUN = function(x) {
      x[['dataset_code']]
    }
  )
  
  names(dataset_grid) <- dataset_names
  
  res <- lapply(
    X = dataset_names,
    FUN = function(x) {
      
      dataset_info <- dataset_grid[[x]]
      dataset <- dataset_info[['dataset']]
      
      if (dataset_info$q_rows > 1) {
        y_name <- dataset[['y_name']]
        x_name <- dataset[['x_names']]
        
        metadata <- generate_metadata_model(template = template, y_name = y_name, x_name = x_name, custom_lambda = custom_lambda)
        
        metadata$custom_name <- dataset_info[['dataset_name']]
        metadata$equal_rows <- dataset_info[['equal_rows']]
        metadata$equal_vars <- dataset_info[['equal_vars']]
        metadata$q_rows <- dataset_info[['q_rows']]
        metadata$q_vars <- dataset_info[['q_vars']]
        metadata$rows_with_na <- dataset_info[['rows_with_na']]
        dataset$dataset$Type <- dataset$dataset[[y_name]]
        dataset$dataset <- data.table::as.data.table(dataset$dataset)
        
        if (!is.null(y_name) && y_name %in% names(dataset$dataset)) {
          remove_cols <- names(dataset$dataset) != y_name
          dataset$dataset <- dataset$dataset[, names(dataset$dataset)[remove_cols], with = FALSE]
        } else {
          stop(paste("Error: La columna", y_name, "no existe en dataset$dataset ", x))
        }
        
        
        
        save_full_dataset(dataset$dataset, x, here::here("input", "paper_consistency"))
        
        return(metadata)
      } else {
        message("Sin filas", x)
      }
    }
  )
  
  names(res) <- dataset_names
  
  return(res)
}

dataset_grid <- readxl::read_excel(path = here::here("input/paper_consistency/classification_datasets.xlsx"))
dataset_grid <- data.table::as.data.table(dataset_grid)

# Quito Kaggle (por ahora)

dataset_grid <- dataset_grid[
  Type != "Kaggle",
]

dataset_grid <- dataset_grid[
  ,
  from_folder := NULL
]

# Algunos dataset no son válidos aunque si se encuentran en UCI no pueden ser descargados por API.

not_valid_uc_dataset <- c("Arrhythmia", "Madelon", "Human activity recognition")

dataset_grid <- dataset_grid[
  !(Name %in% not_valid_uc_dataset),
]


get_dataset <- function(dataset_code, dataset_name, N, p, type, link) {
  dir <- here::here("input/paper_consistency", dataset_code)
  
  if (dir.exists(dir)) {
    "El directorio ya existe!"
  } else {
    dir.create(dir)
  }
  
  
  get_dataset <- switch(
    type,
    Github = get_dataset_gh,
    UCI = get_dataset_uci
  )
  
  dataset <- do.call(get_dataset, args = list(link))
  
  equal_vars <- p == length(dataset$x_names)
  equal_rows <- N == nrow(dataset$dataset)
  
  rows_with_na <- sum(!complete.cases(dataset$dataset))
 
  
  return(
    list(
      dataset_code = dataset_code,
      dataset_name = dataset_name,
      N = N,
      p = p,
      link = link,
      dataset = dataset,
      equal_rows = equal_rows,
      equal_vars = equal_vars,
      q_vars = length(dataset$x_names),
      q_rows = nrow(dataset$dataset),
      rows_with_na = rows_with_na
    )
  )
  
  
}

library(purrr)

dataset_grid <- pmap(
  list(
    dataset_code = dataset_grid$Data,
    dataset_name = dataset_grid$Name,
    N = dataset_grid$N,
    p = dataset_grid$p,
    type = dataset_grid$Type,
    link = dataset_grid$Reference
  ),
  .f = get_dataset
)


template <- jsonlite::read_json(
  here::here("metadata", "metadata_models.json")
)$crab





metadata_modelsv3 <- generate_metadata_models(template, custom_lambda = seq(0,1,0.1), dataset_grid)

jsonlite::write_json(metadata_modelsv3, here::here("metadata","metadata_modelsv3.json"))






