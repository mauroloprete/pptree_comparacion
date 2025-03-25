library(jsonlite)

metadata = jsonlite::fromJSON(here::here("metadata","metadata_models.json"))

example = metadata$image

A = sub("\\.csv$", "", file_names)
for (i in A) {
  metadata[[i]] = example  
}

jsonlite::write_json(metadata, here::here("metadata","metadata_modelsv2.json"))
