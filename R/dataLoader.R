load_datasets <- function(config) {
  # loads dependency data as a list
  
  df.geometry <- readOGR(dsn = file.path(config$dependency_dir, "esa-dhs-geometry-v21-04-04.geojson"))
  df.data = read_parquet(file.path(config$dependency_dir, "esa-dhs-data-v21-04-04.parquet"))
  df.indicators = read_parquet(file.path(config$dependency_dir, "esa-dhs-indicators-v21-04-04.parquet"))
  
  datasets <- list(
    df.geometry = df.geometry,
    df.data = df.data,
    df.indicators = df.indicators,
    indicators = df.indicators %>% select(IndicatorId, Definition) %>% distinct %$% split(IndicatorId, Definition)
  )
  
  return(datasets)
}
