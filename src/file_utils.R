fetch_network <- function(rds_in) {
  site_ids_s1 <- get_seg_ids(table = 1)
  site_ids_s2 <- get_seg_ids(table = 3)
  site_ids <- c(site_ids_s1, site_ids_s2)
  sf_object <- readRDS(rds_in)

  sf_filtered <- sf_object$edges %>%
    select(seg_id_nat) %>%
    filter(seg_id_nat %in% site_ids & !is.na(seg_id_nat)) %>%
    mutate(region = ifelse(seg_id_nat %in% site_ids_s2, 'S2', 'S1')) %>%
    sf::st_transform(crs = 4326) %>%
    distinct()
  return(sf_filtered)
}

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  setwd(dsn)
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}