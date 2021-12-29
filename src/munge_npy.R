get_seg_ids <- function(table) {
  if (table %in% c(1,2)) {
    site_ids <- c('1435', '1436', '1437', '1438', '1439', '1440', '1441', '1442', '1443', '1444', '1445', '1446', '1447', '1448', '1449', '1450', '1451', '1452', '1453', '1454', '1455', '1456', '1457', '1458', '1459', '1460', '1461', '1462', '1463', '1545', '1546', '1547', '1548', '1549', '1550', '1551', '1552', '1553', '1554', '1555', '1556', '1557', '1558', '1559', '1560', '1561', '1562', '1563', '1564', '1565', '1566', '1571', '1572', '1573', '1574', '1575')
  } else {
    site_ids <- c('1634', '1635', '1636', '1637', '1638', '1639', '1641', '1642', '1643', '1644', '1645', '1646', '1648', '1649', '1650', '1652', '1653', '1657')
  }
  return(site_ids)
}

munge_npy <- function(in_file, table) {

  site_ids <- get_seg_ids(table)
  dates <- seq(as.Date('2006-12-26'), as.Date('2020-06-22'), 1)
  
  dat <- npyLoad(in_file) %>%
    as.data.frame()
  names(dat) <- dates
  
  dat_out <- dat %>%
    mutate(seg_id_nat = site_ids) %>%
    tidyr::pivot_longer(cols = -seg_id_nat, names_to = 'date', values_to = 'temp_deg_c') %>%
    mutate(source = in_file)
  
  return(dat_out)
}

gather_tables <- function(out_file, table) {
  if (table %in% 1) {
    tmatch <- 'T1'
  } else if (table %in% 2) {
    tmatch <- 'T2'
  } else {
    tmatch <- 'T3'
  }
  t_files <- list.files('in_data/predictions', full.names = TRUE)
  t_files <- t_files[grepl(tmatch, t_files)]
  core_data <- t_files %>%
    purrr::map_dfr(munge_npy, table = table)
  
  out_data <- core_data %>%
    mutate(model = gsub('_.*', '', basename(source))) %>%
    select(-source) %>% distinct()
  
  write_csv(out_data, out_file)
}

gather_window_exp <- function(out_file) {
  window_exp_files <- list.files('in_data/USGS_outputs/window size', full.names = TRUE)
  window_data <- window_exp_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(window = stringr::str_extract_all(source, '\\d+', simplify = TRUE)) %>%
    select(-source)
  
  write_csv(window_data, out_file)
}

gather_assimilation_exp <- function(out_file) {
  assimilation_files <- list.files('in_data/USGS_outputs/assimilation data', full.names = TRUE)
  assimilation_data <- assimilation_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(prop_obs = stringr::str_extract_all(source, '\\d+', simplify = TRUE)) %>%
    mutate(prop_obs = as.numeric(prop_obs)/1000) %>%
    select(-source)
  
  write_csv(assimilation_data, out_file)
    
}

gather_pretraining_exp <- function(out_file){
  pretrain_exp_files <- list.files('in_data/USGS_outputs/pretrained HRGN', full.names = TRUE)
  pretrain_data <- pretrain_exp_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(pretraining = grepl('_pretrain_', source),
           state_update = ifelse(grepl('assimilation', source), 'invertible network', NA),
           prop_training = as.numeric(stringr::str_extract_all(source, '\\d+', simplify = TRUE))/1000) %>%
    select(-source)
  
  readr::write_csv(pretrain_data, out_file)
}




