library(geojsonsf)
library(janitor)
library(tidyverse)

# LOCATIONS -------------------------------------------------------

match_names_locations <- function(df,field){
  
  field <- enquo(field)
  
  df %>% 
    mutate(!!field := case_when(
      !!field == "Wilmington TP" ~ "Wilmington WWTP Influent",
      !!field == "Airport Road PS" ~ "Airport PS",
      !!field == "Market Street PS" ~ "South Market PS",
      !!field == "White Clay PS" ~ "White Clay Creek PS",
      !!field == "Delaware City TP" ~ "Delaware City WWTP Influent",
      !!field == "Water Farm 1" ~ "MOT Water Farm Influent",
      !!field == "Port Penn TP" ~ "Port Penn WWTP Influent",
      !!field == "Newark - Brookside" ~ "City of Newark (subset of WCC PS)",
      !!field == "Terminal Ave PS" ~ "Terminal Avenue PS",
      TRUE ~ !!field
    ))
}

geojsonsf::geojson_sf("data/data_raw/site_locations.geojson") %>% 
  as.data.frame() %>% 
  select(Name,Latitude,Longitude) %>% 
  janitor::clean_names() %>% 
  match_names_locations(name) %>% 
  dplyr::add_row(data.frame(name = "Christiana Hospital (subset of WCC PS)",latitude = 39.687192, longitude = -75.670289)) %>% 
  saveRDS(file = "data/site_locations.RDS")

# SHAPES -------------------------------------------------------

wilmington_wwtp_shape <- geojsonsf::geojson_sf("data/data_raw/wilmington_wwtp_shape.geojson") %>% 
  as.data.frame() %>% 
  select(Basin,geometry) %>% 
  janitor::clean_names() %>% 
  rename(name = basin) %>% 
  mutate(name = "Wilmington WWTP Influent")

match_names_shapes <- function(df,field){
  
  field <- enquo(field)
  
  df %>% 
    mutate(!!field := case_when(
      #!!field == "Wilmington TP - City of Wilmington" ~ "Wilmington WWTP Influent",
      !!field == "Airport Road PS" ~ "Airport PS",
      !!field == "Market Street PS" ~ "South Market PS",
      !!field == "White Clay PS" ~ "White Clay Creek PS",
      !!field == "Delaware City TP" ~ "Delaware City WWTP Influent",
      !!field == "Water Farm 1" ~ "MOT Water Farm Influent",
      !!field == "Port Penn TP" ~ "Port Penn WWTP Influent",
      !!field == "City of Newark" ~ "City of Newark (subset of WCC PS)",
      TRUE ~ !!field
    ))
}

geojsonsf::geojson_sf("data/data_raw/site_shapes.geojson") %>% 
  select(SampleBasinName,geometry) %>% 
  janitor::clean_names() %>% 
  rename(name = sample_basin_name) %>% 
  match_names_shapes(name) %>% 
  dplyr::add_row(wilmington_wwtp_shape,.before = 1) %>% 
  saveRDS(file = "data/site_shapes.RDS")

geojsonsf::geojson_sf("data/data_raw/ncc_shape.geojson") %>% 
  select(NAME,geometry) %>% 
  janitor::clean_names() %>% 
  saveRDS(file = "data/ncc_shape.RDS")

geojsonsf::geojson_sf("data/data_raw/total_sample_outline.geojson") %>% 
  rename(name = SampleBasinName) %>% 
  select(name,geometry) %>% 
  janitor::clean_names() %>% 
  saveRDS(file = "data/total_sample_outline.RDS")




# EXTERNAL GEO DATA ----------------------------------------------------------------
library(tigris)

## COUNTIES
sf::st_read("data/data_raw/counties_shapes.geojson") %>% as.data.frame() %>% janitor::clean_names() %>% saveRDS(file = "data/de_counties_shape.RDS")