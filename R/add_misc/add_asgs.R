library(strayr)
read_absmap("sa42021")


lga2022 <- read_absmap("lga2022") %>% select(lga_name_2022)

postcode2021 <- read_absmap("postcode2021") %>% select(postcode_2021)


suburb2021 <- read_absmap("suburb2021") %>% select(suburb_name_2021)

ced2021 <- read_absmap("ced2021") %>% select(ced_name_2021)

sed2022<- read_absmap("sed2022") %>% select(sed_name_2022)

sa12021<- read_absmap("sa12021") %>% select(sa1_code_2021,
                                            sa2_code_2021,
                                            sa2_name_2021,
                                            sa3_code_2021,
                                            sa3_name_2021,
                                            sa4_code_2021,
                                            sa4_name_2021)



lat_lons <- dbGetQuery(con, "SELECT lat, lon FROM dwellings_urban_development_program") 

lat_lons_geo <- lat_lons %>% 
  st_as_sf(coords = c("lon","lat"), 
           crs = 4326) %>% 
  st_set_crs(4326)

lat_lons_with_asgs <- lat_lons_geo %>% 
  st_join(lga2022) %>%
  st_join(postcode2021) %>% 
  st_join(suburb2021) %>% 
  st_join(ced2021) %>% 
  st_join(sed2022) %>% 
  st_join(sa12021) %>% 
  bind_cols(lat_lons) %>% 
  st_drop_geometry()

lat_lons_with_asgs <- lat_lons_with_asgs %>% distinct(lat,lon,.keep_all = T)

dbWriteTable(con,"asgs",lat_lons_with_asgs,overwrite = T)
