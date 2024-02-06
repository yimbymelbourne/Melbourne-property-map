library(tidyverse)
library(sf)
library(janitor)
library(leaflet)

#Open databaselibrary(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)




#import data about dwellings as they existed in 2016

read_sf(list.files(path = "data_stock",
                   pattern = "*.shp",
                   recursive = T,
                   full.names = T))  %>% 
  st_write(con,layer = "udp_stock")

stock_raw <- read_sf(list.files(path = "data_stock",
                                pattern = "*.shp",
                                recursive = T,
                                full.names = T))
stock <- stock_raw%>% 
  clean_names()%>% 
  ungroup() %>% 
  select(-c(zonecode,
            suburb,
            lga_name,
            dwelldnsty,
            lotsize)) %>% 
  rename(under_const_in_2016 = underconst) %>% 
  arrange(desc(tlresdwng)) %>% 
  group_by(geometry) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  st_as_sf() 

stock_lat_lon <- st_centroid(stock)
stock$lon <- round(st_coordinates(stock_lat_lon)[,1],5)
stock$lat <- round(st_coordinates(stock_lat_lon)[,2],5)

#A few  (41) really weird shaped properties share a lat/lon
#Mostly driveways and other abberations - lets remove them
stock <- stock %>% 
  group_by(lat,lon) %>% 
  arrange(desc(tlresdwng)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(id_stock = row_number()) 



#Import data about dwellings built from 2016 onwards
sf_importer <- function(x) {
  output <-   read_sf(x) %>% 
  mutate(year_data = parse_number(x))%>% 
    clean_names() %>% 
  rename_with(~gsub("_", "", .x)) %>%
  rename_with(~ifelse(.x == "siteid", "projectid", .x)) %>% 
  rename_with(~ifelse(.x == "stname", "streetname", .x)) %>% 
  rename_with(~ifelse(.x == "sttype", "streettype", .x)) %>% 
  rename_with(~ifelse(.x == "municplty", "lga", .x)) %>% 
  rename_with(~ifelse(.x == "project", "projname", .x))  %>% 
    rename_with(~ifelse(.x == "addmisc", "adrsother", .x)) %>%
    rename_with(~ifelse(.x == "tldwng", "totaldwel", .x)) 

  
  return(output)
}



files <- list.files(path = "data_flow",
                    pattern = "*.shp",
                    recursive = T,
                    full.names = T)

map_df(files[2:11],
                        sf_importer) %>%
  st_write(con,layer = "udp_flow_2016_plus") 


map_df(files[1],
       sf_importer) %>%
  st_write(con,layer = "udp_flow_pre_2016")

flow_raw_2016 <- map_df(files[2:11],
                        sf_importer) %>% 
  select(-shapeleng,-shapearea) %>% 
  filter(status == c("Completed") | (status == "Under Construction" & yeardata == 2022)) %>% # A small number of properties are 'under construction' and never completed. usually these are when the original proejcts were an error. e.g. they were serviced apartments and not resi.  
  mutate(development_year = if_else(is.na(yearcompl),yeardata,yearcompl)) %>% #Now let's filter out the projects that are repeated in multiple years of the flow datasets
  group_by(projectid, yeardata) %>%
  mutate(maxstoreys = max(maxstoreys),
         totaldwel  = sum(totaldwel)) %>% 
  group_by(projectid) %>% 
  arrange(desc(yeardata), status) %>% 
  filter(row_number() == 1) %>%
  group_by(geometry) %>% 
  arrange(desc(yeardata),status) %>% 
  filter(row_number() ==1) %>%
  mutate(address = paste(streetnum,streetname,streettype,adrsother,suburb)) %>% 
  select(development_project_id = projectid,
         development_year = development_year,
         development_data_source_year = yeardata,
         development_dwellings_total = totaldwel,
         development_dwellings_townhouses = townhouses,
         development_dwellings_unknown_type = unknown,
         development_dwellings_apartments = apartments,
         development_dwellings_detached = detached,
         development_project_name_part = projpart,
         development_project_name = projname,
         development_height_storeys = maxstoreys,
         lotsize = areaha,
         address,
         development_status = status) %>% 
  st_as_sf() %>% 
  ungroup()  %>% 
  mutate(id_flow = row_number()) 





#In the older years of the flow datasets, we find often that there will be a huge redevelopment site that then gets scaled down in future years or brought into chunks
#So lets do a filter to remove the outdated projects from the dataset, even when the project name is different or the older project if they intersect with eachother 

# If you intersect raw data you sometimes get slight errors where properties overlap by a tiny amount.
# So we first buffer to make each property a bit smaller. 
flow_smaller_buffered <- flow_raw_2016 %>% 
                         st_transform(crs = 7855) %>% #move from an arc crs to a m oner. 
                         st_buffer(dist = -1.5)

# Now join together to find where there are geometries that overlap
flow_self_intersects <- st_join(flow_smaller_buffered, 
                                flow_smaller_buffered,
                                left = FALSE) %>% 
  st_drop_geometry() %>% 
  filter(id_flow.x != id_flow.y)

#Decide which one should be deleted when there is an overlap - we usually pick the more recent year.
#In some tiny edge cases there may be overlap of the same year, if so we pick the smallest.
#This is because manual inspection showed examples where a huge development gets made smaller as some of the development is delayed
#Or gets turned into a park instead of private property etc. 

flow_ids_to_delete <- flow_self_intersects %>% 
                       mutate(id_delete = case_when( (development_year.x == development_year.y) & (lotsize.x < lotsize.y) ~ id_flow.x, 
                                                     (development_year.x == development_year.y) & (lotsize.y < lotsize.x) ~ id_flow.y, 
                                                     development_year.x < development_year.y                              ~ id_flow.x,
                                                     T                                            ~ id_flow.y)) %>% 
  distinct(id_delete) %>% 
  pull(id_delete)

#Create the final flow post 2016 dataset
flow_since_2016 <- flow_raw_2016 %>% 
  filter(!(id_flow %in% flow_ids_to_delete))



#Great - now the flow dataset is nice and clean, let's join it with the stock dataset

#Four groups exist
# 1) rows where there has been development encompassing the 'stock' site - flow_contains_1
# 2) rows where there has been a development on part of the 'stock' site - flow_subdivision_2
# 3) rows with areas leftover from 2) stock_leftover_from_redev_3
# 4) rows where there has been no development


# 1) rows where there has been development encompassing the 'stock' site - flow_contains_1

#Create a buffer so that slight changes in the boundaries don't affect the join
#Then join where the flow dataset completely encompasses the stock dataset "st_contains"
flow_contains_1_no_geo_raw <- flow_since_2016 %>% 
                            st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
                            st_buffer(dist = 1.5) %>% #Slight changes ~5m are irrelevant to our work. 
                            st_join(stock %>% st_transform(crs = 7855) %>% 
                                      select(-address), 
                                    left = FALSE,
                                    join = st_contains) %>% 
                            st_drop_geometry() 

#Some 'redevelopments' are clearly greenfield subdivisions where the flow sees one development but it's clear they're townhouses
# since there are 100 properties in stock but one big property in flow. 

flow_to_delete_post_2016 <- flow_contains_1_no_geo_raw %>% 
  group_by(id_flow) %>% 
  mutate(new_minimum = case_when(first(development_dwellings_total)*1.2 >= n() ~ pmax(tlresdwng,1),
                                 T ~tlresdwng)) %>% 
  summarise(new = first(development_dwellings_total),
            existing = sum(new_minimum),
            properties = n()) %>% 
  filter(properties >5 & (new-5)*.7 < existing) %>%  # filter to where there aren't too many new properties and where amalgation of more than 5 properties is unlikely. 
  pull(id_flow)


flow_contains_1_no_geo <- flow_contains_1_no_geo_raw %>% 
  filter(!(id_flow %in% flow_to_delete_post_2016)) 
  

#Group this together and create a new list of using the original unbuffered geometry 
flow_contains_1 <- flow_since_2016 %>% 
                   right_join(flow_contains_1_no_geo) %>% 
                   group_by(id_flow) %>% 
                   arrange(desc(development_dwellings_total)) %>% 
                   summarise(across(everything(), first)) %>% 
  mutate(id_stock_former = id_stock,
         id_stock = paste(id_flow,"amalgamation_of_",
                          id_stock))


cord_1 <- st_centroid(flow_contains_1)
flow_contains_1$lon <- round(st_coordinates(cord_1)[,1],5)
flow_contains_1$lat <- round(st_coordinates(cord_1)[,2],5)

                   
# 2) rows where there has been a development on part of the 'stock' site - flow_subdivision_2

#First exclude the sites we've already established in 1 above. 
# Then create a buffer, this time in the reverse direction
#Then find any rows where there is an intersection between stock and flow datasets
flow_subdivision_2_no_geo <- flow_since_2016 %>% 
                         filter(!(id_flow %in% flow_contains_1$id_flow)) %>% 
                         st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
                         st_buffer(dist = -1.5) %>% 
                         st_join(stock %>% 
                                   filter(!(id_stock %in% flow_contains_1_no_geo$id_stock)) %>% 
                                   st_transform(crs = 7855) %>% 
                                   select(-address), 
                                   left = FALSE) %>% 
                         st_drop_geometry()

stock_subsumed_2 <- unique(flow_subdivision_2_no_geo$id_stock)

flow_subdivision_2 <- flow_since_2016 %>% 
                      right_join(flow_subdivision_2_no_geo) %>% 
                      group_by(id_flow) %>% 
                      arrange(desc(development_dwellings_total)) %>% 
                      summarise(across(everything(), first)) %>% 
                      mutate(id_stock_former = id_stock,
                             id_stock = paste(id_flow,"subdivided_from_",
                                              id_stock))
                     
cord_2 <- st_centroid(flow_subdivision_2)
flow_subdivision_2$lon <- round(st_coordinates(cord_2)[,1],5)
flow_subdivision_2$lat <- round(st_coordinates(cord_2)[,2],5)

# 3) rows with areas leftover from 2) stock_leftover_from_redev_3

sf_use_s2(FALSE) # annoying error! 

#Create a single dataset that brings together all the areas that are 
#contained in flow_subdivision_2 and therefore should not be in stock_leftover_from_redev_3

flow_single_row <- st_combine(flow_subdivision_2) %>% 
                   st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
                   st_buffer(dist = 1) %>% 
                   st_make_valid() %>% 
                   st_union() %>% 
                   st_transform(st_crs(stock))

#Filter to just the stock rows with some overlap, then delete the overlapped part
#And delete the old outdated information about that object
stock_leftover_from_redev_3 <-   stock %>% 
  filter(id_stock %in% flow_subdivision_2$id_stock_former) %>% 
  st_difference(flow_single_row) %>% 
  mutate(status = "leftover land from a development",
         across(c("tlresdwng"), ~NA_real_),
                vacant = NA_character_,
                id_stock_former = id_stock,
                id_stock = paste("leftover land from devt ",
                                 id_stock))


cord_3 <- st_centroid(stock_leftover_from_redev_3)
stock_leftover_from_redev_3$lon <- round(st_coordinates(cord_3)[,1],5)
stock_leftover_from_redev_3$lat <- round(st_coordinates(cord_3)[,2],5)

#Now get the original stock, delete all the rows we've replaced with joined stock/flow data
#Then bind it all together

rows_to_delete <- tibble(id_stock = c(flow_contains_1_no_geo$id_stock,
                          flow_subdivision_2$id_stock_former,
                          stock_leftover_from_redev_3$id_stock_former)) %>% 
                            distinct()

stock_with_flow <- stock %>% 
  anti_join(rows_to_delete) %>% 
  mutate(id_stock = as.character(id_stock)) %>% 
  bind_rows(flow_contains_1             %>% st_transform(st_crs(stock))) %>% 
  bind_rows(flow_subdivision_2          %>% st_transform(st_crs(stock))) %>%
  bind_rows(stock_leftover_from_redev_3 %>% st_transform(st_crs(stock))) 

coordinates <- st_centroid(stock_with_flow)
stock_with_flow$lon <- round(st_coordinates(coordinates)[,1],5)
stock_with_flow$lat <- round(st_coordinates(coordinates)[,2],5)

stock_with_flow_clean_names <- stock_with_flow %>% 
  arrange(desc(development_dwellings_total)) %>% 
  group_by(lat,lon) %>% 
  filter(n() == 1) %>% 
  rename(vacant_in_2016 = vacant,
         development_id_flow = id_flow,
         development_id_stock_former = id_stock_former,
         development_data_type = status,
         dwellings_in_2016 = tlresdwng) %>% 
  select(-c(lotsize)) %>% 
  ungroup() %>% 
  rename_with(~ gsub("development", "dev", .x)) %>% #Annoying that the first n characters of a shape file can't be duplicated - so need to shorten stems. 
  rename_with(~ gsub("dwellings", "dwlgs", .x)) %>% 
  rename_with(~ gsub("project", "prj", .x)) %>% 
  mutate(dwellings_est = coalesce(dev_dwlgs_total,dwlgs_in_2016),
         lot_size = as.numeric(st_area(geometry)))
  

sort(names(stock_with_flow_clean_names))


#Output as a shapefile
stock_with_flow_clean_names %>% write_sf("Melbourne_dwellings.shp")


  write_sf(obj = stock_with_flow_clean_names,
           dsn = con, 
           Id(schema="public", 
              table = 'dwellings_urban_development_program'))


