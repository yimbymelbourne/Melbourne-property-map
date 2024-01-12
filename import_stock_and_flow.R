library(tidyverse)
library(sf)
library(janitor)
library(leaflet)

files <- list.files(path = "data_flow",
                    pattern = "*.shp",
                    recursive = T,
                    full.names = T)

#import data about dwellings as they existed in 2016

stock <- read_sf("data_stock/Order_WTRNX9/ll_gda2020/esrishape/whole_of_dataset/victoria/UDP/HDD_STOCK2016.shp") %>% 
  clean_names()%>% 
  ungroup() %>% 
  mutate(id_stock = row_number()) 

#Import data about dwellings built from 2016 onwards
sf_importer <- function(x) {read_sf(x) %>% 
  mutate(year_data = parse_number(x)) 
}

flow_raw_2016 <- map_df(files[2:11],
                        sf_importer) %>% 
  rename(maxstoreys = MAXSTOREYS) %>% 
  clean_names() %>%
  filter(status %in% c("Completed",
                       "Under Construction")) %>%
  mutate(year_development = if_else(is.na(yearcompl),year_data,yearcompl)) %>% #Now let's filter out the projects that are repeated in multiple years of the flow datasets
  group_by(projectid, year_data) %>%
  mutate(maxstoreys = max(maxstoreys),
         total_dwel  = sum(total_dwel)) %>% 
  group_by(projectid) %>% 
  arrange(desc(year_data), status) %>% 
  filter(row_number() == 1) %>%
  group_by(geometry) %>% 
  arrange(desc(year_data),status) %>% 
  filter(row_number() ==1) %>%
  mutate(address = paste(streetnum,streetname,streettype,suburb)) %>% 
  select(development_project_id = projectid,
         year_development,
         year_data,
         dwellings_total_new = total_dwel,
         height_storeys = maxstoreys,
         lotsize = areaha,
         projname,
         address,
         status) %>% 
  st_as_sf() %>% 
  ungroup()  %>% 
  mutate(id_flow = row_number()) 


#In the older years of the flow datasets, we find often that there will be a huge redevelopment site that then gets scaled down in future years or brought into chunks
#So lets do a filter to remove the outdated projects from the dataset, even when the project name is different or the older project if they intersect with eachother 

# If you intersect raw data you sometimes get slight errors where properties overlap by a tiny amount.
# So we first buffer to make each property a bit smaller. 
flow_buffered <- flow_raw_2016 %>% 
                 st_transform(crs = 7855) %>% #move from an arc crs to a m oner. 
                 st_buffer(dist = -1.5)

# Now join together to find where there are geometries that overlap
flow_self_intersects <- st_join(flow_buffered, 
                                flow_buffered,
                                left = FALSE) %>% 
  st_drop_geometry() %>% 
  filter(id_flow.x != id_flow.y)

#Decide which one should be deleted when there is an overlap - we usually pick the more recent year.
#In some tiny edge cases there may be overlap of the same year, if so we pick the smallest.
#This is because manual inspection showed examples where a huge development gets made smaller as some of the development is delayed
#Or gets turned into a park instead of private property etc. 

flow_ids_to_delete <- flow_self_intersects %>% 
                       mutate(id_delete = case_when( (year_development.x == year_development.y) & (lotsize.x < lotsize.y) ~ id_flow.x, 
                                                     (year_development.x == year_development.y) & (lotsize.y < lotsize.x) ~ id_flow.y, 
                                                     year_development.x < year_development.y                              ~ id_flow.x,
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
flow_with_original_stock_buff <- flow_since_2016 %>% 
                            st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
                            st_buffer(dist = 3) %>% #Slight changes ~5m are irrelevant to our work. 
                            st_join(stock %>% st_transform(crs = 7855) %>% 
                                    select(-c(address,lotsize)), 
                                    left = FALSE,
                                    join = st_contains) 

#Group this together and create a new list of using the original unbuffered geometry 
flow_contains_1 <- flow_since_2016 %>% 
                   right_join(flow_with_original_stock_buff %>% 
                              st_drop_geometry()
                              ) %>% 
                   group_by(id_flow) %>% 
                   summarise(across(everything(), first))
                           
# 2) rows where there has been a development on part of the 'stock' site - flow_subdivision_2

#First exclude the sites we've already established in 1 above. 

subdivided_flow <- flow_since_2016 %>% 
  filter(!(id_flow %in% flow_contains_1$id_flow))

#Create a buffer, this time in the reverse direction
#Then find any rows where there is an intersection between stock and flow datasets
flow_subdivision_buff <- subdivided_flow %>% 
                         st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
                         st_buffer(dist = -3) %>% 
                         st_join(stock %>% st_transform(crs = 7855) %>% 
                                   select(-c(address,lotsize)), 
                                   left = FALSE)

flow_subdivision_2 <- subdivided_flow %>% 
                     right_join(flow_subdivision_buff %>% 
                                st_drop_geometry()
                                )

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
  filter(id_stock %in% flow_subdivision_2$id_stock) %>% 
  st_difference(flow_single_row) %>% 
  mutate(status = "leftover land from a development",
         across(c("lotsize","dwelldnsty","tlresdwng"),~NA_real_),
                vacant = NA_character_,
                id_stock_former = id_stock,
                id_stock = paste("leftover land from devt ",
                                 id_stock))

#Now get the original stock, delete all the rows we've replaced with joined stock/flow data
#Then bind it all together

stock_with_flow_2016_plus <- stock %>% 
  filter(!(id_stock %in% c(flow_with_original_stock_buff$id_stock,
                           flow_subdivision_2$id_stock,
                           stock_leftover_from_redev_3$id_stock_former))) %>% 
  bind_rows(flow_contains_1                      %>% st_transform(st_crs(stock)),
            flow_subdivision_2                  %>% st_transform(st_crs(stock))) %>%
  mutate(id_stock = as.character(id_stock)) %>% 
  bind_rows(stock_leftover_from_redev_3  %>% st_transform(st_crs(stock)))



#Always nice to manually check your work in a GIS software like QGIS...
# flow_raw_2016 %>% 
#   write_sf("flow_all_from_2016.shp")
# stock_with_flow %>% write_sf("flow_and_stock.shp")
# flow_contains_1 %>% write_sf("uncomplicated_redevelopments.shp")
# flow_subdivision_2 %>% write_sf("lots_where_the_redevelopment_is_smaller.shp")
# stock_leftover_from_redev_3 %>% write_sf("bits leftover_from_development.shp")

#Great - now let's add in **old** redevelopments from before 2016


#Now we've included flow after 2020, we can also include flow from the older dataset that's in a slightly different format

all_flow <-  read_sf(files[1]) 

#Import the old file and delete repeats of the same geometry
flow_raw_pre_2016 <- read_sf(files[1]) %>% 
  clean_names() %>% 
  mutate(development_project_id = paste0("20062016_",project_id)) %>%
  select(year, 
         dwellings_total_new = tldestdwlg,
         dwellings_net_new = tlnetdwlg,
         development_project_id) %>% 
  group_by(geometry) %>%
  arrange(desc(year)) %>% 
  filter(row_number() ==1) %>% 
  ungroup() %>% 
  mutate(status = "Uncertain - older dataset",
         id_flow = paste0("2006_2016_",row_number())) %>% 
  st_as_sf() 

#Now look for developments where the rows intersect... 
flow_pre_2016_buffered <- flow_raw_pre_2016 %>% 
  st_transform(crs = 7855) %>% #move from an arc crs to a m oner. 
  st_buffer(dist = -1.5) %>% 
  ungroup() %>% 
  st_as_sf()

# Now join together to find where there are geometries that overlap
flow_self_intersects_pre_2016 <- st_join(flow_pre_2016_buffered, 
                                         flow_pre_2016_buffered,
                                         left = FALSE) %>% 
  st_drop_geometry() %>% 
  filter(id_flow.x != id_flow.y)

#Decide which one should be deleted when there is an overlap - we usually pick the more recent year.
#In some tiny edge cases there may be overlap of the same year, if so we pick the biggest development
#This is because manual inspection showed examples where a huge development gets made smaller as some of the development is delayed
#Or gets turned into a park instead of private property etc. 

flow_ids_to_delete_pre_2016 <- flow_self_intersects_pre_2016 %>% 
  mutate(id_delete = case_when( (year.x == year.y) & (dwellings_total_new.x < dwellings_total_new.y) ~ id_flow.y, 
                                (year.x == year.y) & (dwellings_total_new.y < dwellings_total_new.x) ~ id_flow.x, 
                                year.x < year.y                              ~ id_flow.x,
                                T                                            ~ id_flow.y)) %>% 
  distinct(id_delete) %>% 
  pull(id_delete)

#Create the final flow post 2016 dataset
flow_pre_2016_filtered <- flow_raw_pre_2016 %>% 
                 filter(!(id_flow %in% flow_ids_to_delete_pre_2016))

#Now let's join it with the stock dataset....
#Manual inspection found that when there's more than one row in stock per flow dataset it's because of a subivision, so we'll divide the stock by the number of rows evenly as an estimate. 
#We also delete developments that are in the 2016+ flow dataset. While these are not **all** duplicated (e.g. you could redevelop twice) most are. 

id_stock_flow_post_2016 <- stock_with_flow_2016_plus %>% 
                           filter(!is.na(id_flow)) %>% 
                           pull(id_stock)

flow_pre_2016_no_geo <- flow_pre_2016_filtered %>% 
  st_transform(crs = 7855) %>% #move from an arc crs to a meter one. 
  st_buffer(dist = -3) %>% 
  st_join(stock %>% st_transform(crs = 7855) %>% 
            select(-c(address,lotsize)), 
            left = FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(id_stock) %>% 
  mutate(dwellings_total_new = dwellings_total_new / n(),
         dwellings_net_new = dwellings_net_new / n()) %>% 
  filter(!(id_stock %in% id_stock_flow_post_2016)) %>% 
  mutate(status = "development from old dataset before 2016",
         year_development = year,
         year_data = year) %>% 
  select(-year) %>% 
  ungroup()

flow_pre_2016 <- stock %>% 
  right_join(flow_pre_2016_no_geo) %>% 
  mutate(id_stock = as.character(id_stock))

stock_with_flow <- 
  stock_with_flow_2016_plus %>% 
  filter(!(id_stock %in% flow_pre_2016$id_stock)) %>% 
  mutate(id_flow = as.character(id_flow)) %>% 
  bind_rows(flow_pre_2016) %>% 
  rename(dwellings_in_2016 = tlresdwng)


coordinates <- st_centroid(stock_with_flow)
stock_with_flow$lon <- round(st_coordinates(coordinates)[,1],5)
stock_with_flow$lat <- round(st_coordinates(coordinates)[,2],5)


#Output as a shapefile

stock_with_flow %>% write_sf("Melbourne_dwellings.shp")

#write to a database
library(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5433, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = rstudioapi::askForPassword(), # password of user
                 options="-c search_path=public" # specify what schema to connect to
)

  write_sf(obj = stock_with_flow,
           dsn = con, 
           Id(schema="public", 
              table = 'dwellings_urban_development_program'))


