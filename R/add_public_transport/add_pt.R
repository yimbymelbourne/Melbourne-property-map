#Let's bring it all together into a single file....

source("R/00renv.R") #log into DB, load common packages

library(furrr)
options(future.rng.onMisuse = "ignore")


property_table = "dwellings_urban_development_program" 

print("Create a few preparatory datasets.... ")

#one for each PT stop.... 
all_stops <- st_read(con, query = "SELECT id, lat_stop,lon_stop, route_type_description,geometry
                                    FROM pt_stops") %>%
  rename(pt_stop_id = id)

modes <- unique(all_stops$route_type_description)



#Find all pt within 2k of each address for a given transport mode
pt_within_2k_one_mode <- function(route_filter) {
  
  
  #Start by finding the stops for that mode of transport
  print(paste("running for ",route_filter))
  stops <- all_stops %>% 
    filter(route_type_description == route_filter) 
  
  #Now write a function that finds pt near any dataset 'properties'
  
  pt_within_2k_one_mode_one_group_of_properties <- function(properties) {
  
    print(paste0("chunk size ",nrow(properties)))
    plan(multisession) 
    if( nrow(properties) > 0) {
    output <- future_map2_dfr(properties$lat, properties$lon, 
                              ~spatialrisk::points_in_circle(stops, .y, .x, 
                                                             lon = lon_stop, 
                                                             lat = lat_stop, 
                                                             radius = 2000), 
                              .id = "id_A",
                              .progress = TRUE)  %>% 
                full_join(properties %>% 
                             mutate(id_A = as.character(row_number())),
                             by = "id_A") %>% 
                rename(foi_lat                  = lat_stop,
                       foi_lon                  = lon_stop,
                       foi_description_detailed = route_type_description,
                       foi_linkage              = pt_stop_id) %>% 
                group_by(lat,
                         lon) %>% 
                arrange(distance_m) %>% 
                mutate(foi_type  = "pt_within_2k",
                       foi_order = row_number(),
                       date_created = Sys.Date(),
                       foi_description_detailed = route_filter,
                       property_table_source = property_table) %>% 
                ungroup() %>% 
                select(-id_A) %>% 
                st_drop_geometry()
    
      # Check if the table exists
      if (!dbExistsTable(con, "features_of_interest")) {
        # If the table doesn't exist, create it
        dbWriteTable(con, "features_of_interest", output)
         } else {
        # If the table exists, append data to it
        dbWriteTable(con, "features_of_interest", output, append = TRUE)
        }
      return("finished writing to database")
      
    }
  }


  #You could just run properties<- properties_raw and then walk(modes,pt_within_2k_one_mode), but
  # writing to the database for each row is really slow, but if you only write
  # to the db once in a session there's a risk it crashes or you run out of ram
  # So we chunk it into reasonable sizes, updating the database occasionally. 
  # This way is great too because it means you get a progress bar with an ETA 
  # even though you're using furrr to run the request in multiple sessions at once.
  # That's theoretically possible with progressr - but it's a very new package. 

    query <- paste0("SELECT lat,lon FROM ", property_table)
    
    properties_raw <- dbGetQuery(con, query )  %>% 
      distinct() %>% 
      filter(!is.na(lat)) 
    
    print("Check if a database exists where some of these properties have already been uploaded")
    
    if(dbExistsTable(con, "features_of_interest")){
      
      query <- sprintf("SELECT DISTINCT lat, lon FROM features_of_interest WHERE foi_type = '%s' AND foi_description_detailed ='%s'", foi_type,foi_description_detailed)
      existing_data <- dbGetQuery(con, query)
      properties <- properties_raw %>% anti_join(existing_data, by = c("lat","lon"))
      
    } else { properties<- properties_raw}

  if(nrow(properties)== 0){print("no pt rows to update")}
  
  print(paste0("running for ",nrow(properties)," properties"))
  chunks <- floor(nrow(properties)/10^5)
  
  sequences <- seq(1,chunks-1)
  values <- seq(0,nrow(properties),length.out = chunks)
  
  pt_nearby_chunked <- function(x){
             pt_within_2k_one_mode_one_group_of_properties(  properties %>% filter(row_number() > values[x],
                                                                                     row_number() <= values[x+1]))
  }

  walk(sequences,pt_nearby_chunked)
  }

walk(modes,pt_within_2k_one_mode)

#Once complete you can add an id column to make joins easier later on....

dbExecute(con, "
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT FROM information_schema.columns 
        WHERE table_schema = 'public' -- replace with your schema
        AND table_name   = 'features_of_interest'
        AND column_name  = 'id'
    )
    THEN
        ALTER TABLE features_of_interest ADD COLUMN id SERIAL PRIMARY KEY;
    END IF;
END
$$;
")


