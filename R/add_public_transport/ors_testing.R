source("R/00renv.R")

# The goal here is to ping an openrouteservice (ORS) server to figure out the distance walking
# and time to walk to previously coded public transport nodes. 
# For this to work you need a docker ORS on your computer. Encoding on the internet services would be too expensive
# The process of doing this is quite fiddly, but this youtube video is the most helpful
# https://www.youtube.com/watch?v=VQXlbqKArFk


# Because ORS is a bit slow, you can't run the PT query on all of the rows in the original dataset. 
# We'll limit to 2 stops per mode per house, and also places that are <1.5km rather than 2km. 
# Make a new table in the database for this called 'pt_distance' that has fewer rows. 


if(!dbExistsTable(con, "pt_distance")){
  
dbExecute(con, "
CREATE TABLE pt_distance_uncoded AS
SELECT lat, lon, foi_lat, foi_lon, foi_description_detailed, distance_m
FROM (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY lat, lon, foi_description_detailed
      ORDER BY distance_m ASC
    ) AS q02
  FROM features_of_interest
  WHERE distance_m < 1500.0
) q01
WHERE q01.q02 <= 3;
")

  }
#Download the table into ram

pt_points_to_update <- dbGetQuery(con,"SELECT * FROM pt_distance_uncoded
                                  WHERE foi_description_detailed = 'tram'")


test <-  pt_points_to_update %>% 
  filter(lat == -37.80137,
         lon == 144.9746)

library(strayr)
library(sf)
suburbs <- strayr::read_absmap("suburb2021") %>% 
  filter(suburb_name_2021 == "Fitzroy (Vic.)") %>% 
  st_transform(st_crs(pt_points_to_update))

fitzroy <- 
  pt_points_to_update %>% 
  st_as_sf(coords = c("lon","lat"), crs = "wgs84") %>% 
  st_transform(st_crs(suburbs))%>% 
  st_filter(suburbs)


fitzroy_closest <- fitzroy %>% 
  mutate(coords = st_coordinates(geometry)) %>%
  mutate(lon = coords[, "X"], 
         lat = coords[, "Y"]) %>%
  group_by(lon,lat) %>% 
  filter(distance == min(distance),
         lat == -37.80137,
         lon == 144.9746) 


test %>% 
  select(-lat,-lon) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~foi_lon,
                   lat = ~foi_lat,
                  # color = ~pal(distance_m),
                   radius = 10,
                   popup = ~htmlEscape(paste0(distance_m,"stop is ",foi_lat," lon",foi_lon))
  )



fitzroy_closest <- fitzroy %>% 
  mutate(coords = st_coordinates(geometry)) %>%
  mutate(lon = coords[, "X"], 
         lat = coords[, "Y"]) %>%
  group_by(lon,lat) %>% 
  filter(lat == -37.80137,
         lon == 144.9746) 

#Check with leaflet...
pal <- colorNumeric(
  palette = "Blues",
  domain = fitzroy$distance)

library(htmltools)
fitzroy_closest %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~foi_lon,
                   lat = ~foi_lat,
                   color = ~pal(distance),
                   radius = 1,
                   popup = ~htmlEscape(paste0(distance,"stop is ",foi_lat," lon",foi_lon))
  )

pt_points_to_double_check <- pt_points_to_update %>% 
# We'll temporarily store new updated distance in a table called 'pt_distnace_encoded'. 
# Let's check if we've previously done some of the job

#Now write a function to request to the ORS server for a single row of lat/lon co-ordinates
request_directions <- function(lat, lon, foi_lat, foi_lon,foi_description_detailed) {
  base_url <- "http://localhost:8080/ors/v2/directions/foot-walking"
  full_url <- sprintf("%s?start=%f,%f&end=%f,%f", base_url, lon, lat, foi_lon, foi_lat)
  #print(full_url)
  response <- GET(full_url)
  if (status_code(response) == 200) {
    
   directions_json <- fromJSON(content(response, "text"))
    output <- directions_json$features$properties$summary %>% as_tibble() %>% 
    mutate(lat = lat,
           lon = lon,
           foi_lat = foi_lat,
           foi_lon = foi_lon,
           foi_description_detailed = foi_description_detailed)
    return(output)
  } else {
    return(tibble(distance = NA_real_,
                  duration = NA_real_,
                  foi_lon = foi_lon,
                  foi_lat = foi_lat,
                  lat = lat,
                  lon = lon))
  }
}


#Writing to the database for each row is really slow, but if you only write
# to the db once in a session there's a risk it crashes or you run out of ram
# So we chunk it into reasonable sizes, updating the database occasionally. 
# This way is great too because it means you get a progress bar with an ETA 
# even though you're using furrr to run the request in multiple sessions at once.
# That's theoretically possible with progressr - but it's a very new package. 

chunk_size = 50000
chunks <- ceiling(nrow(pt_points_to_update)/chunk_size)
sequences <- seq(1,chunks-1)
values <- seq(0,nrow(pt_points_to_update),length.out = chunks)

if(length(values) == 1) {values <- c(0,nrow(pt_points_to_update))}
pt_by_area_chunked <- function(x){
  
  pt_points_to_run <- pt_points_to_update %>% filter(row_number() > values[x],
                                                     row_number() <= values[x+1])
  
  plan(multisession, workers = 6)
  
distances <- 
  future_pmap(
    .l = list(lat = pt_points_to_run$lat, 
              lon = pt_points_to_run$lon, 
              foi_lat = pt_points_to_run$foi_lat, 
              foi_lon = pt_points_to_run$foi_lon,
              foi_description_detailed = pt_points_to_run$foi_description_detailed),
    .f = ~ {progress(); request_directions(..1, ..2, ..3, ..4, ..5)}
  ) %>% 
    list_rbind()


# Check if the table exists
if (!dbExistsTable(con, "pt_distance_encoded")) {
  # If the table doesn't exist, create it
  dbWriteTable(con, "pt_distance_encoded", distances)
} else {
  # If the table exists, append data to it
  dbWriteTable(con, "pt_distance_encoded", distances, append = TRUE)
}

  return(tibble(worked = x))
  
}



log_output <- map(sequences,pt_by_area_chunked,.progress = T)

