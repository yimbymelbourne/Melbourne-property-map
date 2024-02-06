
rm(list = ls())
source("R/00renv.R")


#Now let's create a table with one row per lat/lon combination, 
#so that we know the closest rail/not rail to every address 
#This really should be done in SQL but I never got around to it 

#First half finds closest distance in time/walking

closest_walking_grouped <-dbGetQuery(con,"SELECT 
    lat,
    lon,
    rail,
    MIN(distance) AS distance,
    MIN(duration) AS duration
FROM (
    SELECT 
        lat,
        lon,
        duration,
        distance,
        CASE 
            WHEN foi_description_detailed IN ('train_future','train','vline_train') THEN 'train'
            WHEN foi_description_detailed IN ('tram') THEN 'tram'
            WHEN foi_description_detailed IN ('bus','vline_bus') THEN 'bus'
            WHEN foi_description_detailed IN ('brt') THEN 'brt'
            ELSE 'error'
        END as rail
    FROM 
        pt_distance_encoded
) AS subquery
GROUP BY 
    lat, lon, rail;") %>% 
  rename(prox_walk_time_s = duration,
         prox_walk_dist_m = distance)



closest_walking_wide <- closest_walking_grouped %>% 
  pivot_wider(names_from = "rail",
              values_from = c("prox_walk_time_s",
                              "prox_walk_dist_m"),
              names_glue = "{.value}_{rail}")  

#Second half finds closest distance as the crow flies

dist_haversine_grouped <-dbGetQuery(con,"SELECT 
    lat,
    lon,
    rail,
    MIN(distance_m) AS min_distance_m
FROM (
    SELECT 
        lat,
        lon,
        distance_m,
        CASE 
            WHEN foi_description_detailed IN ('train_future','train','vline_train') THEN 'train'
            WHEN foi_description_detailed IN ('tram') THEN 'tram'
            WHEN foi_description_detailed IN ('bus','vline_bus') THEN 'bus'
            WHEN foi_description_detailed IN ('brt') THEN 'brt'
            ELSE 'error'
        END as rail
    FROM 
        features_of_interest
) AS subquery
GROUP BY 
    lat, lon, rail;") %>% 
  rename(prox_dist_m = min_distance_m)

#bring it all together into one df

closest_distance_wide <- dist_haversine_grouped %>% 
  pivot_wider(names_from = "rail",
              values_from = c("prox_dist_m"),
              names_glue = "{.value}_{rail}")  


dbWriteTable(con, "pt_distance_wide", closest_walking_wide %>% full_join(closest_distance_wide),overwrite = T)



%>%
  collect() %>% 
  mutate(rail = case_when(foi_description_detailed %in% c("train_future",
                                                          "train",
                                                          "vline_train") ~ "rail_brt",
                          foi_description_detailed == "tram" ~ "tram",
                          foi_description_detailed == "brt" ~ "brt",
                          foi_description_detailed %in% c("vline_bus","bus") ~ "bus",
                          T ~ "error1")
  ) %>% 
  select(-foi_description_detailed) %>% 
  group_by(lat,lon,rail) %>%  
  arrange(prox_walk_dist_m) %>% 
  filter(row_number() ==1 ) %>% 
  pivot_wider(names_from = "rail",
              values_from = c("prox_walk_time_s",
                              "prox_walk_dist_m"),
                names_glue = "{.value}_{rail}")

dbWriteTable(con, "pt_distance_walking_wide", 
             closest_walking %>% 
             full_join(closest_walking_grouped),overwrite = T)

rm(closest_walking)

#Second half finds distance in m and puts it into grouped columns

dist_haversine <-dbGetQuery(con,"SELECT 
    lat,
    lon,
    rail,
    MIN(distance_m) AS min_distance_m
FROM (
    SELECT 
        lat,
        lon,
        distance_m,
        CASE 
            WHEN foi_description_detailed IN ('train_future', 'tram', 'brt','train','vline_train') THEN 'rail_brt'
            ELSE 'bus'
        END as rail
    FROM 
        features_of_interest
) AS subquery
GROUP BY 
    lat, lon, rail
ORDER BY 
    min_distance_m;")


dist_haversine_grouped <-dbGetQuery(con,"SELECT 
    lat,
    lon,
    rail,
    MIN(distance_m) AS min_distance_m
FROM (
    SELECT 
        lat,
        lon,
        distance_m,
        CASE 
            WHEN foi_description_detailed IN ('train_future','train','vline_train') THEN 'train'
            WHEN foi_description_detailed IN ('tram') THEN 'tram'
            WHEN foi_description_detailed IN ('bus','vline_bus') THEN 'bus'
            WHEN foi_description_detailed IN ('brt') THEN 'brt'
            ELSE 'error'
        END as rail
    FROM 
        features_of_interest
) AS subquery
GROUP BY 
    lat, lon, rail
ORDER BY 
    min_distance_m;")


closest_haversine <- dist_haversine %>% 
  filter(rail != "bus") %>% #Bus is in both dataframes only need it in one.
  bind_rows(dist_haversine_grouped) %>% 
  rename(distance_m = min_distance_m) %>% 
  pivot_wider(names_from = "rail",
              values_from = c("distance_m"),
              names_glue = "prox_dist_m_{rail}") 

  
  dbWriteTable(con, "pt_distance_encoded_m_wide", closest_haversine,overwrite = T)
  
  
  

  
  

  