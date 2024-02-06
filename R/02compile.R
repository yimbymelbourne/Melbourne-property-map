
source("R/00renv.R")
dbExecute(con, paste0("DROP TABLE IF EXISTS dwelling_data_clean"))

tbl(con,"dwellings_udp_unique") %>% 
  left_join(tbl(con,"dwellings_udp_poi")) %>% 
  left_join(tbl(con,"dwellings_udp_hos")) %>% 
  left_join(tbl(con,"dwellings_udp_ddos")) %>% 
  left_join(tbl(con,"dwellings_udp_zones")) %>% 
  left_join(tbl(con,"dwellings_udp_ncos")) %>% 
  left_join(tbl(con,"pt_distance_wide")) %>% 
  left_join(tbl(con,"traffic_pollution_aggregated")) %>% 
  left_join(tbl(con,"heritage_data_aggregated")) %>% 
  left_join(tbl(con,"asgs")) %>% 
  mutate(feature_type = if_else(!is.na(feature_type) & zone_category == "Civic land","Civic land",feature_type), #If the POI database doesn't have a POI like a park or school, but it's zoned park or school etc - we probably still can't build homes here. 
         feature_type_short = if_else(feature_type == "Civic land","not developable land",feature_type_short),
         dwellings_est = coalesce(dwellings_est, 0),
         heritage_status = case_when(!is.na(heritage_db_id)   ~ heritage_status,
                                     !is.na(heritage_overlay) ~ "Overlay - Type of Listing Not Recorded",
                                     T ~ "No heritage"),
         heritage = !(heritage_status == "No heritage"),
         res_possible_desc = case_when(feature_type_short == "not developable land"       ~ "Parks and civic facilities that aren't typically developed on",
                                       dwellings_est       > 1                           ~ "Already more than one property on this land",
                                       T ~ "Available for residential development"),
         res_possible = (res_possible_desc == "Available for residential development"),
         res_permitted_possible_desc = case_when(feature_type_short == "not developable land"       ~ "Parks and civic facilities that aren't typically developed on",
                                                 dwellings_est       > 1                            ~ "Already more than one property on this land",
                                                 zone_category == "Housing not generally permitted" ~ "Zoning usually prevents dwelling construction",
                                                 T ~ "Available for residential development"),
         res_permitted_possible = (res_permitted_possible_desc == "Available for residential development")
         )%>%
  compute(name = "dwelling_data_temp", temporary = TRUE)  


# reorder columns

query <- DBI::dbSendQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_name = 'dwelling_data_temp'")
col_names <- DBI::dbFetch(query) %>% pull(column_name)


first_cols <- c("lon","lat","id_stock","address","dwellings_est","dwlgs_in_2016","lot_size",
                grep("zone_",col_names, value = T),
                grep("heritage",col_names, value = T),
                grep("ddo",col_names, value = T),
                grep("neighbourhood_character_overlay",col_names, value = T),
                grep("feature_",col_names, value = T),
                grep("res_",col_names, value = T),
                grep("prox_",col_names, value = T),
                grep("traffic_pollution",col_names, value = T))

last_cols <- "geometry"

# Reorder the data frame so the selected row is first
new_order <- c(first_cols, setdiff(col_names, c(first_cols,last_cols)),last_cols)


# Construct the SQL for creating a new table or view
# This is a simplified example; actual SQL will depend on your database system

dbExecute(con, paste0("CREATE TABLE dwelling_data_clean AS SELECT ", paste(new_order, collapse= ", "), " FROM dwelling_data_temp")
          )


dwelling_data_clean <- read_sf(con,
                "dwelling_data_clean") 

# Function to capitalize letters following underscores and then remove underscores
capitalize_after_underscore <- function(s) {
  # Find all occurrences of an underscore followed by a lowercase letter
  matches <- gregexpr('(?<=_)[a-z]', s, perl = TRUE)
  
  # Extract the matched characters (letters following underscores)
  matched_chars <- regmatches(s, matches)
  
  # Capitalize these characters
  capitalized_chars <- lapply(matched_chars, toupper)
  
  # Replace the matched characters in the original string with their capitalized versions
  regmatches(s, matches) <- capitalized_chars
  
  # Finally, remove all underscores
  s <- gsub('_', '', s)
  
  return(s)
}

# Custom function to capitalize the first letter of each word
capitalize_first_letter <- function(x) {
  sapply(x, function(word) {
    paste0(toupper(substring(word, 1, 1)), tolower(substring(word, 2)))
  }, USE.NAMES = FALSE)
}

dwelling_data_clean %>% st_drop_geometry() %>% write_csv("Melbourne dwelling data.csv")


#Sf requires short variable names which is very frustrating! 
cols <- tibble(long_col_name = names(dwelling_data_clean)) %>% 
  filter(long_col_name != "geometry") %>% 
  mutate(shp_file_col_name_short = str_replace(long_col_name,"dev_","d_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"prox_","p_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"code_","c_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"dist_",""),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"_time",""),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"walk_","wk_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"name_","n_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"heritage_","he_"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"neighbourhood_character_overlay","nco"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"res_permitted_possible_desc","res_per_des"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"res_permitted_possible","res_per"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"res_possible_desc","res_poss_des"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"res_possible","res_poss"),
         shp_file_col_name_short = str_replace(shp_file_col_name_short,"neighbourhood_character_overlay","nco"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"_in_2016",""),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"dwlgs","dwlg"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"feature_type_short","fetre_type"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"townhouses","twn"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"apartments","ap"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"total","tl"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"unknown_type","unkn"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"d_data_source_year","d_data_yr"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"d_id_stock_former","d_id_st_old"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"d_height_storeys","d_height"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"d_dwlg_detached","d_dwlg_det"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"zone_category","zone_cat"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"dwellings_est","dwlg_est"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"_2021",""),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"traffic_pollution","pollution"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"feature","ftr"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"possible","poss"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"under_",""),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"traffic_pollution","pollution"),
        shp_file_col_name_short = capitalize_after_underscore(shp_file_col_name_short),
        shp_file_col_name_short = capitalize_first_letter(shp_file_col_name_short),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"Geometry","geometry"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"Lat","lat"),
        shp_file_col_name_short = str_replace(shp_file_col_name_short,"Lon","lon"),
        
        
        length =nchar(shp_file_col_name_short)) %>% 
  arrange(desc(length))
rename_vector <- setNames(cols$long_col_name,cols$shp_file_col_name_short)

dwellings_for_shape <- dwelling_data_clean %>% 
  mutate(lot_size = round(lot_size)) %>% #shp doesn't cope with huge values after decimal point
    #Due to file size limits let's only include the most useful variables
  rename(!!! rename_vector[rename_vector %in% names(.)])

dwellings_for_shape %>% 
  write_sf("Melbourne dwelling data.shp",quiet = FALSE)
dwellings_for_shape %>% select(lat,
                               lon) %>% 
  write_sf("working_data/Melbourne Dwelling Data geometry_only.shp")

#write col lookup table for data dictionary

write_csv(cols %>% filter(shp_file_col_name_short %in% names(dwellings_for_shape)), "working_data/shp_file_column_lookup.csv")
