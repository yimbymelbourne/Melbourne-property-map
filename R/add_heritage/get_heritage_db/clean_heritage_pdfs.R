source("R/00renv.R")
options(java.parameters = "-Xmx2g")
library(rJava)
library(tidyverse)
library(tabulizer)
library(healthyAddress)
library(strayr)

postcodes <- read_csv("working_data/au_postcodes.csv") %>% 
  mutate(suburb = toupper(place_name)) %>% 
  filter(state_name == "Victoria") %>%
  select(suburb,postcode) %>% 
  filter(substr(postcode,1,1) == "3") %>% 
  bind_rows(tibble(suburb = "JOLIMONT",postcode = 3004))

suburbs_vector <- setNames(postcodes$suburb,postcodes$suburb)
find_suburb <- function(address, suburbs_vector) {
  # Create a pattern that matches any of the suburbs at the end of the address string
  pattern <- paste0("\\b(", paste(names(suburbs_vector), collapse = "|"), ")$")
  possible_matches <- c(stringr::str_extract(address, pattern),"")
  
  sorted_matches <- possible_matches[order(nchar(possible_matches), decreasing = TRUE)]
  return(sorted_matches[1])
}

#Function takes 11-15 and turns it into 11,13,15. 
expand_range <- function(range_with_text) {
  # Extract just the numeric range part from the text
  numeric_range <- str_extract(range_with_text, "\\b\\d+-\\d+\\b")
  
  # Proceed only if the numeric range was successfully extracted
  if (!is.na(numeric_range)) {
    bounds <- as.numeric(strsplit(numeric_range, "-")[[1]])
    if (length(bounds) != 2 || any(is.na(bounds)) || bounds[1] >= bounds[2]) {
      return(NA)
    }
    
    # Ensure the sequence starts with the same parity as the first number
    if (bounds[1] %% 2 != bounds[2] %% 2) {
      bounds[2] <- bounds[2] - 1
    }
    
    # Generate the sequence, incrementing by 2 to maintain even/odd parity
    seq <- seq(from = bounds[1], to = bounds[2], by = 2)
    
    return(seq)
  } else {
    return(NA)
  }
}




##YARRA - Imported here but it's not that different to the heritage DB so I've not imported it. 

filepath <- "data/heritage_files/Yarra heritage.pdf"  # Update this to the path of your PDF

# Example column boundaries (adjust these based on your PDF)
columns <- list(c(50,   # Overlay 
                  106,  # Address
                  152,  # Type
                  200,  # number
                  245,  # suburb
                  518,  # property type
                  577,  # property number
                  652)) # heritage status

# Example area (adjust these based on your PDF)
area <- list(c(30,0, 30+570,0+780))  # top, left, bottom, right

yarra_list <- extract_tables(filepath, pages = seq(3,454), columns = columns, area = area,guess = F)
# Assuming tables is a list of matrices returned by extract_tables
yarra <- lapply(yarra_list, function(x) as.data.frame(x, stringsAsFactors = FALSE)) %>%
  bind_rows()%>% 
  set_names(c("overlay number","street_name","street_type","street_number","suburb","property_name","council_no","heritage_status","constructed_date")) %>%
  filter(!is.na(council_no),
         !(council_no %in% c("","Property No."))) %>% 
  mutate(suburb = toupper(suburb)) %>% 
  mutate(street_type = if_else(street_type == "Crescen","Crescent",street_type),
         heritage_authority_name = "Yarra",
         id = paste0("Yarra pdf",council_no)) %>% 
  left_join(postcodes)





## Melbourne

melbourne_importer <- function(page_no) {
melbourne_filepath <- "data/heritage_files/City of Melbourne heritage.pdf"  # Update this to the path of your PDF
suburb_name <- extract_tables(melbourne_filepath, pages = page_no,
                              area = list(c(70,50,70+18,800)),
                              output = "character",
                              guess = F)  # Specify pages if needed


com_list <- extract_tables(melbourne_filepath, 
                           pages = page_no,
                           area = list(c(120,50,769,800)),
                           columns = list(c(170,
                                            317,
                                            426)),
                           guess = F
                           )  # Specify pages if needed




com <- lapply(com_list, function(x) as.data.frame(x, stringsAsFactors = FALSE)) %>%
  bind_rows() %>% 
  filter(V3 != "",
         V3 != "(",
         V2 != "Number",
         V3 != "Stree",
         V3 != "uilding Category Sign") %>% 
  mutate(V3 = trimws(if_else(str_detect(V2, "\\s[CSI]$"), paste0(str_extract(V2, "\\s[CSI]$"), V3), V3)),
         V2 = str_replace(V2, "\\s[CSI]$", "")) %>% 
  mutate(V1 = if_else(V1 == "",NA_character_,V1),
         V2 = str_replace_all(V2, "[^\\x20-\\x7E]", "")) %>% 
  mutate(V2 = str_remove_all(V2,", includes:"),
         V2 = trimws(V2)) %>% 
  mutate(street_name = if_else(is.na(V1), 
                               str_match(V2, "^(?:\\d+\\s*[-–/]\\s*)*\\d+\\s+([A-Za-z'\\s]+)")[,2], 
                               NA_character_),
          V2 = if_else(is.na(V1), 
                       str_remove_all(V2, street_name),
                       V2),
          V1 = if_else(is.na(V1), street_name, V1)) %>%
  filter(!is.na(V1) ) %>%
  mutate(V4 = if_else(V4 =="ficant","Significant",V4)) %>% 
  mutate(V4 = if_else(V4 =="Significant (applies to","Significant",V4)) %>% 
  mutate(V3 = if_else(V3 == "Signi","Significant",V3)) %>% 
  mutate(V3 = str_extract(V3, "^[^\\s]+")) %>%
            select(-street_name) %>% 
  mutate(suburb = suburb_name[[1]][1])

}

melbourne_data <- map_df(seq(6,242),
                         melbourne_importer) %>% 
  set_names(c("street_name",
              "street_number",
              "heritage_status",
              "heritage_status_streetscape",
              "suburb")) %>% 
  mutate(suburb = str_replace(suburb," AND ",","),
         id = paste0("Melbourne pdf",row_number()),
         street_number = if_else(street_number == "407B-407D","407",street_number),
         street_number = if_else(str_detect(street_number,"relates to Harris"),"92-132",street_number)
         ) %>%  
  separate(suburb, sep = ",",into = c("s1","s2","s3","s4")) %>%
  pivot_longer(cols = c("s1","s2","s3","s4"),
               names_to = "s",
               values_to = "suburb")  %>%
    select(-s)%>%
  mutate(suburb = trimws(if_else(suburb == "NORTH","NORTH MELBOURNE",suburb)),
         heritage_authority_name = "Melbourne") %>%
  filter(!is.na(suburb)) %>% 
  left_join(postcodes) 

### full heritage

source("R/add_heritage/get_heritage_db/03import_filtered_heritage_db.R")


 full_db <- import_filtered_heritage_db() %>% 
  mutate(address = toupper(address),
         address = str_remove(address, "^UNIT \\d+,\\s*")) %>%
  separate(address, 
           into = c("address", "lga"), 
           sep = ",(?=[^,]+$)", # Regex for splitting at the last comma
           extra = "merge") %>%
   mutate(address         = str_replace(address," AND ",", "),
          street_number   = str_extract(address, "^[^A-Za-z]+"),
          rest_of_address = str_remove(address, "^[^A-Za-z]+"),
          id = paste0("heritage_db_geocoded_manually_with_pnaf_add",id)
   ) %>% 
   separate_rows(street_number, sep = "\\s*(,|&)\\s*") %>%
   # Clean up the street_numbers column to remove any trailing non-numeric characters and additional spaces
   mutate(street_number = str_trim(street_number, 
                                   side = "both")) %>% 
  mutate(address = toupper(paste(street_number,rest_of_address))) %>% 
  mutate(address = str_replace(address,"GEELONG EAST","EAST GEELONG"),
         address = str_replace(address,"EXFORD ROAD WEIR VIEWS","EXFORD ROAD MELTON SOUTH"),
         address = str_replace(address,"279 MONT ALBERT ROAD ","279 MONT ALBERT ROAD SURREY HILLS")) %>% 
  rowwise() %>%
  mutate(suburb = find_suburb(address, suburbs_vector),  # Find the suburb in the address
         street_name = if_else(!is.na(suburb),  # If a suburb is found
                           str_remove(rest_of_address, paste0("\\b", suburb, "$")),  # Remove it from the end of the address
                           rest_of_address)) %>%
  ungroup() %>% 
  left_join(postcodes) %>% 
   filter(!is.na(street_number)) %>% 
   select(-lga,
          -rest_of_address)
 

 
## into one row
 
mel_yarra_db_raw <- melbourne_data %>%
  bind_rows(yarra) %>% 
  rename(status = heritage_status) %>% 
  bind_rows(full_db) %>% 
  mutate(expanded_range = map(street_number, expand_range)) %>% 
  unnest(expanded_range) %>% 
  mutate(street_number = coalesce(expanded_range,parse_number(street_number))) %>% 
  filter(!is.na(postcode),
         !is.na(suburb),
         !is.na(street_name),
         !is.na(street_number)) %>% 
  mutate(address = paste(street_number,
                         street_name,
                         replace_na(street_type,""),
                         suburb,
                         postcode),
         address = str_replace(address,"  "," ")) %>%
  filter(!str_detect(address,"SEE ALSO"),
         !str_detect(address,"SEE ALSO"),
         !str_detect(address,"DOVER ROAD AND "),
         !str_detect(address,"105 A, 1-10/107-109,"))


#####
source("R/add_heritage/get_gnaf.R")
gnaf <- create_gnaf_dataset()
#######

mel_yarra_db_1 <- standardize_address(mel_yarra_db_raw$address) %>% 
  bind_cols(mel_yarra_db_raw) %>% 
  filter(STREET_NAME != "") %>% 
  select(-street_name,
         -street_number,
         -expanded_range,
         -street_name,
         -street_type) %>% 
  rename(LOCALITY_NAME = suburb,
         ) %>% 
  left_join(gnaf) %>% 
  mutate(
    lon = coalesce(LONGITUDE,sf::st_coordinates(.$geometry)[,1],),
    lat = coalesce(LATITUDE,sf::st_coordinates(.$geometry)[,2])) %>% 
  select(-LATITUDE,
         -LONGITUDE,
         -geometry
         )  
mel_yarra_db <- mel_yarra_db_1%>% 
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # Ensure to set the correct CRS for your data
  bind_rows(mel_yarra_db_1 %>% filter(is.na(lat)))

 
mel_yarra_db %>% write_rds("working_data/com_and_coy_heritage_db.rds")


