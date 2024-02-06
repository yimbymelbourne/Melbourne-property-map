#Download the JSON for every heritage register entry in the Victorian Heritage Database

library(tidyverse)
library(httr)
library(jsonlite)


# script.R

#Find out the id number for each item on the heritage database
#The database has 'pages' so we first right a function to pull one page.... where x is the page number
get_full_json_for_database_search <- function(x) {
  start_time <- Sys.time()
  # Initializing API Call
  call <- paste0("https://api.heritagecouncil.vic.gov.au/v1/places?rpp=1000&page=",x)
  
  
  # Define the headers as a named list
  headers <- c("Content-Type" = "application/json", 
               "Accept"       = "application/json", 
               "apikey"       = Sys.getenv("vic_gov_api"))
  
  # Send a GET request to the API with headers
  response <- GET(call, add_headers(headers))
  
  # Check the status code of the response
  json_content <- content(response, as = "text")
  
  
  parsed_data <- jsonlite::fromJSON(json_content, flatten = TRUE)
  Sys.sleep(2)
  
  return(parsed_data)
}

#Then we pull one page to find out how many page numbers there are... 

heritage_search_result <- get_full_json_for_database_search(1) 
pages_in_heritage_database <- heritage_search_result$page_count

#Now we pull the id numbers on a given page
get_just_place_ids <- function(x) {
  parsed_data <- get_full_json_for_database_search(x)
  return(parsed_data$`_embedded`$places)
  
}

#and we pull each page number sequentially: 
list_of_heritage_ids  <-map_df(seq(1,pages_in_heritage_database,1),get_just_place_ids)

#Now for each page number pull the heritage DB json record

get_place_json <- function(x) {
  
  start_time <- Sys.time()
  # Initializing API Call
  call <- paste0("https://api.heritagecouncil.vic.gov.au/v1/places/",x)
  
  
  # Define the headers as a named list
  headers <- c("Content-Type" = "application/json", 
               "Accept"       = "application/json", 
               "apikey"       = Sys.getenv("vic_gov_api"))
  
  # Send a GET request to the API with headers
  response <- GET(call, add_headers(headers))
  
  # Check the status code of the response
  json_content <- content(response, as = "text")
  
  write_json(json_content,paste0("heritage_jsons/",x,".json"))
  
  existing_files <- list.files("heritage_jsons/",) %>% 
    gsub(pattern=".json$", replacement="") %>% length()
  files_left <- nrow(list_of_heritage_ids) - existing_files
  time_left <- round(files_left*2.4/60/60,1)
  end_time <- Sys.time()
  diff = end_time-start_time
  sleep_time = max(0,2.4 -(diff ))
  print(paste0("original diff ",
               round(diff,1),
               " sleeping for ",
               round(sleep_time,1)))
  Sys.sleep(sleep_time)
  print(paste0("final_diff ",round(Sys.time()-start_time,1),". ",time_left," hours left."))
}
  
download_missing_jsons <- function(x) {
  #Don't download jsons we already have....
  existing_files <- list.files("heritage_jsons/",) %>% 
                    gsub(pattern=".json$", 
                    replacement="")

  files_left <- list_of_heritage_ids %>% 
                filter(!(id %in% existing_files)) %>% 
                pull(id)

  #Download one json for each item on the heritage register
  walk(files_left,get_place_json)
  }

download_missing_jsons()
#Check again what we've downloaded... 
jsons <- list.files("heritage_jsons/",full.names = T)
 
 #Check for bad jsons - usually this happens if there was a rate limit imposed and hte file couldn't be downloaded...   
delete_bad_jsons <- function(x) {
   error_string <- "502 Bad Gateway"
   output <- fromJSON(x) 
   if(str_detect(output,error_string)){
      file.remove(x)
     print(paste0("deleted ",x," because it was not a proper heritage entry. Re-run lines 106-123 again."))
     }
   }
 
walk(jsons,delete_bad_jsons) 

#re-download the jsons that errored. 
download_missing_jsons()

#repeat lines 106-123 just to make sure no jsons are deleted, and therefore all are working 
