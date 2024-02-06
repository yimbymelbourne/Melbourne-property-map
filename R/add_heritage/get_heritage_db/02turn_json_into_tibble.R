library(tidyverse)
# Loading packages
library(httr)
library(jsonlite)
library(tidyjson)


heritage_register <- function(x) {
    print(x)
 # x<-"heritage_jsons//181636.json"
    output <- fromJSON(x) %>% 
              spread_all() %>% 
              mutate(status = as.character(status)) %>% 
      janitor::clean_names()
    

    clean_column_names <- colnames(output) %>% 
      tibble(v =.) %>% 
      mutate(v = str_replace_all(v,'[[:digit:]]+',"")) %>%
      mutate(v = str_replace_all(v,'___',"_")) %>% 
      mutate(v = str_replace_all(v,'__',"_")) %>% 
      mutate(v = str_replace_all(v,'_$',"")) %>% 
      mutate(id = 1) %>% 
      group_by(v) %>% 
      mutate(id = cumsum(id)) %>% 
      mutate(v2 = paste(v,id, sep = "_")) %>%
      mutate(v2 = ifelse(id==1, v, v2))
    
    colnames(output) <- clean_column_names$v2 
    
    output <- output %>% as_tibble() %>%  select(-(contains("images")))
    
    return(output)
    }


jsons <- list.files("heritage_jsons/",full.names = T)

heritage_db <- map_df(jsons,heritage_register)

heritage_db %>% 
  write_csv("heritage_db_verbose.csv")

heritage_db %>%   
  select(-matches("[[:digit:]]")) %>% 
  write_csv("working_data/heritage_db_abridged.csv")
