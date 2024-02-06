library(tidyverse)
library(codebook)
library(sf)
source("R/00renv.R")



convert_to_factors <- function(df) {
  # Loop through each column of the dataframe
  for (col_name in names(df)) {
    # If the number of unique values in the column is fewer than 20
    if (length(unique(df[[col_name]])) < 20) {
      # Convert the column to a factor
      df[[col_name]] <- as.factor(df[[col_name]])
    }
  }
  # Return the modified dataframe
  return(df)
}

codebook_data <-  read_sf(con,
                          "dwelling_data_clean")  %>% 
  convert_to_factors() %>% 
  st_drop_geometry()

library(pointblank) # install with install.packages if needed



#CSV to manually enter info about each variable. 
query <- DBI::dbSendQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_name = 'dwelling_data_clean'")
col_names <- DBI::dbFetch(query) %>% pull(column_name)
tibble(col_name = col_names) %>% write_csv("working_data/col_names.csv")

#Once info is entered.... 
metadata <- read_csv("readme_files/data_dictionary.csv") %>%
  filter(col_name != "geometry") %>% 
  left_join(read_csv("working_data/shp_file_column_lookup.csv") %>% rename(col_name = long_col_name) )


my_informant <- create_informant(codebook_data) %>% 
  info_tabular(
    Description = "This project provides a map of every physical piece of land in Melbourne, with an estimate of how many residential dwellings are on each site. It also includes information on how many new dwellings have been constructed on the site since 2016. The data is provided as R code which makes a shapefile for use in mapping software such as QGIS. The file is reproducible using R.",
    `Creation of dwelling number information` = "The Department released a list of every piece of physical land in 2016 (stock), as well as physical land with new dwellings from 2016 to 2022 (flow).
<br><br>Because the department releases the data in yearly files, it's not simple to bring them together into a single view of what housing stock there is in 2022. Some lots get subdivided, consolidated, or developed more than once.
<br><br>The cleaning process in the file aims to keep a row for each piece of physical land in Melbourne, with information about it's most recent development.
<br><br>To do this we start with the 2016 'stock' map of Melbourne and complete the following steps with the 'flow' data:
<br><br>
<ol>   
<li>Only the most recent redevelopment on a piece of land is considered.</li>
 <li>Where there has been no consolidation or subdivision of land since 2016, information about the development is joined to the stock data</li>
 <li>Where redevelopment since 2016 has subdivided or consolidated the land, the new properties replace the older properties in the data</li>
 </ol>",
    `Other data sources` =  "Data has also been added from the heritage register, zoning, overlays, distance to public transport (including walking distance), and traffic pollution",
    Updates = "Updated yearly when the UDP program flow data is updated",
    `GitHub repo` = "[Sample Data GitHub repository](https://github.com/smach/SampleData)"
  )

for (i in 1:nrow(metadata)) {
  
  if(!is.na(metadata$shp_file_col_name_short[i])){
    my_informant <- my_informant %>%
      info_columns(
        columns = metadata$col_name[i],
        `Shapefile Short Column Name` = metadata$shp_file_col_name_short[i])
  }
  if(!is.na(metadata$Description[i])){
    my_informant <- my_informant %>%
      info_columns(
      columns = metadata$col_name[i],
      Description = metadata$Description[i])
  }
    
    if(!is.na(metadata$creation_method[i])){
      my_informant <- my_informant %>%
        info_columns(
        columns = metadata$col_name[i],
        `Creation method` = metadata$creation_method[i])
    }
    
    
    if(!is.na(metadata$Source[i])){
      source_with_link <- sprintf("<a href='%s'>%s</a>", metadata$`source link`[i], metadata$Source[i])
  
      my_informant <- my_informant %>%
        info_columns(
        columns = metadata$col_name[i],
        Source = source_with_link)
    }
    
    
    if(!is.na(metadata$`Data quality issues`[i])){
      my_informant <- my_informant %>%
        info_columns(
        columns = metadata$col_name[i],
        `Data quality issues` = metadata$`Data quality issues`[i])
    }
    
  if(is.factor(codebook_data[[metadata$col_name[i]]])) {
    # Get levels and their frequencies, including NA
    levels_freq <- table(codebook_data[[metadata$col_name[i]]], useNA = "ifany")
    # Convert levels and frequencies to a string
    levels_str <- paste(names(levels_freq), "(n=", prettyNum(levels_freq,big.mark = ","), ")", collapse = "<br>")
    # Construct the info string for factor levels
    factor_info_str <- paste("<br>", levels_str)
    
    my_informant <- my_informant %>%
      info_columns(
        columns = metadata$col_name[i],
        `Factor levels` = factor_info_str)
  }
  
  
  if (is.numeric(codebook_data[[metadata$col_name[i]]])) {
    
    string_summary <- paste(
      "<br>Average value:",round(mean(codebook_data[[metadata$col_name[i]]], na.rm = TRUE),1),
      "<br>Median value:", round(median(codebook_data[[metadata$col_name[i]]], na.rm = TRUE),1),
      "<br>Maximum value:",round(max(codebook_data[[metadata$col_name[i]]], na.rm = TRUE),1),
      "<br>Minimum value:",round(min(codebook_data[[metadata$col_name[i]]], na.rm = TRUE),1),
      "<br>Standard deviation:",round(sd(codebook_data[[metadata$col_name[i]]], na.rm = TRUE),1),
      "<br>NA:", sum(is.na(codebook_data[[metadata$col_name[i]]]))
    )
    
    my_informant <- my_informant %>%
      info_columns(
        columns = metadata$col_name[i],
        `Summary statistics` = string_summary)
    
  }
    
  }

my_informant %>% 
export_report("codebook.html") 

