#This script takes each property from the property database and finds out which HO has the greatest overlap with it. 

source("R/00renv.R") #log into DB, load common packages

#Heritage overlays
dbExecute(con,  "UPDATE vicmap_heritage_overlays
SET geoms = ST_MakeValid(geoms)
WHERE NOT ST_IsValid(geoms);")

sql_query <- 'CREATE INDEX ON vicmap_heritage_overlays USING GIST (geoms);'
dbExecute(con, sql_query)

dbExecute(con, sql_query)

dbExecute(con,"DROP TABLE IF EXISTS dwellings_udp_hos")
sql_query <- "CREATE TABLE dwellings_udp_hos AS
SELECT 
    d.lat, d.lon,
    v.heritage_overlay
FROM 
    dwellings_urban_development_program d
LEFT JOIN LATERAL (
    SELECT 
        vicmap_heritage_overlays.heritage_overlay,
        vicmap_heritage_overlays.geoms  -- Required for the ST_Intersects and ST_Area conditions
    FROM 
        vicmap_heritage_overlays
    WHERE 
        ST_Intersects(d.geometry, vicmap_heritage_overlays.geoms)
    ORDER BY 
        ST_Area(ST_Intersection(d.geometry, vicmap_heritage_overlays.geoms)) DESC
    LIMIT 1
) v ON true;
"

dbExecute(con, sql_query)
print("run heritage overlays! ")