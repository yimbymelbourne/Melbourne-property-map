#This script takes each property from the property database and finds out which Feature of interest has the greatest overlap with it. 

source("R/00renv.R") #log into DB, load common packages



#POIS

dbExecute(con,"ALTER TABLE vicmap_pois
  ALTER COLUMN geoms TYPE geometry(Geometry, 7844)
    USING ST_Transform(geoms, 7844);")

dbExecute(con,  "UPDATE vicmap_pois
SET geoms = ST_MakeValid(geoms)
WHERE NOT ST_IsValid(geoms);")

dbExecute(con, 'CREATE INDEX ON vicmap_pois USING GIST (geoms);')

dbExecute(con,"DROP TABLE IF EXISTS dwellings_udp_poi")

sql_query <- "CREATE TABLE dwellings_udp_poi AS
SELECT 
    d.lat, d.lon,
    v.feature_type_short, v.feature_type
FROM 
    dwellings_urban_development_program d
LEFT JOIN LATERAL (
    SELECT 
        vicmap_pois.feature_type_short,
        vicmap_pois.feature_type,
        vicmap_pois.geoms  -- Required for the ST_Intersects and ST_Area conditions
    FROM 
        vicmap_pois
    WHERE 
        ST_Intersects(d.geometry, vicmap_pois.geoms)
    ORDER BY 
        ST_Area(ST_Intersection(d.geometry, vicmap_pois.geoms)) DESC
    LIMIT 1
) v ON true;
"
dbExecute(con, sql_query)

print("pois run!")

#ZONES

dbExecute(con,"ALTER TABLE vicmap_zones
  ALTER COLUMN geoms TYPE geometry(Geometry, 7844)
    USING ST_Transform(geoms, 7844);")

dbExecute(con,  "UPDATE vicmap_zones
SET geoms = ST_MakeValid(geoms)
WHERE NOT ST_IsValid(geoms);")

sql_query <- 'CREATE INDEX ON vicmap_zones USING GIST (geoms);'
dbExecute(con, sql_query)

dbExecute(con,"DROP TABLE IF EXISTS dwellings_udp_zones")
sql_query <- "CREATE TABLE dwellings_udp_zones AS
SELECT 
    d.lat, d.lon,
    v.zone_code, v.zone_short, v.zone_category
FROM 
    dwellings_urban_development_program d
LEFT JOIN LATERAL (
    SELECT 
        vicmap_zones.zone_code,
        vicmap_zones.zone_short,
        vicmap_zones.zone_category,
        vicmap_zones.zone_description,
        vicmap_zones.geoms  -- Required for the ST_Intersects and ST_Area conditions
    FROM 
        vicmap_zones
    WHERE 
        ST_Intersects(d.geometry, vicmap_zones.geoms)
    ORDER BY 
        ST_Area(ST_Intersection(d.geometry, vicmap_zones.geoms)) DESC
    LIMIT 1
) v ON true;
"
dbExecute(con, sql_query)

