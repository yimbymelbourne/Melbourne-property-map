
library(sfarrow)
source("R/00renv.R")
all_walkability_points <- st_read_parquet("data/walkability_by_node.parquet") %>% 
  st_set_crs("wgs84") %>% 
  st_transform(7844)

library(sf)

st_write(obj = all_walkability_points, 
         dsn = con, 
         Id(schema="public", 
            table = 'walkability_metrics'))


dbExecute(con, 'CREATE INDEX index_points ON walkability_metrics USING GIST (geometry);')


dbExecute(con, 'CREATE TABLE osmid_linkage AS
SELECT 
    d.lat AS lat,
    d.lon AS lon,
    d.geometry AS dwelling_map_geometry,
    pt.osmid AS osmid,
    pt.geometry AS osm_geometry,
    ST_Distance(d.geometry, pt.geometry) AS dist_from_osm_point_to_property
FROM 
    dwelling_data_clean d
LEFT JOIN LATERAL (
    SELECT
        w.osmid,
        w.geometry
    FROM
        walkability_metrics w
    ORDER BY 
        d.geometry <-> w.geometry
    LIMIT 1
) pt ON true;')

linkage <- dbGetQuery(con,'SELECT lat,lon,osmid,dist_from_osm_point_to_property FROM osmid_linkage')

linkage %>% write_csv("osm_linkage_to_property_db.csv")
