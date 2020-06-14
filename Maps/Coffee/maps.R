
# Torrance ----------------------------------------------------------------

pacman::p_load(ggmap,data.table,ggimage)
library(ggmap)
library(data.table)
library(ggimage)
test <- fread("test.csv")

register_google(key="AIzaSyCpTFuK3V56DMLmYLECMP79SZKTiJdheDA")
torrance <- c(lon = -118.3406, lat = 33.8358)
torrance_map <- get_map(location = torrance, zoom = 10, maptype = "terrain", source = "google")
torrance_map <- get_map(location = torrance, zoom = 12, maptype = "watercolor", source = "stamen")
torrance_map <- get_map(location = torrance, zoom = 10, maptype = "toner-2011", source = "stamen")
torrance_map <- get_map(location = torrance, zoom = 10, maptype="terrain-lines",source="stamen")
p <- ggmap(torrance_map, extent = 'device')
p
p + geom_image(aes(x = Lon, y = Lat, image = Image), data = test, size = 0.02)


 
satelite <- c(lon = -99.249999, lat = 19.5166646)
satelite_map <- get_map(location = satelite, zoom = 10, maptype = "terrain", source = "google")
satelite_map <- get_map(location = satelite, zoom = 14, maptype = "toner-2011", source = "stamen")
ggmap(satelite_map)



# Rexburg ------------------------------------------------------------------
# https://www.youtube.com/watch?v=2UZKm2Kc88U


pacman::p_load(tmaptools,sf,tmap,dplyr)

# unzip("acs2018_5yr_B01003_16000US1667420.zip", exdir = "rexburg_shaoefile", junkpaths = TRUE, overwrite = TRUE)

zippcode_geo <- sf::st_read("rexburg_shaoefile/acs2018_5yr_B01003_16000US1667420.shp")

zipcode_geo <- dplyr::filter(zippcode_geo, name != "Rexburg")

rex <- qtm(zipcode_geo) + tm_legend(show = FALSE)
str(rex)
rex

