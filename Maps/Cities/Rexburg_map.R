pacman::p_load(osmdata, sf, tidyverse, tigris, tictoc)


# bbx <- getbb("Boston, MA")
available_tags("highway")


min_lon <- -111.820135; max_lon <- -111.751728
min_lat <- 43.796500; max_lat <- 43.846227
bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
colnames(bbx) <- c("min","max")




highways <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

streets <- bbx %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()



# color_roads <- rgb(0.42,0.449,0.488)
color_roads <- rgb(0.255,0.255,0.51)

map1 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          col = color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          col = color_roads,
          size = .6,
          alpha = .8)+
  coord_sf(xlim = c(min_lon,max_lon),
           ylim = c(min_lat,max_lat),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

# tic()
# ggsave(map1, 
#        filename = "MAP1_southbay.png",
#        scale = 1, 
#        width = 36, 
#        height = 24, 
#        units = "in",
#        dpi = 500)
# toc()


require(tigris)
library(rappdirs)

user_cache_dir()
options(tigris_use_cache = FALSE)
counties_MA <- tigris::counties(state="ID")
counties_MA <- st_crop(counties_MA,
                       xmin=min_lon,xmax=max_lon,
                       ymin=min_lat,ymax=max_lat)
ggplot() + 
  geom_sf(data=counties_MA,fill="gray",lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()

get_water <- function(county_GEOID){
  area_water("ID", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties_MA$COUNTYFP,get_water))
water <- st_crop(water,
                 xmin=min_lon,xmax=max_lon,
                 ymin=min_lat,ymax=max_lat)



ggplot() + 
  geom_sf(data=counties_MA)+
  geom_sf(data=water,
          inherit.aes = F,
          col="blue")+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()



st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
counties_MA <- st_erase(counties_MA,water)


ggplot() + 
  geom_sf(data=counties_MA,
          lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()




ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill=rgb(0.211,0.211,0.211))+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = rgb(0.24,0.24,0.24)))+
  ggtitle("Dark + Yellow theme")



ggplot() + 
  geom_sf(data=counties_MA,
          inherit.aes= FALSE,
          lwd=0.0,fill=rgb(0.205,0.234,0.277))+
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .8,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .9,
          alpha = .65) +
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = rgb(0.24,0.24,0.24)))











  tic()
ggsave(map1, 
       filename = "Map_Rexburg.png",
       scale = 1, 
       width = 12, 
       height = 8, 
       units = "in",
       dpi = 100)
toc()




