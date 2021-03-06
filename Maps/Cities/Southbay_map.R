pacman::p_load(osmdata, sf, tidyverse, tigris, tictoc, dutchmasters, ochRe)
library(showtext)
font_add_google("Gochi Hand", "gochi")
# available_tags("highway")


min_lon <- -118.437767; max_lon <- -118.205338
min_lat <- 33.702635; max_lat <- 33.930542
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



color_roads <- rgb(0.42,0.449,0.488)
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

map2 <- map1 +  labs(caption = "Los Angeles -southbay")

counties_CA <- counties(state="CA",cb=T, class="sf")
counties_CA <- st_crop(counties_CA,
                       xmin=min_lon,xmax=max_lon,
                       ymin=min_lat,ymax=max_lat)
ggplot() + 
  geom_sf(data=counties_CA,fill="tan",lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()




get_water <- function(county_GEOID){
  area_water("CA", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties_CA$COUNTYFP,get_water))
water <- st_crop(water,
                 xmin=min_lon,xmax=max_lon,
                 ymin=min_lat,ymax=max_lat)

ggplot() + 
  geom_sf(data=counties_CA)+
  geom_sf(data=water,
          inherit.aes = F,
          col="red")+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()


st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
counties_CA <- st_erase(counties_CA,water)


ggplot() + 
  geom_sf(data=counties_CA,
          lwd=0)+
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE)+
  theme(legend.position = F) + theme_void()







d <- ggplot() + 
  # geom_sf(data=counties_CA,
  #         inherit.aes= FALSE,
  #         lwd=0.0,fill=rgb(0.203,0.234,0.277))+
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .4,
          alpha = .65) +
  geom_sf(data = highways$osm_lines,
          inherit.aes = FALSE,
          color=color_roads,
          size = .6,
          alpha = .65) +
  coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])), 
           ylim = c(min(bbx[2,]), max(bbx[2,])),
           expand = FALSE) +
  theme(legend.position = F) + theme_void()+
  theme(panel.background=
          element_rect(fill = "#FCF9F0"))

# d

tic()
ggsave(d,
       filename = "MAP3_southbay.png",
       scale = 1,
       width = 12,
       height = 8,
       units = "in",
       dpi = 100)
toc()






# tic()
# ggsave(map1, 
#        filename = "MAP1_southbay.png",
#        scale = 1, 
#        width = 36, 
#        height = 24, 
#        units = "in",
#        dpi = 500)
# toc()






