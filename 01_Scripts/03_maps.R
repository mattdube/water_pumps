library(ggmap)
library(dplyr)
library(ggthemes)
library(lemon)


register_google(key = Sys.getenv("GOOGLE_API_KEY"), "standard")

tz_map <- get_googlemap(center = c(lon=34.6, lat= -7.27), zoom = 6, maptype = "roadmap", color="bw")
ggmap(tz_map)

tz_map_terrain <- get_googlemap(center = c(lon=34.6, lat= -7.17), zoom = 6, maptype = "terrain", color="bw")
ggmap(tz_map_terrain)

tz_map_terrain_small <- get_googlemap(center = c(lon=34.6, lat= -7.17), zoom = 5, maptype = "terrain", color="bw")
ggmap(tz_map_terrain)

# us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
# map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
# ggmap(map)

# box <- geocode('Tanzania', output = 'more')

lon <- train_sample$longitude
lat <- train_sample$latitude

map_sample <- train_sample %>%
    select(latitude, longitude, status_group) 

ggmap(tz_map) +
    geom_point(data=map_sample, aes(x=longitude, y=latitude, color = status_group), alpha = 0.5) +
    scale_color_manual(values = c("green3", "steelblue3", "orangered3"))

ggmap(tz_map) +
    geom_density_2d(data=map_sample, aes(x=longitude, y=latitude, color = status_group))


train_repair <- 
    train %>%
    filter(status_group == "functional needs repair")

train_broken <- 
    train %>%
    filter(status_group == "non functional")



ggmap(tz_map_terrain) +
    geom_point(data=train, aes(x=longitude, y=latitude, color = status_group), alpha = 0.4) +
    scale_color_manual(values = c("springgreen3", "purple3", "orangered3")) +
    theme(legend.position = c(0.26, 0.17), 
          legend.background = element_rect(fill = "grey90"),
          legend.text = element_text(size = 11, face = "bold"),
          legend.title = element_text(size=12, color="red",face = "bold"))

ggmap(tz_map_terrain) +
    geom_density_2d(data=train, aes(x=longitude, y=latitude, color = status_group), alpha = 0.9) +
    #geom_point(data=train, aes(x=longitude, y=latitude, color = status_group), alpha=0.3) +
    scale_color_manual(values = c("springgreen3", "purple3", "orangered3")) +
    facet_wrap(. ~status_group, nrow=2, ncol=2) +
    theme(legend.position = "none")

ggmap(tz_map_terrain) +
    geom_density_2d(data=train, aes(x=longitude, y=latitude, color = status_group)) +
    theme(legend.position = "none")

