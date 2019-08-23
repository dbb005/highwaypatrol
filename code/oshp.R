library(tidyverse)
install.packages("ggplot2")
install.packages("tidycensus")
library(ggplot2)
library(tidyverse)
library(tidycensus)
library(tigris)
library(maptools)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
###
setwd("D:/HP/Maps")
###

ggplot() +
  geom_sf(data = oh.st) + 
  geom_sf(data = oshp.places)

deptaverages <- read_csv("database_deptaverages.csv")

### Andrew Code
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  axis.title = element_blank()
)
oshp <- read_csv("oshp.csv")
response <- read_csv("response.csv")
posts <- read_csv("posts.csv") %>%
  select(-DISTRICT) %>%
  mutate(PRATE = RESP/INDIV)
###
oh.places <- places(cb = TRUE, state = "oh")
oh.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Ohio")
###
oh.ctys <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP)))

districts <- oshp %>%
  select(-STATE,-(POST:GHQ)) %>%
  full_join(.,response, by = "DISTRICT") %>%
  select(-RESP_RATE) %>%
  mutate(RATE = RESP/INDIV) %>%
  full_join(.,oh.ctys, BY = "FIPS") %>%
  distinct()
### FOR POSTS
oshp.places <- inner_join(oh.places,oshp, by = "NAME") %>%
  full_join(.,posts, by = "POST") %>%
  mutate(centroids = st_centroid(geometry)) 

latlon <- oshp.places$centroids
latlon <- data.frame(t(sapply(latlon,c)))
oshp.dots <- cbind(as.data.frame(oshp.places),as.data.frame(latlon))%>%
  mutate(Y = X2,
         X = X1,
         X2 = NULL,
         X1 = NULL) %>%
  st_as_sf()
#geom_sf(data = oh.st) +

ggplot() +
  geom_sf(data = districts,
          aes(fill = RATE),
          color = "black") +
  labs(fill = "District", size = "Post") +
  scale_fill_gradient(high = "#e6550d", 
                      low = "#fee6ce", 
                      guide = "colorbar",
                      labels = percent) +
  ggtitle("Survey One Response Rate", 
          subtitle = "(By District and Post)") +
  theme_bw() +
  ditch_the_axes + 
  geom_point(data = oshp.dots, 
             aes(x = X, y = Y, size = PRATE),
             color='white',
             fill = "black",
             shape = 21) +
  scale_size(labels = percent)

### Daniel Code
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  axis.title = element_blank()
)
oshp <- read_csv("oshp.csv")
deptavg <- read_csv("database_deptaverages.csv")
posts <- read_csv("posts.csv") %>%
  select(-DISTRICT) %>%
  mutate(PRATE = RESP/INDIV)
###
oh.places <- places(cb = TRUE, state = "oh")
oh.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Ohio")
###
oh.ctys <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP)))
###
oshp.places <- inner_join(oh.places,oshp, by = "NAME") %>%
  full_join(.,posts, by = "POST") %>%
  mutate(centroids = st_centroid(geometry)) 

###
oshp.dots <- cbind(as.data.frame(oshp.places),as.data.frame(latlon))
