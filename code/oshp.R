library(tidyverse)
library(ggplot2)
library(tidyverse)
library(tigris)
library(maptools)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Read In Data ------------------------------------------------------------
response <- read_csv("data/response.csv")
oshp <- read_csv("data/OSHP.csv") %>% 
  rename(postname = NAME)
avg <- read_csv("data/database_deptaverages.csv")
posts <- read_csv("data/posts.csv") %>%
  select(-DISTRICT) %>%
  mutate(PRATE = RESP/INDIV)
# Merges ------------------------------------------------------------------
postids <- oshp %>% filter(POST>0) %>% select(POST) %>% pull()
df <- avg %>% filter(POST %in% postids) %>% write_csv("data/df.csv")
ctys <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP)))
posts <- oshp %>%
  full_join(.,ctys, by = "FIPS") %>% 
  select(-(POST2:COUNTYNS),-(AFFGEOID:LSAD)) %>% 
  left_join(., df, by = "POST") %>% 
  st_as_sf()

# Maps --------------------------------------------------------------------
ggplot() +
  geom_sf(data = posts,
          aes(fill = avgage),
          color = "black") +
  scale_fill_gradient(high = "#4a1486", #
                      low = "#dadaeb", 
                      guide = "colorbar") +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Average Age Across OSHP Posts", 
          subtitle = "(By District and Post)") +
  labs(fill = "Average Age") +
  theme_void() +
  ggsave("plots/map_avgage.png", width = 10)

# Posts using Lat/Lon -----------------------------------------------------
cities <- places(cb = TRUE, state = "oh") %>% 
  rename(postname = NAME) %>% 
  inner_join(.,oshp, by = ("postname")) %>%
  full_join(.,posts, by = "POST") %>%
  mutate(centroids = st_centroid(geometry)) 
latlon <- cities$centroids
latlon <- data.frame(t(sapply(latlon,c)))
dots <- cbind(as.data.frame(cities),as.data.frame(latlon))%>%
  mutate(Y = X2,
         X = X1,
         X2 = NULL,
         X1 = NULL) %>% 
  left_join(., df, by = "POST") %>%
  st_as_sf()

# Maps, round 2 -----------------------------------------------------------
ggplot() +
  geom_sf(data = posts) +
  geom_point(data = dots,
             aes(x = X, y = Y,
                 fill = avgage),
             size = 3.5,
             shape = 21) +
  scale_fill_gradient(high = "#99000d", #
                      low = "#fee5d9", 
                      guide = "colorbar") +
  ggtitle("Average Age Across OSHP Posts", 
          subtitle = "(By District and Post)") +
  labs(fill = "Average Age") +
  theme_void() +
  ggsave("plots/map_avgage.png", width = 10)
###
oshp.places <- inner_join(oh.places,oshp, by = "NAME") %>%
  full_join(.,posts, by = "POST") %>%
  mutate(centroids = st_centroid(geometry)) 

###
oshp.dots <- cbind(as.data.frame(oshp.places),as.data.frame(latlon))
