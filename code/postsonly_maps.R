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