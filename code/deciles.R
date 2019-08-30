install.packages(devtools)
library(devtools)
devtools::install_github("andrewvanleuven/rleuven")
library(tidyverse)
library(ggplot2)
library(rleuven)
library(tigris)
library(maptools)
library(sf)
library(ggrepel)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Read In Data ------------------------------------------------------------
response <- read_csv("data/response.csv")
oshp <- read_csv("data/OSHP.csv") %>% 
  rename(postname = NAME)
avg <- read_csv("data/dept_avgs.csv")

# Merges ------------------------------------------------------------------
postids <- oshp %>% filter(POST>0) %>% select(POST) %>% pull()
df <- avg %>% filter(POST %in% postids) %>% write_csv("data/df.csv")
df_ntile <- df %>%
  mutate_at(.funs = list(ntile = ~ntile(., 10)), .vars = vars(2:43)) %>% 
  select(-(avgage:woman_not_suitableagree))
# Dividing Posts in Same County -------------------------------------------
oshp_map <- read_csv("data/OSHP.csv") %>% 
  rename(postname = NAME)
ctys_map <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP))) %>% st_transform(crs = 4326) %>% 
  rename(CNAME = NAME) %>% 
  select(FIPS, CNAME, geometry)
posts_map <- oshp %>% 
  select(FIPS,POST) %>% distinct()
# Unique Geography --------------------------------------------------------
portage_sf <- st_polygon(list(rbind(c(-81.403070,41.193248),c(-80.993491,41.207760),c(-80.988483,41.362894),c(-81.412207,41.360750),c(-81.403070,41.193248)))) %>% st_sfc(.,crs = 4326) %>% st_sf()
portage <- st_difference((ctys_map %>% filter(CNAME == "Portage")),(portage_sf)) 
portage2 <- st_intersection((portage_sf),(ctys_map %>% filter(CNAME == "Portage"))) 
ggplot() +
  geom_sf(data = portage) +
  geom_sf(data = portage2, fill = "black")

erie_sf <- st_polygon(list(rbind(c(-82.682285,41.279720),c(-82.958856,41.279116),c(-82.988897,41.694765),c(-82.535017,41.651446),c(-82.682285,41.279720)))) %>% st_sfc(.,crs = 4326) %>% st_sf()
erie <- st_difference((ctys_map %>% filter(CNAME == "Erie")),(erie_sf)) 
erie2 <- st_intersection((erie_sf),(ctys_map %>% filter(CNAME == "Erie"))) 
ggplot() +
  geom_sf(data = erie) +
  geom_sf(data = erie2, fill = "black")

ctys2 <- ctys_map %>% filter(!CNAME %in% c("Portage","Erie")) %>% 
  rbind(portage) %>% 
  rbind(portage2) %>%
  rbind(erie) %>% 
  rbind(erie2) %>% 
  left_join(.,posts_map) %>% 
  mutate(area = as.numeric(st_area(.)))

ctys2 <- ctys2[-(92:93),]
ctys2 <- ctys2[-90,]
unique_geog <- ctys2[-87,]
ggplot() +
  geom_sf(data = unique_geog,
          aes(fill = factor(POST)))

# Percentiles -------------------------------------------------------------
ctys <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP)))
posts <- oshp %>%
  full_join(.,ctys, by = "FIPS") %>% 
  select(-(POST2:COUNTYNS),-(AFFGEOID:LSAD)) %>% 
  left_join(., df_ntile, by = "POST") %>% 
  st_as_sf()
post_xw <- posts %>% 
  filter(!is.na(postname)) %>% 
  select(POST, postname) %>% 
  st_drop_geometry() 
rm(ctys, posts)
post_sf <- st_dissolve(unique_geog, POST) %>% 
  select(-STATUS) %>% 
  left_join(.,df_ntile) %>% 
  left_join(.,post_xw) %>% 
  select(POST,postname,everything(),geometry) %>% 
  st_centroid_xy()
# Maps --------------------------------------------------------------------
ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgage_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(32)","", "50th\n(37)","", "99th\n(43)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Average Age Across OSHP Posts") + #,subtitle = "") +
  ggrepel::geom_label_repel(data = post_sf,
                           aes(x = x, y = y, label = postname)) +
  labs(fill = "Average Age\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_avgage.png", width = 10, height = 10)

#the solve for the "but what if the spread is small?" question may be the have the labels be... 
#1st
#(number associated with that percentile)
#
#50th
#(number associated with that percentile)
#
#99th
#(number associated with that percentile)
# Graveyard - Posts using lat/lon -----------------------------------------------------
dots <- places(cb = TRUE, state = "oh") %>% 
  rename(postname = NAME) %>% 
  inner_join(.,oshp, by = "postname") %>%
  select(postname,POST,DISTRICT,geometry) %>% 
  left_join(., df, by = "POST") %>%
  rleuven::st_centroid_xy() 
# Graveyard - Posts using dots -----------------------------------------------------------
ggplot() +
  geom_sf(data = posts) +
  geom_point(data = dots,
             aes(x = x, y = y,
                 fill = avgtenure),
             size = 3.5, shape = 21, alpha = 0.75) +
  #ggrepel::geom_label_repel(data = dots,
  #                         aes(x = x, y = y, label = postname)) +
  scale_fill_gradient(high = "#005824", #
                      low = "#e5f5f9", 
                      guide = "colorbar") +
  ggtitle("Average Tenure Across OSHP Posts") + 
  labs(fill = "Average\nTenure") +
  theme_void() #+
  #ggsave("plots/map_avgage.png", width = 10, height = 6)
