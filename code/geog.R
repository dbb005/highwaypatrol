# Read In Data ------------------------------------------------------------
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
          aes(fill = area))