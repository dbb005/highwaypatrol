library(tidyverse)
library(ggplot2)
library(tidyverse)
library(tigris)
library(maptools)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
###

response <- read_csv("response.csv")
oshp <- read_csv("data/OSHP.csv")
avg <- read_csv("data/database_deptaverages.csv")
postids <- oshp %>% filter(POST>0) %>% select(POST) %>% pull()
df <- avg %>% filter(POST %in% postids) %>% write_csv("data/df.csv")

# Mappage -----------------------------------------------------------------

oh.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Ohio")
oh.ctys <- counties(cb = TRUE, state = "oh") %>%
  mutate(FIPS = as.numeric(paste0(STATEFP,COUNTYFP)))

ggplot()+
  geom_sf(data=oh.ctys)
districts <- oshp %>%
  select(-STATE,-(POST:GHQ)) %>%
  full_join(.,response, by = "DISTRICT") %>%
  select(-RESP_RATE) %>%
  mutate(RATE = RESP/INDIV) %>%
  full_join(.,oh.ctys, BY = "FIPS") %>%
  distinct()
