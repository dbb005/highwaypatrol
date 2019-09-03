install.packages(devtools)
install.packages("lwgeom")
devtools::install_github("andrewvanleuven/rleuven")
library(lwgeom)
library(devtools)
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
avg <- read_csv("data/averages_new.csv")

# Merges ------------------------------------------------------------------
postids <- oshp %>% filter(POST>0) %>% select(POST) %>% pull()
df <- avg %>% filter(POST %in% postids) %>% write_csv("data/df.csv")
df_ntile <- df %>%
  mutate_at(.funs = list(ntile = ~ntile(., 10)), .vars = vars(2:54)) %>% 
  select(-(avgage:report_force_agree))
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

write_csv(df, "")
# Maps --------------------------------------------------------------------
ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgage_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(33)","", "50th","", "99th\n(44)")) +
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
  ggsave("plots/map_avgage1.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgtenure_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(8.7)","", "50th","", "99th\n(17.7)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Average Tenure Across OSHP Posts") + #,subtitle = "") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Average Tenure\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_avgtenure.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgwhite_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(33)","", "50th","", "99th\n(100)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Percent of White Respondents Across OSHP Posts") +
  #subtitle = "'Meaningful public service is very important to me.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Average White\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_avgwhite.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = orgfair2agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(50%")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Organizational Fairness Across OSHP Posts",
          subtitle = "'All employees are treated the same regardless of their ethnicity, gender, race or religion.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Fair Treatment Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreeorgfair2.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = psm1agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(33%)","", "50th","", "99th\n(95%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Public Service Motivation Across OSHP Posts",
          subtitle = "'Meaningful public service is very important to me.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Average PSM Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreepsm1.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = diversityclimate1agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(33%)","", "50th","", "99th\n(100%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Diversity Climate Across OSHP Posts",
          subtitle = "'Highway Patrol welcomes employees of different races and ethnicities.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Diversity Climate Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreedivclim1.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = diversityclimate3agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(25%)","", "50th","", "99th\n(92%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Work Environment for Women Across OSHP Posts",
          subtitle = "'Highway Patrol fosters a positive work climate for women employees.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Work Climate Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreedivclim3.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = woman_not_suitableagree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(20%)","", "50th","", "99th\n(82%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Female Officers Across OSHP Posts",
          subtitle = "'Law enforcement is NOT a suitable occupation for women.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Women NOT Suitable Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreewomenofficers1.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = burnout_1agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(38%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Officer Burnout Across OSHP Posts",
          subtitle = "'How would you classify your level of burnout?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Officer Burnout Percentile\n\n(% Frustrated or Completely Burned Out)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreeburnout.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = morale_3agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(53%)","", "50th","", "99th\n(67%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Officer Morale Across OSHP Posts",
          subtitle = "'I feel inspired about my work.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Officer Morale Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreemorale.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = morale_4agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(26%)","", "50th","", "99th\n(85%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Officer Morale Across OSHP Posts",
          subtitle = "'I feel proud of the work that I do.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Officer Morale Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreemorale4.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = career_satisfactionagree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(11%)","", "50th","", "99th\n(85%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Officer Job Satisfaction Across OSHP Posts",
          subtitle = "'All in all, how satisfied are you with your job?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Job Satisfaction Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreesatisfaction.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = arrestpressureagree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(42%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Pressure to Make Arrests Across OSHP Posts",
          subtitle = "'How much pressure is there in your unit to make arrests\n in order to keep a good standing?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Arrest Pressure Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreearrestpressure.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = contactpressureagree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(33%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Pressure for Citations Across OSHP Posts",
          subtitle = "'How much pressure is there in your unit to keep up the\ncount of citations and contact cards?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Citation Pressure Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreecontactpressure.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgphysicalharm_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(2.4)","", "50th","", "99th\n(4.4)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Exposure to Physical Harm Across OSHP Posts",
          subtitle = "'How often does your job expose you to the threat of physical harm or injury?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Physical Harm\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_physicalharm.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgobserved_discrimated_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0)","", "50th","", "99th\n(0.44)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Observing Discrimination Across OSHP Posts",
          subtitle = "'In your unit, have you observed any employee being discriminated\nbecause of their ethnicity, gender, race, religion or sexual orientation?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Discrimination in\nPostsPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_obsdiscrim.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgyou_discrimated_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0)","", "50th","", "99th\n(0.5)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Feeling Discrimination Across OSHP Posts",
          subtitle = "'While working in your unit, have you ever felt discriminated\nbecause of your ethnicity, gender, race, religion, or sexual orientation?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Felt Discrimination\nPostsPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_youdiscrim.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgdobserved_excluded_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0)","", "50th","", "99th\n(0.44)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Observing Exclusion Across OSHP Posts",
          subtitle = "'In your unit, have you ever observed exclusion because of your\nethnicity, gender, race, religion, or sexual orientation?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Exclusion\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_obsexcl.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgyou_excluded_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(67%)","", "50th","", "99th\n(100%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Feeling Exclusion Across OSHP Posts",
          subtitle = "'In your unit, have you ever felt excluded because of your\nethnicity, gender, race, religion, or sexual orientation?'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Experienced Exclusion Percentile\n\n(% Experienced Exclusion)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_youexcluded.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = force_policy_3agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(41%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Use of Force Across OSHP Posts",
          subtitle = "'We are not permitted to use as much force as is often necessary.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Use of Force Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_agreeforcepoli3.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = avgintfair6_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0.17)","", "50th","", "99th\n(0.43)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Supervisor Fairness Across OSHP Posts",
          subtitle = "'Your Supervisor treats you the same way he or she treats other employees.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Supervisor Fairness\nPercentile") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_intfair6.png", width = 9, height = 9)

#### NEW ONES HERE ####
ggplot() +
  geom_sf(data = post_sf,
          aes(fill = orgfair5agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(0%)","", "50th","", "99th\n(50%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Fair Promotion Processes Across OSHP Posts",
          subtitle = "'Promotion decisions are NOT influenced by employee race, ethnicity, gender, or religion.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Fairness of Promotions Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_orgfair5agree.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = lt_care_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(7%)","", "50th","", "99th\n(80%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Lieutenant Support Across OSHP Posts ",
          subtitle = "'The Lieutenant really cares about my personal well-being.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Lt Support Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_ltcare.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = lt_proud_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(7%)","", "50th","", "99th\n(71%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Lieutenant Support Across OSHP Posts",
          subtitle = "'The Lieutenant takes pride in my accomplishments at work.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Lt Support Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_ltproud.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = minority_pref_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(11%)","", "50th","", "99th\n(77%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Preferential Treatment for Minority Employees Across OSHP Posts",
          subtitle = "'In OHP, minority employees often get preferential treatment over nonminority employees.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Minorities: Pref. Treatment Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_minoritypref.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = female_pref_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(6%)","", "50th","", "99th\n(71%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Preferential Treatment for Female Employees Across OSHP Posts",
          subtitle = "'In OHP, female employees often get preferential treatment over male employees.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Female: Pref. Treatment Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_femalepref.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = work_helps_comm_2agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(20%)","", "50th","", "99th\n(85%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Perceptions of Work Helping the Community Across OSHP Posts",
          subtitle = "'What I do at work makes a big difference in the community.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Work Helps Community Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_work_helps_comm_2agree.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = proc_polic_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(30%)","", "50th","", "99th\n(92%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Fair Treatment of Citizens Across OSHP Posts",
          subtitle = "'I feel I have a duty to treat everyone the same way when I stop them on the road.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Treating Citizens Fairly Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_proc_polic_agree.png", width = 9, height = 9)

ggplot() +
  geom_sf(data = post_sf,
          aes(fill = report_force_agree_ntile),
          color = "black") +
  scale_fill_gradientn(breaks=c(1, 2.5, 5.0, 7.5, 9.9), 
                       colors=c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f"),
                       labels=c("1st\n(19%)","", "50th","", "99th\n(85%)")) +
  #http://colorbrewer2.org/#type=sequential&scheme=Purples&n=7
  ggtitle("Willingness to Report Mistreatment Across OSHP Posts",
          subtitle = "'If I observed a coworker being disrespectful to public, I would notify my superiors.'") +
  ggrepel::geom_label_repel(data = post_sf,
                            aes(x = x, y = y, label = postname)) +
  labs(fill = "Report Disrespect Percentile\n\n(% Agree/Strongly Agree)") +
  theme_void() +
  theme(legend.position='bottom',
        plot.title = element_text(face="bold", size = 20, hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        legend.spacing.x = unit(1.0, 'cm')) +
  ggsave("plots/map_report_force_agree.png", width = 9, height = 9)

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
