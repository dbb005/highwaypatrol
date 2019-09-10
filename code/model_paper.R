library(tidyverse)
library(dplyr)
library(broom)
library(readr)

###
df <- read_csv("data/data.csv")
df_skinny <- df %>% 
  mutate(male=if_else(gender>1, 1, 0, missing = NULL)) %>% 
  mutate(white=if_else(race<1, 1, 0, missing = NULL)) %>% 
  mutate(tenure=(2019-joinyear)) %>% 
  select(-(gender:race),-(joinyear)) %>% 
  select(-(email),-(lastname),-(firstname),-(supname))
#  select(lastname:selfevaluation12,psm5:psm3) %>% 
#  mutate(psm=(psm1+psm2+psm3+psm4+psm5)/5) %>% 
#  mutate_at(vars(selfevaluation2,selfevaluation4,selfevaluation6,
#                 selfevaluation8,selfevaluation10,selfevaluation12),
#            funs(abs(.-6))) %>% 
#  mutate_at(vars(socialdominance3,socialdominance7),
#            funs(abs(.-6))) %>% 
#  mutate(selfevaluation=(selfevaluation2+selfevaluation4+selfevaluation6+
#    selfevaluation8+selfevaluation10+selfevaluation12+
#    selfevaluation1+selfevaluation3+selfevaluation5+
#    selfevaluation7+selfevaluation9+selfevaluation11)/12) %>% 
#  mutate(dom=(socialdominance1+socialdominance2+socialdominance3)/3) %>% 
#  mutate(antiegal=(socialdominance5+socialdominance6+socialdominance7)/3) %>% 
#  select(-c(18:45)) %>% 


# Tidy Up Data ------------------------------------------------------------
sgt <- df_skinny %>% filter(rank=="Sergeant") %>% 
  setNames(paste0('sgt_', names(.))) %>% 
  rename(sup_uniqueid=sgt_supid) %>% 
  select(1:7, 183:185, everything())
lts <- df_skinny %>% filter(rank=="Lieutenant") %>% 
  setNames(paste0('lt_', names(.))) %>% 
  rename(stlt_uniqueid=lt_supid) %>% 
  select(-c(204:317)) %>% 
  select(1:10, 183:185, 11:103)
stlt <- df_skinny %>% filter(rank=="Staff Lieutenant") %>% 
  setNames(paste0('stlt_', names(.))) %>% 
  select(-c(204:317)) %>% 
  select(-c(2:6)) %>% 
  select(1:2, 178:180, 3:98)
 
  

lts_join <- left_join(lts, stlt, by = ("stlt_uniqueid")) %>% 
  rename(sup_uniqueid=lt_uniqueid)
#sgt_join <- left_join(sgt, lts_join, by = ("lt_uniqueid")) %>%   
#  select(-(sgt_supname:sgt_supid),-(lt_supname:lt_supid),-(stlt_supname:stlt_supid))

write_csv(sgt, "data/sgt.csv")
write_csv(lts_join, "data/supervisors.csv")


# example -----------------------------------------------------------------
trooperclean <- ntrpr %>%
  select(lastname:joinyear) %>%
  mutate(trprID=uniqueid, 
         sgtID=supid) %>%
  select(trprID, sgtID,lastname,firstname,rank:joinyear)

sgtclean <- new_nsgts %>%
  mutate(sgtID=uniqueid) %>%
  select(-uniqueid)

join1 <- left_join(trooperclean, sgtclean, by=("sgtID"))


# Old ---------------------------------------------------------------------
nsgts <- subset(database_s1s2allenforcement, rank=="Sergeant", select=c(lastname:minority_employee_preferential)) 
nlts <- subset(database_s1s2allenforcement, rank=="Lieutenant", select=c(lastname:minority_employee_preferential)) 
nstlts <- subset(database_s1s2allenforcement, rank=="Staff Lieutenant", select=c(lastname:minority_employee_preferential)) 
ntrpr <- subset(database_s1s2allenforcement, rank=="Trooper", select=c(lastname:minority_employee_preferential)) 
write.csv(nsgts,"C:\\Users\\baker.2442\\Documents\\OSHP\\nsgts.csv")
write.csv(nlts,"C:\\Users\\baker.2442\\Documents\\OSHP\\nlts.csv")
write.csv(nstlts,"C:\\Users\\baker.2442\\Documents\\OSHP\\nstlts.csv")
write.csv(ntrpr,"C:\\Users\\baker.2442\\Documents\\OSHP\\ntrpr.csv")

merge_1 <- merge(ntrpr, new_nsgts, by.x="supid", by.y="uniqueid")
merge_2 <- merge(merge_1, new_nlts, by.x="supid.y", by.y="uniqueid")
merge_3 <- merge(merge_2, new_nstlts, by.x="supid.y.y", by.y="uniqueid")

