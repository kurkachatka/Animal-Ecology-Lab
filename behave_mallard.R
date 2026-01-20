
library(tidyverse)
library(readxl)

setwd("C:/Users/marta/OneDrive - University of Gdansk/Dydaktyka/Ekologia zwierz¹t/Animal-Ecology-Lab")


bhv <- read_excel('BehaveDB.xlsx')

str(bhv)

summary(bhv)

bhv$Behav <- factor(bhv$Behav,
                   levels = c("FOOD", "OOF", "AGR_M", "AGR_F", 'SUB_M',
                              'SUB_F'))


bhv.count <- bhv %>% 
  count(ID, Behav) %>% 
  pivot_wider(names_from = Behav,
              values_from = n,
              values_fill = 0) %>% 
  mutate(Sex = substr(ID, 1, 1))



bhv.oof <- bhv %>% group_by(ID) %>% 
  summarise(Last_Frame = max(Frame))


bhv.count <- bhv.count %>%
  left_join(bhv.oof, by = 'ID') %>% 
  select(-OOF) %>% 
  mutate(prop_FOOD = FOOD/Last_Frame,
         prop_AGR_M = AGR_M/Last_Frame,
         prop_AGR_F = AGR_F/Last_Frame,
         prop_SUB_M = SUB_M/Last_Frame,
         prop_SUB_F = SUB_F/Last_Frame,
         sum_AGR = (AGR_M + AGR_F)/Last_Frame,
         sum_SUB = (SUB_M + SUB_F)/Last_Frame)



bhv.count %>% filter(sum_AGR != 0) %>% ggplot(aes(x = Sex, y = sum_AGR)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 0.0025))
  theme_classic()

bhv.count %>% ggplot(aes(x = Sex, y = sum_SUB)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 0.0025)) +
  theme_classic()

