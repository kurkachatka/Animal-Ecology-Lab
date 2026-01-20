
library(tidyverse)
library(readxl)
library(gridExtra)

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

#diagnostic plots

di1 <- bhv.count %>% ggplot(aes(x = Last_Frame, y = FOOD)) +
  geom_smooth(method = 'lm') +
  theme_classic()

di2 <- bhv.count %>% ggplot(aes(x = Last_Frame, y = AGR_M)) +
  geom_smooth(method = 'lm') +
  theme_classic()

di3 <- bhv.count %>% ggplot(aes(x = Last_Frame, y = AGR_F)) +
  geom_smooth(method = 'lm') +
  theme_classic()

di4 <- bhv.count %>% ggplot(aes(x = Last_Frame, y = SUB_M)) +
  geom_smooth(method = 'lm') +
  theme_classic()

di5 <- bhv.count %>% ggplot(aes(x = Last_Frame, y = SUB_F)) +
  geom_smooth(method = 'lm') +
  theme_classic()

grid.arrange(di1, di2, di3,
             di4, di5)


bhv.count <- bhv.count %>%
  left_join(bhv.oof, by = 'ID') %>% 
  select(-OOF) %>% 
  mutate(prop_FOOD = FOOD/Last_Frame,
         prop_AGR_M = AGR_M/Last_Frame,
         prop_AGR_F = AGR_F/Last_Frame,
         prop_SUB_M = SUB_M/Last_Frame,
         prop_SUB_F = SUB_F/Last_Frame,
         sum_AGR = (AGR_M + AGR_F)/Last_Frame,
         sum_SUB = (SUB_M + SUB_F)/Last_Frame,
         sum_BEH = FOOD + AGR_M + AGR_F + SUB_M + SUB_F) %>% 
  filter(sum_BEH != 0)





# porowanie dwoch plci 

pb1 <- bhv.count %>% ggplot(aes(x = Sex, y = sum_AGR)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 0.0035)) +
  theme_classic()

pb2 <- bhv.count %>% ggplot(aes(x = Sex, y = sum_SUB)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 0.0035)) +
  theme_classic()

grid.arrange(pb1, pb2, nrow = 1)

wilcox.test(data = bhv.count, sum_AGR ~ Sex)
wilcox.test(data = bhv.count, sum_SUB ~ Sex)


bhv.count %>% ggplot(aes(x = Sex, y = FOOD)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 45)) +
  theme_classic()

wilcox.test(data = bhv.count, FOOD ~ Sex)

bhv.count %>% ggplot(aes(x = sum_AGR, y = prop_FOOD)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  theme_classic()


cor.test(bhv.count$FOOD, bhv.count$sum_AGR)


