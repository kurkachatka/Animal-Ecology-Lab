
# potrzebne pakiety

library(tidyverse)
library(readxl)
library(gridExtra)


# ustaw WD

setwd("C:/Users/marta/OneDrive - University of Gdansk/Dydaktyka/Ekologia zwierz¹t/Animal-Ecology-Lab")

# wgraj i sprawdz dane


bhv_vid <- read_excel('BehaveDB_vid.xlsx')
bhv_real <- read_excel('BehaveDB_real.xlsx')

str(bhv_vid)
str(bhv_real)

summary(bhv)


# wybierz odpowiedni rok

bhv_vid <- bhv_vid %>% filter(Year == '26_27')


# zmien skale

bhv_vid$Behav <- factor(bhv_vid$Behav,
                   levels = c("FOOD", "OOF", "AGR_M", "AGR_F", 'SUB_M',
                              'SUB_F'))



# zlicz zachowania


bhv.count_vid <- bhv_vid %>% 
  count(ID, Behav, Method) %>% 
  pivot_wider(names_from = Behav,
              values_from = n,
              values_fill = 0) %>% 
  mutate(Sex = substr(ID, 1, 1)) %>% 
  arrange(ID)


# wyciagnij ostatnia klatke
# zmien na minuty: 20klatek - 1 s

bhv.oof <- bhv_vid %>% group_by(ID) %>% 
  summarise(Last_Frame = max(Frame)) %>% 
  mutate(Obsrv_time = (Last_Frame/20)/60)


# zlacz 

bhv.count_vid <- bhv.count_vid %>% left_join(bhv.oof, by = 'ID')


# wylicz proporcje i sumy dla wideo


bhv.count_vid <- bhv.count_vid %>%
  select(-OOF) %>% 
  mutate(prop_FOOD = FOOD/Obsrv_time,
         prop_AGR_M = AGR_M/Obsrv_time,
         prop_AGR_F = AGR_F/Obsrv_time,
         prop_SUB_M = SUB_M/Obsrv_time,
         prop_SUB_F = SUB_F/Obsrv_time,
         sum_AGR = (AGR_M + AGR_F)/Obsrv_time,
         sum_SUB = (SUB_M + SUB_F)/Obsrv_time,
         sum_BEH = FOOD + AGR_M + AGR_F + SUB_M + SUB_F) %>% 
  filter(sum_BEH != 0)


# wylicz proporcje i sumy dla obserwacji w realu

bhv.count_real <- bhv_real %>%
  mutate(prop_FOOD = FOOD/Obsrv_time,
         prop_AGR_M = AGR_M/Obsrv_time,
         prop_AGR_F = AGR_F/Obsrv_time,
         prop_SUB_M = SUB_M/Obsrv_time,
         prop_SUB_F = SUB_F/Obsrv_time,
         sum_AGR = (AGR_M + AGR_F)/Obsrv_time,
         sum_SUB = (SUB_M + SUB_F)/Obsrv_time,
         sum_BEH = FOOD + AGR_M + AGR_F + SUB_M + SUB_F) %>% 
  filter(sum_BEH != 0)


# zlacz obie ramki danych


bhv <- bind_rows(bhv.count_vid, bhv.count_real)




# porowanie dwoch plci 

pb1 <- bhv %>% ggplot(aes(x = Sex, y = sum_AGR)) +
  geom_boxplot(aes(fill = Sex), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(y = 'Suma zachowañ agresywnych', x = 'P³eæ') +
  theme_classic() +
  facet_wrap(~Method)

pb2 <- bhv %>% ggplot(aes(x = Sex, y = sum_SUB)) +
  geom_boxplot(aes(fill = Sex), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(y = 'Suma zachowañ uleg³ych', x = 'P³eæ') +
  theme_classic() +
  facet_wrap(~Method)

grid.arrange(pb1, pb2, nrow = 1)

# porownanie plci z wideo

wilcox.test(data = bhv.count_vid, sum_AGR ~ Sex)
wilcox.test(data = bhv.count_vid, sum_SUB ~ Sex)

# porownanie plci z real

wilcox.test(data = bhv.count_real, sum_AGR ~ Sex)
wilcox.test(data = bhv.count_real, sum_SUB ~ Sex)


# porownanie wideo i real

wilcox.test(data = bhv, sum_AGR ~ Method)
wilcox.test(data = bhv, sum_SUB ~ Method)





pf1 <- bhv %>% ggplot(aes(x = Sex, y = FOOD)) +
  geom_boxplot(aes(fill = Sex), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 300)) +
  labs(y = 'Liczba aktów pobierania pokarmu', x = 'P³eæ') +
  theme_classic() +
  facet_wrap(~Method)



pf2 <- bhv %>% ggplot(aes(x = sum_AGR, y = prop_FOOD)) +
  geom_point(aes(color = Sex), size = 4, alpha = 0.7) +
  geom_smooth(method = 'lm', color = 'black', linewidth = 1.5) + 
  labs(y = 'Liczba aktów pobierania pokarmu', x = 'Suma zachowañ agresywnych') +
  theme_classic() +
  facet_wrap(~Method)


grid.arrange(pf1, pf2, ncol = 2)

# analiza dla wideo

wilcox.test(data = bhv.count_vid, FOOD ~ Sex)
cor.test(bhv.count_vid$FOOD, bhv.count_vid$sum_AGR)

# analiza dla real

wilcox.test(data = bhv.count_real, FOOD ~ Sex)
cor.test(bhv.count_real$FOOD, bhv.count_real$sum_AGR)

# porownanie wideo i real

wilcox.test(data = bhv, FOOD ~ Method)

