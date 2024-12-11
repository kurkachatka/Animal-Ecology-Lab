# 
# install.packages('tidyverse')
# install.packages('readxl')
# install.packages('gridExtra')

library(tidyverse)
library(readxl)
library(gridExtra)

# wczytaj plik
hier <- read_excel('Hier_MainDB.xlsx')


#sumowanie agresji

hier <- hier %>% mutate(AGR = AGR_F + AGR_M)

# obliczanie % zachowan uleglych


hier <- hier %>% mutate(ULE = ULE_F + ULE_M) %>% 
  mutate(ALL = ULE+AGR) %>% 
  mutate(procULE = ULE/ALL)


# obliczanie przecietnej pozycji (na podstawie mediany)

hier <- hier %>% mutate(POZ = apply(select(., POZ1, POZ2, POZ3, POZ4, POZ5),
                   1, median, na.rm = TRUE))

hier$POZ <-  as.integer(hier$POZ) # zmiana na liczby calkowite


# Porownanie plci pod wzgledem liczby zachowan agresywnych

hier %>% 
  filter(!is.na(Plec)) %>% # odfiltrowanie obserwacji bez plci
  ggplot(aes(x = Plec, y = AGR)) +
  geom_boxplot() +
  theme_classic()



# Porownanie plci pod wzgledem procentu zachowan uleglych


hier %>% 
  filter(!is.na(Plec)) %>% # odfiltrowanie obserwacji bez plci
  ggplot(aes(x = Plec, y = procULE)) +
  geom_boxplot() +
  theme_classic()


# to samo, z uwzglednieniem plci odbiorcy


f1 <- hier %>% 
  filter(!is.na(Plec)) %>% # odfiltrowanie obserwacji bez plci
  ggplot(aes(x = Plec, y = AGR_M)) +
  geom_boxplot() +
  ylim(-1, 30) +
  theme_classic()

f2 <- hier %>% 
  filter(!is.na(Plec)) %>% # odfiltrowanie obserwacji bez plci
  ggplot(aes(x = Plec, y = AGR_F)) +
  geom_boxplot() +
  ylim(-1, 30) +
  theme_classic()

grid.arrange(f1, f2, ncol = 2)


# podsumowanie statystyk opisowych

podsum1 <- hier %>% 
  filter(!is.na(Plec)) %>% 
  group_by(Plec) %>% 
  summarise(mediana.AGR = median(AGR, na.rm = T),
            roz.kwar.AGR = IQR(AGR, na.rm = T),
            mediana.AGR_M = median(AGR_M, na.rm = T),
            roz.kwar.AGR_M = IQR(AGR_M, na.rm = T),
            mediana.AGR_F = median(AGR_F, na.rm = T),
            roz.kwar.AGR_F = IQR(AGR_F, na.rm = T),
            mediana.procULE = median(procULE, na.rm = T),
            roz.kwar.procULE = IQR(procULE, na.rm = T))


View(podsum1)


# test Wilcoxona

wilcox.test(data=hier, AGR ~ Plec)
wilcox.test(data=hier, AGR_M ~ Plec)
wilcox.test(data=hier, AGR_F ~ Plec)
wilcox.test(data=hier, procULE ~ Plec)



# agresja a jedzenie

hier %>% 
  ggplot(aes(y = ZJAD, x = AGR)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()


cor.test(hier$ZJAD, hier$AGR, method = 'spearman')



hier %>% 
  filter(!is.na(Proba)) %>% 
  ggplot(aes(y = AGR, x = Proba)) +
  geom_boxplot() +
  theme_classic()



# Pozycja a agresja


hier %>% 
  filter(!is.na(POZ)) %>% 
  ggplot(aes(x = as.factor(POZ), y = AGR)) +
  geom_boxplot() +
  theme_classic()


kruskal.test(data = hier, AGR~as.factor(POZ))

