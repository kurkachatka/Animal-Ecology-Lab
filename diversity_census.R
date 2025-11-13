library(readxl)
library(tidyverse)
library(gridExtra)
setwd("C:/Users/marta/OneDrive - University of Gdansk/Dydaktyka/Ekologia zwierząt/Animal-Ecology-Lab")


div.data1 <- read_excel('DiversityDB1.xlsx', sheet = 'Census')
div.data2 <- read_excel('DiversityDB2.xlsx', sheet = 'Census')




div.data1 <- div.data1 %>% mutate(VEGETATION = VEGETATION/100,
                                WATER = WATER/100,
                                SEALED = SEALED/100)


div.data <-  bind_rows(div.data1, div.data2)


# przygotowanie danych ze zliczonymi gatunkami

# stwórz dane zliczające gatunki dla kazdego punktu
n_spec <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_spec=n_distinct(SPECIES),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))

n_spec <- as.data.frame(n_spec)


# Wizualizacja

spec_plot1 <- n_spec %>% 
  ggplot(aes(y = N_spec, x = VEGETATION)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'red') +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba gatunków', x = 'Indeks roślinności',
       tag = 'A')


spec_plot2 <- n_spec %>% 
  ggplot(aes(y = N_spec, x = WATER)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
  geom_smooth(method = 'lm', color = 'red') +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba gatunków', x = 'Indeks dostępności wody',
       tag = 'B')


spec_plot3 <- n_spec %>% 
  ggplot(aes(y = N_spec, x = SEALED)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'red') +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba gatunków', x = 'Indeks powierzchni uszczelnionej',
       tag = 'C')


grid.arrange(spec_plot1, spec_plot2, spec_plot3, ncol = 3)


# analiza korelacji Spearmana (ze wzgledu na brak normalnosci rozkladu zmiennych)

cor.test(n_spec$N_spec, n_spec$VEGETATION, method = 'spearman')

cor.test(n_spec$N_spec, n_spec$WATER, method = 'spearman')

cor.test(n_spec$N_spec, n_spec$SEALED, method = 'spearman')



# przygotowanie danych ze zliczonymi rodzinami



n_fam <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_fam=n_distinct(FAMILY),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))

n_fam <- as.data.frame(n_fam)

# Wizualizacja

fam_plot1 <- n_fam %>% ggplot(aes(N_fam, x = VEGETATION)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'seagreen', size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rodzin', x = 'Indeks roślinności',
       tag = 'A')

fam_plot2 <- n_fam %>% ggplot(aes(N_fam, x = WATER)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'seagreen', size = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rodzin', x = 'Indeks dostępności wody',
       tag = 'B')
n_fam %>% ggplot(aes(N_fam, x = SEALED)) +
  geom_point() +
  geom_smooth(method = 'lm')


# przygotowanie danych ze zliczonymi rzędami

n_ord <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_ord=n_distinct(ORDER),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_ord <- as.data.frame(n_ord)


n_ord %>% ggplot(aes(N_ord, x = VEGETATION)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_ord %>% ggplot(aes(N_ord, x = WATER)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_ord %>% ggplot(aes(N_ord, x = SEALED)) +
  geom_point() +
  geom_smooth(method = 'lm')



# przygotowanie danych ze zliczonymi grupami pokarmowymi

n_food <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_food = n_distinct(FOOD),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_food <- as.data.frame(n_food)


n_food %>% ggplot(aes(N_food, x = VEGETATION)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_food %>% ggplot(aes(N_food, x = WATER)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_food %>% ggplot(aes(N_food, x = SEALED)) +
  geom_point() +
  geom_smooth(method = 'lm')



# przygotowanie danych ze zliczonymi grupami behawioralnymi

n_behav <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_behav = n_distinct(BEHAV),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_behav <- as.data.frame(n_behav)


n_behav %>% ggplot(aes(N_behav, x = VEGETATION)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_behav %>% ggplot(aes(N_behav, x = WATER)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_behav %>% ggplot(aes(N_behav, x = SEALED)) +
  geom_point() +
  geom_smooth(method = 'lm')



# porowanie grup takosnomicznych

unique(div.data$ORDER)

div.data %>% 
  ggplot(aes(x = ORDER, y = VEGETATION)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = ORDER, y = WATER)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = ORDER, y = SEALED)) +
  geom_boxplot()


# porowanie grup pokarmowych

unique(div.data$FOOD)

div.data %>% 
  ggplot(aes(x = FOOD, y = VEGETATION)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = FOOD, y = WATER)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = FOOD, y = SEALED)) +
  geom_boxplot()


# porowanie grup behavioralnych

unique(div.data$BEHAV)

div.data %>% 
  ggplot(aes(x = BEHAV, y = VEGETATION)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = BEHAV, y = WATER)) +
  geom_boxplot()

div.data %>% 
  ggplot(aes(x = BEHAV, y = SEALED)) +
  geom_boxplot()









