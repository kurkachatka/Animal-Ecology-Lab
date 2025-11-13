library(readxl)
library(tidyverse)
setwd("C:/Users/marta/OneDrive - University of Gdansk/Dydaktyka/Ekologia zwierząt/Animal-Ecology-Lab")


div.data <- read_excel('DiversityDB1.xlsx', sheet = 'Census')

str(div.data)


# przygotowanie danych ze zliczonymi gatunkami

n_spec <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_spec=n(),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))

n_spec <- as.data.frame(n_spec)


n_spec %>% 
  ggplot(aes(y = N_spec, x = VEGETATION)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_spec %>% 
  ggplot(aes(y = N_spec, x = WATER)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_spec %>% 
  ggplot(aes(y = N_spec, x = SEALED)) +
  geom_point() +
  geom_smooth(method = 'lm')


# przygotowanie danych ze zliczonymi rodzinami

n_fam <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_fam=n_distinct(FAMILY),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))

n_fam <- as.data.frame(n_fam)


n_fam %>% ggplot(aes(N_fam, x = VEGETATION)) +
  geom_point() +
  geom_smooth(method = 'lm')

n_fam %>% ggplot(aes(N_fam, x = WATER)) +
  geom_point() +
  geom_smooth(method = 'lm')

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

n_fam %>% ggplot(aes(N_fam, x = SEALED)) +
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









