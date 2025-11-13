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



