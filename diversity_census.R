library(readxl)
library(tidyverse)
library(gridExtra)
library(FSA)

# --------------- Zaplecze ------------------

setwd("C:/Users/marta/OneDrive - University of Gdansk/Dydaktyka/Ekologia zwierz¹t/Animal-Ecology-Lab")


div.data <- read_excel('DiversityDB1.xlsx', sheet = 'Census')



# ------------- ZwiÄ…zek biorÃ³Å¼norodnoÅ›ci i gradientu zurbanizowania --------------


# przygotowanie danych ze zliczonymi gatunkami dla kaÅ¼dego punktu

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
  geom_smooth(method = 'lm', color = 'red4', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba gatunków', x = 'Indeks roœlinnoœci',
       tag = 'A1')


# spec_plot2 <- n_spec %>% 
#   ggplot(aes(y = N_spec, x = WATER)) +
#   geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
#   geom_smooth(method = 'lm', color = 'red4', linewidth = 2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Liczba gatunkÃ³w', x = 'Indeks dostêpnoœci wody',
#        tag = 'B')


spec_plot3 <- n_spec %>% 
  ggplot(aes(y = N_spec, x = SEALED)) +
  geom_point(size = 6,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'red4', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba gatunków', x = 'Indeks powierzchni uszczelnionej',
       tag = 'A2')


spec_plot <- grid.arrange(spec_plot1, 
             #spec_plot2, 
             spec_plot3, ncol = 2)


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
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'seagreen', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rodzin', x = 'Indeks roœlinnoœci',
       tag = 'B1')

# fam_plot2 <- n_fam %>% ggplot(aes(N_fam, x = WATER)) +
#   geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
#   geom_smooth(method = 'lm', color = 'seagreen', size = 2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Liczba rodzin', x = 'Indeks dostêpnoœci wody',
#        tag = 'B')


fam_plot3 <- n_fam %>% ggplot(aes(N_fam, x = SEALED)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'seagreen', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rodzin', x = 'Indeks powierzchni uszczelnionej',
       tag = 'B2')


fam_plot <- grid.arrange(fam_plot1, 
                         #fam_plot2,
                         fam_plot3, ncol = 2)


# przygotowanie danych ze zliczonymi rzÄ™dami

n_ord <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_ord=n_distinct(ORDER),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_ord <- as.data.frame(n_ord)

# Wizualizacja

ord_plot1 <- n_ord %>% ggplot(aes(N_ord, x = VEGETATION)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'royalblue', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rzêdów', x = 'Indeks roœlinnoœci',
       tag = 'C1')


# ord_plot2 <- n_ord %>% ggplot(aes(N_ord, x = WATER)) +
#   geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
#   geom_smooth(method = 'lm', color = 'royalblue', size = 2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Liczba rzÄ™dow', x = 'Indeks dostêpnoœci wody',
#        tag = 'B')

ord_plot3 <- n_ord %>% ggplot(aes(N_ord, x = SEALED)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'royalblue', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba rzêdów', x = 'Indeks powierzchni uszczelnionej',
       tag = 'C2')

ord_plot <- grid.arrange(ord_plot1,
                         #ord_plot2,
                         ord_plot3, ncol = 2)

# wspó³ny wykres

grid.arrange(spec_plot,
             fam_plot,
             ord_plot, nrow = 2)



# analiza korelacji Spearmana (ze wzgledu na brak normalnosci rozkladu zmiennych)

cor.test(n_spec$N_spec, n_spec$VEGETATION, method = 'spearman')


#cor.test(n_spec$N_spec, n_spec$WATER, method = 'spearman')

cor.test(n_spec$N_spec, n_spec$SEALED, method = 'spearman')



# analiza korelacji Spearmana

cor.test(n_fam$N_fam, n_fam$VEGETATION, method = 'spearman')

# cor.test(n_fam$N_fam, n_fam$WATER, method = 'spearman')

cor.test(n_fam$N_fam, n_fam$SEALED, method = 'spearman')




# Korelacje Spearmana

cor.test(n_ord$N_ord, n_ord$VEGETATION, method = 'spearman')

# cor.test(n_ord$N_ord, n_ord$WATER, method = 'spearman')

cor.test(n_ord$N_ord, n_ord$SEALED, method = 'spearman')


# Bioroznorodnosc funckjonalna

# przygotowanie danych ze zliczonymi grupami pokarmowymi

n_food <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_food = n_distinct(FOOD),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_food <- as.data.frame(n_food)



# Wizualizacja

food_plot1 <- n_food %>% ggplot(aes(N_food, x = VEGETATION)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'goldenrod2', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba grup pokarmowych', x = 'Indeks roœlinnoœci',
       tag = 'A1')

# food_plot2 <- n_food %>% ggplot(aes(N_food, x = WATER)) +
#   geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
#   geom_smooth(method = 'lm', color = 'goldenrod2', size = 2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Liczba grup pokarmowych', x = 'Indeks dostêpnoœci wody',
#        tag = 'B')

food_plot3 <- n_food %>% ggplot(aes(N_food, x = SEALED)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'goldenrod2', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba grup pokarmowych', x = 'Indeks powierzchni uszczelnionej',
       tag = 'A2')


food_plot <- grid.arrange(food_plot1,
             # food_plot2,
             food_plot3, ncol = 2)



# przygotowanie danych ze zliczonymi grupami behawioralnymi

n_behav <- div.data %>% 
  group_by(GRUP, POINT) %>% 
  summarise(N_behav = n_distinct(BEHAV),
            VEGETATION = mean(VEGETATION),
            WATER = mean(WATER),
            SEALED = mean(SEALED))


n_behav <- as.data.frame(n_behav)

# Wizualizacja


behav_plot1 <- n_behav %>% ggplot(aes(N_behav, x = VEGETATION)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'deeppink4', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba grup behawioralnych', x = 'Indeks roœlinnoœci',
       tag = 'B1')

# behav_plot2 <- n_behav %>% ggplot(aes(N_behav, x = WATER)) +
#   geom_point(size = 6,alpha = 0.7, position = position_jitter(width = 0.01)) +
#   geom_smooth(method = 'lm', color = 'deeppink4', linewdth = 2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Liczba grup behawioralnych', x = 'Indeks dostêpnoœci wody',
#        tag = 'B')

behav_plot3 <- n_behav %>% ggplot(aes(N_behav, x = SEALED)) +
  geom_point(size = 4,alpha = 0.7, position = position_jitter()) +
  geom_smooth(method = 'lm', color = 'deeppink4', linewidth = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Liczba grup behawioralnych', x = 'Indeks powierzchni uszczelnionej',
       tag = 'B2')

behav_plot <- grid.arrange(behav_plot1,
             #behav_plot2,
             behav_plot3, ncol = 2)


# wspólny wykres

grid.arrange(food_plot,
             behav_plot, nrow = 1)


# Korelacje Spearmana

cor.test(n_food$N_food, n_food$VEGETATION, method = 'spearman')

# cor.test(n_food$N_food, n_food$WATER, method = 'spearman')

cor.test(n_food$N_food, n_food$SEALED, method = 'spearman')



# Korelacje Spearmana

cor.test(n_behav$N_behav, n_behav$VEGETATION, method = 'spearman')

#cor.test(n_behav$N_behav, n_behav$WATER, method = 'spearman')

cor.test(n_behav$N_behav, n_behav$SEALED, method = 'spearman')





#-------Porownanie wymagaÅ„ siedliskowych grup w Å›rodowisku zurbanizowanym-------

# Porowanie grup takosnomicznych

div.data %>% group_by(ORDER) %>% summarise(N_GAT = n_distinct(SPECIES),
                                           MEDIANA_VEGE = median(VEGETATION),
                                           IQR_VEGE = IQR(VEGETATION),
                                           MEDIANA_WATER = median(WATER),
                                           IQR_WATER = IQR(WATER),
                                           MEDIANA_SEALED = median(SEALED),
                                           IQR_SEALED = IQR(SEALED))


# Wizualizacja

ord_boxplot1 <- div.data %>% 
  ggplot(aes(x = ORDER, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Rz¹d',
       tag = 'A')

# ord_boxplot2 <- div.data %>% 
#   ggplot(aes(x = ORDER, y = WATER)) +
#   geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Indeks dostêpnoœci wody', x = 'Rz¹d',
#        tag = 'B')

ord_boxplot3 <- div.data %>% 
  ggplot(aes(x = ORDER, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Rz¹d',
       tag = 'B')

grid.arrange(ord_boxplot1,
             #ord_boxplot2,
             ord_boxplot3, ncol = 2)




# porowanie grup pokarmowych

div.data %>% group_by(FOOD) %>% summarise(N_GAT = n_distinct(SPECIES),
                                           MEDIANA_VEGE = median(VEGETATION),
                                           IQR_VEGE = IQR(VEGETATION),
                                           MEDIANA_WATER = median(WATER),
                                           IQR_WATER = IQR(WATER),
                                           MEDIANA_SEALED = median(SEALED),
                                           IQR_SEALED = IQR(SEALED))

food_boxplot1 <- div.data %>%
  ggplot(aes(x = FOOD, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'goldenrod2', alpha = 0.9) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Grupa pokarmowa',
       tag = 'A')

# food_boxplot2 <- div.data %>% 
#   ggplot(aes(x = FOOD, y = WATER)) +
#   geom_boxplot(size = 0.8, fill = 'goldenrod2', alpha = 0.9) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Indeks dostêpnoœci wody', x = 'Grupa pokarmowa',
#        tag = 'B')

food_boxplot3 <- div.data %>% na.omit() %>% 
  ggplot(aes(x = FOOD, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'goldenrod2', alpha = 0.9) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Grupa pokarmowa',
       tag = 'B')


grid.arrange(food_boxplot1,
             #food_boxplot2,
             food_boxplot3, ncol = 2)



# porowanie grup behawioralnych

div.data %>% group_by(BEHAV) %>% summarise(N_GAT = n_distinct(SPECIES),
                                          MEDIANA_VEGE = median(VEGETATION),
                                          IQR_VEGE = IQR(VEGETATION),
                                          MEDIANA_WATER = median(WATER),
                                          IQR_WATER = IQR(WATER),
                                          MEDIANA_SEALED = median(SEALED),
                                          IQR_SEALED = IQR(SEALED))

behav_boxplot1 <- div.data %>% 
  ggplot(aes(x = BEHAV, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'deeppink4', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Grupa behawioralna',
       tag = 'A')

# behav_boxplot2 <- div.data %>% 
#   ggplot(aes(x = BEHAV, y = WATER)) +
#   geom_boxplot(size = 0.8, fill = 'deeppink4', alpha = 0.5) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Indeks dostêpnoœci wody', x = 'Grupa behawioralna',
#        tag = 'B')

behav_boxplot3 <- div.data %>%
  ggplot(aes(x = BEHAV, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'deeppink4', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Grupa behawioralna',
       tag = 'B')

grid.arrange(behav_boxplot1,
             #behav_boxplot2,
             behav_boxplot3, ncol = 2)


# lepsze wykresy


ord_boxplot1.1 <- div.data %>% filter(ORDER != 'Coraciiformes') %>% 
  ggplot(aes(x = ORDER, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Rz¹d',
       tag = 'A')

# ord_boxplot2 <- div.data %>% 
#   ggplot(aes(x = ORDER, y = WATER)) +
#   geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#         plot.tag = element_text(size = 20)) +
#   labs(y = 'Indeks dostêpnoœci wody', x = 'Rz¹d',
#        tag = 'B')

ord_boxplot3.1 <- div.data %>% filter(ORDER != 'Alcedinidae') %>% 
  ggplot(aes(x = ORDER, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'royalblue', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Rz¹d',
       tag = 'B')

grid.arrange(ord_boxplot1.1,
             #ord_boxplot2,
             ord_boxplot3.1, ncol = 2)


#porownanie grup

div.data_reduced <- div.data %>%
  filter(BEHAV != 'Aerial Forager' & BEHAV != 'Aerial insectivore'
         & BEHAV != 'Aerial Diver') %>% 
  filter(FOOD != 'Fish')


kruskal.test(data = div.data_reduced, VEGETATION~ORDER)

#kruskal.test(data = div.data_reduced, WATER~BEHAV)

kruskal.test(data = div.data_reduced, SEALED~ORDER)


# test post-hoc Dunna


dunnTest(data = div.data_reduced, VEGETATION~as.factor(ORDER), method = 'bonferroni')

# dunnTest(data = div.data, WATER~as.factor(ORDER), method = 'bonferroni')

dunnTest(data = div.data_reduced, SEALED~as.factor(ORDER), method = 'bonferroni')





behav_boxplot1.1 <- div.data %>% filter(BEHAV != 'Aerial Forager' & BEHAV != 'Aerial insectivore' & BEHAV != 'Aerial Dive') %>%
  ggplot(aes(x = BEHAV, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'deeppink4', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Grupa behawioralna',
       tag = 'A')


behav_boxplot3.1 <- div.data %>% filter(BEHAV != 'Aerial Forager' & BEHAV != 'Aerial insectivore') %>%
  ggplot(aes(x = BEHAV, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'deeppink4', alpha = 0.5) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Grupa behawioralna',
       tag = 'B')

grid.arrange(behav_boxplot1.1,
             #behav_boxplot2.1,
             behav_boxplot3.1, ncol = 2)




food_boxplot1.1 <- div.data_reduced %>%
  ggplot(aes(x = FOOD, y = VEGETATION)) +
  geom_boxplot(size = 0.8, fill = 'goldenrod2', alpha = 0.9) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks roœlinnoœci', x = 'Grupa pokarmowa',
       tag = 'A')

food_boxplot3.1 <- div.data_reduced %>% na.omit() %>% 
  ggplot(aes(x = FOOD, y = SEALED)) +
  geom_boxplot(size = 0.8, fill = 'goldenrod2', alpha = 0.9) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag = element_text(size = 20)) +
  labs(y = 'Indeks powierzchni uszczelnionej', x = 'Grupa pokarmowa',
       tag = 'B')


grid.arrange(food_boxplot1,
             food_boxplot3, ncol = 2)





# test Kruskala-Wallisa



kruskal.test(data = div.data_reduced, VEGETATION~FOOD)

#kruskal.test(data = div.data, WATER~FOOD)

kruskal.test(data = div.data_reduced, SEALED~FOOD)

# test post-hoc Dunna


dunnTest(data = div.data_reduced, VEGETATION~as.factor(FOOD), method = 'bonferroni')

#dunnTest(data = div.data, WATER~as.factor(FOOD), method = 'bonferroni')

dunnTest(data = div.data, SEALED~as.factor(FOOD), method = 'bonferroni')






# test Kruskala-Wallisa

div.data_reduced <- div.data %>%
  filter(BEHAV != 'Aerial Forager' & BEHAV != 'Aerial insectivore'
         & BEHAV != 'Aerial Diver') %>% 
  filter(ORDER != 'Alce')

kruskal.test(data = div.data_reduced, VEGETATION~BEHAV)

kruskal.test(data = div.data_reduced, WATER~BEHAV)

kruskal.test(data = div.data_reduced, SEALED~BEHAV)


# test post-hoc Dunna


dunnTest(data = div.data_reduced, VEGETATION~as.factor(BEHAV), method = 'bonferroni')

dunnTest(data = div.data_reduced, WATER~as.factor(BEHAV), method = 'bonferroni')

dunnTest(data = div.data_reduced, SEALED~as.factor(BEHAV), method = 'bonferroni')





