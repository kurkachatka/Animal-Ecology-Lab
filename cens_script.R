

# install.packages('tidyverse')
# install.packages('readxl')
# install.packages('factoextra')
# install.packages('gridExtra')

library(tidyverse)
library(readxl)
library(gridExtra)
library(factoextra)


# wczytaj plik
cens <- read_excel('Census_MainDB.xlsx')

# przygotowanie danych ze zliczonymi gatunkami

n_gat <- cens %>% 
  group_by(GRUPA, PUNKT) %>% 
  summarise(N_GAT=n(),
            AUTA = median(AUTA),
            PIESI = median(PIESI),
            BUD = median(BUD),
            DRZEW = median(DRZEW),
            WODA = median(WODA))

n_gat <- as.data.frame(n_gat)


# przygotowanie danych do PCA, tylko ze zmiennych ?rodowiskowych

db_pca <- n_gat %>% select(!GRUPA & !PUNKT & !N_GAT)

# obliczenie PCA

wyniki.pca <- prcomp(db_pca, scale = T)


# wizualizacja wariancji objasnionej przez kazde PC

fviz_eig(wyniki.pca)

# biplot

fviz_pca_var(wyniki.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


# obliczenie wartosci PC dla kazdego punktu obserwacyjnego


kalk_pca <- predict(wyniki.pca, newdata = n_gat)

n_gat_pca <- bind_cols(n_gat, kalk_pca)


# zmiany bioroznorodnosci w zaleznosci od cech srodowiska zurbanizowanego

p1 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = PC1)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()


p2 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = AUTA)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()

p3 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = PIESI)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()

p4 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = DRZEW)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()


p5 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = WODA)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()

p6 <- n_gat_pca %>% 
  ggplot(aes(y = N_GAT, x = BUD)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()


p_s <- grid.arrange(p2, p3, p4, p5, p6)

grid.arrange(p1, p_s, ncol = 2)



# Korelacja Spearmana miedzy liczba gatunkow a gradientem zurbanizowania
# i wszystkimi badanymi zmiennymi srodowiskowymi

cor.test(n_gat_pca$N_GAT, n_gat_pca$PC1, method = 'spearman')

cor.test(n_gat_pca$N_GAT, n_gat_pca$AUTA, method = 'spearman')

cor.test(n_gat_pca$N_GAT, n_gat_pca$PIESI, method = 'spearman')

cor.test(n_gat_pca$N_GAT, n_gat_pca$DRZEW, method = 'spearman')

cor.test(n_gat_pca$N_GAT, n_gat_pca$WODA, method = 'spearman')

cor.test(n_gat_pca$N_GAT, n_gat_pca$BUD, method = 'spearman')


