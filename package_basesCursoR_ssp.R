# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 05/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(gridExtra)

# Identificar bases disponíveis ------------------------------------------------------------------------------------------------------------

basesCursoR::bases_disponiveis()

# Carregar base de dados -------------------------------------------------------------------------------------------------------------------

ssp <- basesCursoR::pegar_base("ssp")
View(ssp)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

ssp1 <- ssp %>%
  select(ano, estupro_total, furto_veiculos, hom_doloso, 
         roubo_banco, vit_latrocinio) %>%
  filter(ano %in% c("2010", "2011", "2012", "2013", 
                         "2014", "2015", "2016", 
                       "2017", "2018")) 
View(ssp1)  
glimpse(ssp1)
ssp1$ano <- as.factor(ssp1$ano)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

ssp2 <- ssp1 %>%
  group_by(ano) %>%
  summarise(med_est = mean(estupro_total),
            med_fur = mean(furto_veiculos),
            med_hom = mean(hom_doloso),
            med_roubo = mean(roubo_banco),
            med_vit_lat = mean(vit_latrocinio),
            sd_est = sd(estupro_total),n_est = n(),
            se_est = sd_est/sqrt(n_est),
            sd_vit_lat = sd(vit_latrocinio), n_vit_lat = n(),
            se_vit_lat = sd_vit_lat/sqrt(n_vit_lat),
            sd_fur = sd(furto_veiculos),n_fur = n(),
            se_fur = sd_fur/sqrt(n_fur),
            sd_hom = sd(hom_doloso),n_hom = n(),
            se_hom = sd_hom/sqrt(n_hom),
            sd_roubo = sd(roubo_banco),n_roubo = n(),
            se_roubo = sd_roubo/sqrt(n_roubo))

ssp2$ano <- factor(ssp2$ano,
                       levels = c("2010",
                                  "2011",
                                  "2012",
                                  "2013",
                                  "2014",
                                  "2015",
                                  "2016",
                                  "2017",
                                  "2018"))

p1 <- ggplot(ssp2, aes(x = ano, y = med_est)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = ano, y = med_est, ymin = med_est - se_est,
                    ymax = med_est + se_est), width = 0.3, size = 0.9) +
  labs(x = "Ano", y = "Estupros")
p1

p2 <- ggplot(ssp2, aes(x = ano, y = med_fur)) +
  geom_col(fill = "#beaed4", color = "black") +
  geom_errorbar(aes(x = ano, y = med_fur, ymin = med_fur - se_fur,
                    ymax = med_fur + se_fur), width = 0.3, size = 0.9) +
  labs(x = "Ano", y = "Furtos de veículos")
p2

p3 <- ggplot(ssp2, aes(x = ano, y = med_hom)) +
  geom_col(fill = "#fdc086", color = "black") +
  geom_errorbar(aes(x = ano, y = med_hom, ymin = med_hom - se_hom,
                    ymax = med_hom + se_hom), width = 0.3, size = 0.9) +
  labs(x = "Ano", y = "Homicídios doloso")
p3

p4 <- ggplot(ssp2, aes(x = ano, y = med_roubo)) +
  geom_col(fill = "#ffff99", color = "black") +
  geom_errorbar(aes(x = ano, y = med_roubo, ymin = med_roubo - se_roubo,
                    ymax = med_roubo + se_roubo), width = 0.3, size = 0.9) +
  labs(x = "Ano", y = "Roubos de banco")
p4

p5 <- ggplot(ssp2, aes(x = ano, y = med_vit_lat)) +
  geom_col(fill = "#386cb0", color = "black") +
  geom_errorbar(aes(x = ano, y = med_vit_lat, ymin = med_vit_lat - se_vit_lat,
                    ymax = med_vit_lat + se_vit_lat), width = 0.3, size = 0.9) +
  labs(x = "Ano", y = "Vítimas de latrocínio")
p5

grid.arrange(p1, p2, p3, p4, p5)

