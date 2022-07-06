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
            med_vit_lat = mean(vit_latrocinio))

p1 <- ggplot(ssp2, aes(x = fct_reorder(ano, med_est), y = med_est)) +
  geom_col(fill = "#7fc97f", color = "black")
p1

p2 <- ggplot(ssp2, aes(x = fct_reorder(ano, med_fur), y = med_fur)) +
  geom_col(fill = "#7fc97f", color = "black")
p2
