#source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

library(tidyverse)
data("ToothGrowth")



dados_dose <- ToothGrowth %>%
  group_by(dose) %>%
  summarise(media_len = mean(len))

dados_supp <- ToothGrowth %>%
  group_by(supp) %>%
  summarise(media_len = mean(len))




head(ToothGrowth)
library(ggplot2)

analise3 <- ggplot(ToothGrowth, aes(x = supp, fill = supp)) +
  geom_bar(color = "yellow", linewidth = 3) +
  scale_fill_manual(values = c("green", "pink")) +
  geom_text(stat = "count", aes(label = ..count..), color = "black", size = 10) +
  labs(
    x = "",
    y = "",
    title = "TESTE"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "red")
  )
