# ---- Librerias ----

library(BiodiversityR)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(FD)

set.seed(2025)

# ---- Carga de datos ----

# Para análisis diversidad funcional
abund_hormigas <- read.csv("datos/abund_hormigas.csv", header = TRUE, row.names = 1)
medidas <- read.csv("datos/medidas_hormigas.csv", header = TRUE, row.names = 1)
categs <- read.csv("datos/categ_hormigas.csv", header = TRUE, row.names = 1, colClasses = "factor")

# Para análisis diversidad taxonomica
abundancia <- read.csv("datos/abundancia.csv", header = TRUE, row.names = 1)
taxa <- read.csv("datos/taxa.csv", header = TRUE, row.names = 1)

# Para guardar tablas
dir.create("salida")

# Para guardar gráficos
dir.create("graficos")

# --- Diversidad funcional ----

# Diversidad con variables continuas
div_medidas <- dbFD(medidas, abund_hormigas)

# Diversidad con variables categoricas
div_categs <- dbFD(categs, abund_hormigas)

# Tablas resumen
tb_div_medidas <- rbind(
  div_medidas$FRic,
  div_medidas$FEve,
  div_medidas$FDiv,
  div_medidas$FDis
) %>% as.data.frame()

tb_div_medidas <- cbind(Index = c("FRic", "FEve", "FDiv", "FDis"), tb_div_medidas)
tb_div_medidas

tb_div_categs <- rbind(
  div_categs$FRic,
  div_categs$FEve,
  div_categs$FDis
) %>% as.data.frame()

tb_div_categs <- cbind(Index = c("FRic", "FEve", "FDis"), tb_div_categs)
tb_div_categs

# Guardamos las tablas
write.csv(tb_div_medidas, "salida/diversidad_medidas.csv")
write.csv(tb_div_categs, "salida/diversidad_categorias.csv")

## Gráficos
##
# Transformamos las tablas a formato largo para ggplot
div_medidas_long  <- tb_div_medidas %>%
  pivot_longer(
    ECB:ECD,
    names_to = "Ambiente",
    values_to = "val"
  ) %>%
  mutate(Ambiente = factor(Ambiente, levels = c("ECB", "ECI", "ECD")))

div_categs_long  <- tb_div_categs %>%
  pivot_longer(
    ECB:ECD,
    names_to = "Ambiente",
    values_to = "val"
  ) %>%
  mutate(Ambiente = factor(Ambiente, levels = c("ECB", "ECI", "ECD")))

# Gráfico diversidad rasgos continuos
div_medidas_plot <- ggplot(div_medidas_long, aes(x = Ambiente, y = val, fill = Ambiente)) +
  ylab("") +
  xlab("") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#20854E", "#E18727", "#BC3C29")) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(Index ~ ., scale = "free", axes = "all", ncol = 2, strip.position = "left") +
  theme_classic() +
  theme(strip.placement = "outside", strip.background = element_blank())
div_medidas_plot

#Gráfico diversidad rasgos categoricos
div_categs_plot <- ggplot(div_categs_long, aes(x = Ambiente, y = val, fill = Ambiente)) +
  ylab("") +
  xlab("") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#20854E", "#E18727", "#BC3C29")) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(Index ~ ., scale = "free", axes = "all", ncol = 2, strip.position = "left") +
  theme_classic() +
  theme(strip.placement = "outside", strip.background = element_blank())
div_categs_plot

# Guardamos los gráficos
ggsave("graficos/diversidad_medidas.png", div_medidas_plot, dpi = 300, width = 2000, height = 2000, units = "px")
ggsave("graficos/diversidad_categorias.png", div_categs_plot, dpi = 300, width = 2000, height = 2000, units = "px")

# ---- Diversidad taxonómica ----

# Primero es necesatio crear una matriz de distancia de los taxones
dist_taxa <- taxa2dist(taxa)

# Calculo diversidad
div_taxonomica <- taxondive(abundancia, dist_taxa)
div_taxonomica

## Gráifco
##
# Transformamos los resultados a una data.frame
div_taxonomica <- as.data.frame(
  do.call(cbind, div_taxonomica)
)

div_taxa_plot <- ggplot(div_taxonomica, aes(x = Species, y = Dplus)) +
  geom_point() +
  ylab("Δ+") +
  xlab("Riqueza") +
  geom_hline(aes(yintercept = EDplus), linetype = "dotted") +
  geom_ribbon(aes(ymax = EDplus + sd.Dplus * 2, ymin = EDplus - sd.Dplus * 2), fill = NA, color = "black") +
  geom_text_repel(aes(label = row.names(div_taxonomica)), size = 3.5, color = "blue3") +
  theme_classic()
div_taxa_plot

ggsave("graficos/div_taxonomica.png", div_taxa_plot, dpi = 300, width = 2000, height = 2000, units = "px")

tabla <- data.frame(
  x1 = c(5, 7, 9),
  x2 = c(9, 6, 4)
)
tabla

colnames(tabla)
row.names(tabla)

colnames(tabla) <- c("rasgo1", "rasgo2")

row.names(tabla) <- c("sp1", "sp2", "sp3")

# Gráfico diversidad rasgos continuos

div_medidas_plot <- ggplot(div_medidas_long, aes(x = Ambiente, y = val, fill = Ambiente)) +

  ylab("") +

  xlab("") +

  geom_col(show.legend = FALSE) +

  scale_fill_manual(values = c("#20854E", "#E18727", "#BC3C29")) +

  scale_y_continuous(expand = c(0, 0)) +

  facet_wrap(Index ~ ., scale = "free", axes = "all", ncol = 2, strip.position = "left") +

  theme_classic() +

  theme(strip.placement = "outside", strip.background = element_blank())

div_medidas_plot

#Gráfico diversidad rasgos categoricos

div_categs_plot <- ggplot(div_categs_long, aes(x = Ambiente, y = val, fill = Ambiente)) +

  ylab("") +

  xlab("") +

  geom_col(show.legend = FALSE) +

  scale_fill_manual(values = c("#20854E", "#E18727", "#BC3C29")) +

  scale_y_continuous(expand = c(0, 0)) +

  facet_wrap(Index ~ ., scale = "free", axes = "all", ncol = 2, strip.position = "left") +

  theme_classic() +

  theme(strip.placement = "outside", strip.background = element_blank())

div_categs_plot
