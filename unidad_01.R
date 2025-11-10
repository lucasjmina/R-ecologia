# ---- Librerias ----

library(vegan)
library(ggplot2)
library(tidyverse)
library(iNEXT)

# ---- Datos ----
#
datos <- read.csv("datos/abundancia_tratamiento.csv", header = TRUE)
abund_ec <- read.csv("datos/abundancia_conservacion.csv", header = TRUE, row.names = 1)

# Tabla de abundancias (solo valores numéricos)
abundancia <- select(datos, !localidad:estadoConservacion)
row.names(abundancia) <- datos$localidad

# Variable categorica como factor
datos$estadoConservacion <- factor(datos$estadoConservacion, levels = c("ECB", "ECI", "ECD"))

# ---- Riqueza ----

riqueza <- specnumber(abundancia)
riqueza

# Tabla resumen
tabla_riqueza <- riqueza %>%
  enframe(name = "localidad", value = "S") %>%
  left_join(select(datos, localidad:estadoConservacion), by = "localidad")

# Boxplot
ggplot(tabla_riqueza, aes(x = estadoConservacion, y = S, fill = estadoConservacion)) +
  geom_boxplot() +
  labs(x = "Estado de conservación", y = "Riqueza") +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(legend.position = "none")

# ---- Estimadores ----

estimadores <- specpool(abundancia, datos$estadoConservacion)
estimadores

# ---- ACE/ICE -----

ace_ice <- pivot_longer(
  datos,
  Acanthoponera.mucronata:Gnamptogenys.sulcata,
  values_to = "abund"
) %>%
  reframe(
    .by = estadoConservacion,
    ACE = fossil::ACE(abund),
    ICE = fossil::ICE(abund)
  ) %>%
  unique()
ace_ice

# ---- Shannon y Simpson

D <- diversity(abundancia, index = "simpson", datos$estadoConservacion) %>%
  enframe(name = "Tratamiento", value = "D")
iD <- diversity(abundancia, index = "invsimpson", datos$estadoConservacion) %>%
  enframe(name = "Tratamiento", value = "iD")
H <- diversity(abundancia, index = "shannon", datos$estadoConservacion) %>%
  enframe(name = "Tratamiento", value = "H")

res_div <- D %>%
  left_join(iD, by = "Tratamiento") %>%
  left_join(H, by = "Tratamiento")
res_div


# ---- Diversidad verdadera ----

est_puntos <- estimateD(
  abund_ec, datatype = "abundance", base = "coverage",
  conf = 0.95, level = NULL, q = seq(0, 2, by = 0.1)
)
est_puntos

est_puntos$Assemblage <- factor(est_puntos$Assemblage, levels = c("ECB", "ECI", "ECD"))

ggplot(est_puntos, aes(x = Order.q, y = qD, group = Assemblage, color = Assemblage)) +
  geom_line() +
  geom_point(
    data = filter(est_puntos, Order.q == 0 | Order.q == 1 | Order.q == 2),
    aes(x = Order.q, y = qD, group = Assemblage, color = Assemblage)
  ) +
  labs(x = "Orden q", y = "Diversidad", color = "Estado\nconservación") +
  scale_x_continuous(breaks = c(0, 1, 2)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic()
