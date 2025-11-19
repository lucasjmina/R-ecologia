# ---- Librerias ----

library(BiodiversityR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(GGally)
library(geosphere)

install.packages("GGally")
install.packages("corrplot")

set.seed(2025)


# ---- Datos ----

# Para PCA
moscas <- read.csv("datos/moscas.csv", header = TRUE, row.names = 1)

# Para CCA
abundancia <- read.csv("datos/abundancia.csv", header = TRUE, row.names = 1)
ambiente <- read.csv("datos/ambiente.csv", header = TRUE, row.names = 1)

# Para mantel
coords <- read.csv("datos/coordenadas.csv", header = TRUE, row.names = 1)

# ---- Regresíon lineal ----

# Correlaciones

# Matriz
matriz_cors <- cor(moscas[-1])
matriz_cors

# Gráfico correlaciones (GGally)
ggcorr(moscas)
ggpairs(moscas[-1])

corrplot::corrplot(matriz_cors)

test_correlación <- cor.test(moscas$IHH, moscas$Abund_log)
test_correlación

regresion <- lm(Abund_log ~ IHH, data = moscas)
summary(regresion)

# Gráfico
reglineal_plot <- ggplot(moscas, aes(x = IHH, y = Abund_log)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    formula = y ~ x,
    method = "lm",
    color = "black",
    linewidth = 1,
    lineend = "round",
    se = TRUE
  ) +
  theme_classic()
reglineal_plot

# ---- PCA ----

datos_estandarizados <- scale(select(moscas[-1], !Abund_log))

pca <- prcomp(datos_estandarizados, scale = TRUE)
summary(pca)

# Gráfico
plot_pca <- fviz_pca_biplot(pca, label = "var", addEllipses = TRUE, ellipse.level = 0.95, habillage = moscas$Ambiente, palette = "Set2") +
  theme_classic()
plot_pca

# ---- CCA ----

cca0 <- cca(abundancia ~ ., ambiente)
summary(cca0)

# CCA por pasos
cca_steps <- ordistep(cca0)

# Correlaciones
ggcorr(ambiente)

resultado_cca <- cca(abundancia ~ t_min + ihh + hr_max + hr_min, data = ambiente)
summary(resultado_cca)

anova_cca <- anova.cca(resultado_cca, by = "terms")
anova_cca

## Gráficos

# Gráficos base
plot_cca <- plot(resultado_cca)
elipses_plot_cca <- ordiellipse(resultado_cca, ambiente$estado_conservacion)

# Extracción de datos para graficar
sitios <- scores(resultado_cca, display = "sites", scaling = 2) %>%
  as.data.frame() %>%
  rownames_to_column("label")

especies <- scores(resultado_cca, display = "species", scaling = 2) %>%
  as.data.frame() %>%
  rownames_to_column("label")

vectores <- scores(resultado_cca, display = "bp", scaling = 2) %>%
  as.data.frame() %>%
  rownames_to_column("label")

elipses <- ordiellipse.long(elipses_plot_cca)
colnames(elipses) <- c("Grupo", "CCA1", "CCA2")

# Gráfico
ggplot(especies, aes(x = CCA1, y = CCA2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(alpha = 0.5) +
  geom_path(data = elipses, aes(color = Grupo)) +
  geom_segment(
    data = vectores,
    aes(x = 0, y = 0, xend = CCA1*2, yend = CCA2*2),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "blue"
  ) +
  geom_text_repel(
    data = vectores,
    aes(x = CCA1*2, y = CCA2*2, label = label),
    color = "blue",
    direction = "y"
  ) +
  theme_classic()

# Más facil?

#install.packages("remotes")
#remotes::install_github("gavinsimpson/ggvegan")

library(ggvegan)

cca_ggvegan <- autoplot(resultado_cca)
cca_ggvegan

# ---- Test de mantel ----

dist_abundancia <- vegdist(abundancia, method = "bray")

dist_geografica <- as.dist(distm(coords, fun = distGeo))

resultado_mantel <- mantel(dist_abundancia, dist_geografica, method = "pearson", permutations = 9999)
resultado_mantel

## Gráfico de dispersión

plot(as.matrix(dist_geografica), as.matrix(dist_abundancia))

dist_abund_vect <- as.vector(dist_abundancia)
dist_geo_vect <- as.vector(dist_geografica)

puntos_mantel <- data.frame(
  dist_abund = dist_abund_vect,
  dist_geo = dist_geo_vect
)

# Distancias geográficas de 0 a 1
min_dist <- min(puntos_mantel$dist_geo)
max_dist <- max(puntos_mantel$dist_geo)
puntos_mantel$geo_scaled <- (puntos_mantel$dist_geo - min_dist) / (max_dist - min_dist)

ggplot(puntos_mantel, aes(x = geo_scaled, y = dist_abund)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Distancia geográfica", y = "Distancia de abundancias") +
  coord_fixed() +
  theme_classic()
