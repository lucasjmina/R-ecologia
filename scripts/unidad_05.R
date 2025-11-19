# ---- Librerias ----

library(vegan)
library(car)
library(emmeans)
library(multcomp)
library(ggplot2)

# ---- Datos ----

hormigas <- read.csv("datos/hormigas.csv", header = TRUE)
moscas <- read.csv("datos/moscas.csv", header = TRUE)

abundancia <- read.csv("datos/abundancia.csv", header = TRUE, row.names = 1)
ambiente <- read.csv("datos/ambiente.csv", header = TRUE, row.names = 1)

# ---- Prueba supuestos ----

shapiro_test <- by(
  data = hormigas$D2,
  INDICES = hormigas$TRATAMIENTO,
  FUN = shapiro.test
)
shapiro_test

ggplot(hormigas, aes(sample = D2)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ TRATAMIENTO) +
  theme_classic()

levene_test <- leveneTest(D2 ~ TRATAMIENTO, data = hormigas)
levene_test

# ---- T de Student ----

grupo1 <- hormigas$Taxa_S[hormigas$TRATAMIENTO == "ECB"]
grupo2 <- hormigas$Taxa_S[hormigas$TRATAMIENTO == "ECD"]

res_ttest <- t.test(grupo1, grupo2)
res_ttest

# ---- ANOVA ----

modelo_anova <- aov(Taxa_S ~ TRATAMIENTO, data = hormigas)
summary(modelo_anova)

# Ver diferencias entre grupos
emms <- emmeans(modelo_anova, specs = ~ TRATAMIENTO)
emms

pairs(emms)

# ---- MANOVA ----

modelo_manova <- manova(cbind(Taxa_S, D1, D2, INCIDENCIA) ~ TRATAMIENTO, data = hormigas)
summary(modelo_manova)

# ---- Mann-Whitney ----

ecb_vs_ecd <- subset(hormigas, TRATAMIENTO %in% c("ECB", "ECD"))

test_whitney <- wilcox.test(D2 ~ TRATAMIENTO, data = ecb_vs_ecd)
test_whitney


# ---- Kruskal Wallis ----

test_kruskal <- kruskal.test(D2 ~ TRATAMIENTO, data = hormigas)
test_kruskal

# ---- Spearman ----

spearman_cor <- cor.test(moscas$Div_Shan, moscas$IHH, method = "spearman")
spearman_cor

# ---- PERMANOVA ----

dist_abundancia <- vegdist(abundancia, method = "bray")

res_permanova <- adonis2(dist_abundancia ~ estado_conservacion, data = ambiente, permutations = 999)
res_permanova

modelo_betadisp <- betadisper(dist_abundancia, group = ambiente$estado_conservacion)
anova(modelo_betadisp)
