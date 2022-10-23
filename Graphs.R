library('tidyverse')
library('ggplot2')

base_datos <- read.csv("**/base_datos_talla_peso.csv")

nous_pesos <- base_datos[base_datos$clase == "CLASE2",]

names(nous_pesos)

Pesos_1 <- median(nous_pesos[nous_pesos$PESO <= 50,]$PESO) 
PAPP_P1 <- median(nous_pesos[nous_pesos$PESO <= 50,]$PAPP.A..0.2.3.0.)
hCG_P1 <- median(nous_pesos[nous_pesos$PESO <= 50,]$hCG.0.3.5.0.)

Pesos_2 <- median(nous_pesos[nous_pesos$PESO > 50 & nous_pesos$PESO <= 60,]$PESO) 
PAPP_P2 <- median(nous_pesos[nous_pesos$PESO > 50 & nous_pesos$PESO <= 60,]$PAPP.A..0.2.3.0.)
hCG_P2 <- median(nous_pesos[nous_pesos$PESO > 50 & nous_pesos$PESO <= 60,]$hCG.0.3.5.0.)

Pesos_3 <- median(nous_pesos[nous_pesos$PESO > 60 & nous_pesos$PESO <= 70,]$PESO) 
PAPP_P3 <- median(nous_pesos[nous_pesos$PESO > 60 & nous_pesos$PESO <= 70,]$PAPP.A..0.2.3.0.)
hCG_P3 <- median(nous_pesos[nous_pesos$PESO > 60 & nous_pesos$PESO <= 70,]$hCG.0.3.5.0.)

Pesos_4 <- median(nous_pesos[nous_pesos$PESO > 70 & nous_pesos$PESO <= 80,]$PESO) 
PAPP_P4 <- median(nous_pesos[nous_pesos$PESO > 70 & nous_pesos$PESO <= 80,]$PAPP.A..0.2.3.0.)
hCG_P4 <- median(nous_pesos[nous_pesos$PESO > 70 & nous_pesos$PESO <= 80,]$hCG.0.3.5.0.)

Pesos_5 <- median(nous_pesos[nous_pesos$PESO > 80 & nous_pesos$PESO <= 90,]$PESO) 
PAPP_P5 <- median(nous_pesos[nous_pesos$PESO > 80 & nous_pesos$PESO <= 90,]$PAPP.A..0.2.3.0.)
hCG_P5 <- median(nous_pesos[nous_pesos$PESO > 80 & nous_pesos$PESO <= 90,]$hCG.0.3.5.0.)

Pesos_6 <- median(nous_pesos[nous_pesos$PESO > 90 & nous_pesos$PESO <= 100,]$PESO) 
PAPP_P6 <- median(nous_pesos[nous_pesos$PESO > 90 & nous_pesos$PESO <= 100,]$PAPP.A..0.2.3.0.)
hCG_P6 <- median(nous_pesos[nous_pesos$PESO > 90 & nous_pesos$PESO <= 100,]$hCG.0.3.5.0.)

Pesos_7 <- median(nous_pesos[nous_pesos$PESO > 100 & nous_pesos$PESO <= 110,]$PESO) 
PAPP_P7 <- median(nous_pesos[nous_pesos$PESO > 100 & nous_pesos$PESO <= 110,]$PAPP.A..0.2.3.0.)
hCG_P7 <- median(nous_pesos[nous_pesos$PESO > 100 & nous_pesos$PESO <= 110,]$hCG.0.3.5.0.)

Pesos_8 <- median(nous_pesos[nous_pesos$PESO > 110 & nous_pesos$PESO <= 120,]$PESO) 
PAPP_P8 <- median(nous_pesos[nous_pesos$PESO > 110 & nous_pesos$PESO <= 120,]$PAPP.A..0.2.3.0.)
hCG_P8 <- median(nous_pesos[nous_pesos$PESO > 110 & nous_pesos$PESO <= 120,]$hCG.0.3.5.0.)

Pesos_9 <- median(nous_pesos[nous_pesos$PESO > 120,]$PESO) 
PAPP_P9 <- median(nous_pesos[nous_pesos$PESO > 120,]$PAPP.A..0.2.3.0.)
hCG_P9 <- median(nous_pesos[nous_pesos$PESO > 120,]$hCG.0.3.5.0.)

Median_pesos <- c(Pesos_1, Pesos_2, Pesos_3, Pesos_4, Pesos_5, Pesos_6, Pesos_7, Pesos_8, Pesos_9)
Median_PAPP_P <- c(PAPP_P1, PAPP_P2, PAPP_P3, PAPP_P4, PAPP_P5, PAPP_P6, PAPP_P7, PAPP_P8, PAPP_P9)
Median_hCG_P <- c(hCG_P1, hCG_P2, hCG_P3, hCG_P4, hCG_P5, hCG_P6, hCG_P7, hCG_P8, hCG_P9)

linear_model_PAPP <- lm(Median_PAPP_P ~ Median_pesos)
summary(linear_model_PAPP)

linear_model_hCG <- lm(Median_hCG_P ~ Median_pesos)
summary(linear_model_hCG)

ggplot(mapping = aes(Median_pesos, Median_PAPP_P)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = 'Peso vs Log (PAPP MoM)', x='Peso', y='Log (PAPP MoM)')

ggplot(mapping = aes(Median_pesos, Median_hCG_P)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = 'Peso vs Log (hCG MoM)', x='Peso', y='Log (hCG MoM)')

########################################################################

NA_values <- !is.na(nous_pesos$TALLA)
base_datos_pt <- nous_pesos[NA_values,]
str(base_datos_pt)

base_datos_pt$IMC <- (base_datos_pt$PESO)/(base_datos_pt$TALLA/100)^2
hist(base_datos_pt$IMC)

linear_model_PAPP <- lm(nous_pesos$PAPP.A..0.2.3.0. ~ nous_pesos$PESO)
summary(linear_model_PAPP)

linear_model_hCG <- lm(nous_pesos$hCG.0.3.5.0. ~ nous_pesos$PESO)
summary(linear_model_hCG)

linear_model_PAPP_1 <- lm(base_datos_pt[base_datos_pt$IMC <= 20,]$PAPP.A..0.2.3.0. ~ base_datos_pt[base_datos_pt$IMC <= 20,]$PESO)
summary(linear_model_PAPP_1)

linear_model_PAPP_2 <- lm(base_datos_pt[base_datos_pt$IMC > 20 & base_datos_pt$IMC <= 25,]$PAPP.A..0.2.3.0. ~ base_datos_pt[base_datos_pt$IMC > 20 & base_datos_pt$IMC <= 25,]$PESO)
summary(linear_model_PAPP_2)

linear_model_PAPP_3 <- lm(base_datos_pt[base_datos_pt$IMC > 25 & base_datos_pt$IMC <= 30,]$PAPP.A..0.2.3.0. ~ base_datos_pt[base_datos_pt$IMC > 25 & base_datos_pt$IMC <= 30,]$PESO)
summary(linear_model_PAPP_3)

linear_model_PAPP_4 <- lm(base_datos_pt[base_datos_pt$IMC > 30,]$PAPP.A..0.2.3.0. ~ base_datos_pt[base_datos_pt$IMC > 30,]$PESO)
summary(linear_model_PAPP_4)

linear_model_hCG_1 <- lm(base_datos_pt[base_datos_pt$IMC <= 20,]$hCG.0.3.5.0. ~ base_datos_pt[base_datos_pt$IMC <= 20,]$PESO)
summary(linear_model_hCG_1)

linear_model_hCG_2 <- lm(base_datos_pt[base_datos_pt$IMC > 20 & base_datos_pt$IMC <= 25,]$hCG.0.3.5.0. ~ base_datos_pt[base_datos_pt$IMC > 20 & base_datos_pt$IMC <= 25,]$PESO)
summary(linear_model_hCG_2)

linear_model_hCG_3 <- lm(base_datos_pt[base_datos_pt$IMC > 25 & base_datos_pt$IMC <= 30,]$hCG.0.3.5.0. ~ base_datos_pt[base_datos_pt$IMC > 25 & base_datos_pt$IMC <= 30,]$PESO)
summary(linear_model_hCG_3)

linear_model_hCG_4 <- lm(base_datos_pt[base_datos_pt$IMC > 30,]$hCG.0.3.5.0. ~ base_datos_pt[base_datos_pt$IMC > 30,]$PESO)
summary(linear_model_hCG_4)

ggplot() +
  geom_smooth(mapping = aes(nous_pesos$PESO, nous_pesos$PAPP.A..0.2.3.0.), method = lm, colour = 'black', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC<=20,]$PESO, base_datos_pt[base_datos_pt$IMC<=20,]$PAPP.A..0.2.3.0.), method = lm, colour = 'red', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>20 & base_datos_pt$IMC<=25,]$PESO, base_datos_pt[base_datos_pt$IMC>20 & base_datos_pt$IMC<=25,]$PAPP.A..0.2.3.0.), method = lm, colour = 'blue', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>25 & base_datos_pt$IMC<=30,]$PESO, base_datos_pt[base_datos_pt$IMC>25 & base_datos_pt$IMC<=30,]$PAPP.A..0.2.3.0.), method = lm, colour = 'green', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>30,]$PESO, base_datos_pt[base_datos_pt$IMC>30,]$PAPP.A..0.2.3.0.), method = lm, colour = 'yellow', se=F)


ggplot() +
  geom_smooth(mapping = aes(nous_pesos$PESO, nous_pesos$hCG.0.3.5.0.), method = lm, colour = 'black', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC<=20,]$PESO, base_datos_pt[base_datos_pt$IMC<=20,]$hCG.0.3.5.0.), method = lm, colour = 'red', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>20 & base_datos_pt$IMC<=25,]$PESO, base_datos_pt[base_datos_pt$IMC>20 & base_datos_pt$IMC<=25,]$hCG.0.3.5.0.), method = lm, colour = 'blue', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>25 & base_datos_pt$IMC<=30,]$PESO, base_datos_pt[base_datos_pt$IMC>25 & base_datos_pt$IMC<=30,]$hCG.0.3.5.0.), method = lm, colour = 'green', se=F) +
  geom_smooth(mapping = aes(base_datos_pt[base_datos_pt$IMC>30,]$PESO, base_datos_pt[base_datos_pt$IMC>30,]$hCG.0.3.5.0.), method = lm, colour = 'yellow', se=F)
