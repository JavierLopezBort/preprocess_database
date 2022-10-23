base_datos <- read.csv("**/base_datos_talla_peso.csv")

str(base_datos)

nous_pesos <- base_datos[base_datos$clase == "CLASE2",]

str(nous_presos)

####################
# Comparar pesos en totes les variables explicatives
#####################

plot (nous_pesos$PESO, nous_pesos$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "Peso vs PAPP")
plot (nous_pesos$PESO, nous_pesos$hCG.0.3.5.0., ylim=c(-1,1), main = "Peso vs hCG")
plot (nous_pesos$PESO, nous_pesos$NT..0.5.2.5., ylim=c(-1,1), main = "Peso vs NT")
plot (nous_pesos$PESO, nous_pesos$DBP, ylim=c(-1,1), main = "Peso vs DBP")
plot (nous_pesos$PESO, nous_pesos$PER_CEFAL, ylim=c(-1,1), main = "Peso vs PER_CEFAL")
plot (nous_pesos$PESO, nous_pesos$PER_ABDOM, ylim=c(-1,1), main = "Peso vs PER_ABDOM")
plot (nous_pesos$PESO, nous_pesos$LF, ylim=c(-1,1), main = "Peso vs LF")
plot (nous_pesos$PESO, nous_pesos$edad, main = "Peso vs LF")

linear_model_PAPP_pesos_preagrupar <- lm(PAPP.A..0.2.3.0. ~ PESO, data = nous_pesos)
summary(linear_model_PAPP_pesos_preagrupar)

linear_model_hCG_pesos_preagrupar <- lm(hCG.0.3.5.0. ~ PESO, data = nous_pesos)
summary(linear_model_hCG_pesos_preagrupar)

linear_model_NT_pesos_preagrupar <- lm(NT..0.5.2.5. ~ PESO, data = nous_pesos)
summary(linear_model_NT_pesos_preagrupar)

linear_model_DBP_pesos_preagrupar <- lm(DBP ~ PESO, data = nous_pesos)
summary(linear_model_DBP_pesos_preagrupar)

linear_model_PER_CEFAL_pesos_preagrupar <- lm(PER_CEFAL ~ PESO, data = nous_pesos)
summary(linear_model_PER_CEFAL_pesos_preagrupar)

linear_model_PER_ABDOM_pesos_preagrupar <- lm(PER_ABDOM ~ PESO, data = nous_pesos)
summary(linear_model_PER_ABDOM_pesos_preagrupar)

linear_model_LF_pesos_preagrupar <- lm(LF ~ PESO, data = nous_pesos)
summary(linear_model_LF_pesos_preagrupar)

####################
# Comparar talla en totes les variables explicatives
#####################

NA_values <- !is.na(nous_pesos$TALLA)
noves_talles <- base_datos[NA_values,]

str(noves_talles)

plot (noves_talles$TALLA, noves_talles$PAPP.A..0.2.3.0., ylim=c(-1,1), main = "Talla vs PAPP")
plot (noves_talles$TALLA, noves_talles$hCG.0.3.5.0., ylim=c(-1,1), main = "Talla vs hCG")
plot (noves_talles$TALLA, noves_talles$NT..0.5.2.5., ylim=c(-1,1), main = "Talla vs NT")
plot (noves_talles$TALLA, noves_talles$DBP, ylim=c(-1,1), main = "Talla vs DBP")
plot (noves_talles$TALLA, noves_talles$PER_CEFAL, ylim=c(-1,1), main = "Talla vs PER_CEFAL")
plot (noves_talles$TALLA, noves_talles$PER_ABDOM, ylim=c(-1,1), main = "Talla vs PER_ABDOM")
plot (noves_talles$TALLA, noves_talles$LF, ylim=c(-1,1), main = "Talla vs LF")

linear_model_PAPP_talla_preagrupar <- lm(PAPP.A..0.2.3.0. ~ TALLA, data = noves_talles)
summary(linear_model_PAPP_talla_preagrupar)

linear_model_hCG_talla_preagrupar <- lm(hCG.0.3.5.0. ~ TALLA, data = noves_talles)
summary(linear_model_hCG_talla_preagrupar)

linear_model_NT_talla_preagrupar <- lm(NT..0.5.2.5. ~ TALLA, data = noves_talles)
summary(linear_model_NT_talla_preagrupar)

linear_model_DBP_talla_preagrupar <- lm(DBP ~ TALLA, data = noves_talles)
summary(linear_model_DBP_talla_preagrupar)

linear_model_PER_CEFAL_talla_preagrupar <- lm(PER_CEFAL ~ TALLA, data = noves_talles)
summary(linear_model_PER_CEFAL_talla_preagrupar)

linear_model_PER_ABDOM_talla_preagrupar <- lm(PER_ABDOM ~ TALLA, data = noves_talles)
summary(linear_model_PER_ABDOM_talla_preagrupar)

linear_model_LF_talla_preagrupar <- lm(LF ~ TALLA, data = noves_talles)
summary(linear_model_LF_talla_preagrupar)

#####################
#IMC
#####################

IMC <- (base_datos_pt$PESO)/(base_datos_pt$TALLA/100)^2

plot (IMC, base_datos_pt$PAPP.A..0.2?..3.0., ylim=c(-1,1), main = "IMC vs PAPP")
plot (IMC, base_datos_pt$hCG.0.3?..5.0., ylim=c(-1,1), main = "IMC vs hCG")
plot (IMC, base_datos_pt$NT..0.5?..2.5., ylim=c(-1,1), main = "IMC vs NT")
plot (IMC, base_datos_pt$DBP, ylim=c(-1,1), main = "IMC vs DBP")
plot (IMC, base_datos_pt$PER_CEFAL, ylim=c(-1,1), main = "IMC vs PER_CEFAL")
plot (IMC, base_datos_pt$PER_ABDOM, ylim=c(-1,1), main = "IMC vs PER_ABDOM")
plot (IMC, base_datos_pt$LF, ylim=c(-1,1), main = "IMC vs LF")

linear_model_PAPP_IMC_preagrupar <- lm(PAPP.A..0.2.3.0. ~ IMC, data = base_datos_pt)
summary(linear_model_PAPP_IMC_preagrupar)

linear_model_hCG_IMC_preagrupar <- lm(hCG.0.3.5.0. ~ IMC, data = base_datos_pt)
summary(linear_model_hCG_IMC_preagrupar)

linear_model_NT_IMC_preagrupar <- lm(NT..0.5.2.5. ~ IMC, data = base_datos_pt)
summary(linear_model_NT_IMC_preagrupar)

linear_model_DBP_IMC_preagrupar <- lm(DBP ~ IMC, data = base_datos_pt)
summary(linear_model_DBP_IMC_preagrupar)

linear_model_PER_CEFAL_IMC_preagrupar <- lm(PER_CEFAL ~ IMC, data = base_datos_pt)
summary(linear_model_PER_CEFAL_IMC_preagrupar)

linear_model_PER_ABDOM_IMC_preagrupar <- lm(PER_ABDOM ~ IMC, data = base_datos_pt)
summary(linear_model_PER_ABDOM_IMC_preagrupar)

linear_model_LF_IMC_preagrupar <- lm(LF ~ IMC, data = base_datos_pt)
summary(linear_model_LF_IMC_preagrupar)