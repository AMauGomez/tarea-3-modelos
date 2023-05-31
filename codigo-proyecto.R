# Tarea 01 -----
# 
# Modelos no parametricos y de Regresión
# 
# Francisca Vilca Sanchez

# PAQUETES A USAR

library(rio)
library(dplyr)
library(beepr)
library(ggplot2)
library(ggrepel)
library(kableExtra)
library(GGally)
library(corrplot)

datos <- import("insulinoresistencia.csv") %>% as_tibble()


# Analisis exploratorio ----------------------------------------------

View(datos)

GGally::ggpairs(datos)

corrplot::corrplot(cor(datos),method = "ellipse")

datos %>% 
  correlation::correlation()%>%
  as_tibble() %>%
  View()


# Ajuste o estimacioon del modelo ----------------------------------------------

modelo_full <- lm(col_tot~. ,datos )
summary(modelo_full)

modelo_nulo <- lm(col_tot ~ 1, datos)
summary(modelo_nulo)

# Paso 1

tabla1_add1 <- add1(
  modelo_nulo, 
  test  = "F",
  scope = formula(modelo_full)
)

tabla1_add1

modelo_add1 <- lm(
  col_tot ~ LDL, 
  datos
)
summary(modelo_add1)

# Paso 2

tabla2_add1 <- add1(
  modelo_add1, 
  test  = "F",
  scope = formula(modelo_full)
)

tabla2_add1

modelo_add2 <- lm(
  col_tot ~ LDL + Trigliceridos, 
  datos
)
summary(modelo_add2)

# Paso 3

tabla3_add1 <- add1(
  modelo_add2, 
  test  = "F",
  scope = formula(modelo_full)
)

tabla3_add1

modelo_add3 <- lm(
  col_tot ~ LDL + Trigliceridos + HDL, 
  datos
)
summary(modelo_add3)

# Paso 4

tabla4_add1 <- add1(
  modelo_add3, 
  test  = "F",
  scope = formula(modelo_full)
)

tabla4_add1

# no hay más variables significativas

modelo_final <- modelo_add3

# Verificacion del modelo ----------------------------------------------



# Evaluacion del modelo ---------------------------------------------



