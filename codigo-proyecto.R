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
library(broom)
library(lmtest)

datos <- import("insulinoresistencia.csv") %>% as_tibble()


# Analisis exploratorio ----------------------------------------------

str(datos)
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

resumen <- summary(modelo_final)
dato_anova <- anova(modelo_final)

# Error estándar de la regresión.

sqrt(dato_anova$`Sum Sq`[4]/(resumen$df[2]))
resumen$sigma

# Coeficiente de determinación.

resumen$r.squared

# Estadística F y su valor-p

resumen$fstatistic

# Coeficiente de determinación ajustado

resumen$adj.r.squared

# Evaluacion del modelo ---------------------------------------------

# Precisión de los coeficientes 
# Errores estandar por coeficiente

resumen$coefficients[,2] 

#Prueba T

resumen$coefficients[,c(3,4)]

#Intervalos de confianza para los coeficientes.

betas <- resumen$coefficients[,1]

confint(modelo_final) %>% 
  cbind(betas) %>% 
  round(3)


# Verificación de los supuestos -------------------------------------------

# Linealidad en los parámetros.

GGally::ggpairs(datos)

# Todas las variables relevantes están incluídas.

modelo_final |> 
  broom::augment() |> 
  ggplot(aes(.fitted, .resid)) + # vectores ajustados y los residuos
  geom_point(size = 3, alpha = 0.7, col = "magenta")+
  geom_hline(yintercept = 0) +
  labs(title = "Gráfico de vectores ajustados y residuos",
       x = "Valores ajustados",
       y = "Residuos")

modelo_final |> 
  broom::augment() |> 
  filter(col_tot == 298 & LDL == 162.8)

modelo_final |> 
  broom::augment() |>
  filter(col_tot == 144 & LDL == 33) 

modelo_final |> 
  broom::augment() |> 
  filter(col_tot != 298 & LDL != 162.8) %>% 
  filter(col_tot != 144 & LDL != 33) %>% 
  ggplot(aes(.fitted, .resid)) + # vectores ajustados y los residuos
  geom_point(size = 3, alpha = 0.7, col = "magenta")+
  geom_hline(yintercept = 0) +
  labs(title = "Gráfico de vectores ajustados y residuos",
       x = "Valores ajustados",
       y = "Residuos")

# Homocedasticidad

bptest(modelo_final) # esta sobrexplicado
gqtest(modelo_final) # las varianzas difieren

# No autocorrelación 

acf(modelo_final$residuals, col = "#FF0054",
    lwd = 3,xlab = "Residuos",ylab = "Autocorrelación",
    main = "Gráfico de Autocorrelacion para \n los residuos del modelo final")

dwtest(modelo_final)

plot(residuals(modelo_final), pch=19, col="deepskyblue1",
     ylim = c(-10,10))

# Errores normalmente distribuidos

qqnorm(modelo_final$residuals, pch = 1, frame = FALSE, ylim = c(-10,10))
qqline(modelo_final$residuals, col = "steelblue", lwd = 2)

ks.test(modelo_final$residuals, "pnorm")

# No hay multicolinealidad perfecta.

car::vif(modelo_final)

# Valores atípicos

ggplot(datos, aes(col_tot))+
    geom_histogram(bins= 30, alpha = 0.6,fill ="#FF0054",position = "identity")+
    labs(title = "Histograma de la distribución de la Tolerancia al colesterol",
         x = "Tolerancia al colesterol",
         y = "Frecuencia") +
  theme_light()

ggplot(datos, aes( x = col_tot, y = "")) +
  geom_boxplot(fill = "#FF0054", color = "black",outlier.colour= "#FFBD00" ,outlier.size=2) +
  labs(x="Tolerancia al colesterol",
       y= "")+
  ggtitle("Dispersión de la tolerancia al colesterol")+
  coord_flip()+
  theme_bw(base_size = 12)

boxplot.stats(datos$col_tot)$out

out <- boxplot.stats(datos$col_tot)$out
out_ind <- which(datos$col_tot %in% c(out))
out_ind

datos[out_ind,]
