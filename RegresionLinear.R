library(tidyverse)
augura <- read_csv("/Users/jesusernestoguevaravillarreal/Desktop/Augura/Augura\ Datos.csv")

head(augura)

names(augura)

augura <- augura %>%
  select(temperaturaSuelo1, humedadAmbiente, temperaturaAmbiente, date, fecha, hora)





### Regresión linear humedad ambiente en relación a temperatura ambiente ### 

ggplot(data = augura, mapping = aes(x = temperaturaAmbiente, y = humedadAmbiente)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)

cor(augura$temperaturaAmbiente, augura$humedadAmbiente)
# r = -0.8102479

### Un día ###

ggplot(data = augura %>%
         filter(fecha == "05-25-2020"), mapping = aes(x = temperaturaAmbiente, y = humedadAmbiente)) +
  geom_point() +
  geom_smooth(method = "lm")




### Regresión linear temperatura ambiente en relación a temperatura ambiente ### 

ggplot(data = augura_no_outlier, 
       mapping = aes(x = temperaturaAmbiente, y = temperaturaSuelo1)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)

cor(augura_no_outlier$temperaturaAmbiente, augura_no_outlier$temperaturaSuelo1)
# r = 0.9762879

### Un día ###

ggplot(data = augura %>%
         filter(fecha == "05-25-2020"), mapping = aes(x = temperaturaAmbiente, y = temperaturaSuelo1)) +
  geom_point() +
  geom_smooth(method = "lm")


# Linear model for weight as a function of height
model <- lm(formula = temperaturaSuelo1 ~ temperaturaAmbiente, data = augura_no_outlier)

# Coefficients:
# (Intercept)  temperaturaAmbiente  
# -2.096                1.019  

# temperaturaSuelo1 = -2.096 + 1.019 * temperaturaAmbiente

-2.096 + 1.019 * 25

augura_prediccion <- augura_no_outlier %>%
  mutate(tempsuelo_pred = -2.096 + 1.019 * temperaturaAmbiente)

View(augura_prediccion)

predice_temp_suelo <- function(x) {
  -2.096 + 1.019 * x
} ###### Función que construí para la predicción.

predice_temp_suelo(50)

library(broom)
View(augment(model))

nuevos_datos <- data.frame(temperaturaAmbiente = 50) 
###### Dataframe con datos a predecir

predict(model, newdata = nuevos_datos)
###### Función predeterminada para la predicción.

broom::augment(model, newdata = nuevos_datos)
###### Mismo resultado que predict pero también arroja los valores de la 
###### variable que nos permitirán hacer la predicción.


# R-squared = el nivel de la variabilidad de la variable dependiente (response) se explica
# 95% por la variable independiente (explanatory).


