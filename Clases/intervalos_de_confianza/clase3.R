# Clase Intervalos

library(dplyr)
library(ggplot2)
library(nortest) # instalar


cangrejos <- read.table(file='https://raw.githubusercontent.com/fhernanb/datos/master/crab', header=T)

head(cangrejos)

# probemos la normalidad de los datos

cangrejos  %>% 
  ggplot(aes(W,  color=factor(C))) +
  geom_density() 


# Test: 
# 
#  Shapiro-Wilk : shapiro.test()   # menos de 5000 datos
#  Anderson-Darling : ad.test() del paquete nortest.
#  Lilliefors (Kolmogorov-Smirnov) : lillie.test() del paquete nortest.
# 
# donde H0: datos provienen de una dist normal vs H1: No son normalmente distribuidos.
# aceptamos la normalidad si p-value es mayor o igual a 0.05 o el considerado.

# todos los datos, sin separar por categorias de "C"
shapiro.test(cangrejos$W)

cangrejos  %>% 
  ggplot(aes(W)) +
  geom_density()

# dividiremos los datos segun C

# split(x,y)  funcion que divide x por categorias de y
pesos <- split(cangrejos$W,cangrejos$C) # listas

shapiro.test(pesos$`4`)


# Como los datos provienen de distribucion normal, no tenemos problemas, en caso de no ser
# normales, podemos usar igual las siguientes funciones si n (tamaño muestra) es >30


#  para la media ----


resultado = t.test(x=pesos$`1`, conf.level=0.90)

resultado$conf.int


# aplciar funciones a listas: lapply(datos, funcion) # datos en listas

lapply(pesos,t.test,conf.level=0.90)


#  para diferencia de medias ----

(t.test(x=pesos$`1`, conf.level=0.90))$conf.int
(t.test(x=pesos$`2`, conf.level=0.90))$conf.int


t.test(x=pesos$`1`, pesos$`2`, conf.level=0.90)
# como el valor 0 pertenece al intervalo, a un 90% de confianza
# las medias de los grupos podrian ser iguales.

t.test(x=pesos$`1`, pesos$`3`, conf.level=0.90)

# como el valor 0 NO pertenece al intervalo, a un 90% de confianza
# las medias de los grupos NO SON iguales.

t.test(x=pesos$`3`, pesos$`1`, conf.level=0.90)

t.test(x=pesos$`4`, pesos$`3`, conf.level=0.90)

# se pueden hacer todos los pares de grupos...

#  para diferencia de medias pareadas ----

#(-)
perro = 5 # almacena en el objeto perro el valor 5
perro # muestre lo que hay en el objeto perro equivalente a print()

#(-)

antes = pesos$`1`
length(antes)
set.seed(2022)
despues = round(rnorm(12,24,2),1)
despues

t.test(x=antes,y=despues,paired = T, conf.level=0.90)

#   para la media unilateral ----

t.test(pesos$`3`, alternative = "greater", conf.level = 0.90)$conf.int
t.test(pesos$`3`, alternative = "less", conf.level = 0.90)$conf.int

t.test(pesos$`3`, alternative = "two.sided", conf.level = 0.90)$conf.int

# conclusion?

#  para la varianza ----

install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
library(stests)

# https://github.com/fhernanb/stests/blob/master/R/var_test.R


#  stats::var.test() para 1 pob
# stests::var.test() para 2 pob

stests::var.test(x=cangrejos$W, conf.level=0.95)

#  para la razon de la varianza de 2 poblaciones ----
stests::var.test(x=antes,y=despues, conf.level=0.95)

# como  en el intervalo de la razon de varianzas esta contenido el valor
# 1, a un 95% la varianzas son/pueden ser iguales.

stests::var.test(x=pesos$`3`,y=pesos$`4`, conf.level=0.95)

boxplot(cangrejos$W ~ cangrejos$C)


# para proporciones y diferencia de proporciones prop.test ----


# si de cada 1500 celulares 75 vienen con defecto, ¿cual es la verdadera proporcion
# de celulares defectuosos a un 90% de confianza?

prop.test(x = 75, n = 1500 ,conf.level = 0.9)

# conclusion?


# luego de unos ajustes en la produccion de celulares se tuvieron los siguientes resultados:
# de 2000 celulares 80 vienen con defecto, ¿cual es la verdadera proporcion
# de la diferencia de proporcion de celulares defectuosos a un 90% de confianza?

prop.test(x = c(75,80), n=c(1500,2000) ,conf.level = 0.90)

# como el 0 pertenece al IC, p1-p2 puede ser 0, entonces podria ser que p1=p2 a un
# 90% de confianza



# EJERCICIO: 
# realizar calculos sobre intervalos definiendo condiciones para poder aplicar lo visto
# anteriormente.

temp <-  read.table("https://www.ics.uci.edu/~babaks/BWR/Home_files/BodyTemperature.txt",header = T)
temp

head(temp)




plot(density(temp$Temperature))



temperatura <- split(temp$Temperature,temp$Gender)


temp  %>% 
  ggplot(aes(Temperature,  color=factor(Gender))) +
  geom_density() + theme_void()

shapiro.test(temperatura$F)
shapiro.test(temperatura$M)








