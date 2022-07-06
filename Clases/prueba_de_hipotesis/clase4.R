# instalacion librerias ----
library(readxl)
library(nortest)
library(stests)
library(ggplot2)
library(dplyr)
source("C:/Users/rober/Documents/R estadistica aplicada/clase4/clase_prueba_de_hipotesis/funcionesvar.R")


install.packages('devtools')
library(devtools)

devtools::install_github('fhernanb/stets', force = TRUE)
library(stests)

devtools::install_github('fhernanb/usefultools', force = TRUE)
library(usefultools)

# Base de datos ----
bases <- Placement_Data_Full_Class

head(bases)
dim(bases)
str(bases)

# na.rm = TRUE para quitar espacios en blanco
plot(density(bases$mba_p,na.rm = TRUE))   

lillie.test(bases$mba_p)
# si p value es > 0.05 se acepta H0
# si p value es < 0.05 se acepta H1


# hacemos lista por cada categoria
base_mba <- split(bases$mba_p,bases$degree_t)

# aplicar test a cada lista de datos
lapply(base_mba, lillie.test)

# si usamos shapiro.test

# shapiro conviene usarlo cuando los datos son pocos
lapply(base_mba, shapiro.test)


boxplot(bases$mba_p ~ bases$degree_t)


bases %>% 
  ggplot(aes(degree_p, color=degree_t))+
  geom_density()

dim(bases)
lillie.test(bases$mba_p)

# cuando usemos bases, tomaremos una muestra para comparar
set.seed(2022)
muestra <- sample(bases$mba_p, size = 100, replace = F)
muestra

mean(bases$mba_p)
var(bases$mba_p)


# para la media ----
# H0 mu>65  H1 <=65
t.test(x=muestra, alternative = "less", 
       mu=65, conf.level = 0.95)
# p value es menor que 0.05, por lo tanto, se rechaza H0 y se acepta H1


# para la proporcion ----
prop.test(x = 75, n = 1500,
          conf.level = 0.9, p = 0.07, alternative = "less")
# p value es menor que 0.05, se acepta H1
prop.test(x = 75, n = 1500,
          conf.level = 0.95, p = 0.04, alternative = "two.sided")
# p value es mayor que 0.05 acepto H0

# para la varianza ----
var.test(muestra, alternative = "greater", null.value = 30)
# p value mayor que 0.05, se acepta el H0

var.test(muestra, alternative = "two.sided", null.value = 34)
# p value mayor que 0.05, se acepta el H0

# para el cociente de varianza ----
# trabajamos con base_mba

var(base_mba$`Comm&Mgmt`)
var(base_mba$Others)

var.test(x=base_mba$`Sci&Tech`, y = base_mba$Others,
         alternative = "two.sided", null.value = 1)


sd(base_mba$`Sci&Tech`)
sd(base_mba$Others)

# para la diferencia de medias----
t.test(base_mba$`Sci&Tech`, base_mba$Others, alternative = "two.sided",
       mu = 0, var.equal = TRUE, conf.level = 0.95)
# p value mayor que 0.05, se acepta H0

t.test(base_mba$`Sci&Tech`, base_mba$Others, alternative = "less",
       mu = 7, var.equal = TRUE, conf.level = 0.95)


# con varianzas iguales ----



#para la diferencia de proporciones ----
prop.test(x = c(75, 80), n = c(1500, 2000), conf.level = 0.90,
          alternative = "less", p = c(0.07, 0.06))

# para la diferencia de medias pareadas ----






