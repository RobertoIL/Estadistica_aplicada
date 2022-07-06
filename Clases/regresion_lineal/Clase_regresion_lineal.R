# Regresión Lineal ----


library(dplyr)
library(ggplot2)
library(GGally)  # ggpairs



test<-read.csv("C:/Users/rober/Documents/R estadistica aplicada/clase5/test.csv")

head(test)

ggpairs(test)

plot(density(test$x))

test %>% 
  ggplot(aes(x=x,y=y))+
           geom_point()

# obtenemos el coeficiente de correlacion lineal "rho"
cor(test)


# Para estimar los Bi esiten varios metodos:
# MCO : minimos cuadrados ordinarios  
# MCO : poderados
# EMV : estimador de maxima verosimilitud


# funcion lm() : linear regression 
# Y = b0 + b1 X
# Y = b1 X 
mod1 <- lm(y ~ x, data = test) # modelo, m1, modelo_1
mod1


mod2 <- lm(y ~ 0 + x, data = test)
mod2

class(lm)
names(mod1)

summary(mod1)
summary(mod2)

# R - squared > 80% esta bien

test %>% 
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x, se = F, col = 'dodgerblue1') +
  geom_smooth(method = 'lm', formula = y~x, se = F, col = 'tomato')

mod1$coefficients
mod1$residuals  # diferencia entre y - y gorro

mod1$fitted.values

# e = y - y barra

test$predicciones <- predict(mod1)

test %>% filter(x<25) %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_smooth(method = 'lm', formula = y~0+x, se = F, color = 'lightgrey') +
  geom_segment(aes(xend=x, yend=predicciones), col = 'red', lty = 'dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col = 'red') +
  theme_light()

nuevo <- data.frame(x=c(23,2,14))

predict(mod2, newdata = nuevo)


# test para los coeficientes beta_i
summary(mod2)
confint(mod2, level = 0.90)
shapiro.test(mod2$residuals)
qqnorm(mod2$residuals)
qqline(mod2$residuals, col = 'red')



 # Actividad 1 ----

car <- read.csv("C:/Users/rober/Documents/R estadistica aplicada/clase5/car_data.csv")

glimpse(car)

ggpairs(car[,c(2,3,4,5,9)])

car %>% 
  ggplot(aes(x=Present_Price, y=Selling_Price)) +
  geom_point() +
  geom_smooth(method = 'lm')

# encontramos una dato atipico (outlier) lo quitaremos

auto <- car %>% filter(Present_Price<75)

auto %>% 
  ggplot(aes(x=Present_Price, y=Selling_Price)) +
  geom_point() +
  geom_smooth(method = 'lm')

m1 <- lm(Selling_Price ~ Present_Price, data = auto)   # y~x
m1

summary(m1)


m2 <- lm(Selling_Price ~ 0 + Present_Price, data = auto)   # y~x
m2
summary(m2)


auto %>%  
  ggplot(aes(x=Present_Price, y=Selling_Price)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~0+x) +
  geom_smooth(method = 'lm', formula = y~x, color = 'tomato')

nuevo <- data.frame(Present_Price = c(24.2,33.1))

predict(m2, nuevo)




# Actividad 2 -----

california <- read.csv("https://download.mlcc.google.com/mledu-datasets/california_housing_train.csv")
head(california)

ggpairs(california[,5:9])


#total_bedrooms y population

california %>% 
  ggplot(aes(x=total_bedrooms, y=population)) +
  geom_point() +
  geom_smooth(method = 'lm')

# correcion de datos atipicos

poblacionCorregida <- california %>% filter(population<20000)

poblacionCorregida %>% 
  ggplot(aes(x=total_bedrooms, y=population)) +
  geom_point() +
  geom_smooth(method = 'lm')

m1 <- lm(total_bedrooms ~ population, data = california)
m1

summary(m1)

m2 <- lm(total_bedrooms ~ 0 + population, data = california)
m2
summary(m2)

california %>% 
  ggplot(aes(x=population, y=total_bedrooms)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~0+x) +
  geom_smooth(method = 'lm', formula = y~x, color = 'tomato')


