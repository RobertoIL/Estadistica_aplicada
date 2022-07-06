# EJERCICIO: 
# realizar calculos sobre intervalos definiendo condiciones para poder aplicar lo visto
# anteriormente.

temp <-  read.table("https://www.ics.uci.edu/~babaks/BWR/Home_files/BodyTemperature.txt",header = T)
temp

head(temp)

plot(density(temp$Temperature))

temperatura = split(temp$Temperature, temp$Gender)

# test de normalidad ----

temp  %>% 
  ggplot(aes(Temperature, color=factor(Gender))) +
  geom_density() + theme_minimal()

shapiro.test(temperatura$F)
shapiro.test(temperatura$M)
# Aceptamos las normalidad, porque p value >= 0.05






         