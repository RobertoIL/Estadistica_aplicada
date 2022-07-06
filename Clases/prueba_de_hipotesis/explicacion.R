# PRUEBAS DE HIPOTESIS

# Los argumentos a definir dentro de t.test para hacer la prueba son:
#   
# x: vector numérico con los datos.
# alternative: tipo de hipótesis alternativa posibilidades 
# "two.sided" cuando ≠ , 
# "less" para el caso <   
# "greater" para  >
# mu: valor de referencia  o mu_0.
# conf.level: nivel de confianza para reportar el intervalo de confianza asociado (opcional).

# en prop.test
# p: valor de referencia de la prueba, p_0.
# correct: valor lógico para indicar si se usa la corrección de Yates. usamos FALSE

# en var.test 
# null.value: valor de referencia de la prueba. sigma2_0
# install.packages('devtools')
# devtools::install_github('fhernanb/stests', force=TRUE)
# devtools::install_github('fhernanb/usefultools', force=TRUE)

bases<-read.csv("~/R estadistica aplicada/clase4/Placement_Data_Full_Class.csv",dec=".")
head(bases)
dim(bases)
str(bases)



# variables:

# ssc_p           : Secondary Education percentage- 10th Grade
# ssc_b           : Board of Education- Central/ Others
# hsc_p           : Higher Secondary Education percentage- 12th Grade
# hsc_b           : Board of Education- Central/ Others
# hsc_s           : Specialization in Higher Secondary Education
# degree_p        : Degree Percentage
# degree_t        : Under Graduation(Degree type)- Field of degree education
# workex          : Work Experience
# etest_p         : Employability test percentage ( conducted by college)
# specialisation  : Post Graduation(MBA)- Specialization
# mba_p           : MBA percentage
# status          : Status of placement- Placed/Not placed
# salary          : Salary offered by corporate to candidates

