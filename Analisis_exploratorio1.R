#Analisis exploratorio en R

#El siguiente data set trata sobre:
#Datos transversales procedentes de la Encuesta de Población Actual de mayo de 1985 
#realizada por el Censo de los Estados Unidos Oficina 
#(muestra aleatoria extraída para Berndt 1991).

install.packages("AER")
library(AER)

data("CPS1985") #nombre del data set
print(CPS1985) #Mostramos en consola una preview de los datos

str(CPS1985) #Muestra de forma compacta la estructura interna del data set
head(CPS1985) #Muestra los primeros 6 elementos del data set
tail(CPS1985) #Muestra los ultimos 6 elementos del data set

summary(CPS1985) #Muestra un resumen de todas las variables que contiene nuestro data frame 
summary(occupation) #Muestra un resumen de la columa 'occupation' (cualitativa)
summary(age) #Muestra un resumen de la columa 'age' (cuantitativo)
mean(experience) #Muestra la media de una columna (solo se puede con datos numericos)
mean (sector) #Arroja error, ya que son datos cualitativos
min(age) #Retorna el valor minimo de la columna
max(age) #Retorna el valor maximo de de la columna

var(age) #Funcion que nos retorna la varianza de la columna 'age'
sd(age) #Desviacion estandar 

CPS1985$wage #Acceso directo a la columna 'wage' y todos sus datos
CPS1985$wage[19] #Acceso directo a la columna 'wage' en la posicion 19

#Histograma de frecuencia
hist(age, main = "Edades", ylab = "Cantidad de personas", xlab = "Edad" ,col = blues9)

#Crea una tabla de la columna 'Occupation' con la finalidad de comprender 
#la distribucion de categorias dentro de la variable
tab <- table(occupation) 
grafica <- barplot(tab, main = "Puestos de trabajos", col = rainbow(6), 
        ylim = c (0, 160), #Para poner el numero arriba de la grafica
        legend.text = rownames(tab), #Valores de la leyenda
        args.legend = list(x = "topright", inset = c (0.0, 0))) #Argumentos de la leyenda
text(grafica, tab + 2.5, labels = tab)

pie(tab) #grafico de pastel


#Para 2 variables categoricas: Ocupacion y genero

#xtabs permite crear tablas de contingencia y es especialmente útil para datos 
#agrupados y cuando se trabaja con data frames

xtabs(~gender + occupation, data = CPS1985)
plot(gender ~ occupation, data = CPS1985) #Crea una grafica comparativa entre las variables

xtabs(~gender + married, data = CPS1985)
plot(gender ~ married, data = CPS1985)


#Para 2 variables numericas: Salario y educacion

# cor() calcula la correlacion de entre 2 variables
cor(wage, education)

# Calcula la correlacion de pearson
cor(log(wage), education)

# Calcula la correlacion de Spearman
cor(log(wage), education, method = "spearman")

plot(log(wage) ~ education)

#Para una variable numerica y una categorica: Salario y genero

tapply(log(wage), gender, mean) #Media del salario hombres vs mujeres
tapply(log(wage), gender, summary) #Resumen entre ambas variables

#Deshabilitar el acceso directo a los datos
detach(CPS1985)
