#2.0
library(readxl)
library(DT)
library(ggplot2)
library(agricolae)


list.files()

data1 <- read_excel("poblacion1.xlsx",sheet=1,col_names = TRUE,na = "")
View(data1)
str(data1)
dim(data1)


#"por tanto existen 4 columnas con 44 filas en poblacion1"

data2 <- read_excel("poblacion2.xlsx",sheet=1,col_names = TRUE,na = "")
View(data2)
str(data2)
dim(data2)

#"por tanto existen 7 columnas con 40 filas en poblacion2"
#debemos recordar que la columna con nombre identificador está en poblacion1 y en poblacion2

#2.1
poblacion <- merge(x = data1 ,y = data2, by = "identificador", suffixes = c("","")) 
View(poblacion)
str(poblacion)
names(poblacion)
dim(poblacion)

#"por tanto existen 10 columnas con 40 filas en poblacion2"
#esto es asi puesto que una columna es comun en poblacion1 y en poblacion2 ademas de que existen 40 filas en la data mas pequeña

#2.2
#apply(poblacion,2,class) deberia funcionar, pero no lo hace, no encuentro el problema...

cl <- c(1:ncol(poblacion))


for (i in 1:ncol(poblacion)){
  cl[i] <- class(poblacion[,i])
}

cl


for(i in 1:ncol(poblacion)) 
  {
  if(is.numeric(poblacion[,i]))
    {
    boxplot(poblacion[,i], xlab = "", ylab=names(poblacion)[i], main=paste("Diag cajas de",names(poblacion)[i]),
            col="steelblue", border="gray1") 
  }
  if(is.character(poblacion[,i]))
    {
    barplot(table(poblacion[,i]), xlab = names(poblacion)[i], ylab="Frecuencia", main="Diagrama de barras",
            col="steelblue", border="gray1")  
  }
}

#en i igual a 1 no se tiene ningun sentido ya que son los codigos unicos, por tanto podriamos usar
#for(i in 2:ncol(poblacion))


#2.3

install.packages("agricolae", dependencies=TRUE)


for(i in 1:ncol(poblacion))
  {
  if(is.numeric(poblacion[,i]))
    {
    print( paste("Para ",names(poblacion)[i],"   El minimo es de ",min(poblacion[,i]),
    "     El maximo es de ",max(poblacion[,i]),"    La media es de ",mean(poblacion[,i]),
    "     La desviacion estandar es de ", sd(poblacion[,i]),"    Y el primer cuartil es ",
    quantile(poblacion[,i], probs = seq(0, 1, 0.25), na.rm = FALSE)))
  }
  if(is.character(poblacion[,i]))
    {
    print(paste("Las frecuencias de ",names(poblacion)[i],"son representadas así:"))
    print(table(poblacion[,i]))
  }
}



#en i igual a 1 no se tiene ningun sentido ya que son los codigos unicos, por tanto podriamos usar
#for(i in 2:ncol(poblacion))



#2.4 

co <- c(1:ncol(poblacion))

for(i in 1:ncol(poblacion))
  {
  if(is.numeric(poblacion[,i]))
    {
    co[i]<-cor(poblacion$poblacion,poblacion[,i])
    
  }
  if(is.character(poblacion[,i]))
  {
    co[i]<-0
  }
  
}
co


#en i igual a 1 no se tiene ningun sentido ya que son los codigos unicos,
#en i igual a 2 se tiene a poblacion por tanto podriamos usar
#for(i in 3:ncol(poblacion))





#2.5

x <- c(1:dim(poblacion)[1])
y <- c(1:dim(poblacion)[1])

for(i in 1: dim(poblacion)[1])
{
  
  x[i] <- 0
  
  if(poblacion[i,10]=="SI")
  {
  
  y[i] <- 1
  
  }
  else
  {
    y[i] <- 0
  }
  
  }
  
  
  
t.test(x,y, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.90)


#los datos con "si y "no" no funcionavan bien, por tanto se tiene que
#0 es no y 1 es si, donde x es un vector con no en cada entrada y y representa la variable
#serv.bas.compl, la prueba dice que las medias son distintas


#2.6

k=0
for (i in 1:ncol(poblacion))
{
  
  if (names(poblacion)[i]=="poblacion")
  {
    
  }
  else
  {
    
  if (abs(co[i]) > 0.35)
  {
    k=k+1
  }
  }
  }


nm <- c(1:k)
num <- c(1:k)

j=1

for (i in 1:ncol(poblacion))
{
  
  if (names(poblacion)[i]=="poblacion")
  {
    
  }
  else
  {
    
    if (abs(co[i]) > 0.35)
    {
      
      num[j] <- i
      nm[j] <- names(poblacion)[i]
      j=j+1
    }
  }
}



g <- ggplot(data = NULL, aes(x=poblacion[,2], y=poblacion[,num[1]]))
g + geom_point() + geom_smooth(method="lm")


g <- ggplot(data = NULL, aes(x=poblacion[,2], y=poblacion[,num[2]]))
g + geom_point() + geom_smooth(method="lm")





reg <- lm(poblacion[,2]~poblacion[,num[1]]+poblacion[,num[2]], NULL)
summary(reg)


#No es muy recomendable usar la variable 1 ya que son codigos y pueden cambiar o podrian tener problemas
#por la forma en que esta hecho el ejercicio se debe usar puesto que si no se lo usa no se tiene regresion
#las demas variables no nos sirben asi que omitire lo antes dicho y la usare



#2.7

#El modelo de regresión lineal obtenido explica el 
100*summary(reg)$r.squared
#por ciento de la variabilidad total._


#2.8

#Para B1 Si

abs(summary(reg)$coefficients[[7]])

#es mayor que

qt(0.975,(nrow(poblacion)-2))
  
#entonces rechazo B1 igual a 0
#en este caso se rechaza




#Para B2 Si

abs(summary(reg)$coefficients[[8]])

#es mayor que

qt(0.975,(nrow(poblacion)-2))

#entonces rechazo B2 igual a 0
#en este caso se rechaza





#Para B3 Si

abs(summary(reg)$coefficients[[9]])

#es mayor que

qt(0.975,(nrow(poblacion)-2))

#entonces rechazo B1 igual a 0
#en este caso no se rechaza



#Realizando la tabla __ANOVA__ tenemos los siguientes resultados:

anova <- aov(reg)
summary(anova)[[1]]


#Se tiene que si

abs(summary(anova)[[1]][1,4])

    #es mayor que

    qf(0.95,1,(nrow(poblacion)-2)) 

#rechazo que el modelo no sea significante

    
    
    
#2.9
    
    residuo <- reg[["residuals"]]
    prediccion <- reg[["fitted.values"]]
    data2 <- data.frame(poblacion, prediccion,residuo)
    datatable(data2,filter="top", options = list(
    searching = TRUE,
    pageLength = 5,
    lengthMenu = c(5, 10, 15)))
    hist(residuo,15)
    mean(residuo)
    qqnorm(residuo)
    qqline(residuo,col="red")
    plot(residuo,prediccion)
    plot(residuo,poblacion[,num[1]])
    plot(residuo,poblacion[,num[2]])
    