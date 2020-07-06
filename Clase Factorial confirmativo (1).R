#Ejercicio de Clase: Analisis Factorial Explorativo y Confirmatorio

#Trayendo bases de datos

library(rio)
linkToData='https://github.com/PsicologiaPUCP/ArchivosDeDatos/raw/master/hsb_ok.sav'
hsb=import(linkToData)
hsb=as.data.frame(hsb)
str(hsb)

####Realizamos el Análisis Factorial Explorativo

#Quitamos las variables que no nos interesan
hsb=hsb[,-c(1,2,3,4,5,6)]

#Explorar correlaciones
library(polycor)
corMatrix=polycor::hetcor(hsb)$correlations

library(ggcorrplot)
ggcorrplot(corMatrix)

#Verificar Factorización
install.packages("psych")
library(psych)
psych::KMO(corMatrix) 
#observamos el OVERALL

#Descartando una posible matriz identidad
cortest.bartlett(corMatrix,n=nrow(hsb))$p.value>0.05

#Descartando una posible matriz singular
library(matrixcalc)
is.singular.matrix(corMatrix)

#Identificar en cuántos grupos se factorizará
fa.parallel(hsb,fm = 'ML', fa = 'fa')

#Resultado mejorado de factorización
library(GPArotation)
resfa=fa(hsb,nfactors = 3,cor = 'mixed',rotate = "varimax",fm="minres")
print(resfa$loadings)

#Gráfico final de la factorización explorativa
fa.diagram(resfa)

#########Análisis Factorial Confirmativo

#Armamos el modelo
names(hsb)
model <- 'depe =~ CAR 
          var1 =~ RDG + MATH + WRTG + SCI + CIV + LOCUS
          var2 =~ CONCPT + MOT'

#dependiente = independiente*A + C
#DEPE <- var1 + var2

#Ahora se pide el análisis factorial confirmatorio
library(lavaan)
cfa_fit <- cfa(model, data=hsb, 
               std.lv=TRUE,  
               missing="fiml")
cfa_fit
# Verificando si cada indicador tiene una buena conexión con su latente 
#Preparamos primero las pruebas
allParamCFA=parameterEstimates(cfa_fit,standardized = T)
allFitCFA=as.list(fitMeasures(cfa_fit))

#Ahora sí vemos el P VALOR
library(knitr)
kable(allParamCFA[allParamCFA$op=="=~",])

#El P_value debe ser mayor a 0.05 para que sea bueno)
allFitCFA[c("chisq", "df", "pvalue")]

#Indice de Tucker
allFitCFA$tli # > 0.90

#Raiz de error cuadrático debe ser menos que 0.05
allFitCFA[c('rmsea.ci.lower','rmsea' ,'rmsea.ci.upper')] # 0.05 en el Int de Conf?
#Los límites no cumplen pues el upper es 0.07

#Añadimos los índices a la data de indicadores:
hsb1=as.data.frame(cbind(hsb,lavPredict(cfa_fit)))
#Veamos un resumen:
summary(hsb1)

######Ahora podemos correr una regresión con los índices:
summary(lm(depe~var1+var2,data = hsb1))

old=apply(hsb1[,c('RDG','MATH','WRTG','SCI','CIV','LOCUS','CONCPT','MOT')],1,mean)
new=hsb1$depe
plot(old,new)

cor(old,new)
