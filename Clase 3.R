#En esta clase, se repasará el cluster particionante.
#Para ello, vamos a utilizar la base de datos "Surgery" la cual detalla las características de los pacientes que van a entrar a cirugía
#Entonces, lo primero es cargar la base de datos en SAV a tu rstudio, y luego utilizas los siguientes comandos:

#Cargando la base de datos SAV.
library(foreign) #sirve para poder cargar, y visualizar bases de datos.
data<-read.spss("Cosmetic Surgery_BASE.sav", to.data.frame=TRUE)

#Inspeccionamos la data
str(data)
summary(data)
names(data)

#Por comodidad, vamos a atachear la base
attach(data)

###Estrategia Particionante - No jerárquica ###
#En dicha estrategia, el investigador decide cuántos grupos de clusters va a utilizar.

#Creamos un nuevo elemento
data1<-data.frame(Age, Base_QoL, BDI)

#Visualizamos en un gráfico los posibles grupos de conglomerados. Esto solo como referencia
plot(data1)

#llamamos a la libreria que nos permite hacer los clusters
library(cluster)

# Creamos un nuevo elemento. Usar en C() las columnas de interes:
g.dist = daisy(data1[,c(1:3)], metric="gower")
g.dist #te muestra las distancias entre puntos

#proponiendo la distancia entre los elementos de los conglomerados
pam.resultado=pam(g.dist,4,cluster.only = F)

#incluyendo una nueva variable cluster a la data
data1$clusterPT=pam.resultado$cluster
data1

library(plyr) # para usar la funcion "each" que permite agregar la data a partir del uso de la media por las columnas de interés.

head(data1) #queremos usar las columnas de la 1 a la 3

# nota el uso de as.matrix con cbind:

#Creamos un nuevo elemento AGG 
agg=aggregate(as.matrix(cbind(data1[,c(1:3)]))~ data1$clusterPT, data=data1, FUN=plyr::each(Media = mean))
agg

library(factoextra) #permite visualizar gráficamente

#visualizamos
fviz_cluster(object = list(data1=g.dist, 
            cluster = data1$clusterPT),
             geom = c("text"), 
             ellipse.type = "convex")

library(ggrepel)

fviz_cluster(object = list(data1=g.dist, 
              cluster = data1$clusterPT),
             geom = c("text"), 
             ellipse = FALSE,labelsize = 5,
             repel = T)
