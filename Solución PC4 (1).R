##Solucionario
  
#Utilizar la data "2019 ranking with more attributes" del siguiente link: 
#Index of Economic Freedom

#Cargando la data

library(rio)
EcoFree="https://github.com/jcgcjuan/Magallanes-Clases-/raw/master/Data%20EconoFreedom.xlsx"
EcoFree2019=import(EcoFree)
names(EcoFree2019)

EcoFree2019[,c(1,3,4,5,6)] = NULL

EcoFree2019[,c(2:13)]= replace(EcoFree2019[,c(2:13)], 
                               EcoFree2019[,c(2:13)] == "N/A",NA)


View(EcoFree2019)

str(EcoFree2019)

EcoFree2019[,c(2:13)]=lapply(EcoFree2019[,c(2:13)],as.numeric)

str(EcoFree2019)

EcoFree2019=na.omit(EcoFree2019)

library(cluster)
g.dist = daisy(EcoFree2019[,c(2:13)], metric="gower")

pam.resultado=pam(g.dist,4,cluster.only = F)

EcoFree2019$clusterPT=pam.resultado$cluster

library(plyr)

agg=aggregate(as.matrix(cbind(EcoFree2019[,c(2:13)]))~ clusterPT, data=EcoFree2019,
              FUN=plyr::each(MD = median, Media = mean))
agg

library(factoextra)
fviz_cluster(object = list(data=g.dist, cluster = EcoFree2019$clusterPT),
             geom = c("text"), 
             ellipse.type = "convex")

library(ggrepel)

fviz_cluster(object = list(data=g.dist, cluster = EcoFree2019$clusterPT),
             geom = c("text"), 
             ellipse = FALSE,labelsize = 5,
             repel = T)