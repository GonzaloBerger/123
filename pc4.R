
install.packages('rio')
library(rio)

install.packages('cluster')
library(cluster)		

install.packages('plyr')
library(plyr)		

install.packages('knitr')
library(knitr)	

install.packages('kableExtra')
library(kableExtra)		

install.packages('factoextra')
library(factoextra)	

install.packages('tidyr')
library(tidyr)

lkdata='https://github.com/GonzaloBerger/123/raw/master/Data%20EconoFreedom%20(1).xlsx'
dataexcel=import(lkdata)

dataexcel[,c(1,3:6)]=NULL

table(dataexcel$`Country Name`, useNA = 'always')
table(dataexcel$`Property Rights`, useNA = 'always')
table(dataexcel$`Judical Effectiveness`, useNA = 'always')
table(dataexcel$`Government Integrity`, useNA = 'always')
table(dataexcel$`Tax Burden`, useNA = 'always')
table(dataexcel$`Gov't Spending`, useNA = 'always')
table(dataexcel$`Fiscal Health`, useNA = 'always')
table(dataexcel$`Business Freedom`, useNA = 'always')
table(dataexcel$`Labor Freedom`, useNA = 'always')
table(dataexcel$`Monetary Freedom`, useNA = 'always')
table(dataexcel$`Trade Freedom`, useNA = 'always')
table(dataexcel$`Investment Freedom`, useNA = 'always')
table(dataexcel$`Financial Freedom`, useNA = 'always')

dataexcel=na.omit(dataexcel)

str(dataexcel)

dataexcel$`Property Rights`=as.numeric(dataexcel$`Property Rights`)
dataexcel$`Judical Effectiveness`=as.numeric(dataexcel$`Judical Effectiveness`)
dataexcel$`Government Integrity`=as.numeric(dataexcel$`Government Integrity`)
dataexcel$`Tax Burden`=as.numeric(dataexcel$`Tax Burden`)
dataexcel$`Gov't Spending`=as.numeric(dataexcel$`Gov't Spending`)
dataexcel$`Fiscal Health`=as.numeric(dataexcel$`Fiscal Health`)
dataexcel$`Business Freedom`=as.numeric(dataexcel$`Business Freedom`)
dataexcel$`Labor Freedom`=as.numeric(dataexcel$`Labor Freedom`)
dataexcel$`Monetary Freedom`=as.numeric(dataexcel$`Monetary Freedom`)
dataexcel$`Trade Freedom`=as.numeric(dataexcel$`Trade Freedom`)
dataexcel$`Investment Freedom`=as.numeric(dataexcel$`Investment Freedom`)
dataexcel$`Financial Freedom`=as.numeric(dataexcel$`Financial Freedom`)

str(dataexcel)

row.names(dataexcel)=dataexcel$`Country Name`

g.dist=daisy(dataexcel[,c(2:13)],metric = 'gower')

pam.resultado=pam(g.dist,4,cluster.only = F)

dataexcel$clusterPT=pam.resultado$cluster

aggregate(as.matrix(cbind(dataexcel[,c(2:13)]))~ clusterPT, data=dataexcel, FUN=plyr::each(MD = median, Media = mean))

agg=aggregate(as.matrix(cbind(dataexcel[,c(2:13)]))~ clusterPT, data=dataexcel, FUN=plyr::each(MD = median, Media = mean))

tablaResumen=t(as.data.frame(agg))

kable(tablaResumen, format = "html", digits = 2)%>%
  kable_styling()

fviz_cluster(object = list(data=g.dist, cluster = dataexcel$clusterPT),
             geom = c('text'), 
             ellipse.type = 'convex')

#A) No es óptimo socilicitar 4 clusters, porque hay traslape en tres de los cuatro clusters.
#B) Este método de conglomerados no es ideal, porque hay valores atípicos. Una mejor alternativa es la técnica de jerarquización.
