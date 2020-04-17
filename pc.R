
miLLAVE='86cf46851c5eb4e0d72296a912c717bad03e6941'
GUID='http://api.datos.agrorural.gob.pe/api/v2/datastreams/PLANE-DE-NEGOC-DE-PROYE/'
FORMATO='data.json/'
request=paste0(GUID,FORMATO,'?auth_key',miLLAVE)
install.packages("jsonlite")
library(jsonlite)
