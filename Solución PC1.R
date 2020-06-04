####Problema 1####

linkPage="https://en.wikipedia.org/wiki/Index_of_Economic_Freedom" 
linkPath = '//*[@id="mw-content-text"]/div/table[4]'

library(htmltab)
economic = htmltab(doc = linkPage, 
                     which =linkPath) 

View(economic)

str(economic)

#¿Qué comando se puede realizar para convertir los caracteres en numéricos?

###Problema 2###

miLLAVE = "448270efa5f4c033e0028ae15d3e68d952b85aa0"
GUID = "http://api.datos.agrorural.gob.pe/api/v2/datastreams/PLANE-DE-NEGOC-DE-PROYE/"
FORMATO='data.json/'
request=paste0(GUID,FORMATO,'?auth_key=',miLLAVE)

request

library(jsonlite) 

agrorural = fromJSON(request)

#miralo
agrorural

FORMATO='data.pjson/'
request2=paste0(GUID,FORMATO,'?auth_key=',miLLAVE)

agrorural = fromJSON(request2)

head(agrorural$result)

Parametros='&fDescripcion-de-Linea-del-Negocio=LACTEOS'
request3=paste0(GUID,FORMATO,'?auth_key=',miLLAVE,Parametros)
agronew = fromJSON(request3)$result
