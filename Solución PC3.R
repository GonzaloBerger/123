library(htmltab)
library(tidyr)
library(stringr)
library(magrittr)

#Primera Base

url1 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/223.html" 

gini = htmltab(doc = url1, 
              which = '//*[@id="fieldListing"]', #herramientas de desarrollador
              encoding = "UTF-8") 

names(gini)
names(gini)=c("País","Gini")

new_gini=separate(gini,Gini,into=c("Gini",'xtra1'), "[(]")

new_gini$xtra1=NULL

str(new_gini)

new_gini$Gini=as.numeric(new_gini$Gini)

summary(new_gini)

#Segunda Base

url2 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/225.html"

taxes = htmltab(doc = url2, 
               which = '//*[@id="fieldListing"]', #herramientas de desarrollador
               encoding = "UTF-8") 

names(taxes)
names(taxes)=c("País","Taxes")
new_taxes=separate(taxes,Taxes,into=c("Taxes",'xtra1'), "of GDP")

new_taxes$xtra1=NULL

new_taxes$Taxes= replace(new_taxes$Taxes, new_taxes$Taxes == "NA",NA)

new_taxes$Taxes= str_extract_all(new_taxes$Taxes,"\\d+\\.*\\d*")

new_taxes$Taxes=as.numeric(new_taxes$Taxes)

str(new_taxes)

#Merging

final=merge(new_gini,new_taxes,by.x='País', by.y='País') 
head(final)

attach(final)
plot(Gini,Taxes)