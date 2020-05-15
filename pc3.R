
install.packages('htmltab')
library(htmltab)

lkpage='https://www.cia.gov/library/publications/resources/the-world-factbook/fields/225.html'
lkpath='//*[@id="fieldListing"]'

taxesnrevenues=htmltab(doc=lkpage,which = lkpath)

lkpage2='https://www.cia.gov/library/publications/resources/the-world-factbook/fields/223.html'
lkpath2='//*[@id="fieldListing"]'

income=htmltab(doc=lkpage2,which = lkpath2)

names(taxesnrevenues)
names(taxesnrevenues)=c("país","taxes")

names(income)
names(income)=c("país","income")

table(taxesnrevenues, useNA = 'always')
table(income, useNA = 'always')

install.packages('tidyr')
library(tidyr)
install.packages('dplyr')
library(dplyr)

income=separate(income,income,into=c('income','delete'), "\\(")[,-3]
income$delete=NULL

taxesnrevenues=separate(taxesnrevenues,taxes,into=c('taxes','delete'), "\\%")[,-3]
taxesnrevenues$delete=NULL

taxesnrevenues$taxes=as.numeric(taxesnrevenues$taxes)
income$income=as.numeric(income$income)

str(taxesnrevenues)
str(income)

base_final=merge(taxesnrevenues,income)

install.packages('cluster')
library(cluster)

plot(taxesnrevenues,income)


