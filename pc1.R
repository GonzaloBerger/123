LinkPage='https://en.wikipedia.org/wiki/Index_of_Economic_Freedom'
Linkpath='//*[@id="mw-content-text"]/div/table[4]'
install.packages("htmltab")
library(htmltab)
historicalpositions=htmltab(doc=LinkPage,
                            wich=Linkpath)