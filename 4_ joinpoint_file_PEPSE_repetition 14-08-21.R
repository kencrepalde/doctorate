####Primeiros passos#####
#pacotes necessarios
library(tidyverse)
library(lubridate)

#descobrindo em que pasta esta o diretorio
#o banco tinha celulas vazias que o R ao abrir nao entendia como NA por interpretar
#como classe fator
getwd()
setwd("C:/Users/kencr/Dropbox/Tese Kennedy/Análises/Artigo 1/Projeto R")
pepsu <- read.csv2("pepsu.csv",header=TRUE, na.strings=c(""," ","NA"))
names(pepsu)

####minirep####
minirep <- pepsu %>% select(id, dtfill, index, bd, repyn)


#calc total rep by year
rept <- minirep %>% filter(repyn == 1 & index=="2011") %>% 
        summarize(rep = n()) %>% as.data.frame(.)
rept <- minirep %>% filter(repyn == 1 & index=="2012") %>% 
        summarize(rep = n()) %>% union(rept,.) 
rept<-  minirep %>% filter(repyn == 1 & index=="2013") %>% 
        summarize(rep = n()) %>% union(rept,.)
rept<-  minirep %>% filter(repyn == 1 & index=="2014") %>% 
        summarize(rep = n()) %>% union(rept,.)
rept<-  minirep %>% filter(repyn == 1 & index=="2015") %>% 
        summarize(rep = n()) %>% union(rept,.)
rept<-  minirep %>% filter(repyn == 1 & index=="2016") %>% 
        summarize(rep = n()) %>% union(rept,.)
rept<-  minirep %>% filter(repyn == 1 & index=="2017") %>% 
        summarize(rep = n()) %>% union(rept,.)
rept<-  minirep %>% filter(repyn == 1 & index=="2018") %>% 
        summarize(rep = n()) %>% union(rept,.)
        
total <- minirep %>% group_by(index) %>% summarize(yeartotal = n()) %>% as.data.frame(.)

total <- bind_cols(total, rept)
total <- total %>% mutate(rep100 = (rep/yeartotal)*100)


#exporting
write.table(total, file="repjpbr.txt", append = FALSE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)

####dataset cleaning####
rm(minirep)
rm(pepsu)
rm(total)
rm(rept)

