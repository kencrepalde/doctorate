####Primeiros passos#####
#pacotes necessarios
library(tidyverse)
library(lubridate)

#descobrindo em que pasta esta o diretorio
#o banco tinha celulas vazias que o R ao abrir nao entendia como NA por interpretar
#como classe fator
getwd()
setwd()
peps <- read.csv2("peps.csv",header=TRUE, na.strings=c(""," ","NA"))
names(peps)

####minipep####
minipep <- peps %>% select(id, dtfill, yearfill, bd, stateudm)

#### total by year ####
minipep <- minipep %>% arrange(dtfill) %>% distinct(id, .keep_all = TRUE) %>% as.data.frame(.)
#right

####brazil bank####

minipep$dtfill <- as.Date(minipep$dtfill)

t <- minipep %>%
        mutate(month = format(dtfill, "%m"), year = format(dtfill, "%Y")) %>%
        group_by(month, year) %>%
        summarise(total = n())

popbr<-read.csv2("popbrazil.csv",header=TRUE)
names(popbr)

#I need create region var to join
t$regionudm<-rep(6,108)

#population joining
pepjpbr<-left_join(t, popbr, by="regionudm")

#selecting the correct pop
pepjpbr<- within(pepjpbr, {pop <- ifelse(year=="2011", paste (pop2011), 
                              ifelse(year=="2012", paste (pop2012), 
                              ifelse(year=="2013", paste(pop2013),
                              ifelse(year=="2014", paste(pop2014),
                              ifelse(year=="2015", paste(pop2015),
                              ifelse(year=="2016", paste(pop2016),
                              ifelse(year=="2017", paste(pop2017),
                              ifelse(year=="2018", paste(pop2018),
                              ifelse(year=="2019", paste(pop2019),
                              0)))))))))})
#ok

#excluding unnecessary variables
pepjpbr$pop2011<-NULL
pepjpbr$pop2012<-NULL
pepjpbr$pop2013<-NULL
pepjpbr$pop2014<-NULL
pepjpbr$pop2015<-NULL
pepjpbr$pop2016<-NULL
pepjpbr$pop2017<-NULL
pepjpbr$pop2018<-NULL
pepjpbr$pop2019<-NULL
pepjpbr$regiaoudm<-NULL

#creating prevalence variable
pepjpbr$pop<-as.integer(pepjpbr$pop)
pepjpbr$year<-as.integer(pepjpbr$year)
pepjpbr$total<-as.integer(pepjpbr$total)
pepjpbr$month<-as.integer(pepjpbr$month)

pepjpbr <- pepjpbr %>% mutate(pepmilion = (total/pop)*1000000)

#arranging by month and year (ordering for jp software)
pepjpbr <- pepjpbr %>% arrange(month) %>% arrange(year)
#pasting independent variable jp
pepjpbr$jp<-1:108
class(pepjpbr$jp)

#PEPSE by year
y <- minipep %>%
        mutate(year = format(dtfill, "%y")) %>% #y - 2 number Y - 4 numbers
        group_by(year) %>%
        summarise(total = n())

y$regionudm<-rep(6,9)

pepybr<-left_join(y, popbr, by="regionudm")

pepybr<- within(pepybr, {pop <- ifelse(year=="11", paste (pop2011), 
                                         ifelse(year=="12", paste (pop2012), 
                                                ifelse(year=="13", paste(pop2013),
                                                       ifelse(year=="14", paste(pop2014),
                                                              ifelse(year=="15", paste(pop2015),
                                                                     ifelse(year=="16", paste(pop2016),
                                                                            ifelse(year=="17", paste(pop2017),
                                                                                   ifelse(year=="18", paste(pop2018),
                                                                                          ifelse(year=="19", paste(pop2019),
                                                                                                 0)))))))))})
#excluding unnecessary variables
pepybr$pop2011<-NULL
pepybr$pop2012<-NULL
pepybr$pop2013<-NULL
pepybr$pop2014<-NULL
pepybr$pop2015<-NULL
pepybr$pop2016<-NULL
pepybr$pop2017<-NULL
pepybr$pop2018<-NULL
pepybr$pop2019<-NULL
pepybr$regiaoudm<-NULL

pepybr$pop<-as.integer(pepybr$pop)
pepybr$year<-as.integer(pepybr$year)


#exporting
write.table(pepjpbr, file="pepjpbr.txt", append = FALSE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(pepybr, file="pepybr.txt", append = FALSE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)

####dataset cleaner####
rm(minipep)
rm(pepjpbr)
rm(pepybr)
rm(peps)
rm(popbr)
rm(t)
rm(y)

