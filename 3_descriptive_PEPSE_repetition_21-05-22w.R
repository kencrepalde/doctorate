####First Steps#####
#Packages
library(tidyverse)
library(lubridate)
library(summarytools)
library(compareGroups)
library(maditr)

#Discovring work diretory
getwd()
setwd("C:/Users/kencr/Dropbox/Tese Kennedy/Análises/Artigo 1/Projeto R")

#loading
peps<-read.csv2("peps.csv",header=TRUE,sep=";", dec=",", na.strings=c(""," ","NA"))

#names
as.data.frame(names(peps))
n_distinct(peps$id)
#ok

#### first date fill ####
psex <- peps %>% select(id, dtfill, exp, yearfill, bd)
#Dates ordering, because it's important
psex <- psex %>% arrange(dtfill)
psex <- dcast(setDT(psex), id~paste0("dtfills",
                                     rowid(id)), value.var = "dtfill")

#reordering... variables are out of order
psex <- psex[,c("id", "dtfills1", "dtfills2", "dtfills3", "dtfills4", 
                "dtfills5", "dtfills6", "dtfills7", "dtfills8", "dtfills9", 
                "dtfills10", "dtfills11", "dtfills12", "dtfills13", "dtfills14", 
                "dtfills15", "dtfills16", "dtfills17", "dtfills18", "dtfills19", 
                "dtfills20", "dtfills21", "dtfills22", "dtfills23", "dtfills24", 
                "dtfills25", "dtfills26", "dtfills27", "dtfills28", "dtfills29", 
                "dtfills30", "dtfills31", "dtfills32", "dtfills33", "dtfills34", 
                "dtfills35", "dtfills36", "dtfills37", "dtfills38", "dtfills39")]

psex$index<- year(psex$dtfills1)

table(psex$index)
#ok

peps <- psex %>% select (id, dtfills1, index) %>% left_join(peps, ., by="id")

summary(as.Date(peps$dtfills1))
summary(is.na(peps$dtfills1))
summary(is.na(peps$index))

#ok

##### creating 1y date ####
class(peps$dtfills1)
peps$dtfills1 <- as.Date(peps$dtfills1)
peps$y1 <- peps$dtfills1 + days(366)
summary(peps$y1)

#### creating included and excluded obs ####
peps$y1less <- if_else(peps$dtfill < peps$y1, 1, 2)
summary(as.factor(peps$y1less))

reps <- peps %>% filter (y1less == 1)

#### repetition through 1y variable ####

reps$seq <- ave(reps$id, reps$id, FUN=seq_along)

freq(reps$seq)

# max of repetition during 1y
k <- reps %>% arrange (desc(seq)) %>% distinct(id, .keep_all = TRUE) %>%
        mutate (repmax = seq)

reps <- k %>% select (id, repmax) %>% left_join(peps, ., by="id")

reps %>% filter (repmax > 1) %>% descr(repmax)

#It's important to make the rep var - 2 yes, 1 no
reps$repyn<- if_else(reps$repmax>1,2,1)
table(as.factor(reps$repyn))

#right
table(is.na(reps$repyn))
#ok

rm(k)

#### descriptive - sociodemographic and medicines dispensing units vars ####
#classes

names(reps)
class(reps$genital)
reps$genital <- as.factor(reps$genital)
class(reps$genre)
reps$genre <- as.factor(reps$genre)
class(reps$sorient)
reps$sorient <- as.factor(reps$sorient)
class(reps$alcdrug)
reps$alcdrug <- as.factor(reps$alcdrug)
class(reps$monsex)
reps$monsex <- as.factor(reps$monsex)
class(reps$exptimecat)
reps$exptimecat <- as.factor(reps$exptimecat)
class(reps$agefillcat)
reps$agefillcat <- as.factor(reps$agefillcat)
class(reps$age1fillcat)
reps$age1fillcat <- as.factor(reps$age1fillcat)
class(reps$yearfill)
reps$yearfill <- as.factor(reps$yearfill)
class(reps$repyn)
reps$repyn <- as.factor(reps$repyn)
class(reps$week)
reps$week <- as.factor(reps$week)
class(reps$weekcat)
reps$weekcat <- as.factor(reps$weekcat)

#### brazillian region ####

#creating varible about region
reg <- read.csv2("pop.csv",header=TRUE,sep=";", dec=",", na.strings=c(""," ","NA"))
reg <- reg %>% select(ï..UF, regionudm)
reps<- left_join(reps, reg, by=c("stateudm" = "ï..UF"))
rm(reg)

reps$regionudm <- as.factor(reps$regionudm)
freq(reps$regionudm)

#### cities ####
reps$pop18cat <- reps$pop18
reps$pop18cat <- as.integer(reps$pop18cat)
reps$pop18cat[reps$pop18cat<100001] <- 1
reps$pop18cat[reps$pop18cat>100000 & reps$pop18cat<500001] <- 2
reps$pop18cat[reps$pop18cat>500000] <- 3

summary(as.factor(reps$pop18cat))

reps$pop18cat <- as.factor(reps$pop18cat)

#### caseload ####
class(reps$udm2018)
reps %>% filter (yearfill=="2018") %>% distinct (udm, .keep_all = TRUE) %>% 
        select (udm2018) %>% quantile(., probs = c(0.9), na.rm=T)
#154.6
reps$udm2018int <- reps$udm2018
reps$udm2018int[reps$udm2018int>154.5] <- 22222
reps$udm2018int[reps$udm2018int<155.6] <- 11111

freq(reps$udm2018int)

#there are NA because we have UDM that didn't have case in 2018

#creating

reps$udm2018int <- as.factor(reps$udm2018int)

#### counting NA ####
#recategorizing travesti as women trans
reps$genre <- recode(reps$genre, "Homem" = "Cis Men",
                     "Homem Transexual"  = "Trans Men",
                     "Mulher" = "Cis Women",
                     "Mulher Transexual"  = "Trans Women",
"Travesti / Mulher Travesti" = "Trans Women")
reps$genreNA <- as.character(reps$genre)
reps$genreNA[is.na(reps$genreNA)] <- "Missing" 
reps$genreNA <- as.factor(reps$genreNA)

table(reps$genreNA)
table(reps$genre)


reps$sorientNA <- as.character(reps$sorient)
reps$sorientNA[is.na(reps$sorientNA)] <- "Missing" 
reps$sorientNA <- as.factor(reps$sorientNA)
table(reps$sorientNA)
table(reps$sorient)

reps$alcdrugNA <- as.character(reps$alcdrug)
reps$alcdrugNA[is.na(reps$alcdrugNA)] <- "Missing" 
reps$alcdrugNA <- as.factor(reps$alcdrugNA)
table(reps$alcdrugNA)
table(reps$alcdrug)

reps$monsexNA <- as.character(reps$monsex)
reps$monsexNA[is.na(reps$monsexNA)] <- "Missing" 
reps$monsexNA <- as.factor(reps$monsexNA)
table(reps$monsexNA)
table(reps$monsex)

####deduplicating####
reps$index <- as.numeric(reps$index)
summary(reps$index)
r2018 <- reps %>% filter(index==2018) %>% arrange(dtfill) %>% distinct(id, .keep_all = TRUE)
pepsu <- reps %>% filter(index<2019) %>% arrange(dtfill) %>% distinct(id, .keep_all = TRUE)

table(r2018$repyn)
#3,867
table(pepsu$repyn)
#10,012
table(r2018$index)
table(pepsu$index)

#ok

#counting
pepsu %>% filter (repmax>1) %>% descr(repmax)
#mean 2,29 median 2 q3 2 max 13
pepsu %>% filter (repmax>2) %>% n_distinct()
#1,871 repeated more than twice
pepsu %>% filter (repmax==2) %>% n_distinct()
#8,141 repeated twice
pepsu %>% filter (repmax>1) %>% n_distinct()
#10,012

#time between two repeated PEP
psex$tmfills12 <- as.numeric(round(difftime(as.Date(psex$dtfills2), as.Date(psex$dtfills1), units="days"), digits=1))
pepsu <- psex %>% select(id, tmfills12) %>% left_join(pepsu, .)
pepsu %>% filter (repmax>1) %>% descr(tmfills12)

names(pepsu)

# counting people less than 50 years

summary(r2018$age1fill < 50)
#44,334 less than 50, so 95,9%

#### comparegroups ####

export2word(createTable(compareGroups(repyn ~ age1fillcat + genreNA + 
                                        sorientNA + alcdrugNA + monsexNA +  
                                        exptimecat + weekcat + pop18cat + 
                                        udm2018int + week + regionudm,
                                      data = r2018, method=3, byrow = T), 
                        show.all = TRUE, show.ci = TRUE,  digits = 1), 
            "tsocio18row2.docx")


#absolute numbers
export2word(createTable(compareGroups(repyn ~ age1fillcat + genreNA + 
                                        sorientNA + alcdrugNA + monsexNA +  
                                        exptimecat + weekcat + pop18cat + 
                                        udm2018int + regionudm + week,
                                      data = r2018, method=3, byrow = T), 
                        show.all = TRUE, digits = 1), 
            "tsocio18numberow2.docx")

#Calc p-value

chisq.test(r2018$age1fillcat, r2018$repyn)
chisq.test(r2018$genre, r2018$repyn)
chisq.test(r2018$sorient, r2018$repyn)
chisq.test(r2018$alcdrug, r2018$repyn)
chisq.test(r2018$monsex, r2018$repyn)
chisq.test(r2018$weekcat, r2018$repyn)
chisq.test(r2018$regionudm, r2018$repyn)
chisq.test(r2018$pop18cat, r2018$repyn)
chisq.test(r2018$udm2018int, r2018$repyn)


#### saving all banks ####

#peps2

write.csv2(peps, "peps2.csv")

#reps

write.csv2(reps, "reps.csv")

#### saving all banks ####

#r2018

write.csv2(r2018, "r2018.csv")

#pepsu

write.csv2(pepsu, "pepsu.csv")

#psex

write.csv2(psex, "psex.csv")

rm(peps, pepsu, psex, r2018, reps)

