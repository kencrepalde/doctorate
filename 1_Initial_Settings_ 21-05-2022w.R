#!!!!!!!!!!!!!always remember to check packages actualization

####First Steps#####
#Packages
library(tidyverse)
library(lubridate)
library(summarytools)


#Discovring work diretory
getwd()
setwd()
#The bank had empty cells that R couldn't understand that was NA because this was factor
#The bank has changed initially, considering that was created the schemes variable -
#Without spaces and special characters - I had made this for medicines join

pep<-read.csv("db_pep_5med.csv",header=TRUE,sep=";", dec=",", na.strings=c(""," ","NA"))
#use fileEncoding to avoid "ï.." in first variable

#for total of observations
u<-as.data.frame(unique(pep$nome_paciente))
summary(u)
#It is 448,563 observations, 394,360 registries and 17 variables 
rm(u)

#bank verification, was it read correctly?
summary(pep)
str(pep)
class(pep)

#There are incorrectly classification of variables

####First settings, renaming, etc####

#renaming for simple names in English language
names(pep)
pep <- rename(pep, id = nome_paciente, bd = dt_nasc, 
              exp = circunstancia_expo, dtexp = dt_exposicao, 
              sorient = orientacao_sexual, dtsolic = data_solicitacao, 
              dtfill = data_dispensa, contraindic = st_contraindicacao_esquema,
              alcdrug = st_alcool_drogas, monsex = st_dinheiro_sexo,
              genre = genero, cityudm = municipio_udm, stateudm = uf_udm, 
              med = esquema, med2 = esquema2)

str(pep)

#### Counting sexual exposure ####
summary(as.factor(pep$exp))
#extracting wrong exposions not related to PEP
pep$nexp <- pep$exp
class(pep$nexp)
pep$nexp[pep$nexp=="E"]<-"X"
pep$nexp[pep$nexp=="F"]<-"U"
pep$nexp[pep$nexp=="G"]<-"U"
pep$nexp[pep$nexp=="O"]<-"U"
pep$nexp[pep$nexp=="P"]<-"U"

#cases by year
pep$dtfill<-dmy(pep$dtfill)
pep$yearfill<-cut.Date(pep$dtfill, breaks = "year", 
                        labels=c(2011,2012,2013,2014,2015,2016,2017,2018,2019),
                        right = FALSE)
pep %>% freq(yearfill)

a <- pep %>% filter (nexp=="A") %>% distinct(id, .keep_all = TRUE)
#143.769
b <- pep %>% filter (nexp=="B" | exp=="C" | exp=="D") %>% distinct(id, .keep_all = TRUE)
#200.283
s <- pep %>% filter (nexp=="S") %>% distinct(id, .keep_all = TRUE) 
#47.860
x <- pep %>% filter (nexp=="X") %>% distinct(id, .keep_all = TRUE) 
#8.305
u <- pep %>% filter (nexp=="U") %>% distinct(id, .keep_all = TRUE) 
#648

rm(a, b, s, u, x)

#extracting the important expositions

pep <- pep %>% filter (exp=="B" | exp=="C" | exp=="D")
summary(unique(pep$id))
# 200.283 people, 236,930 registries and 17 variables

summary(as.factor(pep$exp))

#B	Reproduction: 941 reg
#C	Serodiscordant couple: 2,150 reg
#D	Consensual Sex: 233,839 reg

#First, I will exclude the "Paciente -" sufix in id
pep$id<-str_replace(pep$id,"Paciente - ","")
pep$id <- as.integer(pep$id)
class(pep$id)
str(pep)
summary(pep$id)

####Deduplicating####

#---> Same fill date and same person

#filling date - have to be > 2010 and < 2020
#first, it's necessary to change dt type (it need to be date class)

pep$dtexp<-dmy(pep$dtexp)

#the dmy order have to be the same order in date format, in this case dd/mm/yyyy = dmy
summary(pep$dtfill)
summary(pep$dtexp)
#no filling registry less than 2011 and nor more than 2019 - it's correct

#ok, now it's possible to deduplicating

#selcting first filling

#.keep_all preserve all variables in df

peps <- pep %>% group_by(id) %>% arrange (dtfill) %>% distinct(dtfill, .keep_all = TRUE) %>% as.data.frame(.)

#as.data.frame was added to avoid "Unknown or uninitialised column: `varible`"

n_distinct(peps$id)
#now, we have 231,613 reg and 200,283 individuals

summary(as.factor(peps$exp))

#B	Reproduction: 941 obs --> 937
#C	Serodiscordant couple: 2.150 obs --> 2,136
#D	Consensual sex: 233.839 obs --> 228,540

#### Age settings ####

#changing bd to date

class(peps$bd)
peps$bd <- dmy(peps$bd)

summary(peps$bd)

#right

#age variable, ie filling date - bd
#Trucating option, ie maintaing the absolut value - age in completed years
peps$agefill <- trunc(as.numeric(difftime(peps$dtfill,peps$bd,units="days")/365.25))
summary(peps$agefill>110)
#53 with > 110 years old
summary(peps$agefill<14)
#1,436 < 14 anos years old

#excluding

peps<-peps[peps$agefill<111,]
summary(peps$agefill>110)
n_distinct(peps$id)
#231,560 registries exactly -53 in the bank. We have 200,230 people.

peps<-peps[peps$agefill>13,]
summary(peps$agefill<14)
n_distinct(peps$id)
#230,124, exactly - 1.436 in the bank. We have 198,801 people.

#ok

#### Time to exposure variable ####

#classes
class(peps$dtexp)
class(peps$dtfill)

peps$exptime <- difftime(peps$dtfill, peps$dtexp, units = c("days"))
#about the function: need to have the recent date first and doesn't work in tidy format

#categories
#It's important to think if you are categorizing to higher or lower values
#Because the order of categorization is important

peps$exptimecat <- peps$exptime
peps$exptimecat[peps$exptimecat<4] <- 1
peps$exptimecat[peps$exptimecat>3] <- 2


#checking out
table(as.factor(peps$exptimecat))

#checking out 2
summary(peps$exptime<4)
#right

#### medicines ####
#joining medicine variable
med<-read.csv("medlistN.csv",header=TRUE,sep=";", dec=",", na.strings=c(""," ","NA"))
med<-med %>% select(med, medfarmaco)
peps<-left_join(peps, med, by="med")
summary(as.factor(peps$medfarmaco))

#### Caseload ####

#It is presented the cases by the year and UDM - after this, joining and division
#first, yearfill variable
peps$yearfill<-cut.Date(peps$dtfill, breaks = "year", 
                       labels=c(2011,2012,2013,2014,2015,2016,2017,2018,2019),
                       right = FALSE)

#cases by year
peps %>% arrange(dtfill) %>% distinct(id, .keep_all = T) %>% freq(yearfill)

#fisrt, cases by year
pep11 <- peps %>% filter(yearfill == 2011)
pep12 <- peps %>% filter(yearfill == 2012)
pep13 <- peps %>% filter(yearfill == 2013)
pep14 <- peps %>% filter(yearfill == 2014)
pep15 <- peps %>% filter(yearfill == 2015)
pep16 <- peps %>% filter(yearfill == 2016)
pep17 <- peps %>% filter(yearfill == 2017)
pep18 <- peps %>% filter(yearfill == 2018)
pep19 <- peps %>% filter(yearfill == 2019)

#second, by UDM, filtering the unique individuals
pep11 <- pep11 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep12 <- pep12 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep13 <- pep13 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep14 <- pep14 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep15 <- pep15 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep16 <- pep16 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep17 <- pep17 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep18 <- pep18 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)
pep19 <- pep19 %>% group_by(udm) %>% distinct(id,.keep_all = TRUE)

#third, calculating the total of individuals by UDM
u2011 <- as.data.frame(table(pep11$udm))
u2012 <- as.data.frame(table(pep12$udm))
u2013 <- as.data.frame(table(pep13$udm))
u2014 <- as.data.frame(table(pep14$udm))
u2015 <- as.data.frame(table(pep15$udm))
u2016 <- as.data.frame(table(pep16$udm))
u2017 <- as.data.frame(table(pep17$udm))
u2018 <- as.data.frame(table(pep18$udm))
u2019 <- as.data.frame(table(pep19$udm))

#columns renaming
colnames(u2011) <- c("udm","udm2011")
colnames(u2012) <- c("udm","udm2012")
colnames(u2013) <- c("udm","udm2013")
colnames(u2014) <- c("udm","udm2014")
colnames(u2015) <- c("udm","udm2015")
colnames(u2016) <- c("udm","udm2016")
colnames(u2017) <- c("udm","udm2017")
colnames(u2018) <- c("udm","udm2018")
colnames(u2019) <- c("udm","udm2019")

#joining the total of individuals by udm and year
peps <- left_join(peps, u2011, by="udm")
peps <- left_join(peps, u2012, by="udm")
peps <- left_join(peps, u2013, by="udm")
peps <- left_join(peps, u2014, by="udm")
peps <- left_join(peps, u2015, by="udm")
peps <- left_join(peps, u2016, by="udm")
peps <- left_join(peps, u2017, by="udm")
peps <- left_join(peps, u2018, by="udm")
peps <- left_join(peps, u2019, by="udm")

rm(u2011, u2012, u2013, u2014, u2015, u2016, u2017, u2018, u2019)
rm(pep11, pep12, pep13, pep14, pep15, pep16, pep17, pep18, pep19)


#### city variable ####
#duplicated city names - cascavel (states PR and CE), valenca (BA and RJ)
#I had to correct some cities names: Armação de Búzios 
#Embu, Herval d´Oeste, Luis Eduardo Magalhães, Mogi-Mirim, Parati, 
#Santa Bárbara d´Oeste, Santana do Livramento
city<-read.csv2("citydataN.csv", header=TRUE, sep=";", encoding="UTF-8", dec=",", na.strings=c(""," ","NA"))

#it's necessary to use cities and states because we have homonimous cities
city <- city %>% select (cityudm, stateudm, pop18)
peps <- left_join(peps, city, by=c("cityudm", "stateudm"))
#checking
names(peps)

#### day week of dispensing ####

peps$week <- with(peps,
                    tapply(as.integer(id), 
                           format(as.Date(dtfill, "%Y-%m-%d"), "%w")))
#remember, this week format start on sunday

table(as.factor(peps$week))
names(peps)

peps$weekcat <- peps$week
peps$weekcat[peps$weekcat==7] <- "weekend"
peps$weekcat[peps$weekcat==1] <- "weekend"
peps$weekcat[peps$weekcat>1 & peps$weekcat<7] <- "weekday"

table(as.factor(peps$weekcat))

#right

#### age in first fill ####

#age categorizing

summary(peps$agefill)

peps$agefillcat <- peps$agefill
peps$agefillcat[peps$agefillcat>39] <- 88888
peps$agefillcat[peps$agefillcat>34 & peps$agefillcat<40] <- 77777
peps$agefillcat[peps$agefillcat>29 & peps$agefillcat<35] <- 66666
peps$agefillcat[peps$agefillcat>24 & peps$agefillcat<30] <- 55555
peps$agefillcat[peps$agefillcat>19 & peps$agefillcat<25] <- 44444
peps$agefillcat[peps$agefillcat<20] <- 33333

peps$agefillcat <- as.factor(peps$agefillcat)
table(peps$agefillcat)
names(peps)

# now age in first fill
peps <- peps %>% arrange(dtfill) %>% mutate(age1fill = agefill, age1fillcat = agefillcat) %>% 
        select (id, age1fill, age1fillcat) %>% distinct(id, .keep_all = TRUE) %>% 
        left_join(peps,., by="id")
names(peps)
freq(peps$age1fillcat)
freq(peps$agefillcat)


#### saving all banks ####

#pep

write.csv2(pep, "pep.csv")

#peps

write.csv2(peps, "peps.csv")

#time to remove
rm(pep, peps, city, med)


##### END #####

