####First Steps#####
#Packages
library(tidyverse)
library(lubridate)
library(summarytools)

#Discovring work diretory
getwd()
setwd()

#loading
peps<-read.csv2("peps.csv",header=TRUE,sep=";", dec=",", na.strings=c(""," ","NA"))

#### Medicines graph ####

### all period ###

peps$med2 <- as.factor(peps$med2)

#table(peps$med2) %>% write.csv2(., "medicines.csv")

#top 5
class(peps$medfarmaco)
peps$medfarmaco <- as.factor(peps$medfarmaco)
#this order is most appropriate (begins with more common in 2011)
peps$regimen [peps$medfarmaco=="3TC+DTG+TDF"] <- "1"

peps$regimen [peps$medfarmaco=="3TC+ATV+RTV+TDF"] <- "2"

peps$regimen [peps$medfarmaco=="3TC+AZT+TDF"] <- "3"

peps$regimen [peps$medfarmaco=="3TC+AZT+LPV+RTV"] <- "4"

peps$regimen [is.na(peps$regimen)] <- "5" #the other turns NA.

peps$regimen <- factor(peps$regimen, labels = c("DTG+3TC+TDF","ATV/r+3TC+TDF", 
                                                    "3TC+AZT+TDF", "LPV/r+3TC+AZT", 
                                                  "Other"))


summary(as.factor(peps$regimen))

#Creating tacle with the appropriate columns

columns <- c("yearfill", "regimen")
data_graph <- peps[ ,columns]
rm(columns)

#number of observacoes by year and regimen

data_graph <- data_graph  %>% 
        
        group_by(yearfill, regimen) %>%
        
        summarize(count = n())

# % by year and regimen
data_plot <- data_graph  %>%
        
        group_by(yearfill, regimen) %>%
        
        summarise(n = sum(count)) %>%
        
        mutate(percent = (n / sum(n))*100)

#calibri font
windowsFonts(Calibri=windowsFont("Calibri"))


#plotting stack bar

fig <- ggplot(data = data_plot, aes(fill=regimen, x=yearfill, y= percent))+
  
  geom_histogram( position="stack", stat="identity")+
  
  labs(x="Year", y="Proportion of individuals")+
  
  scale_y_continuous(breaks = seq(0,100,20))+
  
  scale_x_continuous(breaks=seq(2011,2019,1))+
  
  theme_bw()+
  
  theme(plot.title = element_text(size = 14, family="Calibri"))+
  
  theme(axis.title.y = element_text(size = 14, face="bold", family="Calibri")) +
  
  theme(axis.title.x = element_text(size = 14, face="bold", family="Calibri")) +
  
  theme(axis.text.y=element_text(size = 12, hjust=0.25, vjust=0.30, face="bold", family="Calibri"))+
  
  theme(axis.text.x=element_text(size = 12, hjust=0.25, vjust=0.30, face="bold", family="Calibri"))+
  
  scale_fill_manual("regimen",
                    values = c("LPV/r+3TC+AZT" = "#7b3294",
                               "3TC+AZT+TDF" = "#c2a5cf",
                               "ATV/r+3TC+TDF" = "springgreen4", 
                               "DTG+3TC+TDF" = "#a6dba0",
                               "Other"="black"))  +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  
  theme(legend.text = element_text(colour="black", size=14)) +
  
  labs(caption = "3TC: lamivudine; ATV/r: atazanavir/ritonavir; AZT: zidovudine; DTG: dolutegravir; LPV/r: lopinavir/ritonavir; TDF: tenofovir disoproxil") + 
  
  theme(plot.caption = element_text(hjust = 0.5, size = 11))

ggsave("PEPBR.tiff", width = 30, height = 20, units = 'cm', dpi = 300)


#Removing all objects

rm(data_graph, data_plot, fig, peps)
