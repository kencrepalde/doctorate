#Packages
library(readr)

library(readxl)

library(ggplot2)

library(lubridate)


#Discovring work diretory
getwd()
setwd()

#Data

joint <- read_excel("Joinpoint.xlsx")

joint$Series <-factor(joint$Series)

cols <- c("Jan/11 to Aug/14" = "gray65", 'Aug/14 to Sep/15' = "gray40", 'Sep/15 to Dec/19' = "gray5") 

#calibri font
windowsFonts(Calibri=windowsFont("Calibri"))

#Graph

joint_plot <- ggplot(joint, aes(x=jp, y=Crude, colour=Series)) + 
        
        geom_point(aes(shape=Series), size=3, alpha = 1/2, show.legend = FALSE) +                                       
        
        labs(x="Time in Months", y="PEPSE use per million population")+ 
        
        theme_bw()+
        
        scale_color_manual(values=cols) +
        
        scale_y_continuous(breaks = seq(0,35,5)) +
        
  scale_x_continuous(breaks = seq(1,109,6), 
                     labels=c("Jan\n11", "Jul\n11", "Jan\n12", "Jul\n12",
                              "Jan\n13", "Jul\n13", "Jan\n14", "Jul\n14", 
                              "Jan\n15", "Jul\n15", "Jan\n16", "Jul\n16",
                              "Jan\n17", "Jul\n17", "Jan\n18", "Jul\n18",
                              "Jan\n19", "Jul\n19", "Jan\n20")) +
       
        theme(plot.title = element_text(size = 11, family="Calibri"))+
        
        theme(axis.title.y = element_text(size = 11, family="Calibri")) +
        
        theme(axis.text.y=element_text(size = 11, family="Calibri"))+
        
        theme(axis.text.x=element_text(size = 8.5, family="Calibri"))+
        
        theme(axis.title.x = element_text(size = 11, family="Calibri")) +
        
        theme(panel.grid.major.x = element_line(linetype = 1), panel.grid.minor = element_blank(), 
              
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        
        theme(legend.position = "bottom") +
        
        theme(legend.text = element_text(colour="black", size=12)) +
        
        geom_line(aes(y = Model, group=1), size = 2)



#saving

ggsave ("Joint_month_plot.tiff", plot=joint_plot, width = 20, height = 15, units = "cm", dpi = 300)

#Removing all objects

rm(cols, joint, joint_plot)
