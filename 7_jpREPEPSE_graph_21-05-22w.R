#Packages
library(readr)

library(readxl)

library(ggplot2)

library(lubridate)


#Discovring work diretory
getwd()
setwd()

#Data

joint <- read_excel("joinpoint_repepse.xlsx")

joint$Series <-factor(joint$Series, levels = c("1"))

cols <- c("1" = "gray10") 

#calibri font
windowsFonts(Calibri=windowsFont("Calibri"))

#Graph


joint_plot <- ggplot(joint, aes(x=jp, y=Proportion, colour="Series")) + 
        
        geom_point(size=4, alpha = 1/2, show.legend = F, colour = "black") +
  
        geom_text(data = joint, aes(label=Proportion), hjust = 0, nudge_x = 0.1,  colour = "black") +
        
        labs(x="Time in Years", y="Proportion of PEPSE repetition (%)")+ 
        
        theme_bw()+
        
        scale_y_continuous(breaks = seq(0,10,1)) +
        
        scale_x_continuous(breaks = seq(1,8,1), 
                     labels=c("2011", "2012", "2013", "2014",
                              "2015", "2016", "2017", "2018")) +
       
        theme(plot.title = element_text(size = 11, family="Calibri"))+
        
        theme(axis.title.y = element_text(size = 11, family="Calibri")) +
        
        theme(axis.text.y=element_text(size = 11, family="Calibri"))+
        
        theme(axis.text.x=element_text(size = 11, family="Calibri"))+
        
        theme(axis.title.x = element_text(size = 11, family="Calibri")) +
        
        theme(panel.grid.major.x = element_line(linetype = 1), panel.grid.minor = element_blank(), 
              
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        
        theme(legend.position = "bottom") +
        
        theme(legend.text = element_text(colour="black", size=12)) +
        
        geom_line(aes(y = Model2, group=1), size = 1.5, show.legend = F, colour = "gray20")



#saving

ggsave ("Joint_repepse_plot.tiff", plot=joint_plot, width = 20, height = 15, units = "cm", dpi = 300)

#Removing all objects

rm(cols, joint, joint_plot)
