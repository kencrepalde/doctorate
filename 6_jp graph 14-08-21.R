#Packages
library(readr)

library(readxl)

library(ggplot2)



#Data

joint <- read_excel("Joinpoint.xlsx")

joint$Series <-factor(joint$Series, levels = c("1", "2", "3"))

cols <- c("1" = "gray45", '2' = "gray30", '3' = "gray5") 

#calibri font
windowsFonts(Calibri=windowsFont("Calibri"))

#Graph

joint_plot <- ggplot(joint, aes(x=jp, y=Crude, colour=Series)) + 
        
        geom_point(aes(shape=Series), size=3, alpha = 1/2) +                                       
        
        labs(x="Month", y="PEPSE use per million population",     
             title = "Monthly rates of Post-Exposure Prophylaxis following Consented Sexual Exposure 
             \n(PEPSE), Brazil (2011-2019)")+ 
        
        theme_bw()+
        
        scale_color_manual(values=cols) +
        
        scale_y_continuous(breaks = seq(0,35,5)) +
        
        scale_x_continuous(breaks = seq(0,108,12)) + 
        theme(plot.title = element_text(size = 14, family="Calibri"))+
        
        theme(axis.title.y = element_text(size = 14, family="Calibri")) +
        
        theme(axis.text.y=element_text(size = 14, family="Calibri"))+
        
        theme(axis.text.x=element_text(size = 14, family="Calibri"))+
        
        theme(axis.title.x = element_text(size = 14, family="Calibri")) +
        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        
        theme(legend.position = "bottom") +
        
        theme(legend.text = element_text(colour="black", size=12)) +
        
        geom_line(aes(y = Model, group=1), size = 2)



#saving

ggsave ("Joint_month_plot.tiff", plot=joint_plot, width = 20, height = 15, units = "cm", dpi = 300)

#Removing all objects

rm(cols, joint, joint_plot)
