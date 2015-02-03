library(ggplot2)
library(gridExtra)
library(grid)

data <- read.csv("Acessory FUNCTIONS/wind_scaling_with_size_data.csv")

for (i in 1:nrow(data)){
  # sample data 
  temp_data <- rbeta(10000,data[i,"a"],data[i,"b"])
  
  # arrange it for plotting 
  temp_data <- as.data.frame(temp_data)
  colnames(temp_data) <- c("value")
  
  # for labelling parameters 
  my_grob1 <- grobTree(textGrob(paste("a=",data[i,"a"],sep=""),x=0.5,y=0.85,
                                gp=gpar(fontsize=10)))
  my_grob2 <- grobTree(textGrob(paste("b=",data[i,"b"],sep=""),x=0.5,y=0.70,
                                gp=gpar(fontsize=10)))
  
  temp_plot <- ggplot(temp_data, aes(value)) + geom_histogram(binwidth=0.1,fill="grey", colour="black")
  temp_plot <- temp_plot + xlab("Wind strength")
  temp_plot <- temp_plot + ylab("Frequency")
  temp_plot <- temp_plot + scale_x_continuous(expand=c(0,0),limits=c(0,1),breaks=c(0,0.5,1))
  temp_plot <- temp_plot + scale_y_continuous(expand=c(0,0))
  temp_plot <- temp_plot + theme_classic(base_size=12)
  temp_plot <- temp_plot + annotation_custom(my_grob1)
  temp_plot <- temp_plot + annotation_custom(my_grob2)
    
  assign(paste("plot_",i,sep=""),temp_plot,pos=-1)
}

plot_1 <- plot_1 + ggtitle("0.04ha") + theme(plot.title=element_text(face="bold"))
plot_2 <- plot_2 + ggtitle("1ha") + theme(plot.title=element_text(face="bold"))
plot_3 <- plot_3 + ggtitle("2.25ha") + theme(plot.title=element_text(face="bold"))
plot_4 <- plot_4 + ggtitle("6.25ha") + theme(plot.title=element_text(face="bold"))
plot_5 <- plot_5 + ggtitle("9ha") + theme(plot.title=element_text(face="bold"))
plot_6 <- plot_6 + ggtitle("") + theme(plot.title=element_text(face="bold"))
plot_7 <- plot_7 + ggtitle("") + theme(plot.title=element_text(face="bold"))
plot_8 <- plot_8 + ggtitle("") + theme(plot.title=element_text(face="bold"))
plot_9 <- plot_9 + ggtitle("") + theme(plot.title=element_text(face="bold"))
plot_10 <- plot_10 + ggtitle("") + theme(plot.title=element_text(face="bold"))

plot_1 <- plot_1 + xlab("")
plot_2 <- plot_2 + xlab("")
plot_3 <- plot_3 + xlab("")
plot_4 <- plot_4 + xlab("")
plot_5 <- plot_5 + xlab("")

plot_2 <- plot_2 + ylab("")
plot_3 <- plot_3 + ylab("")
plot_4 <- plot_4 + ylab("")
plot_5 <- plot_5 + ylab("")
plot_7 <- plot_7 + ylab("")
plot_8 <- plot_8 + ylab("")
plot_9 <- plot_9 + ylab("")
plot_10 <- plot_10 + ylab("")

combined_plot <- arrangeGrob(plot_1,plot_2,plot_3,plot_4,plot_5,
                             plot_6,plot_7,plot_8,plot_9,plot_10,
                             ncol=5,nrow=2)
combined_plot

ggsave("wind_scaling_with_size_figure.jpg",combined_plot,height=4,width=10)
