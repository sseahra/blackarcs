library(ggplot2)
data <- read.csv("~/Documents/Work/COVID-19 Research/blackarcs/C_Simulation_Code/Simulation_Results/0Sim_Test.txt")
results = na.omit(as.numeric(unlist(data[10])))
mean(results)
sd(results)
df <- data.frame(x,means, iqr_min, iqr_max)
b <- ggplot(data = df, aes(x,means, ymin = iqr_min, ymax = iqr_max))+geom_point()+geom_line()
b <- b + geom_errorbar()
b <- b + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
b <-b + labs(x = "Time Lag Between Onset of Infectiousness and Quarantine for Patient 0 (Indexed at 1)", 
             y = "Number of People Infected",
             title = "Mean Number of People Infected in Patient 0 Testing") 
b <- b + theme_bw()
ggsave("Mean_Data_1_Index.png", b, "png")