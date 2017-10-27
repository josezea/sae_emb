

######################################## Plot 2  ############################################
# Direct Estimations vs Indirect Estimations (FH): Income

insideEMB_PropUnemployment <-  DIR_Estimations_FH_unemployment[DIR_Estimations_FH_unemployment$Group == "Municipalities Inside EMB", ]
insideEMB_PropUnemployment <- insideEMB_PropUnemployment %>% arrange(-cv.DIR)


library(plyr)
data_mseUnempPlot <- melt(insideEMB_PropUnemployment[c("MCPIO", "mse.DIR",  "mse.FH")], id=c("MCPIO"), na.rm=TRUE)
library(forcats)
data_mseUnempPlot$MCPIO <- fct_rev(as.factor(data_mseUnempPlot$MCPIO))
ggplot(data_mseUnempPlot, aes(x = MCPIO, y = value))  + geom_point(aes(shape = variable))  +
  theme(legend.title = element_blank(), panel.background = element_rect(fill = "white", 
        colour = NA), panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid.major = element_line(colour = "grey92"), 
        panel.grid.minor = element_line(colour = "grey92", size = 0.25), 
        strip.background = element_rect(fill = "grey85",  colour = "grey20")) +  
  coord_flip()


######################################## Plot 3 (Ranking Municipalities)  ############################################

ranking_income <- DIR_Estimations_FH_income[c("MCPIO", "eblup.FH", "Group")] %>% arrange(-eblup.FH)
ranking_income <- ranking_income[1:30,]

ranking_income$MCPIO <- fct_rev(fct_inorder(factor(ranking_income$MCPIO)))

plot_ranking_income <- ggplot(data = ranking_income, aes(x = MCPIO, y = eblup.FH, fill = Group)) +
  geom_bar(stat="identity") + xlab("Municipio") + 
  ylab("Promedio de ingreso (Miles de pesos") + 
  coord_flip() + theme_bw()
print(plot_ranking_income)











