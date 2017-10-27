# Para Visualización de Small Area

# rm(list = ls())

library(forcats)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(forcats)
library(reshape2)
library(tools) # toTitleCase function
library(sae)
library(extrafont)
# loadfonts(device = "win")
# font_import()
windowsFonts(Times=windowsFont("TT Times New Roman"))

Route <- "E:/FODEIN/SAE_EMB/"
setwd(paste0(Route, "processing_data"))

DIR_Estimations_FH_income <- readRDS("DIR_Estimations_FH_income.rds")
DIR_Estimations_FH_income$MCPIO <- toTitleCase(tolower(DIR_Estimations_FH_income$MCPIO))
DIR_Estimations_FH_income$MCPIO  <- gsub("De ", "de ", DIR_Estimations_FH_income$MCPIO )

DIR_Estimations_FH_unemployment <- readRDS("DIR_Estimations_FH_unemployment.rds")
DIR_Estimations_FH_unemployment$MCPIO <- toTitleCase(tolower(DIR_Estimations_FH_unemployment$MCPIO))
DIR_Estimations_FH_unemployment$MCPIO  <- gsub("De ", "de ", DIR_Estimations_FH_unemployment$MCPIO )


# Eje x: ingreso Eje y: desempleo
EBLUP_Estimations <- inner_join(DIR_Estimations_FH_income[c("ID_MUNIC", "MCPIO", "Group", "eblup.FH")],
                                DIR_Estimations_FH_unemployment[c("ID_MUNIC", "eblup.FH")], by = "ID_MUNIC")
names(EBLUP_Estimations)[c(4, 5)] <- c("eblupFH_MeanIncome", "eblupFH_PropUnemplorcion")



######################################## Plot 0  ############################################
plot_incomeUnemp <- ggplot(EBLUP_Estimations, aes(x = eblupFH_MeanIncome,
                             y = eblupFH_PropUnemplorcion, colour = Group, label = MCPIO ) ) + 
  geom_point() +  xlab("Average of Income (Millions of Colombian Pesos)") +
  ylab("Proportion of unemployment") +  
  theme(axis.text.x = element_text(size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain", 
                                   family = "Times New Roman"),
        axis.text.y = element_text(size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain",
                                   family = "Times New Roman"),
        axis.title.x = element_text(size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain",
                                    family = "Times New Roman"),
        axis.title.y = element_text(size = 16, angle = 90, hjust = .5,
                                    vjust = .5, face = "plain", family = "Times New Roman") ) +  
  geom_text(aes(label = MCPIO), hjust=0, vjust=0, size = 3) + #  scale_color_grey() +
  theme_bw() 


 ######################################## Plot 1 (Average Income, Observed Areas)  ############################################
# Direct Estimations vs Indirect Estimations (FH): Income

insideEMB_IncomeEstim <-  DIR_Estimations_FH_income[DIR_Estimations_FH_income$Group == "Munic. Inside EMB",]
insideEMB_IncomeEstim <- insideEMB_IncomeEstim %>% arrange(-cv.DIR)


library(plyr)
data_mseIncomePlot <- melt(insideEMB_IncomeEstim[c("MCPIO", "mse.DIR",  "mse.FH")], id=c("MCPIO"), na.rm=TRUE)
names(data_mseIncomePlot)[2] <- "TYPE"
data_mseIncomePlot <- arrange(data_mseIncomePlot, TYPE, -value)
data_mseIncomePlot$MCPIO <- fct_rev(fct_inorder(as.factor(data_mseIncomePlot$MCPIO)))


plot_ObsMunicIncome <- ggplot(data_mseIncomePlot, aes(x = MCPIO, y = value))  +
  geom_point(aes(shape = TYPE, colour = TYPE))  +
  theme(legend.title = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid.major = element_line(colour = "grey92"), 
        panel.grid.minor = element_line(colour = "grey92", size = 0.25), 
        strip.background = element_rect(fill = "grey85",  colour = "grey20"),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.text.y = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 14, family = "Times New Roman")) +
        xlab("Municipality") + ylab("MSE (Average Income)") +  theme(legend.position="none") +
  coord_flip() # + scale_color_grey()
  

######################################## Plot 2 (Average Income: Ranking Municipalities )  ############################################

ranking_income <- DIR_Estimations_FH_income[c("MCPIO", "eblup.FH", "Group")] %>%
    arrange(-eblup.FH)
ranking_income$MCPIO <- fct_rev(fct_inorder(factor(ranking_income$MCPIO)))
ranking_income$ranking <- 1:nrow(ranking_income)
ranking_income <- ranking_income[c(1:15, 94:108),]

plot_ranking_income <- ggdotchart(ranking_income, x = "MCPIO", y = "eblup.FH",
                                    color = "Group",                                # Color by groups
                                    palette = NULL, # Custom color palette
                                    sorting = "descending", # Sort value in descending order
                                    add = "segments",                             # Add segments from y = 0 to dots
                                    rotate = TRUE,                                # Rotate vertically
                                    group = NULL,                                # Order by groups
                                    dot.size = 6,                                 # Large dot size
                                    label = round(ranking_income$ranking),                        # Add mpg values as dot labels
                                    font.label = list(color = "white", size = 8, 
                                                      vjust = 0.5),               # Adjust label parameters
                                    ggtheme = theme_pubr()                        # ggplot2 theme
)  + ylab("Average Income (Millions of pesos)") + 
  xlab("Municipality") + 
  geom_vline(xintercept = 15.5, color = "black", linetype = 2) +
  scale_colour_grey() + theme(
    axis.text.x = element_text(size=12,angle=90, family = "Times New Roman"),
    axis.text.y = element_text(size=12, family = "Times New Roman"), 
    axis.title.x = element_text(size=14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"))


##############################################################################

######################################## Plot 1 (Unemployment, Observed Areas)  ############################################
# Direct Estimations vs Indirect Estimations (FH): Proportion Unemployment

insideEMB_UnemplEstim <-  DIR_Estimations_FH_unemployment[DIR_Estimations_FH_unemployment$Group == 
                          "Munic. Inside EMB",]

insideEMB_UnemplEstim <- insideEMB_UnemplEstim %>% arrange(-cv.DIR)


data_mseUnemplPlot <- melt(insideEMB_UnemplEstim[c("MCPIO", "mse.DIR",  "mse.FH")], 
                           id=c("MCPIO"), na.rm=TRUE)
names(data_mseUnemplPlot)[2] <- "TYPE"
data_mseUnemplPlot <- arrange(data_mseUnemplPlot, TYPE, -value)
data_mseUnemplPlot$MCPIO <- (fct_inorder(as.factor(data_mseUnemplPlot$MCPIO)))

plot_ObsMunicUnemp <- ggplot(data_mseUnemplPlot, aes(x = MCPIO, y = value))  + 
  geom_point(aes(shape = TYPE, colour = TYPE))  +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  theme(legend.title = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid.major = element_line(colour = "grey92"), 
        panel.grid.minor = element_line(colour = "grey92", size = 0.25), 
        strip.background = element_rect(fill = "grey85",  colour = "grey20"),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.text.y = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 14, family = "Times New Roman")) +  
  xlab("") + ylab("MSE (Unemploment rate)") + 
  coord_flip() # + scale_color_grey()


######################################## Plot 2 (Unemployment: Ranking Municipalities )  ############################################

ranking_unemploy <- DIR_Estimations_FH_unemployment[c("MCPIO", "eblup.FH", "Group")] %>% arrange(eblup.FH)
ranking_unemploy$MCPIO <- (fct_inorder(factor(ranking_unemploy$MCPIO)))
ranking_unemploy$ranking <- 1:nrow(ranking_unemploy)
ranking_unemploy <- ranking_unemploy[c(1:15, 102:116),]

plot_ranking_unemploy <- ggdotchart(ranking_unemploy, x = "MCPIO", y = "eblup.FH",
           color = "Group",                                # Color by groups
           palette = NULL, # Custom color palette
           sorting = "ascending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = NULL,                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(ranking_unemploy$ranking),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 8, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)  + ylab("Unemployment Rate") +   xlab("") + 
  geom_vline(xintercept = 15.5, color = "black", linetype = 2) +
  scale_colour_grey() + theme(
    axis.text.x = element_text(size = 12,angle = 90, family = "Times New Roman"),
    axis.text.y = element_text(size = 12, family = "Times New Roman"), 
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"))


# Join Plots 
plot_ranking <- plot_grid(plot_ranking_income, plot_ranking_unemploy, align = "v",
          nrow = 1,  ncol = 2) 


plot_ObsMunic <- plot_grid(plot_ObsMunicIncome, plot_ObsMunicUnemp, align = "v",
                          nrow = 1,  ncol = 2)
#plot_incomeUnemp


# plot_ranking_unemploy <- ggplot(data = ranking_unemploy, aes(x = MCPIO, y = eblup.FH, fill = Group)) +
#  geom_bar(stat="identity") + xlab("Municipio") + 
#  ylab("Unemployment rate") + coord_flip() + theme_bw()

# print(plot_ranking_unemploy)


# plot_ranking_income <- ggplot(data = ranking_income, aes(x = MCPIO, y = eblup.FH, fill = Group)) +
#  geom_bar(stat="identity") + xlab("Municipio") + 
#  ylab("Promedio de ingreso (Miles de pesos") + 
#  coord_flip() + theme_bw()
# print(plot_ranking_income)
