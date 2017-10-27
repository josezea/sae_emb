############################## Unemployment Proportion Estimation  ######################################
# rm(list = ls())
library(dplyr)
library(xlsx)
library(sae)
library(FWDselect)

# Route <- "D:/Gdrive/Laboral 2017/USTA/FODEIN/SAE_EMB/"
Route <- "E:/FODEIN/SAE_EMB/"
setwd(Route)

######################### 1. Exportation of unified dataframe  ############################
setwd(paste0(Route, "processing_data"))
df <- readRDS("df.rds")

# Rescale in thousand pesos ($COL$) in order to improve storage accuracy
df$AVALUOS_CATASTRALES_RURALES <- df$AVALUOS_CATASTRALES_RURALES / 1000
########################################################################################## 


##################################### 2. Variable selection  ##################################### 

# Correlations Ranking: greatest correlations between income and auxiliary variables
excludevariables_unemployment <- c("ID_MUNIC", "unempByMun", "varunempByMun", "cve.unempByMun", 
                             "me_unemployment", "meansByMun", "varmeansByMun", "cvemeansByMun", 
                             "DEPTO", "MCPIO")
auxily_variables_unemployment <- names(df)[!(names(df) %in% excludevariables_unemployment)]
cor_unemployment <- t(as.matrix(cor(df$unempByMun, df[auxily_variables_unemployment])))
df_cor_unemployment <- as.data.frame(cor_unemployment)
df_cor_unemployment$variables <- rownames(df_cor_unemployment) 
df_cor_unemployment <- arrange(df_cor_unemployment, -V1)

# Keep variables with correlation greater than threshold
threshold_selecvar_unemployment <- 0.3
selected_var_unemployment <- as.character(df_cor_unemployment$variables[abs(df_cor_unemployment$V1) >
                                                              threshold_selecvar_unemployment])
#cor(df[selected_var_unemployment])

df_unemployment_model <- df[, colnames(df) %in% c("ID_MUNIC", "unempByMun",
                                            "varunempByMun", selected_var_unemployment)]

############################### forward stepwise-based selection Procedure to select variables ###############################  
selection_unemployment <- FWDselect::selection(x = df_unemployment_model[,selected_var_unemployment], 
                                               y = df_unemployment_model[,"unempByMun"], q = 3,
                                criterion = "aic", method = "lm",
                                family = "gaussian", seconds = T, nmodels = 1)
# names(selection_unemployment)

formula_unemployment_model <- as.formula(paste("unempByMun ~ ", paste(selection_unemployment$Variable_names, 
                                                                      collapse="+")))

unemployment_model <- lm(formula_unemployment_model, data = df_unemployment_model)
summary(unemployment_model)
########################################################################################## 

################################ 3.  Income Estimation with Fay Herriot  ####################################
FH_unemployment <- mseFH(eval(formula(unemployment_model)),
                         varunempByMun, data = df_unemployment_model)
# FH_ingreso$est$fit$estcoef
# summary( lm(eval(formula(modelo_step_ingreso)), data = datos_modelo_ingreso) )

cv_FH_unemployment<- 100 * sqrt(FH_unemployment$mse) / FH_unemployment$est$eblup

# Direct and Indirect Estimation Inside Sample
#resA_FH_income 
outcome_unemployment_ObservedAreas <- data.frame(ID_MUNIC = df$ID_MUNIC,
                                           DIR = df$unempByMun, eblup.FH = FH_unemployment$est$eblup, 
                                           mse.DIR = df$varunempByMun,
                                           mse.FH = FH_unemployment$mse,
                                           cv.DIR = df$cve.unempByMun, 
                                           cv.FH = cv_FH_unemployment) 
outcome_unemployment_ObservedAreas$ID_MUNIC <- as.character(outcome_unemployment_ObservedAreas$ID_MUNIC)
outcome_unemployment_ObservedAreas$Group <- "Munic. Inside EMB"
########################################################################################## 

########################## 4. Estimation for municipalities out of sample ######################
df_EMB <- readRDS("df_EMB.rds")
AuxInfo <- readRDS("AuxInfo.rds")
AuxInfo$AVALUOS_CATASTRALES_RURALES <- AuxInfo$AVALUOS_CATASTRALES_RURALES / 1000 

Z_unemployment <- cbind(rep(1, nrow(AuxInfo)), as.matrix(
  AuxInfo[substr(rownames(FH_unemployment$est$fit$estcoef[1]), 2, 800)[-1]]))
rownames(Z_unemployment) <- AuxInfo$ID_MUNIC

Beta_unemployment<- as.matrix(FH_unemployment$est$fit$estcoef[1]) 

# Select covariables of out sample municipalities 
Z_noA_unemployment <- Z_unemployment[which(!(rownames(Z_unemployment) %in% df$ID_MUNIC)),]

# Estimation for nonsampled municipalities
eblup.FH_noA_unemployment = as.numeric(Z_noA_unemployment %*% Beta_unemployment)

# MSE estimation for no sampled municipalities del MSE para los municipios no muestreados
sigma2_u_unemployment <- FH_unemployment$est$fit$refvar
V_unemployment <- diag(sigma2_u_unemployment + df$varunempByMun)
Z_A_unemployment <- Z_unemployment[which((rownames(Z_unemployment) %in% df$ID_MUNIC)),]

V_Beta_unemployment <- t(Z_A_unemployment) %*% solve(V_unemployment, tol = 1e-30) %*% Z_A_unemployment
mse.FH_noA_unemployment <- diag(Z_noA_unemployment %*% solve(V_Beta_unemployment, tol = 1e-30) %*% t(Z_noA_unemployment))

# cve out sample
cv.FH_noA_unemployment <- 100 * sqrt(mse.FH_noA_unemployment) / eblup.FH_noA_unemployment

outcome_unemployment_UnobservedAreas <- data.frame(ID_MUNIC = names(mse.FH_noA_unemployment),
                                                   eblup.FH = eblup.FH_noA_unemployment, 
                                                   mse.FH = mse.FH_noA_unemployment, cv.FH = cv.FH_noA_unemployment) 
outcome_unemployment_UnobservedAreas$ID_MUNIC <- as.character(outcome_unemployment_UnobservedAreas$ID_MUNIC)
outcome_unemployment_UnobservedAreas$Group <- "Munic. Outside EMB"

########################################################################################## 

############################# 4. Dataset Unification ################################
DIR_Estimations_FH_unemployment <- bind_rows(outcome_unemployment_ObservedAreas,outcome_unemployment_UnobservedAreas)  
DIR_Estimations_FH_unemployment <- right_join(AuxInfo[c("ID_MUNIC", "MCPIO")],
                                              DIR_Estimations_FH_unemployment, by = "ID_MUNIC")
########################################################################################## 
# cor(DIR_Estimations_FH_unemployment$DIR, DIR_Estimations_FH_unemployment$eblup.FH , use = "pairwise.complete.obs")
# plot(DIR_Estimations_FH_unemployment$DIR, DIR_Estimations_FH_unemployment$eblup.FH , pch = 20,
#      xlim = c(0, 0.12), ylim = c(0, 0.12))


############################# 5. Exportation  ################################
saveRDS(DIR_Estimations_FH_unemployment, "DIR_Estimations_FH_unemployment.rds")
# write.xlsx(DIR_Estimations_FH_unemployment, "DIR_Estimations_FH_unemployment.xlsx", row.names = F)
########################################################################################## 