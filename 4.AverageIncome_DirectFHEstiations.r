############################## Income Mean Computation ######################################

# Estimador de FH (estimación de parámetros y MSE)
# Unificación Estimadores directos y FH

# rm(list = ls())
library(dplyr)
library(xlsx)
library(sae)

Route <- "E:/FODEIN/SAE_EMB/"
setwd(Route)

######################### 1. Exportation of unified dataframe  ############################
setwd(paste0(Route, "processing_data"))
df <- readRDS("df.rds")

# Rescale in thousand pesos ($COL$) in order to improve storage accuracy
df$IncomeMeansByMun <- df$meansByMun / 1000
df$VarIncomeMeansByMun <- df$varmeansByMun / (1000 ^ 2) 
df$AVALUOS_CATASTRALES_RURALES <- df$AVALUOS_CATASTRALES_RURALES / 1000

########################################################################################## 


##################################### 2. Variable selection  ##################################### 

# Correlations Ranking: greatest correlations between income and auxiliary variables
excludevariables_income <- c("ID_MUNIC", "unempByMun", "varunempByMun", "cve.unempByMun", 
                              "me_unemployment", "meansByMun", "varmeansByMun", "cvemeansByMun", 
                              "DEPTO", "MCPIO", "IncomeMeansByMun", "VarIncomeMeansByMun" )
auxily_variables_income <- names(df)[!(names(df) %in% excludevariables_income)]
cor_income <- t(as.matrix(cor(df$meansByMun, df[auxily_variables_income])))
df_cor_income <- as.data.frame(cor_income)
df_cor_income$variables <- rownames(df_cor_income) 
df_cor_income <- arrange(df_cor_income, -V1)

# Keep variables with correlation greater than threshold
threshold_selecvar_income <- 0.55
selected_var_income <- as.character(df_cor_income$variables[abs(df_cor_income$V1) >
                                                              threshold_selecvar_income])
#cor(df[selected_var_income])

df_income_model <- df[, colnames(df) %in% c("ID_MUNIC", "IncomeMeansByMun",
                        "VarIncomeMeansByMun", selected_var_income)]

############################### Stepwise Procedure to select variables ###############################  
formula_income_model <- as.formula(paste("IncomeMeansByMun ~ ", paste(selected_var_income, collapse="+")))

income_model <- lm(formula_income_model, data = df_income_model)

# summary(income_model)
income_step_model <-  step(income_model)

# income_step_model <- lm(eval(formula(income_step_model)), data = df_income_model)
# summary(income_step_model)

# It was excluded non significative variables 

income_step_model2 <- IncomeMeansByMun ~ CONSUMO_ENERGIA_PER_HABIT  + PUNTAJE_SABER + 
  AVALUOS_CATASTRALES_RURALES

income_step_model2 <- lm(eval(formula(income_step_model2)), data = df_income_model)
# summary(income_step_model2)
########################################################################################## 

################################ 3.  Income Estimation with Fay Herriot  ####################################
FH_income <- mseFH(eval(formula(income_step_model2)),
                   VarIncomeMeansByMun, data = df_income_model)
# FH_ingreso$est$fit$estcoef
# summary( lm(eval(formula(modelo_step_ingreso)), data = datos_modelo_ingreso) )


cv_FH_income <- 100 * sqrt(FH_income$mse) / FH_income$est$eblup

# Estimaciones directar e indirectas dentro de muestra
#resA_FH_income 
outcome_income_ObservedAreas <- data.frame(ID_MUNIC = df$ID_MUNIC,
                              DIR = df$meansByMun, eblup.FH = FH_income$est$eblup, 
                              mse.DIR = df$VarIncomeMeansByMun,
                              mse.FH = FH_income$mse,
                              cv.DIR = df$cvemeansByMun, 
                              cv.FH = cv_FH_income) 
outcome_income_ObservedAreas$ID_MUNIC <- as.character(outcome_income_ObservedAreas$ID_MUNIC)
outcome_income_ObservedAreas$Group <- "Munic. Inside EMB"
########################################################################################## 

########################## 4. Estimation for municipalities out of sample ######################
df_EMB <- readRDS("df_EMB.rds")
AuxInfo <- readRDS("AuxInfo.rds")
AuxInfo$AVALUOS_CATASTRALES_RURALES <- AuxInfo$AVALUOS_CATASTRALES_RURALES / 1000 

Z_income <- cbind(rep(1, nrow(AuxInfo)), as.matrix(
AuxInfo[substr(rownames(FH_income$est$fit$estcoef[1]), 2, 800)[-1]]))
rownames(Z_income) <- AuxInfo$ID_MUNIC

Beta_income <- as.matrix(FH_income$est$fit$estcoef[1]) 

# Select covariables of out sample municipalities 
Z_noA_income <- Z_income[which(!(rownames(Z_income) %in% df$ID_MUNIC)),]

# Estimation for nonsampled municipalities
eblup.FH_noA_income = as.numeric(Z_noA_income %*% Beta_income)

# MSE estimation for no sampled municipalities del MSE para los municipios no muestreados
sigma2_u_income <- FH_income$est$fit$refvar
V_income <- diag(sigma2_u_income + df$VarIncomeMeansByMun)
Z_A_income <- Z_income[which((rownames(Z_income) %in% df$ID_MUNIC)),]

V_Beta_income <- t(Z_A_income) %*% solve(V_income, tol = 1e-30) %*% Z_A_income
mse.FH_noA_income = diag(Z_noA_income %*% solve(V_Beta_income, tol = 1e-30) %*% t(Z_noA_income))

# cve out of sample
cv.FH_noA_income <- 100 * sqrt(mse.FH_noA_income) / eblup.FH_noA_income
#a <- as.data.frame(100 * sqrt(mse.FH) / eblup.FH)

outcome_income_UnobservedAreas <- data.frame(ID_MUNIC = names(mse.FH_noA_income),
                                eblup.FH = eblup.FH_noA_income, 
                                mse.FH = mse.FH_noA_income, cv.FH = cv.FH_noA_income) 
outcome_income_UnobservedAreas$ID_MUNIC <- as.character(outcome_income_UnobservedAreas$ID_MUNIC)
outcome_income_UnobservedAreas$Group <- "Munic. Outside EMB"
########################################################################################## 

############################# 4. Dataset Unification ################################
DIR_Estimations_FH_income <- bind_rows(outcome_income_ObservedAreas,outcome_income_UnobservedAreas)  
DIR_Estimations_FH_income <- right_join(AuxInfo[c("ID_MUNIC", "MCPIO")],
                                        DIR_Estimations_FH_income, by = "ID_MUNIC")
########################################################################################## 


############################# 5. Exportation  ################################
saveRDS(DIR_Estimations_FH_income, "DIR_Estimations_FH_income.rds")
# write.xlsx(DIR_Estimations_FH_income, "DIR_Estimations_FH_income.xlsx", row.names = F)
########################################################################################## 
