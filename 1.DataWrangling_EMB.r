
######################################## Required Packages #################### 
rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(purrr)

Route <- "D:/Gdrive/Laboral 2017/USTA/FODEIN/SAE_EMB/"
setwd(Route)
setwd("scripts")
source("ParameterEstimation.r")

######################### Income Multiporpose Survey (Multipurpose Survey) #################

setwd(paste0(Route, "data"))
dir()

HouseholdIncome <- read_excel("HouseholdIncome.xlsx")
names(HouseholdIncome)[3] <- "ID_MUNIC"  
HouseholdIncome$ID_MUNIC <- as.character(HouseholdIncome$ID_MUNIC)
HouseholdIncome <- subset(HouseholdIncome, select = c(ID_MUNIC, DIRECTORIO,
                                      DIRECTORIO_HOG,  INGTOTAL, FEX_C))
colnames(HouseholdIncome) <- c("ID_MUNIC", "ID_HOUSE", "ID_HOUSEHOLD", "INCOME", "SURVEY_WEIGHTS")

## table(is.na(HouseholdIncome$INCOME))
## table(is.na(HouseholdIncome$SURVEY_WEIGHTS))

# Sampling Weights 
## sum(INGRESO$FEX_C)

query1 <- HouseholdIncome %>% group_by(ID_MUNIC) %>% 
  summarise(samplesizeByMun = n()) 

meansByMun <- HouseholdIncome %>% split(.$ID_MUNIC) %>%
  map_dbl(~mean.estimation(x = .$INCOME, weights = .$SURVEY_WEIGHTS))

numbersim <- 1000
varmeansByMun <- HouseholdIncome %>% split(.$ID_MUNIC) %>%
  map_dbl(~ estvar.mean(x = .$INCOME, weights = .$SURVEY_WEIGHTS, nsim = numbersim))


df_est.income <- data.frame(ID_MUNIC = names(meansByMun),
                                   meansByMun = meansByMun,
                                   varmeansByMun = varmeansByMun )
df_est.income$cvemeansByMun <-  sqrt(df_est.income$varmeansByMun) / df_est.income$meansByMun * 100


df_est.income$ID_MUNIC <- as.character(df_est.income$ID_MUNIC)

######################### Proportion Unemployment (Multipurpose Survey) #####################
# Unemployment 
# http://www.icesi.edu.co/cienfi/images/stories/pdf/glosario/poblacion-economicamente-activa.pdf
# PET: población en edad de trabajar (Population of working age)
# PEI: población económicamente inactiva (Economically Inactive Population)  
# PEA: población económica activa (Economically Active Population)
# OC: Ocupados (Employed),	DS: desocupados (Unemployed)

# Unemployment rate = DS / PEA   

setwd(paste0(Route, "data"))
dir()

UNEMPLOYMENT <- read_csv2("Unemployment.csv")
names(UNEMPLOYMENT)[2] <- "ID_HOUSE" 
names(UNEMPLOYMENT)[3] <- "ID_HOUSEHOLD" 
names(UNEMPLOYMENT)[4] <- "ID_MUNIC" 
names(UNEMPLOYMENT)[15] <- "SURVEY_WEIGHTS" 


UNEMPLOYMENT <- UNEMPLOYMENT[c("ID_MUNIC",  "ID_HOUSE", 
                              "ID_HOUSEHOLD", "DS",  "PEA", "SURVEY_WEIGHTS")]
UNEMPLOYMENT$ID_MUNIC <- as.character(UNEMPLOYMENT$ID_MUNIC)
# Test sampling weights 
sum(UNEMPLOYMENT$SURVEY_WEIGHTS)

# Fix sampling weights
HouseholdIncome$SURVEY_WEIGHTS2 <- HouseholdIncome$SURVEY_WEIGHTS 

# table(UNEMPLOYMENT$ID_HOUSEHOLD %in% HouseholdIncome$ID_HOUSEHOLD)
# table(HouseholdIncome$ID_HOUSEHOLD %in% UNEMPLOYMENT$ID_HOUSEHOLD)

UNEMPLOYMENT <- inner_join(UNEMPLOYMENT, 
                           HouseholdIncome[c("ID_HOUSEHOLD", "SURVEY_WEIGHTS2")])
UNEMPLOYMENT$SURVEY_WEIGHTS <- NULL
HouseholdIncome$SURVEY_WEIGHTS2 <- NULL
names(UNEMPLOYMENT)[names(UNEMPLOYMENT) == "SURVEY_WEIGHTS2"] <- "SURVEY_WEIGHTS"

# sum(UNEMPLOYMENT$SURVEY_WEIGHTS)
# sum(HouseholdIncome$SURVEY_WEIGHTS)



unempByMun <- UNEMPLOYMENT %>% split(.$ID_MUNIC) %>%
  map_dbl(~ratio.estimation(y = .$DS, z = .$PEA, weights = .$SURVEY_WEIGHTS))

numbersim <- 1000
varunempByMun <- UNEMPLOYMENT %>% split(.$ID_MUNIC) %>%
  map_dbl(~ estvar.ratio (y = .$DS, z = .$PEA, weights = .$SURVEY_WEIGHTS, nsim = numbersim))


df_est.unemployment <- data.frame(ID_MUNIC = names(unempByMun),
                                   unempByMun = unempByMun,
                                   varunempByMun = varunempByMun)

df_est.unemployment$ID_MUNIC <- as.character(df_est.unemployment$ID_MUNIC)

df_est.unemployment$cve.unempByMun <-  sqrt(df_est.unemployment$varunempByMun) / 
  df_est.unemployment$unempByMun * 100


df_est.unemployment$me_unemployment <- qnorm(0.975) * sqrt(df_est.unemployment$varunempByMun)


df_EMB <- inner_join(df_est.income, df_est.unemployment, by = "ID_MUNIC")
setwd(paste0(Route, "processing_data"))
saveRDS(df_EMB, "df_EMB.rds")