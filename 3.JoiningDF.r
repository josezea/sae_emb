rm(list = ls())
library(dplyr)

Route <-  "D:/Gdrive/Laboral 2017/USTA/FODEIN/SAE_EMB/"
setwd(paste0(Route, "processing_data"))

df_EMB <- readRDS("df_EMB.rds")
AuxInfo <- readRDS("AuxInfo.rds")


intersect(names(df_EMB),  names(AuxInfo))
df <- inner_join(df_EMB, AuxInfo, by = "ID_MUNIC")
saveRDS(df, "df.rds")
