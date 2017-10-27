######################################## Required Packages #################### 
rm(list = ls())

library(readxl)
library(readr)
library(dplyr)
library(stringr)


Route <- "D:/Gdrive/Laboral 2017/USTA/FODEIN/SAE_EMB/"
setwd(Route)

######################### Income Multiporpose Survey (Multipurpose Survey) #################

setwd(paste0(Route, "data/AuxiliarInformation"))

# Metadata of CONSOLIDADO.xlsx in METADATOS sheet
consolidado <- read_excel("CONSOLIDADO.xlsx", sheet = "DATOS")
names(consolidado)[1] <- "COD_DANE" 
##############################################################################

################# Pruebas Saber Año 2016 ##################################


saber_20161 <- read.delim("SB11-20161-RGSTRO-CLFCCN-V3-0.txt", 
                          sep = "|")
saber_20161 <- subset(saber_20161, ESTU_COD_RESIDE_DEPTO %in% c("11","25"),
                      select = c(ESTU_COD_RESIDE_DEPTO, ESTU_COD_RESIDE_MCPIO,
                                 PUNT_GLOBAL))

saber_20162 <- read.delim("SB11-20162-RGSTRO-CLFCCN-V1-0.txt", 
                          sep = "|")
saber_20162 <- subset(saber_20162, ESTU_COD_RESIDE_DEPTO %in% c("11","25"),
                      select = c(ESTU_COD_RESIDE_DEPTO,
                                 ESTU_COD_RESIDE_MCPIO,
                                 PUNT_GLOBAL))

saber_2016 <- bind_rows(saber_20161, saber_20162)

saber2016 <- saber_2016 %>% group_by(ESTU_COD_RESIDE_MCPIO) %>%
  summarise(PUNT_GLOBAL = mean(PUNT_GLOBAL)) %>% arrange(-PUNT_GLOBAL)
names(saber2016)[1] <- "COD_DANE"
saber2016$COD_DANE <- as.character(saber2016$COD_DANE)
rm(saber_2016); rm(saber_20161); rm(saber_20162)
names(saber2016) <- c("COD_DANE", "PUNTAJE_SABER")

##################################################################


################## Información del SISBEN #########################

# Descargado de datos abiertos
SisbenAprobados <- read_excel("SisbenAprobados.xlsx")
names(SisbenAprobados) <- gsub(" ", "", names(SisbenAprobados))
table(SisbenAprobados$CortedelSisbén)
class(SisbenAprobados$CortedelSisbén)
# Período de la encuesta
SisbenAprobados <- subset(SisbenAprobados,
                          CortedelSisbén == 201406)

# Corte a 201406
SisbenAprobados$COD_DANE <- str_pad(SisbenAprobados$CódigoMunicipio,
                                    width = 5, 
                                    pad = "0")
SisbenAprobados <- subset(SisbenAprobados, 
                          substr(SisbenAprobados$COD_DANE, 1, 2) %in% c("11", "25"),
                          select = c(COD_DANE, Municipio, Numerodepersonasvalidadas))

SisbenAprobados$Municipio <- NULL
names(SisbenAprobados)[2] <- "PERSONAS_SISBEN2014"
#######################################################################



############################ Índice de incidencia de pobreza ###########################
# IPM por municipio y dpto 2005 (Incidencias y Privaciones_F).xls
IncidenciaPobreza <- read_excel("Incidencia.xlsx")
names(IncidenciaPobreza) <- gsub(" ", "",names(IncidenciaPobreza))
names(IncidenciaPobreza)[7] <- "Incidencia_pobreza"
IncidenciaPobreza <- subset(IncidenciaPobreza, CodDpto %in% c("25", "11"),
                     select = c(Codigomunicipio, 
                                Incidencia_pobreza))
names(IncidenciaPobreza)[1] <- "COD_DANE"
IncidenciaPobreza$COD_DANE <- as.character(IncidenciaPobreza$COD_DANE)
#####################################################################################


############################## Ejecuciones presupuestales ######################
# https://www.dnp.gov.co/programas/desarrollo-territorial/Paginas/ejecuciones-presupuestales.aspx
# Ejecuciones Presupuestales 2000-2012 WEB V4-9-2013.xlsx
EjecPresup <- read_excel("Ejecuciones Presupuestales 2000-2012 WEB V4-9-2013.xlsx",
              sheet = "Municipios", range = "A3:R36336" )
table(EjecPresup$CUENTA2)
EjecPresup <- subset(EjecPresup, CUENTA2 ==  "INGRESOS TOTALES" )
names(EjecPresup)
EjecPresup$A2002 <- as.numeric(EjecPresup$A2002 )
table(is.na(EjecPresup$A2002), useNA = "always")

EjecPresup$A2004 <- as.numeric(EjecPresup$A2004 )
table(is.na(EjecPresup$A2004), useNA = "always")

EjecPresup$EjecPresup0012 <- rowSums(EjecPresup[,6:18])
EjecPresup <- EjecPresup[c("CODIGO", "EjecPresup0012")]
names(EjecPresup)[1] <- "COD_DANE"
EjecPresup <- subset(EjecPresup, substr(EjecPresup$COD_DANE, 1, 2) %in% 
                       c("25"))
#####################################################################################


################################ Proyecciones de población ################################
# https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion
proypob <- read_excel("Proyecciones.xls")
proypob <- proypob[c("DPMP", "MPIO", "2014")]
names(proypob)[3] <- "POBLACION2014"
#proy$MPIO2 <- iconv(proy$MPIO,from="UTF-8",to="ASCII//TRANSLIT")
proypob <- proypob[substr(proypob$DPMP, 1, 2) %in% c("25","11"), ]
proypob <- subset(proypob, select = c(DPMP, POBLACION2014))
names(proypob)[1] <- "COD_DANE"
#####################################################################################


#################################### Unificación bases ###################################
dim(proypob); dim(SisbenAprobados); dim(saber2016); 
dim(IncidenciaPobreza); dim(EjecPresup); dim(consolidado)

InfoAuxiliar <- inner_join(proypob, SisbenAprobados, by = "COD_DANE")
dim(InfoAuxiliar)

InfoAuxiliar <- inner_join(InfoAuxiliar, saber2016, 
                           by = "COD_DANE")
dim(InfoAuxiliar)

InfoAuxiliar <- inner_join(InfoAuxiliar, IncidenciaPobreza, 
                           by = "COD_DANE")
dim(InfoAuxiliar)


InfoAuxiliar <- inner_join(InfoAuxiliar, EjecPresup, 
                           by = "COD_DANE")
dim(InfoAuxiliar)


InfoAuxiliar <- inner_join(InfoAuxiliar,  consolidado, 
                           by = "COD_DANE")
dim(InfoAuxiliar)


# TASA SISBENIZADOS
InfoAuxiliar$TASA_SISBENIZADOS <- InfoAuxiliar$PERSONAS_SISBEN2014 / 
  InfoAuxiliar$POBLACION2014

Orden_variables <- c("COD_DANE", "DEPTO", "MCPIO", "POBLACION2014", 
                     "PERSONAS_SISBEN2014", "TASA_SISBENIZADOS",
                     "PUNTAJE_SABER", 
  "AREA_KM2", "POBLACION", "AFILIADOS_REG_CONTRIBUTIVO", 
  "TASA_AFILIADOS_CONTRIBUTIVO", "AFILIADOS_REG_SUBSIDIADO", 
  "TASA_AFILIADOS_SUBSIDIADO", 
  "AVALUOS_CATASTRALES_RURALES", "AVALUOS_CATASTRALES_URBANOS", 
  "COBER_MEDIA_TASA", "COBER_PRIMARIA_TASA", "COBER_SECUNDARIA_TASA", 
  "COBER_TRANSICION_TASA", "CONSUMO_ENERGIA_PER_HABIT",
  "DEPENDENCIA_REGALIAS_TRANSFERENCIAS", 
  "DESMINADO_MILITAR_OPERACIONES", "ESFUERZO_FISCAL", 
  "IND_CAPACIDAD_ADMINISTRATIVA", 
  "IND_CUMPLIMIENTO_REQUI_LEGALES", "IND_EFICIENCIA_MUNICIPAL", 
  "IND_EFICACIA_MUNICIPAL", "IND_INTEGRAL_MCIPAL", "NBI_2010", 
  "NBI_RURAL_2010", "NBI_URBANO_2010", "NUM_BIBLIOTECAS", "PROY_CULTURALES_CONCERTADOS", 
  "PERSONAS_EXPULSADOS", "PERSONAS_RECIBIDOS", "PERSONAS_DECLARADOS", 
  "PERSONAS_SECUESTRADAS", "TASA_DELITOS_SEXUALES", "TASA_DESERCION_ESCOLAR", 
  "TASA_VIOLENCIA_INTRAFAMILIAR", "TASA_HOMICIDIOS", "TASA_VACUN_BCG", 
  "TASA_VACUN_DPT", "TASA_VACUN_POLIO", "TASA_VACUN_TRIPLEVIRAL")

InfoAuxiliar <- InfoAuxiliar[Orden_variables]
names(InfoAuxiliar)[1] <- "ID_MUNIC"
setwd(paste0(Route, "processing_data"))
saveRDS(InfoAuxiliar, "AuxInfo.rds")
#####################################################################################
