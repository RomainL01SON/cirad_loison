#' DSSAT PlantGro summary
#'
#' This function computes growing degree days and its sum
#' @param variable List of variable to get max values from (variables names in PlantGro)
#' @param path folder where files .OUT are located
#' @keywords DSSAT PlantGro get max values per RUN and file
#' @export
#' @examples
#' GDD_cirad(13,20,30)
#' variable <- "VWAD"
#'début = Sys.time()
#'cirad_read_PLANTGRO(variable,"D:/Mes Donnees/03. Matériels et méthodes/library_Romain/developpement/read plantgro")
#'durée <- Sys.time() - début


cirad_read_PLANTGRO <- function(variable, path){
 # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # définir le dossier de travail où est enregistré le fichier R
setwd(path)
library(readr)
library(stringr)
library(plyr)
library(tidyr)

for(i in 1:length(unique(dir(pattern=".OUT")))){
 dt1 <- readr::read_fwf(dir(pattern=".OUT")[i],
                        col_positions = fwf_widths(c(5,4,6,6,rep(7,10),8,
                                                     rep(7,10),8,rep(7,9),rep(8,7),9,8)))

 # get variables name
 a<-dt1[dt1$X1=="@YEAR"&!is.na(dt1$X2),]
 names_variables <- unlist(a[1,])
 names(dt1)<- names_variables
 dt1$FileName <- gsub(".OUT", "", dir(pattern=".OUT")[i])

 # new dataset with only variables of interest
 vari <- which(names_variables %in% variable)

  dt <- dt1[,c("@YEAR","DOY",variable,"FileName")]

  dt$year <- as.numeric(unlist(dt[,1]))
  dt$id <- paste0(dt1$DAP,dt1$`L#SD`,dt1$GSTD,dt1$LAID)
  dt$`@YEAR`[is.na(dt$`@YEAR`)] <- ""


  dt$DOY <- as.numeric(gsub(" ","",dt$DOY))
  dt$DOY <- stringr::str_pad(dt$DOY,2,"left",pad="0")
  dt$run[dt$`@YEAR`=="*RUN"] <- paste0("RUN",dt$DOY[dt$`@YEAR`=="*RUN"])
  dt$id <- gsub(" ","",dt$id)
  dt$id <- gsub(":","",dt$id)

  dt$traitement[dt$`@YEAR`=="TREA"] <- dt$id[dt$`@YEAR`=="TREA"]

  dt <- dt[,c(variable, "FileName", "year", "run", "traitement")]
  names(dt)<- c(variable,"fichier_OUT","annee","RUN","traitement")


  dt$RUN[1]<-"RUN01"
  library(tidyr)
  dt2<- dt %>% fill(RUN,traitement)

  dt3 <- dt2[!is.na(dt2$annee),]
  for(k in 1:length(variable)){
      dt3[,variable[k]] <- as.numeric(unlist(dt3[,variable[k]]))
  }


  dt4 <- gather(dt3, variables, values, variable[1]:variable[length(variable)], factor_key=TRUE)


# dt5<- dplyr::case_when(
#     calcul==1  ~ ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=max(values)),
#     calcul==2  ~ ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=min(values)),
#     calcul==3  ~ ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=sd(values)),
#     calcul==4  ~ ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=mean(values)),
#     calcul==5  ~ ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=median(values)))
dt5 <-ddply(dt4,.(fichier_OUT,annee,RUN,traitement,variables),summarise,cal=max(values))

  final <- spread(dt5, variables, cal)

  assign(paste0("sortie_",i),final)

}

aaaa <- mget(ls(pattern="sortie"))
data.table_rbindlist <- as.data.frame(data.table::rbindlist(aaaa))

write.csv(data.table_rbindlist,"RESULTATS.csv",row.names = F)
}

