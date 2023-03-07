
rm(list=ls())

#LOAD DATA, LIBRARIES AND FUNCTIONS
library('readxl')
dades <- read_excel("01-data_download/01_output_idealista_properties.xlsx")
library('data.table')
library('foreach')
library('dplyr')
library('doSNOW')
source('01-data_download/algorithms_and_functions.R')

#DATA CLEANING
setDT(dades)
#clean data
if(sum(dades$status=='')>0) dades$status[which(dades$status=='')] <-'good'
duplicats<-duplicated(dades$propertyCode)
if(sum(duplicats)>0) dades<-dades[-which(duplicats),]
duplicats<-duplicated(dades[,c('price','municipality','address','rooms')])
if(sum(duplicats)>0) dades<-dades[-which(duplicats),]
rm(duplicats)

#create accessory variables
dades$statusRevised<-fifelse(dades$status=='newdevelopment','good',dades$status)
dades[,lat_radians:=latitude/(180/pi)]
dades[,lon_radians:=longitude/(180/pi)]

dades[,floor:=gsub(' ','',floor)]
dades[,floorEsp:=fifelse(floor %in% c(-1:-100,'ss','st'),
                         'Sót.',
                         fifelse(floor %in% c('bj','en'),
                                 'Bj/En',
                                 fifelse(floor=='',
                                         'No Informado',
                                         floor)))]

dades$floorRevised<-NA
dades$floorRevised[which(dades$floor %in% c(-1:-100,'ss','bj','en','0','st'))]<-0
dades$floorRevised[which(is.na(dades$floor))]<-2
dades$floorRevised[which(is.na(dades$floorRevised))]<-as.numeric(dades$floor[which(is.na(dades$floorRevised))])

dades[,propertyTypeEsp:=fifelse(propertyType == 'flat',
                         'Piso',
                         fifelse(propertyType == 'duplex',
                                 'Dúplex',
                                 fifelse(propertyType=='penthouse',
                                         'Ático',
                                         fifelse(propertyType=='studio',
                                                 'Estudio',
                                                 NA_character_))))]
dades[,exteriorEsp:=fifelse(exterior == 'True',
                            'Exterior',
                            fifelse(exterior == 'False',
                                        'Interior',
                                    NA_character_))]

dades[,statusEsp:=fifelse(status == 'good',
                          'Buen Estado',
                          fifelse(status == 'newdevelopment',
                                  'Obra Nueva',
                                  fifelse(status == 'renew',
                                          'A Reformar',
                                          NA_character_)))]

dades[,hasLiftEsp:=fifelse(hasLift == 'True',
                          'Sí',
                          fifelse(hasLift == 'False',
                                  'No',
                                  NA_character_))]

#divide into two datasets
dadesSale<-dades[which(dades$operation=='sale'),]
dadesRent<-dades[which(dades$operation=='rent'),]

#### Parallel computing
## Prepare clusters for parallel computing
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
registerDoSNOW(my.cluster)
#check if it is registered (optional)
getDoParRegistered()

##Run the function in parallel
iterations <- nrow(dadesSale)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

distancesMatrix <- foreach(i = 1:iterations, .combine = rbind, .packages='foreach',
                           .options.snow = opts) %dopar%
  {
    getDistances(dadesSale[i,],dadesRent)
  }

###
dadesSaleROI <- foreach(i = 1:iterations, .combine = rbind, .packages='foreach',
                  .options.snow = opts) %dopar%
  {
    getSuggestedRentalPriceByArea_Algorithm2(dadesSale[i,],
                                             dadesRent,
                                             distancesMatrix=distancesMatrix[i,],
                                             numReferencesNeeded = 3,
                                             numReferencesMax = 6)
  }


close(pb)
stopCluster(my.cluster)

dadesSale<-dadesSaleROI; rm(dadesSaleROI)

#Create the rest of variables
dadesSale[,SuggestedRentalPrice := size*SuggestedRentalPriceByArea]
if(sum(is.na(dadesSale$SuggestedRentalPrice))>0){
  dadesSale<-dadesSale[-which(is.na(SuggestedRentalPrice)),]
}

dadesSale[,ROIpct := 100*12*SuggestedRentalPrice/(price)]
dadesSale[,ROIpct_15 := 100*12*SuggestedRentalPrice/(price*1.15)]

#a bit data preparation for plotting

dadesSale[,ROIpct:= round(ROIpct,1)]
dadesSale[,ROIpct_15:= round(ROIpct_15,1)]
dadesSale[,SuggestedRentalPrice:= round(SuggestedRentalPrice)]

dadesSale[,url_html := paste('<a href = "',url,'" target = "_blank" > Anuncio Idealista </a>',sep='')]
dadesRent[,url_html := paste('<a href = "',url,'" target = "_blank" > Anuncio Idealista </a>',sep='')]
# new column for the popup label

options(scipen=999)
dadesSale <- mutate(dadesSale, cntnt=paste0('<strong>Localidad: </strong>',municipality,
                                            '<br><strong>Rentabilidad: </strong>',paste(ROIpct,'%',sep=''),
                                            '<br><strong>Rentabilidad (precio+15%): </strong>',paste(ROIpct_15,'%',sep=''),
                                            '<br><strong>Precio:</strong> ', price,
                                            '<br><strong>Precio/m2:</strong> ', priceByArea,
                                            '<br><strong>Alquiler estimado:</strong> ', SuggestedRentalPrice,
                                            '<br><strong>Alquiler/m2 estimado:</strong> ', round(SuggestedRentalPriceByArea,1),
                                            '<br><strong>m2:</strong> ',size,
                                            '<br><strong>Tipo de propiedad:</strong> ',propertyTypeEsp,
                                            '<br><strong>Habitaciones:</strong> ',rooms,
                                            '<br><strong>Altura:</strong> ',floorEsp,
                                            '<br><strong>Ascensor:</strong> ',hasLiftEsp,
                                            '<br><strong>Exterior/Interior:</strong> ',exteriorEsp,
                                            '<br><strong>Estado:</strong> ',statusEsp,
                                            '<br><strong>Dirección:</strong> ',address,
                                            '<br><strong>Enlace:</strong> ',url_html))

dadesRent <- mutate(dadesRent, cntnt=paste0('<strong>Piso de referencia de alquiler</strong>',
                                            '<br><strong>Localidad:</strong> ', municipality,
                                            '<br><strong>Alquiler:</strong> ', price,
                                            '<br><strong>Alquiler/m2:</strong> ', priceByArea,
                                            '<br><strong>Tipo de propiedad:</strong> ',propertyTypeEsp,
                                            '<br><strong>m2:</strong> ',size,
                                            '<br><strong>Habitaciones:</strong> ',rooms,
                                            '<br><strong>Altura:</strong> ',floorEsp,
                                            '<br><strong>Ascensor:</strong> ',hasLiftEsp,
                                            '<br><strong>Exterior:</strong> ',exteriorEsp,
                                            '<br><strong>Estado:</strong> ',statusEsp,
                                            '<br><strong>Dirección:</strong> ',address,
                                            '<br><strong>Enlace:</strong> ',url_html))


save(dadesRent,dadesSale,file='01-data_download/03dadesWithROI.RData')
