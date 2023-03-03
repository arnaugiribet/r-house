
rm(list=ls())

#load data, libraries and functions
load('01-data_download/dades.RData')
library('data.table')
library(foreach)
library('dplyr')
library(doSNOW)

euc.dist <- function(x1, x2, units='km') {
  
  #each x1 and x2 are a vector containing a latitude and a longitude
  x1_radians<-unlist(x1/(180/pi))
  x2_radians<-unlist(x2/(180/pi))
  
  d<-6371*acos((sin(x1_radians[1]) * sin(x2_radians[1])) + cos(x1_radians[1]) * cos(x2_radians[1]) * cos(x2_radians[2]-x1_radians[2]))
  d<-round(d,3)
  if(units=='m') d<-d*1000
  
  return(unname(d))
}

setDT(dades)
#clean data
if(sum(dades$status=='')>0) dades$status[which(dades$status=='')] <-'good'

#create accessory variables for
dades[,statusRevised:=fifelse(status=='newdevelopment','good',status)]

#divide into two datasets
dadesSale<-dades[which(dades$operation=='sale'),]
dadesRent<-dades[which(dades$operation=='rent'),]

# hierarchy
##1. estat del pis
##2. habitacions
##3. distància

################## start of function
dadesSale$ReferencedRentals<-NA
dadesSale$NumReferencedRentals<-NA
dadesSale$SuggestedRentalPriceByArea<-NA

getSuggestedRentalPriceByArea <- function(dadesSale,i){


  
  dadesSale_i<-dadesSale[i,]
  
  #find similar rentals. if less than 5 are found, add values to the number of rooms to search for
  dadesRent_i<-data.frame()
  numRentals<-0
  numRentalsNeeded<-5
  numRentalsTooMany<-200
  size_i<-dadesSale_i$size
  sizeMax<-size_i+20
  sizeMin<-size_i-20
  minSizeInterval<-F
  rooms_ref<-dadesSale_i$rooms
  exterior_interior<-unique(c(dadesSale_i$exterior,'True','False')) #obtinc el segon que és el contrari del primer
  it<-1
  alwaysUp<-T
  alwaysDown<-F
  
  while(numRentals<numRentalsNeeded & minSizeInterval!= T){
    
    dadesRent_i<-dadesRent[which(dadesRent$rooms %in% rooms_ref &
                                   dadesRent$statusRevised == dadesSale_i$statusRevised &
                                   dadesRent$exterior %in% exterior_interior &
                                   dadesRent$size<sizeMax & dadesRent$size>sizeMin),]
    numRentals<-nrow(dadesRent_i)
    
    #first we'll see if we have to go always up or always down. we don't want our algorithm to keep moving up and down
    if(numRentals<numRentalsNeeded & it==1) alwaysUp<-T
    if(numRentals>numRentalsTooMany & it==1) alwaysDown<-T
    #stopping condition 1: we have fewer rooms than needed
    
    if(numRentals<numRentalsNeeded & alwaysUp==T) {
      rooms_ref<-(max(0,min(rooms_ref)-1)):(max(rooms_ref)+1)
      sizeMax<-sizeMax+5
      sizeMin<-max(0,sizeMin-5)
      }
    
    #stopping condition 2: we have too many rooms
    if(numRentals>numRentalsTooMany) {
      exterior_interior <- exterior_interior[1]
      if(sizeMax-size_i>3){ #si encara la forquilla encara no és de ±5
        sizeMax<-sizeMax-3
        sizeMin<-sizeMin+3
      }
      if(sizeMax-size_i<3) {
        minSizeInterval <-T
      }
    }
    it<-it+1
  }
  
  #get the distance to such rentals and keep only the number we chose to be needed for the algorithm, originally 5
  distanceToRentals_list_i <- foreach(j = 1:nrow(dadesRent_i),
                                      .combine = 'c') %do% euc.dist(dadesSale_i[,c('latitude','longitude')],
                                                                    dadesRent_i[j,c('latitude','longitude')])
  names(distanceToRentals_list_i)<-dadesRent_i$propertyCode
  distanceToRentals_i<-sort(distanceToRentals_list_i)
  distanceToRentals_i_5closest<-distanceToRentals_i[1:numRentalsNeeded]
  
  #if the distance gets bigger quickly, we filter those
  #currently: the distance to the first is the reference. we cut those which are bigger than 5 times the distance
  w_keep<-which(distanceToRentals_i_5closest/distanceToRentals_i_5closest[1]<5)
  distanceToRentals_i_5closest<-distanceToRentals_i_5closest[w_keep]
  codes_5closest<-names(distanceToRentals_i_5closest)

  #we keep only these
  dadesRent_i<-dadesRent_i[which(dadesRent_i$propertyCode %in% codes_5closest),]  #filter them
  dadesRent_i<-dadesRent_i[match(codes_5closest, dadesRent_i$propertyCode),]   #order them

  #find the mean price/m2
  dadesSale_i$NumReferencedRentals<-nrow(dadesRent_i)
  dadesSale_i$ReferencedRentals<-list(Map(c, dadesRent_i$propertyCode, dadesRent_i$url))
  dadesSale_i$SuggestedRentalPriceByArea<-mean(dadesRent_i$priceByArea)
  
  return(dadesSale_i)
}
################## end of function

#I prepare my clusters for parallel computing
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#check cluster definition (optional)
print(my.cluster)
#register it to be used by %dopar%
registerDoSNOW(my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()

#Run the function in parallel!
iterations <- nrow(dadesSale)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

dadesSaleROI <- foreach(i = 1:iterations, .combine = rbind, .packages='foreach',
                  .options.snow = opts) %dopar%
  {
    getSuggestedRentalPriceByArea(dadesSale,i)
  }

close(pb)
stopCluster(my.cluster)

dadesSale<-dadesSaleROI; rm(dadesSaleROI)

#Create the rest of variables
dadesSale[,SuggestedRentalPrice := size*SuggestedRentalPriceByArea]
if(sum(is.na(dadesSale$SuggestedRentalPrice))>0){
  dadesSale<-dadesSale[-which(is.na(SuggestedRentalPrice)),]
}

dadesSale[,ROIpct := 100*12*SuggestedRentalPrice/(price*1.15)]

#a bit data preparation for plotting

dadesSale[,ROIpct:= round(ROIpct,1)]
dadesSale[,SuggestedRentalPrice:= round(SuggestedRentalPrice)]

dadesSale[,url_html := paste('<a href = "',url,'"> Anuncio Idealista </a>',sep='')]
dadesRent[,url_html := paste('<a href = "',url,'"> Anuncio Idealista </a>',sep='')]
# new column for the popup label

dadesSale <- mutate(dadesSale, cntnt=paste0('<strong>Localidad: </strong>',municipality,
                                            '<br><strong>Rentabilidad: </strong>',paste(ROIpct,'%',sep=''),
                                            '<br><strong>Precio:</strong> ', price,
                                            '<br><strong>Precio por m2:</strong> ', priceByArea,
                                            '<br><strong>Alquiler estimado:</strong> ', SuggestedRentalPrice,
                                            '<br><strong>Tipo de propiedad:</strong> ',propertyType,
                                            '<br><strong>m2:</strong> ',size,
                                            '<br><strong>Habitaciones:</strong> ',rooms,
                                            '<br><strong>Altura:</strong> ',floor,
                                            '<br><strong>Ascensor:</strong> ',hasLift,
                                            '<br><strong>Exterior:</strong> ',exterior,
                                            '<br><strong>Estado:</strong> ',status,
                                            '<br><strong>Dirección:</strong> ',address,
                                            '<br><strong>Enlace:</strong> ',url_html))

dadesRent <- mutate(dadesRent, cntnt=paste0('<strong>Localidad:</strong> ', municipality,
                                            '<br><strong>Alquiler:</strong> ', price,
                                            '<br><strong>Alquiler por m2:</strong> ', priceByArea,
                                            '<br><strong>Tipo de propiedad:</strong> ',propertyType,
                                            '<br><strong>m2:</strong> ',size,
                                            '<br><strong>Habitaciones:</strong> ',rooms,
                                            '<br><strong>Altura:</strong> ',floor,
                                            '<br><strong>Ascensor:</strong> ',hasLift,
                                            '<br><strong>Exterior:</strong> ',exterior,
                                            '<br><strong>Estado:</strong> ',status,
                                            '<br><strong>Dirección:</strong> ',address,
                                            '<br><strong>Enlace:</strong> ',url_html))


save(dadesRent,dadesSale,file='01-data_download/03dadesWithROI.RData')
