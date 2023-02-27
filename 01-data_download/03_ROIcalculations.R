
rm(list=ls())

#load data, libraries and functions
load('01-data_download/dades.RData')
library('data.table')
library(foreach)
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

#code
dadesSale[,SuggestedRentalPriceByArea := NA]

for(i in 1:nrow(dadesSale)){
  
  if(i %in% seq(10,9450,10)) cat(i,end=' ')
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
  
  #get the distance to such rentals and keep only the closest 5
  distanceToRentals_list_i <- foreach(j = 1:nrow(dadesRent_i),
                                      .combine = 'c') %do% euc.dist(dadesSale_i[,c('latitude','longitude')],
                                                                    dadesRent_i[j,c('latitude','longitude')])
  names(distanceToRentals_list_i)<-dadesRent_i$propertyCode
  distanceToRentals_i<-sort(distanceToRentals_list_i)
  distanceToRentals_i_5closest<-distanceToRentals_i[1:numRentalsNeeded]
  codes_5closest<-names(distanceToRentals_i_5closest)
  dadesRent_i<-dadesRent_i[which(dadesRent_i$propertyCode %in% codes_5closest),]
  dadesRent_i<-dadesRent_i[match(codes_5closest, dadesRent_i$propertyCode),]
  
  #find the median price/m2
  dadesSale$ReferencedRentals[i]<-list(Map(list, dadesRent_i$propertyCode, dadesRent_i$url))
  dadesSale$SuggestedRentalPriceByArea[i]<-median(dadesRent_i$priceByArea)
}

dadesSale[,SuggestedRentalPrice := size*SuggestedRentalPriceByArea]

save(dadesRent,dadesSale,file='01-data_download/03dadesWithROI.RData')
