euc.dist <- function(vec1_lat_lon_rad, vec2_lat_lon_rad, units='km') {
  
  #each vec1 and vec2 are a vector containing a latitude and a longitude in radians
  
  d<-6371*acos(
    (sin(vec1_lat_lon_rad[[1]]) * sin(vec2_lat_lon_rad[[1]])) + 
      cos(vec1_lat_lon_rad[[1]]) * 
      cos(vec2_lat_lon_rad[[1]]) *
      cos(vec2_lat_lon_rad[[2]]-vec1_lat_lon_rad[[2]])
  )
  
  d<-round(d,3)
  if(units=='m') d<-d*1000
  
  return(unname(d))
}

getDistances<-function(dadesSale_i,dadesRent){
  #dadesSale_i must be a single row, dadesRent must be the whole dadesRent dataset
  distances<-euc.dist(vec1_lat_lon_rad = dadesRent[,c('lat_radians','lon_radians')],
                      vec2_lat_lon_rad = dadesSale_i[,c('lat_radians','lon_radians')])
  return(distances)
  
}

getSuggestedRentalPriceByArea_Algorithm1 <- function(dadesSale_i){
  
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

getSuggestedRentalPriceByArea_Algorithm2 <- function(dadesSale_i,dadesRent,distancesMatrix_i,numReferencesNeeded=5, numReferencesMax=8){
  
  max_distances<-c(100,250,500,1000,2500,5000)/1000
  
  # The algorithm begins at a 100m radius and searches for those most similar.
  # Then it softens a bit the similarity paremeters.
  # Finally the distance is increased and the similarity parameters are restored.
  
  # This goes on and on until we hit the minimum of 5 similar rentals.
  # In the worst case scenario we get to 5 km radius, then just take the closest ones.
  
  whichReferences<-c()
  numReferences<-length(whichReferences)
  iterationOutsideDistance<-0
  
  for(max_distance in max_distances){
    
    iterationOutsideDistance<-iterationOutsideDistance+1
    print(paste('Distance:',max_distance))
    
    #Similarity parameters initialization for every new distance radius
    size_min<-max(dadesSale_i$size-10,0)
    size_max<-dadesSale_i$size+10
    floor_min<-dadesSale_i$floorRevised
    floor_max<-floor_min
    rooms_min<-dadesSale_i$rooms
    rooms_max<-rooms_min
    has_lift<-c('True','False')
    if(dadesSale_i$floorRevised >= 3) has_lift<-dadesSale_i$hasLift
    
    
    #beginning of iteration within a distance
    iterationInDistance<-1
    while(numReferences<numReferencesNeeded & iterationInDistance<=5){

      whichReferences_it<-which(distancesMatrix_i<max_distance & 
                                  dplyr::between(dadesRent$size,size_min,size_max) &
                                  dplyr::between(dadesRent$floorRevised,floor_min,floor_max) &
                                  dplyr::between(dadesRent$rooms,rooms_min,rooms_max) &
                                  dadesRent$hasLift==has_lift)
      
      whichReferences<-unique(c(whichReferences,whichReferences_it))
      numReferences<-length(whichReferences)
      
      #changes for next iteration inside the same distance
      iterationInDistance<-iterationInDistance+1
      
      #REGARDLESS OF DISTANCE
      #size 1
      
      if(iterationInDistance==2 & iterationOutsideDistance>=1){
        size_min<-max(size_min-10,0)
        size_max<-size_max+10
      }
      
      #floor 1
      if(iterationInDistance==3 & iterationOutsideDistance>=1){
        floor_min<-max(floor_min-1,0)
        floor_max<-floor_max+1
      }
      
      #rooms 1
      if(iterationInDistance==3 & iterationOutsideDistance>=1){
        rooms_min<-max(rooms_min-1,0)
        rooms_max<-rooms_max+1
      }
      
      if(iterationInDistance==3 & iterationOutsideDistance>=1){
        size_min<-max(size_min-10,0)
        size_max<-size_max+10
      }
    
      if(iterationInDistance==4 & iterationOutsideDistance>=1){
        rooms_min<-max(rooms_min-1,0)
        rooms_min<-rooms_max+1
      }
      
      if(iterationInDistance==5 & iterationOutsideDistance>=1){
        floor_min<-max(floor_min-1,0)
        floor_max<-floor_max+1
      }
      
      #DISTANCE[3]
      #floor 3
      if(iterationInDistance==3 & iterationOutsideDistance>=3){
        floor_min<-max(floor_min-1,0)
        floor_max<-floor_max+1
      }
      
      #size 3
      if(iterationInDistance==3 & iterationOutsideDistance>=3){
        size_min<-max(size_min-10,0)
        size_max<-size_max+10
      }

      #print(paste('Num References:',numReferences))
    }
    
    if(numReferences>=numReferencesNeeded){
      break
    }
    
  }
  
  #if we are short some references, we just get the closest one
  if(numReferences<numReferencesNeeded){
    whichReferences<-unique(c(whichReferences,order(distancesMatrix_i)[1:numReferencesMax]))
    numReferences<-length(whichReferences)
  }
  
  #if the distance gets bigger quickly, we filter those out
  #currently: the distance to the second is the reference - because the first could closely lucky
  # we cut those which are bigger than 5 times the distance
  whichReferences<-whichReferences[order(distancesMatrix_i[whichReferences])] #order by distance
  distancesReferences<-distancesMatrix_i[whichReferences] #save distance
  w_keep<-which(distancesReferences/distancesReferences[2]<=5)
  whichReferences<-whichReferences[w_keep]
  distancesReferences<-distancesReferences[w_keep]
  numReferences<-length(whichReferences)
  
  #if still too many, we cut to the Max Number established in the beginning
  whichReferences<-whichReferences[1:min(numReferencesMax,numReferences)]
  numReferences<-length(whichReferences)
  distancesReferences<-distancesReferences[1:length(whichReferences)]
  
  #filter rent dataset
  dadesRent_i<-dadesRent[whichReferences,]
  dadesRent_i$distanceToProperty<-distancesMatrix_i[whichReferences]
  #View(dadesRent_i[,c('floor','price','priceByArea','propertyType','size','exterior','rooms','distanceToProperty')])

  
  #Return New Columns
  dadesSale_i$NumReferencedRentals<-numReferences
  dadesSale_i$ReferencedRentals<-list(dadesRent_i$propertyCode)
  dadesSale_i$ReferencedRentalsDistance<-list(dadesRent_i$distanceToProperty)
  
  #find the mean price/m2
  
  #two step algorithm:
  #if the size of the flat exceeds every one of our referenced ones, we set boundaries on the price/m2
  
  
  ## weighted mean by price/area
  if(dadesSale_i$size>mean(dadesRent_i$size)){
    dadesSale_i$SuggestedRentalPriceByArea<-weighted.mean(dadesRent_i$priceByArea,w=dadesRent_i$size)
  }
  
  if(dadesSale_i$size<=mean(dadesRent_i$size)){
    dadesSale_i$SuggestedRentalPriceByArea<-weighted.mean(dadesRent_i$priceByArea,w=1/dadesRent_i$size)
  }
  
  ## we correct the suggested price/m2 in case the size is either extremely big or small
  if(sum(dadesSale_i$size>dadesRent_i$size)>=nrow(dadesRent_i)*0.85){
    dadesSale_i$SuggestedRentalPriceByArea<-weighted.mean(dadesRent_i$price,w=dadesRent_i$distanceToProperty)/dadesSale_i$size
  }
  
  if(sum(dadesSale_i$size<dadesRent_i$size)<=nrow(dadesRent_i)*0.85){
    dadesSale_i$SuggestedRentalPriceByArea<-weighted.mean(dadesRent_i$price,w=dadesRent_i$distanceToProperty)/dadesSale_i$size
  }
  
  return(dadesSale_i)
  
}









