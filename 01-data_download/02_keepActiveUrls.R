
dades_s <- read.csv("01-data_download/idealista_properties.csv", sep='|', header=T)
dades_r <- read.csv("01-data_download/idealista_rentals.csv", sep='|', header=T)

dades<-rbind(dades_s,dades_r)

w<-1:nrow(dades)
if('urlActive' %in% colnames(dades)){
  w<-which(dades$urlActive==T)
}
if(!'urlActive' %in% colnames(dades)){
  dades$urlActive<-T
}

# must find a way that doesn't block us
# print(length(w))
# for(i in w){
#   if(i %in% seq(50,length(w),50)) cat(i,end=' ')
#   dades$urlActive[i]<-RCurl::url.exists(dades$url[i])
# }

save(dades,file='01-data_download/dades.RData')
