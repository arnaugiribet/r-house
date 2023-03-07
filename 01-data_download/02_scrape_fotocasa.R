
#load data, libraries and functions
load('01-data_download/dades.RData')

dades_ref<-dades


library('rvest')

url<-c('https://www.fotocasa.es/es/comprar/vivienda/tarrega/aire-acondicionado-ascensor-no-amueblado/177029941/d?from=list')

i<-1
for(url in urls){

r_url<-read_html(url)

#trobar la llista general: (1) rendimientos del trabajo, (2) rendimientos del capital mobiliario, (3) bienes i...
price_i<-
(r_url %>% html_nodes(xpath="*//div[@class='re-Page']//main//div//section//div//span[@class='re-CardPrice']") %>% html_text())[1]
price_i<-as.numeric(gsub(',','',gsub('\\.','',gsub('€','',gsub(' ','',price_i)))))

features_i<- r_url %>% html_nodes(xpath="*//div[@class='re-Page']//main//div//section//div//ul//li") %>% html_text()

rooms_i<-features_i[which(grepl('habs',features_i))[1]]
rooms_i<-as.numeric(gsub('\\.','',gsub('habs','',gsub(' ','',rooms_i))))

r_url %>% html_nodes(xpath="*//div[@class='re-Page']//main//section")

r_url %>% html_nodes(xpath="//div//section[@class='sui-SectionInfo']//div[@class='re-DetailMap']")

r_url %>% html_nodes(xpath="*//div//a[@target='_blank']")%>% html_attr('href')

lat_lon %>% stringr::str_match_all("latitude") 
}


pestanyes_excel<-sapply(1:length(arbre1), function(x) arbre1[x] %>% html_nodes('a') %>% html_text())
