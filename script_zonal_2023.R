### 2023 Triatomine Chagas vector maps - raster and bivariate map generation
### Renata Muylaert
### Zonal statistics for suitability rasters
### Bivariate maps for landscape and climate suitability

###-----------------------------------------------------------------------------------------###

rm(list = ls())
gc() 
memory.limit(size = 10000000000) 

# instalar e carregar pacotes
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dep = T)
  sapply(pkg, require, character.only = T)}

packages <- c('raster')

ipak(packages)


###-----------------------------------------------------------------------------------------###

# diretorio
setwd('D://OneDrive - Massey University//Coworks//Chagas_2020//zonal//')

###-----------------------------------------------------------------------------------------###
# import data
enm.clim <- raster("ensemble_climate_freq_all.tif")
plot(enm.clim)

# layer
pr <- shapefile("municipios_parana_media_clima_paisagem_join_gcs_wgs84.shp")
plot(pr)
head(pr@data$FID_1)

pr.raster <- rasterize(pr, enm.clim, field = "FID_1")
pr.raster
plot(pr.raster)

zonal.pr.clim <- zonal(enm.clim, pr.raster, "mean")
zonal.pr.clim
colnames(zonal.pr.clim) <- c("FID_1", "mean")
zonal.pr.clim

pr.merge.clim <- pr

pr.merge.clim@data <- merge(pr@data, zonal.pr.clim, by = "FID_1")

pr.raster.clim <- rasterize(pr.merge.clim, enm.clim, field = "mean")
plot(pr.raster.clim)

writeRaster(pr.raster.clim, "zonal_ensemble_climate_freq_all.asc", format = "ascii", overwrite=TRUE)

###-----------------------------------------------------------------------------------------###

# import data
enm.lands <- raster("ensemble_land_freq_all.tif")

zonal.pr.lands <- zonal(enm.lands, pr.raster, "mean")
zonal.pr.lands
colnames(zonal.pr.lands) <- c("FID_1", "mean")
zonal.pr.lands

pr.merge.lands <- pr

pr.merge.lands@data <- merge(pr@data, zonal.pr.lands, by = "FID_1")

pr.raster.lands <- rasterize(pr.merge.lands, enm.lands, field = "mean")
plot(pr.raster.lands)


writeRaster(pr.raster.lands, "zonal_ensemble_land_freq_all.asc", format = "ascii", overwrite = T)


###-----------------------------------------------------------------------------------------###

# Figures for models containging all triatomine vectors - zonal

enm.clim.z <- raster("zonal_ensemble_climate_freq_all.asc") 
enm.clim.v <- enm.clim.z[]
length(enm.clim.v)

enm.lands.z <- raster("zonal_ensemble_land_freq_all.asc") 
enm.lands.v <- enm.lands.z[]
length(enm.lands.v)

da <- data.frame(clim = enm.clim.v, lands = enm.lands.v)
da.na <- na.omit(da)
da.na

plot(da.na$clim, da.na$lands, pch=19,
       xlab = "Climate Suitability", 
     ylab = "Landscape Suitability",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

# Bivariate map
# Color Options: https://observablehq.com/@benjaminadk/bivariate-choropleth-color-generator

#png(filename = 'scenario_04.png', res = 400, units = 'cm', width = 20, height = 20      )

colmat<-function(nquantiles=10, upperleft="blue", upperright="red", bottomleft="grey", bottomright="yellow", xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classInt::classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-classInt::findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-classInt::findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-classInt::findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    graphics::points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)}
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix<-col.matrix[c(seqs), c(seqs)]}

bivariate.map<-function(rasterx, rastery, colormatrix, nquantiles=10){
  quanmean<-raster::getValues(rasterx)
  temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  r1<-within(temp, quantile <- base::cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr<-data.frame(r1[,2])
  quanvar<-raster::getValues(rastery)
  temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  r2<-within(temp, quantile <- base::cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr2<-data.frame(r2[,2])
  as.numeric.factor<-function(x) {as.numeric(base::levels(x))[x]}
  col.matrix2<-colormatrix
  cn<-unique(colormatrix)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
  cols<-numeric(length(quantr[,1]))
  for(i in 1:length(quantr[,1])){
    a<-as.numeric.factor(quantr[i,1])
    b<-as.numeric.factor(quantr2[i,1])
    cols[i]<-as.numeric(col.matrix2[b,a])}
  r<-rasterx
  r[1:length(r)]<-cols
  return(r)}


col.matrix <- colmat(nquantiles=3,
                                    upperleft= '#52b6b6', #'khaki',#'khaki', #, ##4DDDDD
                                    upperright= '#434e87', #'#ff0000',#"#B22222",#"violetred4", 
                                    bottomleft= '#d3d3d3', #'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright=  '#ad5b9c',#'blue',#'#141414',# "#141414",
                                    xlab="Climate Suitability", 
                                    ylab="Landscape Suitability")


bivmapz <- bivariate.map(enm.clim.z, enm.lands.z, 
                         colormatrix=col.matrix, nquantiles=3)
bivmapz

png(filename = 'risk_map.png', res = 400, units = 'cm', width = 20, height = 20      )

plot(bivmapz,frame.plot=T,axes=T,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Chagas vector suitability')

#map(interior=T, add=T)

dev.off()

#-------------

#Zonal stats for individual species rasters

clist <- c('ensemble_climate_freq_pan_g',
           'ensemble_climate_freq_pan_m',
           'ensemble_climate_freq_rhodnius_n',
           'ensemble_climate_freq_triatoma_s')

lalist <- c("ensemble_land_freq_pan_g",
"ensemble_land_freq_pan_m",
"ensemble_land_freq_rhodnius_n",
"ensemble_land_freq_triatoma_s")


setwd('D://OneDrive - Massey University//Coworks//Chagas_2020//zonal//')
dir.create('zonal_raster_individual_species')

i <- 1

for(i in 1:length(clist) ){
  
    print(i)
  
    enm.clim <- raster(paste0(clist[i], ".tif"))
    
    zonal.pr.clim <- zonal(enm.clim, pr.raster, "mean")
    zonal.pr.clim
    colnames(zonal.pr.clim) <- c("FID_1", "mean")
    zonal.pr.clim
    
    pr.merge.clim <- pr
    
    pr.merge.clim@data <- merge(pr@data, zonal.pr.clim, by = "FID_1")
    
    pr.raster.clim <- rasterize(pr.merge.clim, enm.clim, field = "mean")
    plot(pr.raster.clim)
    
    setwd('zonal_raster_individual_species')
    
    writeRaster(pr.raster.clim, paste0('zonal_', clist[i], ".asc"), format = "ascii", overwrite=TRUE)
    
    setwd('../')
    
    # landscape
    enm.lands <- raster(paste0(lalist[i], ".tif"))
    
    zonal.pr.lands <- zonal(enm.lands, pr.raster, "mean")
    zonal.pr.lands
    colnames(zonal.pr.lands) <- c("FID_1", "mean")
    zonal.pr.lands
    
    pr.merge.lands <- pr
    
    pr.merge.lands@data <- merge(pr@data, zonal.pr.lands, by = "FID_1")
    
    pr.raster.lands <- rasterize(pr.merge.lands, enm.lands, field = "mean")
    plot(pr.raster.lands)
    setwd('zonal_raster_individual_species')
    writeRaster(pr.raster.lands, paste0('zonal_',lalist[i], ".asc"), format = "ascii", overwrite = T)
    setwd('../')
    
}
#-------------------------------------------------