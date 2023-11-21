#----------------------------------------------------------------------------------------------
# Script para avaliar se novos vetores observados seguem valores de adequabilidade
# de um modelo pr?vio (Ferro e Silva et al) e se adequabilidade em pontos observados 
# ? maior do que o esperado pelo acaso para o estado do Paran? (N=10000)
# Renata Muylaert
#----------------------------------------------------------------------------------------------

setwd('D://OneDrive - Massey University//Coworks//Chagas_2020//')

require(xlsx)
require(ggplot2)
require(tidyverse)
require(raster)
library(sf) 

# Open file------------------------------------------------------------------------------------

t <- read.xlsx('Espécies com adequabilidade.xlsx', sheetIndex = 1)

head(t)

colnames(t) <- c('Sp','Lat', 'Long', 'Ano'     ,'Clima',    'Paisagem')

head(t)


o <- ggplot(data=t, aes(y = Clima, x=Paisagem, colour= Sp ) ) +
  geom_point(data=t, alpha=0.85) + 
  scale_colour_brewer(palette="RdBu") +
  theme_bw()+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 0.99, size=13,  face = "bold"))+
  xlab('Adequabilidade por paisagem') + ylab('Adequabilidade por clima') +labs(colour = "Esp?cies")

o

#------------------------------------------------------------------------------------

c <- ggplot(t, aes(x=Sp, y=Clima, fill=Sp)) + 
  coord_flip()+
  geom_violin(trim=FALSE) + stat_summary(fun.data=mean_sdl,
               geom="pointrange", color="black")+
  scale_fill_brewer(palette="RdBu")+ theme_classic()+
  labs(title= 'Observed points')+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        legend.text = element_text(face = "italic"))

c

ggsave("clima.png")

dev.off()


p <- ggplot(t, aes(x=Sp, y=Paisagem, fill=Sp)) + 
  coord_flip()+
  geom_violin(trim=FALSE) + stat_summary(fun.data=mean_sdl,
                                         geom="pointrange", color="black")+
  scale_fill_brewer(palette="RdBu")+ 
   theme_classic()+
  labs(title= 'Observed points')+
  theme(plot.title.position = "plot", axis.title=element_text(size=11),
        axis.text.y = element_text(size=13, face = "italic"),
        legend.text = element_text(face = "italic"))

p
ggsave("paisagem.png")

# Random points -------------------------------------------------------------------------------

nrow(t) # gerar pontos aleatorios em no raster e extrair valores e comparar com t

rc <- raster('climate_all_0k.asc') / 40
crs(rc) <- CRS('+init=EPSG:4326')
climate_predicion <- 'D://OneDrive - Massey University//Coworks//CHAGAS 2017//PR_prediction'
lin <- rasterToContour(is.na(rc))


fastRandomPoints <- function(r, n) {
  if(raster::nlayers(r) > 1) r <- r[[1]]
  v <- raster::getValues(r)
  v.notNA <- which(!is.na(v))
  x <- sample(v.notNA, n)
  pts <- raster::xyFromCell(r, x)
  return(pts)
}

randompoints <- data.frame(fastRandomPoints(rc, 10000))
randompoints

random_sp <- randompoints

coordinates(random_sp) <- ~ x + y

crs(random_sp) <-CRS('+init=EPSG:4326')
plot(random_sp)
class(random_sp)

randompoints$random_climate <- raster::extract(rc, random_sp)
random_sp$random_climate <- raster::extract(rc, random_sp)

rl <- raster('climate_all_0k.asc') / 40 
crs(rl) <- CRS('+init=EPSG:4326')

randompoints$random_landscape <- raster::extract(rl, random_sp)
random_sp$random_landscape <- raster::extract(rl, random_sp)

hist(random_sp$random_landscape)

head(randompoints)

# comparing with real observations ----------------------------------------------

compare <- t
head(compare)  

randompoints$Sp <-'Observa??es aleat?rias'
randompoints$Ano <-'Observa??es aleat?rias'
randompoints$Clima <- randompoints$random_climate
randompoints$Paisagem <- randompoints$random_landscape

randompoints$random_climate <- NULL
randompoints$random_landscape <- NULL
 

randompoints <- randompoints %>% relocate(Sp, .before = x)
randompoints <- randompoints %>% relocate(y, .before = x)
randompoints <- randompoints %>% rename(Lat = y)
randompoints <- randompoints %>% rename(Long = x)

head(randompoints)
head(compare)  

compare <- rbind(compare, randompoints)

# New Graph
custom_colors <- c("gray",   'royalblue', "firebrick3","lightgreen", "lightblue","gold")


compare

compare$spfac <- as.factor(compare$Sp)
levels(compare$spfac) <- c("Random points" ,
                           "Panstrongylus geniculatus" ,"Panstrongylus megistus" ,  
                           "Rhodnius neglectus"   ,     "Cis prolixus group"  ,       "Triatoma sordida" )


cr <- ggplot(compare, aes(x = spfac, y = Clima, fill = spfac)) + 
  coord_flip() +
  geom_violin(trim = FALSE) + 
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
  scale_fill_manual(values = custom_colors,   # Use custom colors
                    breaks = levels(compare$Sp)) +  # Use custom labels
  theme_classic() +
  labs(title = '', x='Species', y= 'Climate suitability') +
  theme(plot.title.position = "plot", axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 13, face = "italic"),
        legend.text = element_text(face = "italic"))

cr

#cr + ylim(0, 1.2) 

ggsave("clima_random.png")

dev.off()

pr <- ggplot(compare, aes(x = spfac, y = Paisagem, fill = spfac)) + 
  coord_flip() +
  geom_violin(trim = FALSE) + 
  stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
  scale_fill_manual(values = custom_colors,   # Use custom colors
                    breaks = levels(compare$Sp)) +  # Use custom labels
  theme_classic() +
  labs(title = '', x='Species', y= 'Landscape suitability') +
  theme(plot.title.position = "plot", axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 13, face = "italic"),
        legend.text = element_text(face = "italic"))

pr

pr + ylim(0, 1.2) 

ggsave("paisagem_random.png")


# Export data ---------------------------------------------------------------------------------

write.xlsx(compare, 'comparacao_10000_pontos.xlsx')

#---------------------------- Fim :)

