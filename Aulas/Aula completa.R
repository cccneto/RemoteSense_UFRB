#######  AULA 2
#   dutra.andeise@gmail.com


# Shapefile ---------------------------------------------------------------
# install.packages(c("sf", "sp", "geobr"))

library(sp)
library(sf)
library(geobr)
library(raster)
library(RStoolbox)
library(ggplot2)
library(ggspatial)
library(rgdal)
library(arulesViz)
library(tidyverse)


dados <- geobr::list_geobr()

municipio <- geobr::read_municipality(
                                      code_muni = 3505104, 
                                      year = 2020
                                      )

shape <- sf::as_Spatial(municipio$geom)

mascara <- sp::spTransform(shape, CRS(
  "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

# Raster ------------------------------------------------------------------

pasta <- file.path(getwd(), "arquivos") # criando caminho e nome pra pasta

dir.create(pasta) # criando pasta

# instalar o pacote  
# remotes::install_github("16EAGLE/getSpatialData") 
library(getSpatialData)


login_USGS(username = "claudianoneto", password = "marilia91020") # login

produtos <- getSpatialData::getLandsat_products() # atribuindo produtos do Landsat
produtos

landsat <- getLandsat_records(
  time_range = c("2020-08-01", "2020-08-15"),
  products = "landsat_8_c1",
  aoi = mascara )

orbita <- landsat[which(landsat$tile_number_vertical == 75), ]   # visualizando imagem 

orbita <- orbita %>%    # criando coluna summary porq pct requer isso
  mutate(summary = "NA")

orbita <- landsat %>%  # filtrando "tile"
  filter(tile_number_vertical == 75)

# criar 
visualizar <- get_previews(records = orbita, # configurar imagem para visualizacao
                           level = "l1", # ta disponivel 0800 ... sr nao ta 
                           dir_out = pasta)
# visualizar
plot_previews(records = visualizar[2,],
              show_aoi = shape)

verificar <- check_availability(orbita) # verificar disponibilidade
pedido <- order_data(orbita[c(5,21),])   # realizar pedido 
download <- getLandsat_data(
                            records = orbita[c(5,21),], 
                            dir_out <- pasta 
                            )

# O exercicio foi fazer o mesmo procedimento acima, 
# mas utilizando uma cidade diferente

###############################################################

# Aula 3

dados <- geobr::list_geobr()

municipio <- geobr::read_municipality(
                                      code_muni = 3505104, 
                                      year = 2020
                                      )

# pegando dado espacializado para recortar imagens
shape <- sf::as_Spatial(municipio) 
plot(shape)

mascara <- sp::spTransform(shape, CRS(
  "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

# Raster e Vetor ------------------------------------------------------------------


#### raster usando apenas uma imagem 

landsat_imagem <- raster::raster(
"C:\\Users\\User\\Documents\\GitHub\\RemoteSense_UFRB\\LC08_L1TP_221075_20200801_20200807_01_T1_LEVEL_sr\\LC08_L1TP_221075_20200801_20200807_01_T1_sr_band4.tif")

plot(landsat_imagem) # plotando a imagem

# criando nossa mascara 
mascara <- spTransform(shape, CRS("+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

# plotando a mascara em cima da banda4
# stack deve ter a mesma extensao
plot(mascara, add = T)



### agora vamos usar multiplas imagens 

lista_imagens <- list.files("./LC08_L1TP_221075_20200801_20200807_01_T1_LEVEL_sr/", 
                            pattern = "band", 
                            full.names = TRUE)

# Fazendo o stack "faz uma colecao de camadas de raster"
bloco <- raster::stack(lista_imagens[c(2, 3, 4, 5)]) 
bloco

# plot de um rggbb
raster::plotRGB(bloco, 3, 2, 1, 
                scale = 32767, 
                stretch = "lin", 
                colNA = "white")

# recortar nosso rasterlayer para nossa area de estudo
recortado <- mask(bloco, mascara) 
final <- crop(recortado, mascara) 

raster::plotRGB(final,  3, 2, 1, 
                scale = 32767, 
                stretch = "lin", 
                colNA = "white", 
                axes = T)

names(final) <- c('b2','b3', 'b4', 'b5')

# rescalonando
rescalonado <- final*0.0001

# fazendo o ndvi - indice de vegetacao
# NDVI: ((NIR - RED) / (NIR + RED))

ndvi <- ((rescalonado$b5 - rescalonado$b4) / 
           (rescalonado$b5 + rescalonado$b4))
plot(ndvi)

# o NDVI pode saturar, 
# pois a relacao nao é linear
de 
# SAVI
# savi <- ((NIR - RED) / (NIR + RED + L))*(1 + L)
savi <- ((rescalonado$b5 - rescalonado$b4) / 
           (rescalonado$b5 + rescalonado$b4 + 0.5)*(1 + 0.5))
plot(savi)

plot(ndvi, zlim = c(-1, 1))
plot(savi, zlim = c(-1, 1))

raster::writeRaster(rescalonado,
                    "./arquivos/Landsat_area_estudo_rgb.tif")
raster::writeRaster(ndvi,
                    "./arquivos/Landsat_area_estudo_ndvi.tif")
raster::writeRaster(savi,
                    "./arquivos/Landsat_area_estudo_savi.tif")

########################################################################

# Aula 4 - 07/06/2021

# como puxar um shapefile direto do pc


# Mozaico -----------------------------------------------------------------

bloco1 <- raster::stack("./Teste_tiles/tile221_2020_OLI3.tif")
bloco2 <- raster::stack("./Teste_tiles/tile222_2020_OLI3.tif")

plot(bloco1)
plot(bloco2)

# plot dos rggbb
raster::plotRGB(
                bloco1, 4, 3, 2, 
                scale = 0.5,  # 0.5 porque ja ta reescalonado - verificar number float
                stretch = "lin", 
                colNA = "white"
                )

raster::plotRGB(
                bloco2, 4, 3, 2, 
                scale = 0.5, 
                stretch = "lin", 
                colNA = "white"
                )

# unindo as imagens do meu mosaico
mosaico <- raster::mosaic(
                          bloco1, 
                          bloco2,
                          fun = max   # podemos escolher "mean", "min", "max"
                          )

raster::plotRGB(mosaico, 4, 3, 2, 
                scale = 0.5, 
                stretch = "lin", # contraste 
                colNA = "white")


# Mascara Nuvem e sombra --------------------------------------------------

mosaico$layer.10      # banda do "Qa" - informa a qualidade da imagem por pixe
qa_image <- mosaico$layer.10 / 0.0001 

qa_image

nuvem <- raster::calc(
                      qa_image,
                      (fun = function(x) bitwAnd(x, 32) == 0
                      ))

sombra <- raster::calc(
                      qa_image,
                      (fun = function(x) bitwAnd(x, 8) == 0
                      ))

mascara_qualidade <- nuvem*sombra # atribuindo mascara_qualidade

plot(mascara_qualidade) # plotando mascara
hist(mascara_qualidade) # plotando histograma 

mascara_qualidade[mascara_qualidade < 1] <- NA  # atribuindo na's na mascara

mosaico_semnuvem <- raster::mask(mosaico, # aplicando a nova mascara no mosaico
                                 mascara_qualidade)   

raster::plotRGB(mosaico_semnuvem, 4, 3, 2, # plotando mosaico sem nuvem
                scale = 0.5,  # 
                stretch = "lin", 
                colNA = "white"
                                )


# Declividade -------------------------------------------------------------
library(elevatr)

raster_elevacao <- elevatr::get_elev_raster(
                                            locations = mosaico_semnuvem,
                                            z = 11   # range de 1 a 14
                                            )
plot(raster_elevacao)

#calculando o slope - "declividade"
declividade <- raster::terrain(raster_elevacao, 
                               opt = "slope",
                               unit = "degrees",
                               neighbors = 4
                               )   

plot(declividade)

raster::writeRaster(declividade,
                    "./arquivos/declividade.tif")

###########################################

################   Aula 5

# Reclassificar -----------------------------------------------------------

library(elevatr)
library(raster)

declividade <- raster::raster("./arquivos/declividade.tif")

classes <- matrix(c(-Inf, 2, 1,
                    2, 5, 2,
                    5, 25, 3,
                    25, 50, 4,
                    50, Inf, 5),
                  ncol = 3,
                  byrow = T)

declividade_classificado <- raster::reclassify(declividade,
                                               classes)

municipio <- geobr::read_municipality(code_muni = 3505104, year = 2020)

shape <- sf::as_Spatial(municipio) 
mascara <- spTransform(shape, CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"))

plot(declividade_classificado)
plot(mascara, add = T)

recortar <- crop(declividade_classificado, mascara)
declividade_area <- mask(recortar, mascara)
plot(declividade_area)


declividade_df <- as.data.frame(declividade_area,
                                xy = T) %>% drop_na()

# renomear nossas classes

declividade_df$declividade <- factor(declividade_df$declividade,
                                     levels = c(1, 2, 3, 4, 5),
                                     labels = c("Plano", "Suave Ondulado",
                                                "Ondulado", "Montanhoso",
                                                "Escarpado"))
head(declividade_df)




n <- ggplot2::ggplot() + 
  geom_raster(aes(x = x, y = y, fill = declividade), data = declividade_df) + 
  #scale_fill_brewer(type = "seq", palette = "Greys")
  scale_fill_brewer(palette = "RdYlGn", name = "Classes", direction = -1) +
  labs(title = "Grafico de Declividade") + 
  theme_bw()

n + annotation_scale(location = "bl", width_hint = .4, plot_unit = "m") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering)
#Para exportar a figura do ultimo plot
ggsave("./arquivos/Declividade.png", dpi = 100, width = 15, height = 10, units = "in")


###########################################
################   Aula 6

library(raster)
library(sp)
library(sf)

municipio <- geobr::read_municipality(code_muni = 3505104, year = 2020)
shape <- sf::as_Spatial(municipio) 
mascara <- sp::spTransform(shape, CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"))

# criando lista de arquivos
arq <- list.files("./arquivos/NDVI_2020/", pattern = ".tif$", full.names = T)
arq

NDVI_bloco <- raster::stack(arq)
plot(NDVI_bloco$LC08_221075_20200614)
plot(mascara, add = T)

# O t1 retorna o valor de um pixel para cada camada que está dentro do ndvi_bloco
t1 <- NDVI_bloco[980, 870]
# quero transformar t1 em valor numerico
t2 <- as.numeric(t1)

# como podemos plotar isso?
serie <- stats::ts(t2, start = c(2020, 1), 
                   end = c(2020, 18),
                   frequency = 18) # temos 18 arquivos 

plot(serie)

# automatizando atraves da funcao for

# isso serve para permitir que todo 
# resultado do loop e for seja guardado aqui dentro

outlist <- list()
media_mes <- list()

for (i in 1:length(NDVI_bloco)) {
  dado <- NDVI_bloco[[i]] 
  
  recortado <- crop(dado, mascara)
  area_estudo <- mask (recortado, mascara)
  plot(area_estudo)
  
  outlist[[i]] <- area_estudo
  
  valores <- getValues(area_estudo)
  media <- mean(valores, na.rm =T)
  
  media_mes[[i]] <- media
  
}

class(outlist)
NDVI_bloco <- 

class(outlist)
NDVI_recortado <- raster::stack(outlist)
class(NDVI_recortado)

media_mes
t3 <- as.numeric(media_mes)
serie_ndvi <- ts(t3, start = c(2020,1), end = c(2020,18), frequency = 18)
plot(serie_ndvi) # plotando a media das 18 imagens para o municipio todo

media_anual_ndvi <- raster::calc(NDVI_recortado,fun = mean, na.rm = T)
plot(media_anual_ndvi)

sd_anual_ndvi <- raster::calc(NDVI_recortado, fun = sd, na.rm = T)
plot(sd_anual_ndvi)

mediana_anual_ndvi <- raster::calc(NDVI_recortado,fun = median, na.rm = T)
plot(mediana_anual_ndvi)

writeRaster(media_anual_ndvi, "./arquivos/NDVI_2020/Media_anual_ndvi.tif")
writeRaster(sd_anual_ndvi, "./arquivos/NDVI_2020/SD_anual_ndvi.tif")
writeRaster(mediana_anual_ndvi, "./arquivos/NDVI_2020/Mediana_anual_ndvi.tif")

###### KRIGAGEM

library(geoR)

parana <- geoR::parana
summary(parana)
class(parana)

x <- c(1, 3, 4, 5, 6)
y <- c(4, 7, 8, 8, 9)
z <- c(5,6,7, 9, 10)
plot(x, y, pch = "+", cex = z/4)

teste <- as.geodata(cbind(x, y, z))
plot(teste)

plot(parana)

parana_vario <- variog(parana)
plot(parana_vario)

parana.ml1 <- likfit(parana, trend = "1st", ini = c(1000, 50),
                     nug = 100)
summary(parana.ml1)
