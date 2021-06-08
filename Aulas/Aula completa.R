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
library(getSpatialData)
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

# remotes::install_github("16EAGLE/getSpatialData") # instalando pacote 



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
                scale = 32767, 
                stretch = "lin", 
                colNA = "white"
                )

raster::plotRGB(
                bloco2, 4, 3, 2, 
                scale = 32767, 
                stretch = "lin", 
                colNA = "white"
                )

# unido as imagens do meu mosaico
mosaico <- raster::mosaic(
                          bloco1, 
                          bloco2,
                          fun = max   # podemos escolher "mean", "min", "max"
                          )

raster::plotRGB(mosaico, 4, 3, 2, 
                scale = 32767, 
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
                scale = 0.5, 
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
