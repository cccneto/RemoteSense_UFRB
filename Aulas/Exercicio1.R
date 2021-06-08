
# Exercicio de Casa -------------------------------------------------------
library(sp)
library(sf)
library(geobr)


dados <- geobr::list_geobr()

municipio <- geobr::read_municipality(code_muni = 2909802, year = 2020)

shape <- sf::as_Spatial(municipio$geom)

mascara <- sp::spTransform(shape, CRS(
  "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"))

# Raster ------------------------------------------------------------------

pasta_ex <- file.path(getwd(), "arquivos") # criando caminho e nome pra pasta
dir.create(pasta_ex) # criando pasta

# remotes::install_github("16EAGLE/getSpatialData") # instalando pacote 

library(getSpatialData)
library(tidyverse)

login_USGS(username = "claudianoneto", password = "marilia91020") # login

produtos <- getSpatialData::getLandsat_products() # atribuindo produtos do Landsat
produtos

landsat <- getLandsat_records(
  time_range = c("2020-08-01", "2020-08-15"),
  products = "landsat_8_c1",
  aoi = mascara )

orbita <- landsat[which(landsat$tile_number_vertical == 69), ]   # visualizando imagem 

orbita <- orbita %>%    # criando coluna summary porq pct requer isso
  mutate(summary = "NA")

orbita <- landsat %>%  # filtrando "tile"
  filter(tile_number_vertical == 69)

# criar 
visualizar <- get_previews(records = orbita, # configurar imagem para visualizacao
                           level = "l1", # ta disponivel 0800 ... sr nao ta 
                           dir_out = pasta_ex)
# visualizar
plot_previews(records = visualizar[2,],
              show_aoi = shape)

verificar <- check_availability(orbita) # verificar disponibilidade
pedido <- order_data(orbita[c(5,21),])   # realizar pedido 
download <- getLandsat_data(records = orbita[c(5,21),], dir_out <-pasta_ex)



# FAZER O DOWNLOAD DA AREA DE ESTUDO DA MINHA CIDADE