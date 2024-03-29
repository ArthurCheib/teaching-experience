### Exerc�cio 00 ###

### ------- Carregue os seguintes arquivos - bases de dados a serem utilizadas no treinamento:

# 1) Base de Escolas georreferenciadas pela Mirow - arquivo 001
tabela_escolas_mirow <- fread(input = "1-Dados/001_geo_escolas_mirow-2019.csv", sep = "|")

# 2) Base de Aluno georreferenciados pela Mirow (alunos de somente 300 escolas) - arquivo 002
tabela_alunos_mirow <- fread(input = "1-Dados/002_geo_alunos_mirow-2019.csv", sep = "|")

# 3) Shape das Regionais da SEE/MG - pasta 003 (arquivo .shp)
shape_regionais <- st_read("1-Dados/003_shape_regionais/Contorno SREs.shp")

### Tratamento dos nomes dos shapes
shape_regionais$Nome.SRE <- toupper(shape_regionais$Nome.SRE)

shape_regionais[[5]][[2]] <- "ITAJUB�"
shape_regionais[[5]][[7]] <- "S�O JO�O DEL REI"
shape_regionais[[5]][[9]] <- "PO�OS DE CALDAS"
shape_regionais[[5]][[10]] <- "MURIA�"   
shape_regionais[[5]][[11]] <- "UB�"   
shape_regionais[[5]][[12]] <- "CONSELHEIRO LAFAIETE" 
shape_regionais[[5]][[15]] <- "S�O SEBASTI�O DO PARA�SO" 
shape_regionais[[5]][[19]] <- "DIVIN�POLIS" 
shape_regionais[[5]][[20]] <- "PATROC�NIO" 
shape_regionais[[5]][[22]] <- "MANHUA�U" 
shape_regionais[[5]][[24]] <- "CORONEL FABRICIANO" 
shape_regionais[[5]][[26]] <- "METROPOLITANA A" 
shape_regionais[[5]][[27]] <- "METROPOLITANA B" 
shape_regionais[[5]][[28]] <- "METROPOLITANA C" 
shape_regionais[[5]][[30]] <- "PAR� DE MINAS" 
shape_regionais[[5]][[32]] <- "UBERL�NDIA" 
shape_regionais[[5]][[34]] <- "GOVERNADOR VALADARES" 
shape_regionais[[5]][[35]] <- "GUANH�ES" 
shape_regionais[[5]][[36]] <- "TE�FILO OTONI" 
shape_regionais[[5]][[43]] <- "ARA�UA�" 
shape_regionais[[5]][[45]] <- "JANA�BA" 
shape_regionais[[5]][[46]] <- "JANU�RIA" 
shape_regionais[[5]][[47]] <- "UNA�"

# 4) Base de Escolas georreferenciadas pelo Censo - arquivo 005

tabela_escolas_censo <- fread(input = "1-Dados/005_geo_escolas_censo-2018.csv", sep = "|") %>% 
  mutate(NU_LONGITUDE = as.numeric(str_replace(NU_LONGITUDE, pattern = ",", replacement = ".")),
         NU_LATITUDE = as.numeric(str_replace(NU_LATITUDE, pattern = ",", replacement = ".")))

### ------- Crie uma data frame que contenha apenas as 100 primeiras escolas da tabela da Mirow

tabela_100_escolas_mirow <- tabela_escolas_mirow %>% 
  head(100) %>%
  as_tibble()

### ------- Crie uma data frame que contenha apenas as 100 �LTIMAS escolas da tabela do Censo

tabela_100_escolas_censo <- tabela_escolas_censo %>% 
  filter(!is.na(NU_LONGITUDE)) %>% 
  tail(100) %>% 
  as_tibble()

### -------------------------------------------------------------------------------------

### Exerc�cio 01 ###

# Construa um mapa com um "Marker" no centro de BH (lng = -43.9386 / lat = -19.9188) com alguma cartografia distinta:
leaflet() %>% 
  addProviderTiles('Stamen.Terrain') %>% 
  addMarkers(lng = -43.9386,
            lat = -19.9188,
            popup = "Beag�")


### -------------------------------------------------------------------------------------
### Exerc�cio 02 ###

# Construa um mapa com limita��o do zoom e e com uma visualiza��o inicial pr�-definida




### -------------------------------------------------------------------------------------
### Exerc�cio 03 ###

# Construa um mapa que utilize "popups" para mostrar o nome do ponto e outro que utilize "labels"




### -------------------------------------------------------------------------------------
### Exerc�cio 04 ###

# Construa um mapa com dez escolas marcadas por "CircleMarkers"

tabela_100_escolas_mirow[1:10, ] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~ESCOLA_GEO_LONGITUDE, 
                   lat = ~ESCOLA_GEO_LATITUDE, 
                   color = "purple", radius = 10)

### -------------------------------------------------------------------------------------
### Exerc�cio 05 ###

# Construa um mapa com 300 escolas e que possua um buscador como "widget"

tabela_100_escolas_mirow[1:300, ] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~ESCOLA_GEO_LONGITUDE, 
                   lat = ~ESCOLA_GEO_LATITUDE, 
                   color = "purple", radius = 10) %>% 
  addSearchOSM()


### -------------------------------------------------------------------------------------
### Exerc�cio 06 ###

# Construa um mapa com 500 escolas e que estejam dividas em 2 grupos de 250 - grupo vermelho e grupo azul

mapa_larissa <- tabela_100_escolas_mirow %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = tabela_escolas_mirow[1:250]$ESCOLA_GEO_LONGITUDE,
                   lat = tabela_escolas_mirow[1:250]$ESCOLA_GEO_LATITUDE, 
                   group = "Vermelho",
                   color = "red", 
                   clusterOptions = markerClusterOptions()) %>% 
  addCircleMarkers(lng = tabela_escolas_mirow[251:500]$ESCOLA_GEO_LONGITUDE, 
                   lat = tabela_escolas_mirow[251:500]$ESCOLA_GEO_LATITUDE, 
                   group = "Azul",
                   color = "blue", 
                   clusterOptions = markerClusterOptions()) %>% 
  addLayersControl(overlayGroups = c("Vermelho", "Azul"))

head()
tail()






### -------------------------------------------------------------------------------------
### Exerc�cio 07 ###

# Construa um mapa com todas as escolas da base da Mirow e que apresente os dados aglomerados numericamente



### -------------------------------------------------------------------------------------
### Exerc�cio 08 ###

# Construa um mapa que contenha o contorno de todas as regionais
tabela_escolas_mirow %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~ESCOLA_GEO_LONGITUDE, 
                   lat = ~ESCOLA_GEO_LATITUDE)  %>% 
  addPolygons(data = shape_regionais, 
              weight = 2, 
              opacity = 0.2, 
              color = "red", 
              dashArray = '3', 
              fillOpacity = 0.2, 
              highlight = highlightOptions( 
                weight = 5, 
                color = "blue", 
                dashArray = "", 
                fillOpacity = 0.7, 
                bringToFront = TRUE))




### -------------------------------------------------------------------------------------
### Exerc�cio 9 ###

# Construa um mapa que plote uma escola como um "Marker" e todos os alunos que nela estudam como "CircleMarkers"
# e ligue os pontos por uma reta





### -------------------------------------------------------------------------------------
### Exerc�cio 10 ###

# Construa E SALVE um mapa que possua todas as escolas da base_Mirow num grupo e todas as escolas do Censo em outro 



### -------------------------------------------------------------------------------------

saveWidget(mapa_larissa, "mapa_larissa.html")
