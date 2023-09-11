#----------------------------#
# Revisar nombres taxonómicos#
#----------------------------#

require(tidyverse)
require(TNRS)

especies <- read.csv("Especies_DiversidadFlora.csv")

# 1. Limpiar nombres taxonómicos

spp<-unique(especies$Especie)

chile_spp <- TNRS(spp,
                  sources = c("wcvp", "wfo"),
                  classification = "wfo",
                  mode = "resolve",
                  matches = "best")

chile_sppp <- chile_spp %>% 
              dplyr::select(., Name_submitted, Accepted_species, Accepted_family) %>% 
              mutate(Accepted_genus =word(Accepted_species,1)) %>% 
              dplyr::select(., Especie =Name_submitted, TAXONNAME= Accepted_species, SPECIES=Accepted_species,
                            GENUS=Accepted_genus) # aún no sé como agregamos la información de la familia

especies<- especies %>% 
           mutate(TAXONNAME=NULL,
                  SPECIES=NULL,
                  GENUS=NULL) %>% 
           left_join(., chile_sppp, by="Especie")

 
write.csv(especies, "Especies_DiversidadFlora.csv", row.names=F)

# 2. Verificar que los nombres == nombres en la lista

my.path<-"images"

img.files <- list.files(path=my.path, pattern = ".jpg$", full.names=TRUE)

img.files<-data.frame(img.files) %>% 
            mutate(img.files=str_replace_all(img.files, "images/",""),
                     img.files=str_replace_all(img.files, ".jpg",""),
                     img.files=str_replace_all(img.files, "_1",""),
                     img.files=str_replace_all(img.files, "_2",""),
                     img.files=str_replace_all(img.files, "_"," ")) %>% 
            rename(., SPECIES=img.files) %>% 
            mutate(imagen=1)

spp_2 <- especies %>% 
         dplyr::select(.,Especie, SPECIES) %>% 
         left_join(., img.files, by="SPECIES")

summary(spp_2$Especie ==spp_2$SPECIES) # all ok
                         