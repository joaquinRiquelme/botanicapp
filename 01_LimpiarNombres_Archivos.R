#----------------------------#
# Revisar nombres taxonómicos#
#----------------------------#

require(tidyverse)
require(TNRS)

especies <- read.csv("Especies_DiversidadFlora.csv")

especies <- especies %>% 
            dplyr::select(., Especie, ownhint_Description_1, ownhint_Description_2)

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
              dplyr::select(., Especie =Name_submitted, 
                            SPECIES=Accepted_species,
                            TAXONNAME= Accepted_species, 
                            GENUS=Accepted_genus,
                            FAMILY=Accepted_family) # aún no sé como agregamos la información de la familia

# volver un par de nombres a su original (ni preguntes)
chile_sppp <- chile_sppp %>% 
              mutate(TAXONNAME = if_else(TAXONNAME=="Pouteria valparadisaea", "Pouteria splendens", TAXONNAME),
                     SPECIES = if_else(SPECIES=="Pouteria valparadisaea", "Pouteria splendens", SPECIES),
                     Especie = if_else(Especie=="Pouteria valparadisaea", "Pouteria splendens", Especie)) 

especies<- especies %>% 
           mutate(Especie = if_else(Especie=="Pouteria valparadisaea", "Pouteria splendens", Especie)) %>% 
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
                     img.files=str_replace_all(img.files, "_3",""),
                     img.files=str_replace_all(img.files, "_4",""),
                     img.files=str_replace_all(img.files, "_"," ")) %>% 
            rename(., SPECIES=img.files) %>% 
            mutate(imagen=1)

spp_2 <- especies %>% 
         dplyr::select(.,Especie, SPECIES) %>% 
         left_join(., img.files, by="SPECIES")

summary(spp_2$Especie ==spp_2$SPECIES) # all ok
                         