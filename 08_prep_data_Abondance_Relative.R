# chemin
  setwd("C:/git/epoc/data")

# packages
  library(ggplot2)
  library(dplyr)
  library(sf)
  library(raster)
  library(reshape2)

# import data ----
  # csv
    epoc.envi.obs <- read.csv(file = paste0(sub("/data","/DS",getwd()),"/epoc_environnement_observation_DS.csv"))
    epoc.oiso <- read.csv(file = paste0(sub("/data","/DS",getwd()),"/epoc_communaute_DS.csv"))
    epoc.envi.liste <- read.csv(file = paste0(sub("/data","/DS",getwd()),"/epoc_environnement_liste_DS.csv"))
    list.oiso.communs <- read.csv(file = paste0(sub("/data","/DS",getwd()),"/liste_oiseaux_communs_DS.csv"))
  
  # raster
    occ.sol <- raster("C:/git/epoc/data/OCS_2018_CESBIO.tif")
    
  # coord des listes
    bary.list <- read.csv(file = paste0(sub("/data","/DS",getwd()),"/epoc_barycentre_liste.csv"))
    bary.list_sf <- st_transform(st_as_sf(bary.list,
                                          coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                          crs=2154),
                                 crs(occ.sol))
  
# Acqusition des donnees relatives aux habitats ----
  # extraction des valeurs du raster autour des barycentres des listes selon un buffer de 2km ----
    bary.list_sf$ID <- c(rep(1:nrow(bary.list_sf)))
    
    vec.iteration <- seq(from = 1, to = length(bary.list_sf$ID), by = 12)
    
    habitat.type <- as.data.frame(matrix(nrow=0,ncol = 3))
    colnames(habitat.type) <- c("ID","habitats","freq")
    
  # boucle de 12 en 12: ----
    for(i in vec.iteration){
      
      bary.list.tmp_sf <- bary.list_sf[i:min((i+11),length(bary.list_sf$ID)),]
      
      bary.list.tmp.habitats <- extract(x = occ.sol, 
                                        y = bary.list.tmp_sf,
                                        buffer = 1000,
                                    df=T,
                                    along=T)
      
      bary.list.tmp.habitats <- as.data.frame(bary.list.tmp.habitats)
      colnames(bary.list.tmp.habitats)[2] <- "habitats"
      
    # agglomeration des habitats ----
      bary.list.tmp.habitats$habitats_agglo <- ifelse(bary.list.tmp.habitats$habitats == 13,
                                                      "Prairies",ifelse(bary.list.tmp.habitats$habitats == 18,
                                                                        "Pelouse",ifelse(bary.list.tmp.habitats$habitats == 19,
                                                                                         "Landes_ligneuses",ifelse(bary.list.tmp.habitats$habitats %in% c(16,17),
                                                                                                                   "Foret",ifelse(bary.list.tmp.habitats$habitats %in% c(20,21,22,23),
                                                                                                                                  "Surfaces_minerales",ifelse(bary.list.tmp.habitats$habitats %in% c(14,15),
                                                                                                                                                              "Cultures_perennes",ifelse(bary.list.tmp.habitats$habitats %in% c(1,2,3,4),
                                                                                                                                                                                         "Batis", "Culture_annuelle")))))))
      
    # gestion dtf des resultats preliminaires -----
      habitat.type.tmp <- group_by(bary.list.tmp.habitats[,c("ID","habitats_agglo")],ID) %>% # decompte du nombre de cellule selon leur categorie d'habitats sur chaque buffer
        plyr::count() %>%
        group_by(ID)
      
    # rbind (need de donner des noms de colonnes qui collent entre dtfs)
      habitat.type.tmp <- as.data.frame(habitat.type.tmp)
      habitat.type.tmp$ID <- habitat.type.tmp$ID + (i - 1)
      
      habitat.type <- as.data.frame(rbind(habitat.type,habitat.type.tmp))  
      
    # rm (vs surcharge de la ram + limiter trop de noms de var differents)
      rm(habitat.type.tmp)
      rm(bary.list.tmp.habitats)
      rm(bary.list.tmp_sf)
      
      
    # avancement boucle
      cat(i,"/ ",vec.iteration[length(vec.iteration)],"\n")
      
    }
    
  # sauvegarde de habitat.type -----
    write.csv(x = habitat.type,
              file = "C:/git/epoc/DS/Habitat_liste_buffer_1km_agglomeration.csv")
    
  # manip dtf pour avoir la part de l'habitat par liste (ID) -----
    habitat.type.part <- habitat.type %>%
      group_by(ID) %>%
      mutate(total_cell = sum(freq))
  
  # calcul de la part d'habitat (en %) -----
    habitat.type.part$part_habitat <- (habitat.type.part$freq / habitat.type.part$total_cell)*100
       
        
  # join de l'ID liste ----
    habitat.liste <- left_join(st_drop_geometry(bary.list_sf[,c("ID","ID_liste")]),
                               habitat.type.part[,c("ID","habitats_agglo","freq","part_habitat")])
    
    habitat.liste$habitat_surface_m2 <- habitat.liste$freq/100 # freq == compte de cellule de 10mx10m par type d'habitats
  
    
    
# exploration part habitats ----
  # ACP ----
    library(tidyr)
    library(ade4)
    
    # prep data
      explo.habitat <- spread(explo.habitat,habitats_agglo,part_habitat)
      
      # gestion des NA --> 0
        i <- 2
        while(i <= ncol(explo.habitat)){
          explo.habitat[which(is.na(explo.habitat[,i]) == TRUE),i] <- 0
          
          i <- i+1
        }
        
    # ACP
      acp.habitat <- dudi.pca(explo.habitat[,-1],center = T,scale=T)
  
      s.label(acp.habitat$li) # distrib nuage de points
      s.corcircle(acp.habitat$co) # cercle des covariables
    
  # Corrplot ----
    library(corrplot)
      
    corrplot(corr = cor(explo.habitat[,-1],method = "spearman"), method="number")

# Formation de la table presence/absence par liste ----
  tabl.communaute <- epoc.oiso[,c("ID_liste","Nom_espece")]
  tabl.communaute <- left_join(tabl.communaute,unique(epoc.envi.obs[,c("ID_liste","Nom_espece","Nom_latin")])) # ajout de la colonne Nom_latin
  tabl.communaute$Presence <- 1
    
  # formation d'une table presence/absence (-> tableau format wide)
    tabl.communaute <- dcast(tabl.communaute,
                             ID_liste ~ Nom_latin)
    
    tabl.communaute$ID <- c(rep(1:nrow(tabl.communaute))) # ajout d'un identifiant de ligne ID pour join
    
  # creation d'une table annexe (conversion des donnees en 0/1 puis join selon l'identifiant de ligne ID)
    tabl.communaute.bis <- tabl.communaute
    tabl.communaute.bis[tabl.communaute.bis>0] <- 1
    tabl.communaute.bis$ID <- c(rep(1:nrow(tabl.communaute.bis)))
    
    tabl.communaute <- left_join(tabl.communaute[,c("ID_liste","ID")],tabl.communaute.bis[,-1])
    rm(tabl.communaute.bis)

  # sauvegarde ----
    write.csv(tabl.communaute, file = "C:/git/epoc/DS/epoc_communaute_PA_DS.csv")
















