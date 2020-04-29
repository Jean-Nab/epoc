# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(raster)
  library(exactextractr)

# load des data
  load("C:/git/epoc/04bis_save3.RData") 

  loc.obs <- epoc.envi.obs[,c("Ref","Nom_espece","Nom_latin","ID_liste","X_Lambert93_m","Y_Lambert93_m")]
  loc.obs_sf <- st_as_sf(loc.obs,
                         coords = c("X_Lambert93_m","Y_Lambert93_m"),
                         crs=2154) # sf des observations
  
  dist.bary_obs <- plyr::join(loc.obs[,c("Ref","Nom_espece","Nom_latin","ID_liste")],
                              bary.reg[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")])
  dist.bary_obs_sf <- st_as_sf(dist.bary_obs,coords = c("X_barycentre_L93","Y_barycentre_L93"),
                               crs=2154)
  
  
  dist.observation <- as.numeric(st_distance(x = loc.obs_sf,
                                             y = dist.bary_obs_sf,
                                             by_element = TRUE))
  
  loc.obs$Distance_observation_m <- dist.observation
  
  # add de l'information à epoc.envi.obs
    epoc.envi.obs <- left_join(epoc.envi.obs,
                               loc.obs[,c("Ref","Distance_observation_m")])
  # selection de qqs genres typiquement retrouve proche de l'observateur ----
        sp.latin <- grep(pattern = "Sylvia|Parus|Phylloscopus|Troglodytes|Prunella|Aegithalos|Passer|Regulus|Fringilla|Carduelis"
                         ,x=epoc.envi.obs$Nom_latin)
        test <- epoc.envi.obs[sp.latin,]
    

  # exploration des data ----
    dist.list <- aggregate(Distance_observation_m ~ ID_liste,
                           data=epoc.envi.obs,
                           FUN=mean)
    cat("Nb liste w/ distance nulle :",length(which(dist.list$Distance_observation_m == 0)),"\n",
        "Nb liste w/ distance > 1km :",length(which(dist.list$Distance_observation_m >1000 )))
    quantile(dist.list$Distance_observation_m,c(c(1:3)*0.1,
                                                #c(31:39)*0.01,
                                                c(4:9)*0.1,
                                                c(91:100)*0.01))

    
    dist.espece <- aggregate(Distance_observation_m ~ Nom_espece,
                             data=epoc.envi.obs,
                             FUN=mean)
    
    
  # selection d'un ensemble de donnees pour le distance sampling (rm des distances nulles et > 1km) -----
    id.list.dist <- which(dist.list$Distance_observation_m < 0.001 | dist.list$Distance_observation_m >1000)
    id.list.dist <- dist.list[-id.list.dist,"ID_liste"]

    epoc.envi.obs$Use_distance_sampling <- epoc.envi.obs$ID_liste %in% id.list.dist

  # subsetting du jeu de donnee pour l'exploration vav du DS
    epoc.envi.obs.DS <- epoc.envi.obs[which(epoc.envi.obs$Use_distance_sampling == T),]
    
    
  # mesure de la variance selon le type d'habitats lors de l'EPOC -----
    # load de la couche raster
      occ.sol <- raster("C:/git/epoc/data/OCS_2018_CESBIO.tif")
    # determination de l'habitat de la liste (extract du raster)
      tabl.dist.list <- bary.reg[bary.reg$ID_liste %in% id.list.dist,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")] # selection des listes compatible au DS
      tabl.dist.list_sf <- st_transform(st_as_sf(tabl.dist.list, 
                                                 coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                                 crs=2154),
                                        crs=crs(occ.sol)) # conversion en sf selon le crs de la couche occupation des sols
      
    # extract selon la categorie majoritaire des cellules dans un buffer de 50m autour du barycentre des listes
      tabl.dist.list_sf$Categories_habitats <- exact_extract(x = occ.sol,
                                                             y = st_buffer(x = tabl.dist.list_sf, 
                                                                           dist= 50),
                                                             fun = "majority")
    
      tabl.dist.list <- left_join(tabl.dist.list,st_drop_geometry(tabl.dist.list_sf)[,c("ID_liste","Categories_habitats")])
      
      # regroupement des categories d'habitats
        tabl.dist.list[which(tabl.dist.list$Categories_habitats == 1 |tabl.dist.list$Categories_habitats == 2 |
                               tabl.dist.list$Categories_habitats == 3 |tabl.dist.list$Categories_habitats == 4),"Categories_habitats"] <- 1
      
        tabl.dist.list[which(tabl.dist.list$Categories_habitats == 5 |
                             tabl.dist.list$Categories_habitats == 6 |tabl.dist.list$Categories_habitats == 7 |
                             tabl.dist.list$Categories_habitats == 8 |tabl.dist.list$Categories_habitats == 9 |
                             tabl.dist.list$Categories_habitats == 10 |tabl.dist.list$Categories_habitats == 11 |
                             tabl.dist.list$Categories_habitats == 12 |tabl.dist.list$Categories_habitats == 13|
                             tabl.dist.list$Categories_habitats == 14 |tabl.dist.list$Categories_habitats == 15 |
                             tabl.dist.list$Categories_habitats == 18 |tabl.dist.list$Categories_habitats == 19 |
                             tabl.dist.list$Categories_habitats == 20|tabl.dist.list$Categories_habitats == 21 |
                             tabl.dist.list$Categories_habitats == 22 |tabl.dist.list$Categories_habitats == 23),"Categories_habitats"] <- 2
        
        tabl.dist.list[which(tabl.dist.list$Categories_habitats == 16 |
                               tabl.dist.list$Categories_habitats == 17),"Categories_habitats"] <- 3
        
    # join w/ dtf regroupant l'ensemble des informations selon les observations ----
      epoc.envi.obs.DS <- left_join(epoc.envi.obs.DS,tabl.dist.list[,c("ID_liste","Categories_habitats")])
      
    # variation distance selon la catégories d'habitats
      epoc.envi.obs.DS$Presence <- c(rep(1,nrow(epoc.envi.obs.DS))) # nvlle colonne pour calculer le nb d'occurence de 'lobservation d'une espece selon l'habitat
      
    # calcul de la mediane de la distance observation-observateurs ----
      tabl.dist.esp <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                 data=epoc.envi.obs.DS,
                                 quantile, 
                                 c(0.5))
      tabl.dist.esp <- tidyr::spread(tabl.dist.esp, Categories_habitats, Distance_observation_m)
      
      # harmonisation
        colnames(tabl.dist.esp)[grep("1",colnames(tabl.dist.esp))] <- "Distance_habitat_1_mediane"
        colnames(tabl.dist.esp)[grep("2",colnames(tabl.dist.esp))] <- "Distance_habitat_2_mediane"
        colnames(tabl.dist.esp)[grep("3",colnames(tabl.dist.esp))] <- "Distance_habitat_3_mediane"
        
    # calcul des 95% de la distance observation-observateurs ----
      tabl.dist.esp.95 <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                   data=epoc.envi.obs.DS,
                                   quantile, 
                                   c(0.95))
      tabl.dist.esp.95 <- tidyr::spread(tabl.dist.esp.95, Categories_habitats, Distance_observation_m)
      colnames(tabl.dist.esp.95)[3:ncol(tabl.dist.esp.95)] <- paste(colnames(tabl.dist.esp.95)[3:ncol(tabl.dist.esp.95)],"X",sep="")
      
      # ajout de la distance de 95% des observations au dtf contenant l'information sur la mediane
        tabl.dist.esp <- left_join(tabl.dist.esp,tabl.dist.esp.95)
        
      # harmonisation
        colnames(tabl.dist.esp)[grep("1X",colnames(tabl.dist.esp))] <- "Distance_habitat_1_95"
        colnames(tabl.dist.esp)[grep("2X",colnames(tabl.dist.esp))] <- "Distance_habitat_2_95"
        colnames(tabl.dist.esp)[grep("3X",colnames(tabl.dist.esp))] <- "Distance_habitat_3_95"
      
      
      
    # calcul du nombre d'occurence d'observation par categories d'habitat----
      tabl.dist.esp.nb.occ <- aggregate(Presence ~ Nom_espece + Nom_latin + Categories_habitats,
                                        data=epoc.envi.obs.DS,
                                        sum)
      tabl.dist.esp.nb.occ <- tidyr::spread(tabl.dist.esp.nb.occ, Categories_habitats, Presence)
      colnames(tabl.dist.esp.nb.occ)[3:ncol(tabl.dist.esp.nb.occ)] <- paste(colnames(tabl.dist.esp.nb.occ)[3:ncol(tabl.dist.esp.nb.occ)],"X",sep="")
        
      # ajout du nombre d'occurence d'observation au dtf globalisant l'information par habitat
        tabl.dist.esp <- left_join(tabl.dist.esp,tabl.dist.esp.nb.occ)
      
      # harmonisation
        colnames(tabl.dist.esp)[grep("1X",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_1"
        colnames(tabl.dist.esp)[grep("2X",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_2"
        colnames(tabl.dist.esp)[grep("3X",colnames(tabl.dist.esp))] <- "Nb_occ_habitat_3"
    
    
    # récuperation des abondances selon differentes classes de distances (25 ; 100 ; 200 ; 200-1000)
    # récupération par liste uniquement (vs double comptage)
    # IDEE : st_within de buffer autour des barycentres
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  # OBSOLETE -----
    # upload data / formation de la table de resultats
      clc <- raster("C:/git/epoc/data/CLC_france_2018_LAEA_ver3_3_categories.tif")
      
      tabl.dist.list <- bary.reg[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")]
      tabl.dist.list_sf <- st_transform(st_as_sf(tabl.dist.list, 
                                                 coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                                 crs=2154),
                                        crs=3035)
      
      tabl.dist.list_sf$Categories_habitats <- extract(x = clc,
                                                      y= tabl.dist.list_sf,
                                                      along=TRUE)
      tabl.dist.list <- left_join(tabl.dist.list,st_drop_geometry(tabl.dist.list_sf)[,c("ID_liste","Categories_habitats")])
      epoc.envi.obs <- left_join(epoc.envi.obs,tabl.dist.list[,c("ID_liste","Categories_habitats")])


    # regroupement selon les especes
      tabl.dist.esp <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                  data=epoc.envi.obs,
                                  FUN=mean)
      tabl.dist.esp <- tidyr::spread(tabl.dist.esp, Categories_habitats, Distance_observation_m)
      
      
      
      tabl.dist.esp <- aggregate(Distance_observation_m ~ Nom_espece + Nom_latin + Categories_habitats,
                                 data=epoc.envi.obs,
                                 quantile, 
                                 c(0.5,0.95,0.99))
      tabl.dist.esp <- data.frame(tabl.dist.esp[,1:ncol(tabl.dist.esp)-1],tabl.dist.esp[,ncol(tabl.dist.esp)])
      
      





