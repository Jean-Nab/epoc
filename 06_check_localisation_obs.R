# chemin
  setwd("C:/git/epoc/data")

# packages
  library(sf)
  library(raster)
  library(rgdal)
  library(ggplot2)
  library(plyr)

# upload des data
  # epoc
  epoc.envi.obs <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                              , encoding="UTF-8",quote="")
  epoc.envi.liste <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),header=T,sep="\t", dec=","
                                , encoding="UTF-8",quote="")
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  
  # donne gps
  check.loc <- read.table(file = "C:/git/epoc/data/list_out.txt",header=F,sep="\t", dec=".")
  colnames(check.loc) <- c("Ref","lat_obs","lon_obs","lat_observateur","lon_observateur","distance_observation","precision_m")

# donnees de localisation selon la periode triee : 5-17h /  1200 > / mars,avril,mai
  #load("C:/git/epoc/06_check_localisation_init.RData")
  
# retrait des observations doublons: non triee dans epoc.envi.obs
  ref.id <- epoc.envi.obs$Ref %in% epoc.oiso$Ref 

  epoc.envi.obs <- epoc.envi.obs[which(ref.id == TRUE),]

# detection des observations geolocalisee
  epoc.envi.obs$geolocalise <- 0

  geo.id <- epoc.envi.obs$Ref %in% check.loc$Ref
  table(geo.id)

  epoc.envi.obs[which(geo.id == TRUE),"geolocalise"] <- 1
  
  
# Comparaison des coordonnees gps precises vs coordonnees des barycentres ( + calcul de la distance d'erreur) ----
# Idee : acquerir donnes gps precise + donnees des barycentres pour les comparer

  # join des dtfs (+ selection des variables interressantes) ----
    obs.loc <- epoc.envi.obs[,c("Ref","ID_liste","Observateur","Lon_WGS84","Lat_WGS84","Mention_EPOC","geolocalise")]
    obs.loc <- join(obs.loc,check.loc,by="Ref")
    
    # selection des donnees bien geoloc
      obs.loc.gps <- obs.loc[obs.loc$geolocalise == 1,]
      
    # ajout du nom des especes pour selectionne les genres plus appropriees a la formation du barycentre
      obs.loc.gps <- join(obs.loc.gps,epoc.oiso,by="Ref")
      
      sp.latin <- grep(pattern = "Sylvia|Parus|Phylloscopus|Troglodytes|Prunella|Aegithalos|Passer|Regulus|Fringilla|Carduelis"
                       ,x=obs.loc.gps$Nom_latin)
      
      # selection des observations contennant uniquement des observations des genres de sp.latin
        obs.loc.gps <- obs.loc.gps[sp.latin,]
  
  # regroupement des observations par ID de liste
    id.loc.gps1 <- aggregate(lon_observateur ~ ID_liste,data=obs.loc.gps, mean) # longitude
    id.loc.gps2 <- aggregate(lat_observateur ~ ID_liste,data=obs.loc.gps, mean) # latitude
    id.loc.gps3 <- aggregate(precision_m ~ ID_liste,data=obs.loc.gps, mean) # latitude
  
    
    # DTF de localisation de l'observateur GEOLOCALISE (d'apres info de faune-france)
    id.loc.gps <- join(id.loc.gps1,id.loc.gps2,by="ID_liste")  
    id.loc.gps <- join(id.loc.gps,id.loc.gps3, by="ID_liste")  
    
  # Acquisition des coordonnees du barycentre des listes ----
    id.list <- unique(obs.loc.gps$ID_liste) # vecteur des listes
    list.centr <- data.frame()
    
    i <- 1
    while(i <= length(id.list)){
      dtf.tmp <- obs.loc.gps[obs.loc.gps$ID_liste == id.list[i],]
      # calcul des coordonnees du centroid de la liste i
      centr_lon <- sum(dtf.tmp$Lon_WGS84)/nrow(dtf.tmp)
      centr_lat <- sum(dtf.tmp$Lat_WGS84)/nrow(dtf.tmp)
      
      list.centr.tmp <- matrix(c(id.list[i],centr_lon,centr_lat),nrow=1,ncol=3,byrow=T)
      
      # ajout dans le dtf de receuille des centroide
      list.centr <- rbind(list.centr,list.centr.tmp)
      
      cat(i," /",length(id.list),"\n")
      i <- i+1
    }  
    
    colnames(list.centr) <- c("ID_liste","lon_centroid","lat_centroid")

  # conversion des deux dtf (id.loc.gps / list.centr) en objets spatiaux -----
    id.loc.gps.sf <- st_as_sf(id.loc.gps,coords = c("lon_observateur","lat_observateur"),crs=4326)
    list.centr.sf <- st_as_sf(list.centr,coords = c("lon_centroid","lat_centroid"),crs=4326)
  
    # conversion en coordonnees planaires (lambert 93)
      id.loc.gps.sf <- st_transform(id.loc.gps.sf,crs=2154)
      list.centr.sf <- st_transform(list.centr.sf,crs=2154)
      
    # order par id_liste
      id.loc.gps.sf <- id.loc.gps.sf[order(id.loc.gps.sf$ID_liste),]
      list.centr.sf <- list.centr.sf[order(list.centr.sf$ID_liste),]
      
    # calcul de la distance entre les points
      j <- st_distance(id.loc.gps.sf,list.centr.sf,by_element = TRUE)
      
      summary(j)
      
    # detecction des valeurs de distance anormales
      j1 <- as.vector(j) ; summary(j1)
      which(j1 >= 10000) # liste w/ ecart centroid-loc_gps de plus de 10km
      
      id.10km <- id.loc.gps.sf[which(j1 >= 10000),c("ID_liste","precision_m")]
      
    
  
# cartographie ----
  fra.adm <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp")
  fra.adm <- st_transform(fra.adm,crs=2154)  
  
ggplot() +
  geom_sf(data=fra.adm) +
  geom_sf(data=id.loc.gps.sf,aes(color="red")) +
  geom_sf(data=list.centr.sf,alpha=0.15)
  
  
# Zoom sur listes problematique
  # detecction des listes problematiques (listes dont centroide/gps precis ont un ecart de 10km en ayant une precision_m elevee)
    id.list.prbl <- id.10km[which(id.10km$precision_m <= 500),]
  # observation des listes problematiques
    obs.list.prbl <- obs.loc.gps[which(obs.loc.gps$ID_liste %in% id.list.prbl$ID_liste ==TRUE),]
    # meme observateur, mais peu de points <=> genre priss en compte ?
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  





