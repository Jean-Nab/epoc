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
    obs.loc <- epoc.envi.obs[,c("Ref","ID_liste","Observateur","Lon_WGS84","Lat_WGS84", "X_Lambert93_m","Y_Lambert93_m","Mention_EPOC","geolocalise")]
    obs.loc <- join(obs.loc,check.loc,by="Ref")
    
    # selection des donnees bien geoloc
      obs.loc.gps <- obs.loc[obs.loc$geolocalise == 1,]
      
    # ajout du nom des especes pour selectionne les genres plus appropriees a la formation du barycentre
      obs.loc.gps <- join(obs.loc.gps,epoc.oiso,by="Ref")
      
      sp.latin <- grep(pattern = "Sylvia|Parus|Phylloscopus|Troglodytes|Prunella|Aegithalos|Passer|Regulus|Fringilla|Carduelis"
                       ,x=obs.loc.gps$Nom_latin)
      
      # selection des observations contennant uniquement des observations des genres de sp.latin
        obs.loc.gps.genre <- obs.loc.gps[sp.latin,]
        
      # selection : retrait des observations a plus de 5 km de distance
        obs.loc.gps.genre <- obs.loc.gps.genre[which(obs.loc.gps.genre$distance_observation <= 5000),]
        
  
  # regroupement des observations par ID de liste
    id.loc.gps.genre1 <- aggregate(lon_observateur ~ ID_liste,data=obs.loc.gps.genre, mean) # longitude
    id.loc.gps.genre2 <- aggregate(lat_observateur ~ ID_liste,data=obs.loc.gps.genre, mean) # latitude
    id.loc.gps.genre3 <- aggregate(precision_m ~ ID_liste,data=obs.loc.gps.genre, mean) # la precision
  
    
    # DTF de localisation de l'observateur GEOLOCALISE (d'apres info de faune-france)
    id.loc.gps.genre <- join(id.loc.gps.genre1,id.loc.gps.genre2,by="ID_liste")  
    id.loc.gps.genre <- join(id.loc.gps.genre,id.loc.gps.genre3, by="ID_liste")  
    
  # Acquisition des coordonnees du barycentre des listes----
    obs.loc.gps.genre$precision_invert <- 1/(obs.loc.gps.genre$precision_m +0.0001) # ajout de 0.0001 pour les listes ou la precision est de 0.000m
  
  # ajout d'un id par liste
    '
    library(data.table)
    test <- obs.loc.gps.genre
    test.dt <- data.table(test)
    test.dt2 <- test.dt[, group_increment := 1:.N, by = "ID_liste"]
    '
    
  
    id.list <- unique(obs.loc.gps.genre$ID_liste) # vecteur des listes
    list.centr <- data.frame()
    
    i <- 1
    while(i <= length(id.list)){
      dtf.tmp <- obs.loc.gps.genre[obs.loc.gps.genre$ID_liste == id.list[i],]
      # calcul des coordonnees du centroid de la liste i
      
      centr_lon <- weighted.mean(x=dtf.tmp$X_Lambert93_m,w=dtf.tmp$precision_invert)
      centr_lat <- weighted.mean(x=dtf.tmp$Y_Lambert93_m,w=dtf.tmp$precision_invert)
      
      list.centr.tmp <- matrix(c(id.list[i],centr_lon,centr_lat),nrow=1,ncol=3,byrow=T)
      
      # ajout dans le dtf de receuille des centroide
      list.centr <- rbind(list.centr,list.centr.tmp)
      
      cat(i," /",length(id.list),"\n")
      i <- i+1
    }  
    
    colnames(list.centr) <- c("ID_liste","lon_centroid","lat_centroid")
    
    
  # conversion des deux dtf (id.loc.gps / list.centr) en objets spatiaux -----
    id.loc.gps.genre.sf <- st_as_sf(id.loc.gps.genre,coords = c("lon_observateur","lat_observateur"),crs=4326)
    list.centr.sf <- st_as_sf(list.centr,coords = c("lon_centroid","lat_centroid"),crs=2154)
  
    # conversion en coordonnees planaires (lambert 93)
      id.loc.gps.genre.sf <- st_transform(id.loc.gps.genre.sf,crs=2154)
      
    # order par id_liste
      id.loc.gps.genre.sf <- id.loc.gps.genre.sf[order(id.loc.gps.genre.sf$ID_liste),]
      list.centr.sf <- list.centr.sf[order(list.centr.sf$ID_liste),]
      
    # calcul de la distance entre les points
      j <- st_distance(id.loc.gps.genre.sf,list.centr.sf,by_element = TRUE)
      
      summary(j)
      
    # detecction des valeurs de distance anormales
      j1 <- as.vector(j) ; summary(j1)
      which(j1 >= 10000) # liste w/ ecart centroid-loc_gps de plus de 10km
      j2 <- j1[-which(j1 >= 10000)] ; summary(j2)
      
      id.10km <- id.loc.gps.genre.sf[which(j1 >= 10000),c("ID_liste","precision_m")]
      
    
  
# cartographie ----
  fra.adm <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp")
  fra.adm <- st_transform(fra.adm,crs=2154)  
  
ggplot() +
  geom_sf(data=fra.adm) +
  geom_sf(data=id.loc.gps.genre.sf,aes(color="red")) +
  geom_sf(data=list.centr.sf,alpha=0.15)
  
  
# Zoom sur listes problematique
  # detecction des listes problematiques (listes dont centroide/gps precis ont un ecart de 10km en ayant une precision_m elevee)
    id.list.prbl <- id.10km[which(id.10km$precision_m <= 500),]
  # observation des listes problematiques
    obs.list.prbl <- obs.loc.gps[which(obs.loc.gps$ID_liste %in% id.list.prbl$ID_liste ==TRUE),]
    
    # formation des objets spatiaux
    list.centr.prbl.sf <- list.centr.sf[which(list.centr.sf$ID_liste %in% id.list.prbl$ID_liste ==TRUE),]
    obs.list.prbl.sf <- st_transform(st_as_sf(obs.list.prbl,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326),crs=2154)
    
      # meme observateur, mais peu de points <=> genre pris en compte ?  (didier perrocheau)
    
  # processus par listes [PROCEDER A LA MAIN]
    id.prbl <- unique(obs.list.prbl$ID_liste)
   
    
    i <- 1
    while(i <= length(id.prbl)){
      
      obs.list.prbl.sf.tmp <- obs.list.prbl.sf[obs.list.prbl.sf$ID_liste == id.prbl[i],]
      list.centr.prbl.sf.tmp <- list.centr.prbl.sf[list.centr.prbl.sf$ID_liste == id.prbl[i],]
      
      dist.prbl.tmp <- st_distance(x = obs.list.prbl.sf.tmp,
                                   y = list.centr.prbl.sf.tmp)

      cat(i," /",length(id.prbl),"\n")
      i <- i+1
    }
    
  # plot points + centroid (col differentes) + st_distance point (eloignement; wrning crs)
    obs.list.prbl.sf <- st_transform(st_as_sf(obs.list.prbl,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326),crs=2154)
  
    ggplot()+
      geom_sf(data=obs.list.prbl.sf[obs.list.prbl.sf$ID_liste == 398389,],colour="blue") +
      geom_sf(data=list.centr.prbl.sf[list.centr.prbl.sf$ID_liste == 398389,],colour="red") +
      geom_sf(data=id.loc.gps.genre.sf[id.loc.gps.genre.sf$ID_liste == 398389,],colour="purple")
    
    # resultats anormaux pb autre part
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  





