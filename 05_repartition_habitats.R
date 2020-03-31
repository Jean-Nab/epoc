# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(raster)
  library(rgdal)
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  
# upload des data
  # Formation du raster CLC couvrant uniquement la france ----
    # NEED DE CHANGER LE RASTER D'ORIGINIE PRENDRE CELUI AVEC LEGENDES MODIFIES
    land <- raster("C:/git/epoc/data/clc2018_clc2018_v2018_20_raster100m/CLC2018_CLC2018_V2018_20.tif")
    france <- st_read(dsn = "C:/git/epoc/data/france_contour_tampon.shp")
    
    france1 <- st_transform(x = france,crs= crs(land))

    land.fra <- crop(land,france1)
    #land.fra.wgs84 <- projectRaster(land.fra,crs="+proj=longlat +datum=WGS84 +no_defs",method="ngb") # OBSOLETE
    
    # verification w/ couche adm de la france en wgs84
    fra.adm <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp")
    # Ok superposition
    
    # ecriture du raster
      #writeRaster(land.fra.wgs84,filename = "C:/git/epoc/data/CLC_france_2018.tif",format = "GTiff") # OBSOLETE
      writeRaster(land.fra,filename = "C:/git/epoc/data/CLC_france_2018_LAEA.tif",format = "GTiff")
      
    # chargement des donnees dans l'environnement -----
      # clc <- raster(x = "C:/git/epoc/data/CLC_france_2018.tif") # crs: WGS84
      clc <- raster(x = "C:/git/epoc/data/CLC_france_2018_LAEA.tif")
      fra.adm <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp")
      
      
    # points des listes ----
      epoc.envi.liste <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),header=T,sep="\t", dec=","
                                    , encoding="UTF-8",quote="")
      
      epoc.envi.observ <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                                     , encoding="UTF-8",quote="")
      # retrait des listes non pris en compte (cf script 04)
        del.juin_jui <- epoc.envi.liste[which(epoc.envi.liste$Mois >= 6),"ID_liste"]
        del.alt <- epoc.envi.liste[which(epoc.envi.liste$Altitude > 1200),"ID_liste"]
        del.hour <- epoc.envi.liste[which(epoc.envi.liste$Heure_de_debut < 5 | epoc.envi.liste$Heure_de_debut > 17),"ID_liste"]
        
        del.juin_jui1 <- epoc.envi.liste$ID_liste %in% del.juin_jui
        del.juin_jui3 <- epoc.envi.observ$ID_liste %in% del.juin_jui
        epoc.envi.observ <- epoc.envi.observ[which(del.juin_jui3 == FALSE),]
        epoc.envi.liste <- epoc.envi.liste[which(del.juin_jui1 == FALSE),]
        
        del.alt1 <- epoc.envi.liste$ID_liste %in% del.alt
        del.alt3 <- epoc.envi.observ$ID_liste %in% del.alt
        epoc.envi.observ <- epoc.envi.observ[which(del.alt3 == FALSE),]
        epoc.envi.liste <- epoc.envi.liste[which(del.alt1 == FALSE),]
        
        del.hour1 <- epoc.envi.liste$ID_liste %in% del.hour
        del.hour3 <- epoc.envi.observ$ID_liste %in% del.hour
        epoc.envi.observ <- epoc.envi.observ[which(del.hour3 == FALSE),]
        epoc.envi.liste <- epoc.envi.liste[which(del.hour1 == FALSE),]
        
      # formation de l'objet spatial (sf)
        loc.obs <- st_as_sf(epoc.envi.observ[,],coords = c("Lon_WGS84","Lat_WGS84"),crs=4326) 
        
# calcul des barycentre (proxy de la localisation de l'observateur) [selon les coordonnees en wgs84 puis conversion en crs du raster land cover] -----
  # idee : proceder par liste; boucle de lecture par liste ==> regroupement dans dtf.tmp des points d'observation
  #       -> calcul du barycentre de la liste (sous crs WGS84), puis conversion dans le crs(clc)
        
id.list <- unique(epoc.envi.observ$ID_liste)

list.centr <- data.frame()
i <- 1
while(i <= length(id.list)){
  dtf.tmp <- epoc.envi.observ[epoc.envi.observ$ID_liste == id.list[i],]
  # calcul des coordonnees du centroid de la liste i
  centr_lon <- sum(dtf.tmp$Lon_WGS84)/nrow(dtf.tmp)
  centr_lat <- sum(dtf.tmp$Lat_WGS84)/nrow(dtf.tmp)
  
  list.centr.tmp <- matrix(c(id.list[i],centr_lon,centr_lat),nrow=1,ncol=3,byrow=T)
  
  # ajout dans le dtf de receuille des centroide
  list.centr <- rbind(list.centr,list.centr.tmp)
  
  cat(i," /",length(id.list),"\n")
  i <- i+1
}

# join du dtf des listes avec la liste regroupant less coords des centroides
  colnames(list.centr) <- c("ID_liste","centroide_lon","centroide_lat")
  loc.list <- plyr::join(epoc.envi.liste,list.centr,by="ID_liste")
  
# conversion sur le crs de la couche corine land cover
  loc.list <- st_as_sf(loc.list,coords=c("centroide_lon","centroide_lat"), crs=4326) # formation du sf selon les coord wgs84
  loc.list <- st_transform(loc.list,crs=crs(clc)) # conversion sur le crs du raster corinne land cov
  fra.adm <- st_transform(fra.adm,crs=crs(clc)) # polygones ou les points vont etre selectionne aleatoirement

  rand.point <- st_as_sf(st_sample(fra.adm,size=nrow(loc.list)),crs=3035)
  
  # formation du buffer
    loc.list.buf <- st_buffer(loc.list,dist = 200)
    rand.point.buf <- st_buffer(rand.point,dist = 200)
    
# SAUVEGARDE DE L'IMAGE SUR DISQUE de l'amont -----
  # save.image("C:/git/epoc/extraction_land_use1.RData")
  load("C:/git/epoc/extraction_land_use1.RData")    
    
# Extraction des habitats ----
      start_time <- Sys.time()
      clc.buf <- raster::extract(clc,loc.list.buf
                         ,along=TRUE
                         ,method="simple"
                         ,df=TRUE
                         ,small=TRUE)
      
      beginCluster()
      clc.rand.buf <- raster::extract(clc,rand.point.buf
                                      ,along=TRUE
                                      ,method="simple"
                                      ,df=TRUE
                                      ,small=TRUE)
      endCluster()
      
      end_time <- Sys.time()
      clc.buf$EPOC <- TRUE
      clc.rand.buf$EPOC <- FALSE
  
      
# SAUVEGARDE DE L'IMAGE SUR DISQUE de l'amont -----
  #save.image("C:/git/epoc/extraction_land_use2.RData")
  load("C:/git/epoc/extraction_land_use2.RData")    
      
# Determination de la part des habitats -----
  # transformation du tableau en "table des communautes d'habitats"
    habi.epoc <- clc.buf %>% dcast(ID + EPOC ~ CLC_france_2018_LAEA)
    habi.rand <- clc.rand.buf %>% dcast(ID + EPOC ~ CLC_france_2018_LAEA)
    habi.epoc$"335" <- c(rep(0,nrow(habi.epoc)))
    
    h <- melt(habi.epoc,id.vars = c("ID","EPOC"))
    
    habi.epoc.tot <- as.data.frame(apply(habi.epoc[,3:ncol(habi.epoc)],2,sum)) 
    habi.epoc.tot$habitat <- rownames(habi.epoc.tot)
    habi.epoc.tot$Points <- "EPOC"
    colnames(habi.epoc.tot)<- c("surface_100m",colnames(habi.epoc.tot[2:ncol(habi.epoc.tot)]))
    #habi.epoc.tot$surface_1km <- (habi.epoc.tot$surface_100m*100)/1000000 # division par 100 -> pour km^2 ?

    habi.rand.tot <- as.data.frame(apply(habi.rand[,3:ncol(habi.rand)],2,sum))
    habi.rand.tot$habitat <- rownames(habi.rand.tot)
    habi.rand.tot$Points <- "Aléatoire"
    colnames(habi.rand.tot)<- c("surface_100m",colnames(habi.rand.tot[2:ncol(habi.rand.tot)]))
    
    #merging
      habi.all.tot <- rbind(habi.epoc.tot,habi.rand.tot)

    # visualisation
      ggplot(habi.all.tot, aes(fill=Points,y=surface_100m,x=habitat)) + geom_bar(position = "dodge",stat = "identity") +
        xlab("Code habitats") + ylab("Surface d'habitats en ha") +
        ggtitle("Répartition des habitats : \nPoints d'écoutes EPOCs vs points aléatoires")
      
# Interpretation
    # 112 : surr ds epoc [Discontinuous urban fabric] [= ville]
    # 211 : sous ds epoc [Non-irrigated arable land] [= champ de culture intens]
    # 231 : bien repre ds epoc [Pastures] = paturage
    # 311 : legerement sous ds epoc [Broad-leaved forest] ==> foret decidue
    # 512 : legerement sur ds epoc [Water bodies] ==> zone d'eau (possible points dans des fleuves)
      
      
      
    # Visualisation de l'ecart de repartition d'habitats
      # calcul de l'ecart
        habi.all.merged <- merge(habi.epoc.tot,habi.rand.tot,by="habitat")
        habi.all.merged$ecart <- abs(habi.all.merged$surface_100m.x - habi.all.merged$surface_100m.y)
        habi.all.merged$ecart_under_random <- habi.all.merged$ecart / habi.all.merged$surface_100m.y
        
      # histogram
        ggplot(habi.all.merged, aes(y=ecart_under_random,x=habitat,color=habitat,fill=habitat)) + geom_bar(stat = "identity") +
          xlab("Habitats") + ylab("(Ecart des habitats) / (habitats aléatoirement tirés)") +
          ggtitle("Visualisation des écarts de représentation des habitats")
        
      
    
# interpretation
  # grande surrepresentation de 141 [Green urban areas] == espaces verts urbains
    
    
# SAUVEGARDE DE L'IMAGE SUR DISQUE de l'amont -----
#save.image("C:/git/epoc/extraction_land_use3.RData")
load("C:/git/epoc/extraction_land_use3.RData")    
    
    
    
    
    
    
    
    
    
    
    


















