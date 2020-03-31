# chemin
  setwd("C:/git/epoc/data")
# packages
  library(ggplot2)
  library(sf)


# upload des data ------
  load("C:/git/epoc/data/STOC/Aire_Pouillot_fitis.Rda")
  load("C:/git/epoc/data/STOC/Area_Gorgebleue.Rda")
  load("C:/git/epoc/data/STOC/Area_Mesange_huppe.Rda")
  load("C:/git/epoc/data/STOC/Area_Pic_epeiche.Rda")
  load("C:/git/epoc/data/STOC/Area_Pipit_farlouse.Rda")
  
  epoc.obs <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  
  fra.adm <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp")
  fra.adm <- st_transform(fra.adm,crs=2154)  

  air_Pf <- Aire_Pouillot_fitis
  air_Gb <- area_Gorgebleue
  air_Mh <- area_Mesange_huppe
  air_Pe <- area_pic_epeiche
  air_Pipf <- Area_species
  
  
# retrait des observations doublons: non triee dans epoc.envi.obs
  ref.id <- epoc.obs$Ref %in% epoc.oiso$Ref 
  
  epoc.obs <- epoc.obs[which(ref.id == TRUE),]

# visualisation de l'air ----
  ggplot()+
    geom_sf(data=Aire_Pouillot_fitis, aes(fill = factor(SEASONAL)), alpha = 0.4)
  
# selection des observations des especes ----
  id.pf <- which(epoc.oiso[,"Nom_espece"] == "Pouillot_fitis")
  id.gb <- which(epoc.oiso[,"Nom_espece"] == "Gorgebleue_à_miroir")
  id.mh <- which(epoc.oiso[,"Nom_espece"] == "Mesange_huppee")
  id.pe <- which(epoc.oiso[,"Nom_espece"] == "Pic_epeiche")
  id.pipf <- which(epoc.oiso[,"Nom_espece"] == "Pipit_farlouse")
  
  ref.pf <- epoc.oiso[id.pf,"Ref"]
  ref.gb <- epoc.oiso[id.gb,"Ref"]
  ref.mh <- epoc.oiso[id.mh,"Ref"]
  ref.pe <- epoc.oiso[id.pe,"Ref"]
  ref.pipf <- epoc.oiso[id.pipf,"Ref"]
  
  id.ref.pf <- epoc.obs$Ref %in% ref.pf
  id.ref.gb <- epoc.obs$Ref %in% ref.gb
  id.ref.mh <- epoc.obs$Ref %in% ref.mh
  id.ref.pe <- epoc.obs$Ref %in% ref.pe
  id.ref.pipf <- epoc.obs$Ref %in% ref.pipf
  
  epoc.pf <- epoc.obs[which(id.ref.pf == TRUE),]
  epoc.gb <- epoc.obs[which(id.ref.gb == TRUE),]
  epoc.mh <- epoc.obs[which(id.ref.mh == TRUE),]
  epoc.pe <- epoc.obs[which(id.ref.pe == TRUE),]
  epoc.pipf <- epoc.obs[which(id.ref.pipf == TRUE),]

# formation d'un sf a partir des observation des especes ----
  epoc.pf_sf <- st_as_sf(epoc.pf,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  epoc.gb_sf <- st_as_sf(epoc.gb,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  epoc.mh_sf <- st_as_sf(epoc.mh,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  epoc.pe_sf <- st_as_sf(epoc.pe,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  epoc.pipf_sf <- st_as_sf(epoc.pipf,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  
# detection des points en dehors du shape ----
  # conversion planaire
    air_Pf1 <- st_transform(air_Pf,crs=2154)
    air_Gb1 <- st_transform(air_Gb,crs=2154)
    air_Mh1 <- st_transform(air_Mh,crs=2154)
    air_Pe1 <- st_transform(air_Pe,crs=2154)
    air_Pipf1 <- st_transform(air_Pipf,crs=2154)
    
    epoc.pf_sf1 <- st_transform(epoc.pf_sf,crs=2154)
    epoc.gb_sf1 <- st_transform(epoc.gb_sf,crs=2154)
    epoc.mh_sf1 <- st_transform(epoc.mh_sf,crs=2154)
    epoc.pe_sf1 <- st_transform(epoc.pe_sf,crs=2154)
    epoc.pipf_sf1 <- st_transform(epoc.pipf_sf,crs=2154)
    

  # intersects ----
    # intersect global ----
      int.all.pf <- st_intersects(x=epoc.pf_sf1,y=air_Pf1,sparse = FALSE)
      int.all.pf1 <- as.integer(apply(int.all.pf,1,any))
      epoc.pf_sf1$intersect_all <- int.all.pf1
      
      int.all.gb <- st_intersects(x=epoc.gb_sf1,y=air_Gb1,sparse = FALSE)
      int.all.gb1 <- as.integer(apply(int.all.gb,1,any))
      epoc.gb_sf1$intersect_all <- int.all.gb1
      
      int.all.mh <- st_intersects(x=epoc.mh_sf1,y=air_Mh1,sparse = FALSE)
      int.all.mh1 <- as.integer(apply(int.all.mh,1,any))
      epoc.mh_sf1$intersect_all <- int.all.mh1
      
      int.all.pe <- st_intersects(x=epoc.pe_sf1,y=air_Pe1,sparse = FALSE)
      int.all.pe1 <- as.integer(apply(int.all.pe,1,any))
      epoc.pe_sf1$intersect_all <- int.all.pe1
      
      int.all.pipf <- st_intersects(x=epoc.pipf_sf1,y=air_Pipf1,sparse = FALSE)
      int.all.pipf1 <- as.integer(apply(int.all.pipf,1,any))
      epoc.pipf_sf1$intersect_all <- int.all.pipf1

    # intersect breeding -----
      int.breed.pf <- st_is_within_distance(x=epoc.pf_sf1,y=air_Pf1[air_Pf1$SEASONAL == 2,],sparse = FALSE,dist=2000)
      int.breed.pf1 <- as.integer(apply(int.breed.pf,1,any))
      epoc.pf_sf1$intersect_breed <- int.breed.pf1
      
      int.breed.gb <- st_is_within_distance(x=epoc.gb_sf1,y=air_Gb1[air_Gb1$SEASONAL == 2,],sparse = FALSE,dist=2000)
      int.breed.gb1 <- as.integer(apply(int.breed.gb,1,any))
      epoc.gb_sf1$intersect_breed <- int.breed.gb1
      
      int.breed.pipf <- st_is_within_distance(x=epoc.pipf_sf1,y=air_Pipf1[air_Pipf1$SEASONAL == 2,],sparse = FALSE,dist=2000)
      int.breed.pipf1 <- as.integer(apply(int.breed.pipf,1,any))
      epoc.pipf_sf1$intersect_breed <- int.breed.pipf1

    # intersect passage -----
      int.pass.pf <- st_is_within_distance(x=epoc.pf_sf1,y=air_Pf1[air_Pf1$SEASONAL == 4,],sparse = FALSE,dist=2000)
      int.pass.pf1 <- as.integer(apply(int.pass.pf,1,any))
      epoc.pf_sf1$intersect_pass <- int.pass.pf1
      
      int.pass.gb <- st_is_within_distance(x=epoc.gb_sf1,y=air_Gb1[air_Gb1$SEASONAL == 4,],sparse = FALSE,dist=2000)
      int.pass.gb1 <- as.integer(apply(int.pass.gb,1,any))
      epoc.gb_sf1$intersect_pass <- int.pass.gb1
      
      int.pass.pipf <- st_is_within_distance(x=epoc.pipf_sf1,y=air_Pipf1[air_Pipf1$SEASONAL == 4,],sparse = FALSE,dist=2000)
      int.pass.pipf1 <- as.integer(apply(int.pass.pipf,1,any))
      epoc.pipf_sf1$intersect_pass <- int.pass.pipf1
      
    # intersect non-breeding season -----
      int.nonbd.pipf <- st_is_within_distance(x=epoc.pipf_sf1,y=air_Pipf1[air_Pipf1$SEASONAL == 3,],sparse = FALSE,dist=2000)
      int.nonbd.pipf1 <- as.integer(apply(int.nonbd.pipf,1,any))
      epoc.pipf_sf1$intersect_non_breeding <- int.nonbd.pipf1
      
    # intersect resident -----
      int.res.mh <- st_is_within_distance(x=epoc.mh_sf1,y=air_Mh1[air_Mh1$SEASONAL == 1,],sparse = FALSE,dist=2000)
      int.res.mh1 <- as.integer(apply(int.res.mh,1,any))
      epoc.mh_sf1$intersect_resident <- int.res.mh1
      
      int.res.pe <- st_is_within_distance(x=epoc.pe_sf1,y=air_Pe1[air_Pe1$SEASONAL == 1,],sparse = FALSE,dist=2000)
      int.res.pe1 <- as.integer(apply(int.res.pe,1,any))
      epoc.pe_sf1$intersect_resident <- int.res.pe1
      
      int.res.pipf <- st_is_within_distance(x=epoc.pipf_sf1,y=air_Pipf1[air_Pipf1$SEASONAL == 1,],sparse = FALSE,dist=2000)
      int.res.pipf1 <- as.integer(apply(int.res.pipf,1,any))
      epoc.pipf_sf1$intersect_resident <- int.res.pipf1

  # cartographie de verif' (version 1) -----
    ggplot()+
      geom_sf(data=air_Pf1, alpha = 0.4) +
      geom_sf(data=epoc.pf_sf1,aes(fill=factor(intersect))) +
      geom_sf_label(data=epoc.pf_sf1,aes(label=intersect))
    
  # cartographie de verif' (version 2) ----
    ggplot() +
        geom_sf(data=air_Pf1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
        geom_sf(data=epoc.pf_sf1,aes(color=intersect_pass))
      
      
  # globalisation de l'intersect (condense l'information de 3 colonnes en une) -----
    epoc.pf_sf1$intersect <- 0
    epoc.pf_sf1[epoc.pf_sf1$intersect_all == 0,"intersect"] <- "all_out"
    epoc.pf_sf1[epoc.pf_sf1$intersect_breed == 1,"intersect"] <- "breeding_in"
    epoc.pf_sf1[epoc.pf_sf1$intersect_pass == 1,"intersect"] <- "passage_in"
    
    epoc.gb_sf1$intersect <- 0
    epoc.gb_sf1[epoc.gb_sf1$intersect_all == 0,"intersect"] <- "all_out"
    epoc.gb_sf1[epoc.gb_sf1$intersect_breed == 1,"intersect"] <- "breeding_in"
    epoc.gb_sf1[epoc.gb_sf1$intersect_pass == 1,"intersect"] <- "passage_in"
    
    epoc.mh_sf1$intersect <- 0
    epoc.mh_sf1[epoc.mh_sf1$intersect_all == 0,"intersect"] <- "all_out"
    epoc.mh_sf1[epoc.mh_sf1$intersect_resident == 1,"intersect"] <- "resident_in"
    
    epoc.pe_sf1$intersect <- 0
    epoc.pe_sf1[epoc.pe_sf1$intersect_all == 0,"intersect"] <- "all_out"
    epoc.pe_sf1[epoc.pe_sf1$intersect_resident == 1,"intersect"] <- "resident_in"
    
    epoc.pipf_sf1$intersect <- 0
    epoc.pipf_sf1[epoc.pipf_sf1$intersect_all == 0,"intersect"] <- "all_out"
    epoc.pipf_sf1[epoc.pipf_sf1$intersect_breed == 1,"intersect"] <- "breeding_in"
    epoc.pipf_sf1[epoc.pipf_sf1$intersect_pass == 1,"intersect"] <- "passage_in"
    epoc.pipf_sf1[epoc.pipf_sf1$intersect_non_breeding == 1,"intersect"] <- "non-breeding_in"
    epoc.pipf_sf1[epoc.pipf_sf1$intersect_resident == 1,"intersect"] <- "resident_in"
    
    
  # cartographie de verif' (version 3) ----
    ggplot() +
      geom_sf(data = fra.adm) +
      geom_sf(data=air_Pf1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.pf_sf1,aes(color=intersect)) +
      ggtitle("Pouillot fitis")
    
    ggplot() +
      geom_sf(data = fra.adm) +
      geom_sf(data=air_Gb1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.gb_sf1,aes(color=intersect)) +
      ggtitle("Gorgebleue a miroir")
    
    ggplot() +
      geom_sf(data = fra.adm) +
      geom_sf(data=air_Mh1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.mh_sf1,aes(color=intersect)) +
      ggtitle("Mesange huppee")
    
    ggplot() +
      geom_sf(data = fra.adm) +
      geom_sf(data=air_Pe1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.pe_sf1,aes(color=intersect)) +
      ggtitle("Pic epeiche")
    
    ggplot() +
      geom_sf(data = fra.adm) +
      geom_sf(data=air_Pipf1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.pipf_sf1,aes(color=intersect)) +
      ggtitle("Pipit farlouse")
    
  # info d'observation -----
    dat.obs.pf <- epoc.pf_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    dat.obs.gb <- epoc.gb_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    dat.obs.mh <- epoc.mh_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    dat.obs.pe <- epoc.pe_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    dat.obs.pipf <- epoc.pipf_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    
  # Pouillot fitis
    ggplot(dat.obs.pf) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de pouillot fitis") 
    
    ggplot(dat.obs.pf) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de pouillot fitis")

  # Gorgebleue a miroir
    ggplot(dat.obs.gb) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de Gorgebleue a miroir") 
    
    ggplot(dat.obs.gb) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de Gorgebleue a miroir")
    
  # Mesange huppee
    ggplot(dat.obs.mh) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de Mesange huppee") 
    
    ggplot(dat.obs.mh) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de Mesange huppee")
    
  # Pic epeiche
    ggplot(dat.obs.pe) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de Pic epeiche") 
    
    ggplot(dat.obs.pe) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de Pic epeiche")
    
  # Pipit farlouse
    ggplot(dat.obs.pipf) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de Pipit farlouse") 
    
    ggplot(dat.obs.pipf) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de Pipit farlouse")
    

















