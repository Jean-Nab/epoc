# chemin
  setwd("C:/git/epoc/data")
# packages
  library(ggplot2)
  library(sf)


# upload des data ------
  load("C:/git/epoc/data/STOC/Aire_Pouillot_fitis.Rda")
  
  epoc.obs <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")

  air_Pf <- Aire_Pouillot_fitis
  
# retrait des observations doublons: non triee dans epoc.envi.obs
  ref.id <- epoc.obs$Ref %in% epoc.oiso$Ref 
  
  epoc.obs <- epoc.obs[which(ref.id == TRUE),]

# visualisation de l'air ----
  ggplot()+
    geom_sf(data=Aire_Pouillot_fitis, aes(fill = factor(SEASONAL)), alpha = 0.4)
  
# selection des observations de pouillot fitis ----
  id.pf <- which(epoc.oiso[,"Nom_espece"] == "Pouillot_fitis")
  
  ref.pf <- epoc.oiso[id.pf,"Ref"]
  
  id.ref.pf <- epoc.obs$Ref %in% ref.pf
  
  epoc.pf <- epoc.obs[which(id.ref.pf == TRUE),]

# formation d'un sf a partir des observation de pouillot fitis ----
  epoc.pf_sf <- st_as_sf(epoc.pf,coords = c("Lon_WGS84","Lat_WGS84"),crs=4326)
  # cartographie
    ggplot()+
      geom_sf(data=Aire_Pouillot_fitis, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.pf_sf)
  
  
# detection des points en dehors du shape ----
  # conversion planaire
    air_Pf1 <- st_transform(air_Pf,crs=2154)
    epoc.pf_sf1 <- st_transform(epoc.pf_sf,crs=2154)
    
  # decouplage des polygones
    air_Pf1.breed <- air_Pf1[air_Pf1$SEASONAL == 2,]
    air_Pf1.pass <- air_Pf1[air_Pf1$SEASONAL == 4,]
    
    
  # intersects ----
    # intersect global
      int.all <- st_intersects(x=epoc.pf_sf1,y=air_Pf1,sparse = FALSE)
      int.all1 <- as.integer(apply(int.all,1,any))
      epoc.pf_sf1$intersect_all <- int.all1
      epoc.pf_sf1[epoc.pf_sf1$intersect_all == 1,"intersect_all"] <- "in"
      epoc.pf_sf1[epoc.pf_sf1$intersect_all == 0,"intersect_all"] <- "out"
      
    # intersect breeding
      int.breed <- st_intersects(x=epoc.pf_sf1,y=air_Pf1.breed,sparse = FALSE)
      int.breed1 <- as.integer(apply(int.breed,1,any))
      epoc.pf_sf1$intersect_breed <- int.breed1
      epoc.pf_sf1[epoc.pf_sf1$intersect_breed == 1,"intersect_breed"] <- "in"
      epoc.pf_sf1[epoc.pf_sf1$intersect_breed == 0,"intersect_breed"] <- "out"
      
    # intersect passage
      int.pass <- st_intersects(x=epoc.pf_sf1,y=air_Pf1.pass,sparse = FALSE)
      int.pass1 <- as.integer(apply(int.pass,1,any))
      epoc.pf_sf1$intersect_pass <- int.pass1
      epoc.pf_sf1[epoc.pf_sf1$intersect_pass == 1,"intersect_pass"] <- "in"
      epoc.pf_sf1[epoc.pf_sf1$intersect_pass == 0,"intersect_pass"] <- "out"

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
    epoc.pf_sf1[epoc.pf_sf1$intersect_breed == "out","intersect"] <- "breeding_out"
    epoc.pf_sf1[epoc.pf_sf1$intersect_pass == "out","intersect"] <- "passage_out"
    epoc.pf_sf1[epoc.pf_sf1$intersect_all == "out","intersect"] <- "all_out"
    
  # cartographie de verif' (version 3) ----
    ggplot() +
      geom_sf(data=air_Pf1, aes(fill = factor(SEASONAL)), alpha = 0.4) +
      geom_sf(data=epoc.pf_sf1,aes(color=intersect))
    
  # info d'observation -----
    dat.obs <- epoc.pf_sf1[,c("ID_liste","Ref","Jour","Mois","Annee","Jour_de_l_annee","intersect")]
    
    ggplot(dat.obs) + geom_histogram(aes(x=Jour_de_l_annee,fill=intersect),position="dodge") +
      xlab("Jour de l'année") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (jour de l'année) des observations de pouillot fitis") 
    
    ggplot(dat.obs) + geom_histogram(aes(x=Mois,fill=intersect),position="dodge") +
      xlab("Mois") + ylab("Nombre d'observation") +
      ggtitle("Repartition temporelle (mois) des observations de pouillot fitis")
    
    

















