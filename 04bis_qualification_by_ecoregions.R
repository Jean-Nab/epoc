# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)


# upload des data ----
  epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                     , encoding="UTF-8",quote="")
  
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  epoc.envi.liste <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),header=T,sep="\t", dec=","
                                , encoding="UTF-8",quote="")
  epoc.envi.obs <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                              , encoding="UTF-8",quote="")
  
  eco.reg <- st_read("C:/git/epoc/data/france_ecoregions1.shp")
  
  
  load("C:/git/epoc/04bis_initialisation.RData")

  # tri des données ----
    # Retrait des listes w/ conditions speciales ----
      # - realise en juin/juillet : abondance superieur a mars/avril/mai
      del.juin_jui <- epoc.envi.liste[which(epoc.envi.liste$Mois >= 6),"ID_liste"]
      del.juin_jui1 <- epoc.envi.liste$ID_liste %in% del.juin_jui
      del.juin_jui2 <- epoc.oiso$ID_liste %in% del.juin_jui
      
      epoc.envi.liste <- epoc.envi.liste[which(del.juin_jui1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.juin_jui2 == FALSE),]
      
      # - realise a plus de 1200m d'altitude (presence d'autre protocoles)
      del.alt <- epoc.envi.liste[which(epoc.envi.liste$Altitude > 1200),"ID_liste"]
      del.alt1 <- epoc.envi.liste$ID_liste %in% del.alt
      del.alt2 <- epoc.oiso$ID_liste %in% del.alt
      
      epoc.envi.liste <- epoc.envi.liste[which(del.alt1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.alt2 == FALSE),]
      
      # - realise en dehors de la periode 5-17h
      del.hour <- epoc.envi.liste[which(epoc.envi.liste$Heure_de_debut < 5 | epoc.envi.liste$Heure_de_debut > 17),"ID_liste"]
      del.hour1 <- epoc.envi.liste$ID_liste %in% del.hour
      del.hour2 <- epoc.oiso$ID_liste %in% del.hour
      
      epoc.envi.liste <- epoc.envi.liste[which(del.hour1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.hour2 == FALSE),]

    # retrait des observations doublons: non triee dans epoc.envi.obs ----
      ref.id <- epoc.envi.obs$Ref %in% epoc.oiso$Ref 
      
      epoc.envi.obs <- epoc.envi.obs[which(ref.id == TRUE),]


# Determination des ecoregions ----
# IDEE: determiner les ecoregions associees aux observations (a l'aide de 2 objets spatiaux : eco.reg / epoc.envi.obs_sf)

  # ANALYSE PRELIMINAIRE ----
    # formation / conversion des objets spatiaux
      epoc.envi.obs_sf <- st_as_sf(epoc.envi.obs,coords=c("X_Lambert93_m","Y_Lambert93_m"),crs=2154)
      eco.reg <- st_transform(eco.reg,crs=2154)
      
    # exploration preleminaire (-> table)
      epoc.envi.obs_sf.preli <- epoc.envi.obs_sf[,]
      
      table.preli <- st_intersects(x=eco.reg,y=epoc.envi.obs_sf.preli,sparse = F)
      table.preli <- t(table.preli)
      table.preli.res <- colSums(table.preli) ; table.preli.res
      
      eco.nam <- as.character(eco.reg$ECO_NAME)
      names(table.preli.res) <- eco.nam
      
      # zoom sur Tyrrhenian-Adriatic sclerophyllous and mixed forests
        table.preli_dtf <- as.data.frame(table.preli)
        zoom.tyrr <- table.preli_dtf$V11
        zoom.tyrr <- which(zoom.tyrr == TRUE)
        
        zoom.epoc.tyrr <- epoc.envi.obs_sf.preli[zoom.tyrr,]
        length(unique(zoom.epoc.tyrr$ID_liste)) # 20 listes diff
        length(unique(zoom.epoc.tyrr$Observateur)) # 3 observateurs diff
        
        ggplot() + geom_sf(data=eco.reg,aes(fill=ECO_NAME)) + geom_sf(data=zoom.epoc.tyrr)
        
      # recolte info par biomes (nb obs / nb listes / nb observateurs)
        reg.nam <- c()
        nb.list <- c()
        nb.obs <- c()
        obs <- c()
        
        i <- 1
        
        while(i <= ncol(table.preli)){
          
          dtf.tmp <- table.preli[,i]
          logi.dtf <- which(dtf.tmp == TRUE) 
          
          sf.tmp <- epoc.envi.obs_sf.preli[logi.dtf,]
          
          #vec.tmp <- c(eco.nam[i],nrow(sf.tmp),length(unique(sf.tmp$ID_liste)),length(unique(sf.tmp$Observateur)))
          reg.nam <- append(reg.nam,eco.nam[i])
          nb.list <- append(nb.list, length(unique(sf.tmp$ID_liste)))
          nb.obs <- append(nb.obs, length(unique(sf.tmp$Observateur)))
          obs <- append(obs, nrow(sf.tmp))
          
          
          cat(i)
          i <- i+1
        }
        
        resul <- matrix(c(reg.nam,nb.list,nb.obs,obs),nrow=ncol(table.preli),byrow=F)
        resul <- data.frame(resul)
        
        colnames(resul) <- c("nom_ecoregion","nb_listes","nb_observateurs","nb_observations")
      
  # ANALYSE ----
    # formation du dtf de synthese (regroupant toutes les informations sur les ecoregions)
      
      
      
      

  # association des ecoregions aux observations
    test <- epoc.envi.obs_sf[1:50,]
    test.res <- st_intersects(x=eco.reg,y=test,sparse = F)


























