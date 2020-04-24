# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(tidyverse)
  library(reshape2)
  library(data.table)


# upload des data ----
  epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                     , encoding="UTF-8",quote="")
  
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                          , encoding="UTF-8",quote="")
  epoc.envi.liste <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),header=T,sep="\t", dec=","
                                , encoding="UTF-8",quote="")
  epoc.envi.obs <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                              , encoding="UTF-8",quote="")
  
  eco.reg <- st_read("C:/git/epoc/data/france_ecoregions_v3.shp")
  
  fra.adm.l93 <- st_transform(st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm0.shp"),crs=2154)
  
  
  load("C:/git/epoc/04bis_initialisation.RData")

  # tri des données ----
    # Retrait des listes w/ conditions speciales ----
      # - realise en juin/juillet : abondance superieur a mars/avril/mai
      del.juin_jui <- epoc.envi.liste[which(epoc.envi.liste$Mois >= 6),"ID_liste"]
      del.juin_jui1 <- epoc.envi.liste$ID_liste %in% del.juin_jui
      del.juin_jui2 <- epoc.oiso$ID_liste %in% del.juin_jui
      del.juin_jui3 <- epoc.envi.obs$ID_liste %in% del.juin_jui
      
      epoc.envi.liste <- epoc.envi.liste[which(del.juin_jui1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.juin_jui2 == FALSE),]
      epoc.envi.obs <- epoc.envi.obs[which(del.juin_jui3 == FALSE),]
      
      
      # - realise en dehors de la periode 5-17h
      del.hour <- epoc.envi.liste[which(epoc.envi.liste$Heure_de_debut < 5 | epoc.envi.liste$Heure_de_debut > 17),"ID_liste"]
      del.hour1 <- epoc.envi.liste$ID_liste %in% del.hour
      del.hour2 <- epoc.oiso$ID_liste %in% del.hour
      del.hour3 <- epoc.envi.obs$ID_liste %in% del.hour
      
      epoc.envi.liste <- epoc.envi.liste[which(del.hour1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.hour2 == FALSE),]
      epoc.envi.obs <- epoc.envi.obs[which(del.hour3 == FALSE),]
      
    # retrait des observations doublons: non triee dans epoc.envi.obs ----
      #ref.id <- epoc.envi.obs$Ref %in% epoc.oiso$Ref 
      
      #epoc.envi.obs <- epoc.envi.obs[which(ref.id == TRUE),]


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
    # Formation des courbes théoriques (proba especes communes / part especes communes) a partir de l'ensemble des observations ----
        
      # recuperation du nombre total d'observations faites par espece
        obs.esp <- plyr::count(epoc.oiso$Nom_espece)
        colnames(obs.esp) <- c("Nom_espece","count")
        obs.esp <- obs.esp[order(obs.esp$count,decreasing = T),] # Nombre d'observations par espece selon les listes
        obs.esp$prob <- obs.esp$count/nrow(epoc.envi.liste) # calcul de la part des especes dans les listes
        
        # calcul proba de detection d'au moins une espece communes a partir des donnees des "champions" ----
        # formation du dtf de nombre d'especes trouver sur les listes "champions" ----
          # qualification d'un champion
          champ <- aggregate(Liste_complete ~ Observateur,data=epoc.envi.liste, FUN=sum)
          champ$part_totale <- champ$Liste_complete / sum(champ$Liste_complete)*100
          
          champ.id <- champ[champ$part_totale > 2,] # champion = observateur ayant fait au moins plus de 2% des listes d'EPOC
          
          champ.id1 <- merge(x = epoc.envi.liste,y=champ.id,by="Observateur")
          # verif
          length(unique(champ.id1$ID_liste)) == sum(champ.id$Liste_complete) # TRUE
        
        
          # selection de toutes les observations faites dans les listes de champions 
          #==> permet de compter le nb de fois qu'une espece a etait observe par les champions
          champ.obs <- merge(x = champ.id1,y=epoc.oiso,by="ID_liste")
          # verif
          length(unique(champ.id1$ID_liste)) == length(unique(champ.obs$ID_liste)) # TRUE
          
          # decompte du nb d'espece dans les listes champions
          # formation du dtf regroupant les especes detectee + le nombre d'occurrence d'observation
          champ.oiso <- aggregate(Abondance_brut ~ ID_liste + Observateur.x + Nom_espece.y,data=champ.obs,FUN=sum)
          champ.esp <- plyr::count(champ.oiso$Nom_espece)
          
          colnames(champ.esp) <- c("Nom_espece","count")
          champ.esp$prob <- champ.esp$count/nrow(champ.id1)
        
        # tirage avec poids sans remise -----
          tir.1esp <- c();       tir.2esp <- c()
          tir.3esp <- c();       tir.4esp <- c()
          tir.5esp <- c();       tir.6esp <- c()
          tir.7esp <- c();       tir.8esp <- c()
          tir.9esp <- c();       tir.10esp <- c()
          tir.11esp <- c();       tir.12esp <- c()
          tir.13esp <- c();       tir.14esp <- c()
          tir.15esp <- c();       tir.16esp <- c()
          tir.17esp <- c();       tir.18esp <- c()
          tir.19esp <- c();       tir.20esp <- c()
          tir.21esp <- c();       tir.22esp <- c()
          tir.23esp <- c();       tir.24esp <- c()
          tir.25esp <- c();       tir.26esp <- c()
          tir.27esp <- c();       tir.28esp <- c()
          tir.29esp <- c();       tir.30esp <- c()
          tir.31esp <- c();       tir.32esp <- c()
          tir.33esp <- c();       tir.34esp <- c()
          tir.35esp <- c()
          
          for(i in 1:10000){
            tir.1esp <- append(tir.1esp,as.character(sample(x = champ.esp$Nom_espece,size=1,replace=FALSE,prob = champ.esp$prob)))
            tir.2esp <- append(tir.2esp,as.character(sample(x = champ.esp$Nom_espece,size=2,replace=FALSE,prob = champ.esp$prob)))
            tir.3esp <- append(tir.3esp,as.character(sample(x = champ.esp$Nom_espece,size=3,replace=FALSE,prob = champ.esp$prob)))
            tir.4esp <- append(tir.4esp,as.character(sample(x = champ.esp$Nom_espece,size=4,replace=FALSE,prob = champ.esp$prob)))
            tir.5esp <- append(tir.5esp,as.character(sample(x = champ.esp$Nom_espece,size=5,replace=FALSE,prob = champ.esp$prob)))
            tir.6esp <- append(tir.6esp,as.character(sample(x = champ.esp$Nom_espece,size=6,replace=FALSE,prob = champ.esp$prob)))
            tir.7esp <- append(tir.7esp,as.character(sample(x = champ.esp$Nom_espece,size=7,replace=FALSE,prob = champ.esp$prob)))
            tir.8esp <- append(tir.8esp,as.character(sample(x = champ.esp$Nom_espece,size=8,replace=FALSE,prob = champ.esp$prob)))
            tir.9esp <- append(tir.9esp,as.character(sample(x = champ.esp$Nom_espece,size=9,replace=FALSE,prob = champ.esp$prob)))
            tir.10esp <- append(tir.10esp,as.character(sample(x = champ.esp$Nom_espece,size=10,replace=FALSE,prob = champ.esp$prob)))
            tir.11esp <- append(tir.11esp,as.character(sample(x = champ.esp$Nom_espece,size=11,replace=FALSE,prob = champ.esp$prob)))
            tir.12esp <- append(tir.12esp,as.character(sample(x = champ.esp$Nom_espece,size=12,replace=FALSE,prob = champ.esp$prob)))
            tir.13esp <- append(tir.13esp,as.character(sample(x = champ.esp$Nom_espece,size=13,replace=FALSE,prob = champ.esp$prob)))
            tir.14esp <- append(tir.14esp,as.character(sample(x = champ.esp$Nom_espece,size=14,replace=FALSE,prob = champ.esp$prob)))
            tir.15esp <- append(tir.15esp,as.character(sample(x = champ.esp$Nom_espece,size=15,replace=FALSE,prob = champ.esp$prob)))
            tir.16esp <- append(tir.16esp,as.character(sample(x = champ.esp$Nom_espece,size=16,replace=FALSE,prob = champ.esp$prob)))
            tir.17esp <- append(tir.17esp,as.character(sample(x = champ.esp$Nom_espece,size=17,replace=FALSE,prob = champ.esp$prob)))
            tir.18esp <- append(tir.18esp,as.character(sample(x = champ.esp$Nom_espece,size=18,replace=FALSE,prob = champ.esp$prob)))
            tir.19esp <- append(tir.19esp,as.character(sample(x = champ.esp$Nom_espece,size=19,replace=FALSE,prob = champ.esp$prob)))
            tir.20esp <- append(tir.20esp,as.character(sample(x = champ.esp$Nom_espece,size=20,replace=FALSE,prob = champ.esp$prob)))
            tir.21esp <- append(tir.21esp,as.character(sample(x = champ.esp$Nom_espece,size=21,replace=FALSE,prob = champ.esp$prob)))
            tir.22esp <- append(tir.22esp,as.character(sample(x = champ.esp$Nom_espece,size=22,replace=FALSE,prob = champ.esp$prob)))
            tir.23esp <- append(tir.23esp,as.character(sample(x = champ.esp$Nom_espece,size=23,replace=FALSE,prob = champ.esp$prob)))
            tir.24esp <- append(tir.24esp,as.character(sample(x = champ.esp$Nom_espece,size=24,replace=FALSE,prob = champ.esp$prob)))
            tir.25esp <- append(tir.25esp,as.character(sample(x = champ.esp$Nom_espece,size=25,replace=FALSE,prob = champ.esp$prob)))
            tir.26esp <- append(tir.26esp,as.character(sample(x = champ.esp$Nom_espece,size=26,replace=FALSE,prob = champ.esp$prob)))
            tir.27esp <- append(tir.27esp,as.character(sample(x = champ.esp$Nom_espece,size=27,replace=FALSE,prob = champ.esp$prob)))
            tir.28esp <- append(tir.28esp,as.character(sample(x = champ.esp$Nom_espece,size=28,replace=FALSE,prob = champ.esp$prob)))
            tir.29esp <- append(tir.29esp,as.character(sample(x = champ.esp$Nom_espece,size=29,replace=FALSE,prob = champ.esp$prob)))
            tir.30esp <- append(tir.30esp,as.character(sample(x = champ.esp$Nom_espece,size=30,replace=FALSE,prob = champ.esp$prob)))
            tir.31esp <- append(tir.31esp,as.character(sample(x = champ.esp$Nom_espece,size=31,replace=FALSE,prob = champ.esp$prob)))
            tir.32esp <- append(tir.32esp,as.character(sample(x = champ.esp$Nom_espece,size=32,replace=FALSE,prob = champ.esp$prob)))
            tir.33esp <- append(tir.33esp,as.character(sample(x = champ.esp$Nom_espece,size=33,replace=FALSE,prob = champ.esp$prob)))
            tir.34esp <- append(tir.34esp,as.character(sample(x = champ.esp$Nom_espece,size=34,replace=FALSE,prob = champ.esp$prob)))
            tir.35esp <- append(tir.35esp,as.character(sample(x = champ.esp$Nom_espece,size=35,replace=FALSE,prob = champ.esp$prob)))
            
            
          }
          
          # changement de format de l'objet : vecteur --> matrixx
          tir.1esp <- matrix(tir.1esp)
          tir.2esp <- matrix(tir.2esp,ncol=2,byrow=T)
          tir.3esp <- matrix(tir.3esp,ncol=3,byrow=T)
          tir.4esp <- matrix(tir.4esp,ncol=4,byrow=T)
          tir.5esp <- matrix(tir.5esp,ncol=5,byrow=T)
          tir.6esp <- matrix(tir.6esp,ncol=6,byrow=T)
          tir.7esp <- matrix(tir.7esp,ncol=7,byrow=T)
          tir.8esp <- matrix(tir.8esp,ncol=8,byrow=T)
          tir.9esp <- matrix(tir.9esp,ncol=9,byrow=T)
          tir.10esp <- matrix(tir.10esp,ncol=10,byrow=T)
          tir.11esp <- matrix(tir.11esp,ncol=11,byrow=T)
          tir.12esp <- matrix(tir.12esp,ncol=12,byrow=T)
          tir.13esp <- matrix(tir.13esp,ncol=13,byrow=T)
          tir.14esp <- matrix(tir.14esp,ncol=14,byrow=T)
          tir.15esp <- matrix(tir.15esp,ncol=15,byrow=T)
          tir.16esp <- matrix(tir.16esp,ncol=16,byrow=T)
          tir.17esp <- matrix(tir.17esp,ncol=17,byrow=T)
          tir.18esp <- matrix(tir.18esp,ncol=18,byrow=T)
          tir.19esp <- matrix(tir.19esp,ncol=19,byrow=T)
          tir.20esp <- matrix(tir.20esp,ncol=20,byrow=T)
          tir.21esp <- matrix(tir.21esp,ncol=21,byrow=T)
          tir.22esp <- matrix(tir.22esp,ncol=22,byrow=T)
          tir.23esp <- matrix(tir.23esp,ncol=23,byrow=T)
          tir.24esp <- matrix(tir.24esp,ncol=24,byrow=T)
          tir.25esp <- matrix(tir.25esp,ncol=25,byrow=T)
          tir.26esp <- matrix(tir.26esp,ncol=26,byrow=T)
          tir.27esp <- matrix(tir.27esp,ncol=27,byrow=T)
          tir.28esp <- matrix(tir.28esp,ncol=28,byrow=T)
          tir.29esp <- matrix(tir.29esp,ncol=29,byrow=T)
          tir.30esp <- matrix(tir.30esp,ncol=30,byrow=T)
          tir.31esp <- matrix(tir.31esp,ncol=31,byrow=T)
          tir.32esp <- matrix(tir.32esp,ncol=32,byrow=T)
          tir.33esp <- matrix(tir.33esp,ncol=33,byrow=T)
          tir.34esp <- matrix(tir.34esp,ncol=34,byrow=T)
          tir.35esp <- matrix(tir.35esp,ncol=35,byrow=T)
          
        
        # bouclage general (part especes communes dans listes) ----
        
          tab.qt.global.part <- data.frame() # pour stack les 35 moyennes des quantiles
          
          champ.esp$communs <- as.numeric(champ.esp$prob > 0.1)
          row.names(champ.esp) <- champ.esp$Nom_espece
          
          for(j in 1:35){
            nb.esp <- j
            
            tab.qt <- data.frame() # dtf 95%
            
            vec.tir <- as.vector(get(paste0("tir.",nb.esp,"esp")))
            
            vec.communs <- champ.esp[vec.tir,"communs"]
            tab.communs <- matrix(vec.communs,nrow = 10000)
            
            for(i in 1:100){
              test <- sample(x = row(tab.communs),size=1000)
              # comm = part en bootstrap
              #tab.communs.boot <- tab.communs[test,]
              
              tab.communs.boot <- as.matrix(tab.communs[test,])
              tab.communs.boot <- cbind(tab.communs.boot,rowSums(tab.communs.boot/ncol(tab.communs.boot))) # part des especes communes dans une liste
              
              tab.communs.boot.qt <- quantile(tab.communs.boot[,ncol(tab.communs.boot)],c(0.025,0.5,0.975)) # montre 95% des data
              
              min_max <- c(min(tab.communs.boot[,ncol(tab.communs.boot)]),max(tab.communs.boot[,ncol(tab.communs.boot)]))
              
              tab.communs.boot.qt <- append(tab.communs.boot.qt,min_max)
              
              tab.qt <- rbind(tab.qt,tab.communs.boot.qt)
              
            }
            
            tab.qt.global.part <- rbind(tab.qt.global.part,apply(X = tab.qt,2,FUN=mean))
            cat(nb.esp," /"," 35\n")
            
          }
        
        # homogeineisation des noms de colonnes
        colnames(tab.qt.global.part) <- c("borne_inf","mediane","borne_sup","min","max")

        # visualisation de la courbe
        ggplot(tab.qt.global.part) + geom_point(aes(x = c(rep(1:35)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:35)),y=mediane)) +
          geom_ribbon(aes(x=c(rep(1:35)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Part des especes communes dans la listes (95%)") + 
          geom_ribbon(aes(x=c(rep(1:35)),ymin=min,ymax=max),alpha=0.15)+
          xlab("Nombre d'especes par listes") + ylab("Part en %")
        
        # globalisation du bootstrap (proba de trouver au moins une espece rare) ----
        
          tab.qt.global.prob <- data.frame() # pour stack les 35 miyennes des quantiles
          champ.esp$communs <- as.numeric(champ.esp$prob > 0.1)
          row.names(champ.esp) <- champ.esp$Nom_espece
          
          for(j in 1:35){
            nb.esp <- j
            
            tab.qt <- data.frame()
            
            vec.tir <- as.vector(get(paste0("tir.",nb.esp,"esp")))
            
            vec.communs <- champ.esp[vec.tir,"communs"]
            tab.communs <- matrix(vec.communs,nrow = 10000)
            
            for(i in 1:100){
              test <- sample(x = row(tab.communs),size=1000)
              # comm = part en bootstrap
              #tab.communs.boot <- tab.communs[test,]
              
              tab.communs.boot <- tab.communs[test,]
              tab.communs.boot <- as.numeric(apply(as.matrix(tab.communs.boot),1,any))
              tab.communs.boot <- sum(tab.communs.boot)/length(tab.communs.boot) # part des especes communes dans une liste
              
              tab.qt <- rbind(tab.qt,tab.communs.boot)
              #tab.communs.boot <- as.matrix(tab.communs.boot)
              #tab.communs.boot <- cbind(tab.communs.boot,rowSums(tab.communs.boot/ncol(tab.communs.boot)))
              #tab.communs.boot.qt <- quantile(tab.communs.boot[,ncol(tab.communs.boot)],c(0.025,0.5,0.975))
              
              
            }
            
            tab.qt.global.prob <- rbind(tab.qt.global.prob,quantile(x = tab.qt[,1],c(0.025,0.5,0.975)))
            cat(nb.esp," /"," 35\n")
            
          }
          
          # homogeineisation des noms de colonnes
          colnames(tab.qt.global.prob) <- c("borne_inf","mediane","borne_sup")
          # visualisation de la courbe
          ggplot(tab.qt.global.prob) + geom_point(aes(x = c(rep(1:35)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:35)),y=mediane)) +
            geom_ribbon(aes(x=c(rep(1:35)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Proba d'avoir au moins une espece communes dans les listes") + 
            xlab("Nombre d'especes par listes") + ylab("Probabilité")
          
    
        
    
    # Sauvegarde 1 ----
      load("C:/git/epoc/04bis_save1.RData")
          
    # Pre-analyse : retrait des epoc de hautes altitudes ----
      high.epoc <- epoc.envi.liste[which(epoc.envi.liste$Altitude >= 1200),"ID_liste"]
      
      # formation de dtf speciaux haute altitudes
        epoc.envi.obs.high <- epoc.envi.obs[which(epoc.envi.obs$ID_liste %in% high.epoc == TRUE),]
        epoc.oiso.high <- epoc.oiso[which(epoc.oiso$ID_liste %in% high.epoc == TRUE),]
          
      # retrait dans le jeu de donnee global
        epoc.envi.minus.obs <- epoc.envi.obs[which(epoc.envi.obs$ID_liste %in% high.epoc == FALSE),]
        epoc.oiso.minus.high <- epoc.oiso[which(epoc.oiso$ID_liste %in% high.epoc == FALSE),]
        
      
    # Dtf de synthese (regroupant toutes les informations sur les ecoregions) [Quels listes appartiennent a quelles régions ?] ----
      # Calcul du barycentre des observations par listes (proxy, position de l'observateur) ----
        bary.x <- aggregate(X_Lambert93_m ~ ID_liste, data=epoc.envi.minus.obs,mean) ; colnames(bary.x) <- c("ID_liste","X_barycentre_L93")
        bary.y <- aggregate(Y_Lambert93_m ~ ID_liste, data=epoc.envi.minus.obs,mean) ; colnames(bary.y) <- c("ID_liste","Y_barycentre_L93")
        
        bary <- plyr::join(bary.x,bary.y,by="ID_liste")
        
      # formation du sf des localisations des barycentres
        bary.sf <- st_as_sf(bary,coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154)
        
        # visualisation localisation des points ----
          ggplot() + 
            geom_sf(data=fra.adm.l93,alpha=0.5) +
            geom_sf(data=eco.reg,aes(fill=ECO_NAME),alpha=0.75) +
            geom_sf(data=bary.sf,alpha=0.05) +
            ggtitle("Représentation des écorégions et de la géolocalisation des listes")
      
        
      # Preparation intersection des ecoregions ----
        # Gestion des frontieres entre ecoregions (formation d'un buffer de 25 km)
          eco.reg.l93 <- st_transform(eco.reg,crs=2154) # conversion planaire en L93 des ecoregions
          eco.reg.l93.buf <- st_buffer(eco.reg.l93,dist = 25000)
          
        # Intersection points d'observation et région
          bary.reg <- st_intersects(x=bary.sf,y=eco.reg.l93.buf,sparse=FALSE)
          bary.reg <- as.data.frame(bary.reg) # dtf contenant les points et les regions associées a leur localisation
          
          # gestion forme du dtf
            colnames(bary.reg) <- eco.reg$ECO_NAME
            bary.reg$ID_liste <- bary$ID_liste
            
            bary.reg <- plyr::join(bary,bary.reg,by="ID_liste")
            
            for(i in 4:ncol(bary.reg)){
              bary.reg[,i] <- as.numeric(bary.reg[,i])
            }
        # Information sur les zones tampons (cas ou un point d'obs recouvre plus d'un polygone de région)
          bary.reg$nb_intersection <- 0
          bary.reg[,"nb_intersection"] <- rowSums(bary.reg[,4:ncol(bary.reg)])
          
        # Cas des epoc d'altitude -----
          bary.x.high <- aggregate(X_Lambert93_m ~ ID_liste, data=epoc.envi.obs.high,mean) ; colnames(bary.x.high) <- c("ID_liste","X_barycentre_L93")
          bary.y.high <- aggregate(Y_Lambert93_m ~ ID_liste, data=epoc.envi.obs.high,mean) ; colnames(bary.y.high) <- c("ID_liste","Y_barycentre_L93")
          
          bary.high <- plyr::join(bary.x.high,bary.y.high,by="ID_liste")
          
          bary.high.sf <- st_as_sf(bary.high,coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154)
          
          bary.high.reg <- st_intersects(x=bary.high.sf,y=eco.reg.l93.buf,sparse=FALSE)
          bary.high.reg <- as.data.frame(bary.high.reg)
          
          for(i in 1:ncol(bary.high.reg)){
            bary.high.reg[,i] <- as.numeric(bary.high.reg[,i])
          }
          
          colnames(bary.high.reg) <- eco.reg$ECO_NAME
          
          bary.high.reg$`Hautes altitudes` <- 1
          bary.high.reg$`Alps conifer and mixed forests` <- NULL
          
          bary.high.reg$nb_intersection <- rowSums(bary.high.reg[,1:ncol(bary.high.reg)])
          
          
          
          bary.high.reg[bary.high.reg$`Hautes altitudes` == 1 & bary.high.reg$nb_intersection == 2,c("European Atlantic mixed forests","Cantabrian mixed forests",
                                                                                                     "Northeast Spain and Southern France Mediterranean forests","Western European broadleaf forests")] <- 0
         
          bary.high.reg$nb_intersection <- rowSums(bary.high.reg[,1:(ncol(bary.high.reg))-1])
          
          
          bary.high.reg$ID_liste <- bary.high$ID_liste
          
          
          bary.high.reg <- plyr::join(bary.high,bary.high.reg,by="ID_liste")
          
          
          # homogeneisation ----
            bary.reg$`Hautes altitudes` <- bary.reg$`Alps conifer and mixed forests`
            bary.reg$`Alps conifer and mixed forests` <- NULL
            
          bary.reg <- rbind(bary.reg,bary.high.reg)
          bary.reg <- bary.reg[,c(1:7,9,8)]
          
        
        bary.reg.sf <- st_as_sf(bary.reg,coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154) # formation de l'objet sf
        
        # cartes ----
          ggplot() + 
            geom_sf(data=fra.adm.l93,alpha=0.5) +
            geom_sf(data=eco.reg.l93,alpha=0.75) +
            geom_sf(data=bary.reg.sf,aes(color=as.factor(nb_intersection)),alpha=0.25) +
            ggtitle("Carte de visualisation des zones tampons\n(Zones à la frontière d'une ou plusieurs écorégions)")
        
        
          ggplot() + 
            geom_sf(data=fra.adm.l93,alpha=0.5) +
            geom_sf(data=eco.reg.l93,alpha=0.75) +
            geom_sf(data=bary.reg.sf,aes(color=as.factor(nb_intersection)),alpha=0.25) +
            geom_sf(data = bary.reg.sf[bary.reg.sf$`Hautes altitudes` == 1 & bary.reg.sf$nb_intersection == 1,],aes(fill="black")) +
            ggtitle("Carte de visualisation des zones tampons\n(Zones à la frontière d'une ou plusieurs écorégions)\n(Points noirs = points de hautes altitudes - > 1200m)")
              
          ggplot() +
            geom_sf(data=fra.adm.l93,alpha=0.5) +
            geom_sf(data=eco.reg.l93,alpha=0.75) + 
            geom_sf(data=eco.reg[eco.reg$ECO_NAME == "European Atlantic mixed forests",],fill="darkseagreen4",alpha=0.5)+
            geom_sf(data=bary.reg.sf[bary.reg.sf$`European Atlantic mixed forests` ==TRUE,],aes(colour=as.factor(nb_intersection))) +
            ggtitle("Zoom sur l'écorégion 'European Atlantic mixed forests'\n(Vérification de la bonne localisation des points)")
          

          
          
      # Formation des listes communes par région + flag [cas hors frontieres] -----
          # initialisation pour la fonction determination_communs_by_regions
            epoc.oiso$diversite <- 1 # indice du nb d'espece dans la liste
            tab.qt.global.part$diversite <- c(rep(1:nrow(tab.qt.global.part))) # permet de comparer les part d'especes communes théoriques vs empiriques
            
            # Table des catégories d'especes selon leur niveau de rarete (dire d'expert : Jeremy dupuy)
              cate.esp <- read.csv(file = "C:/git/epoc/data/listes_especes_all_epoc_JD.csv",header=T,sep=";",dec=",",encoding = "UTF-8")
              cate.esp2 <- cate.esp[,c("Nom_espece","Decision")]  
          
            bary.1reg <- bary.reg[bary.reg$nb_intersection == 1,] # etablissement des listes communes d'especes a partir des listes realiser au coeur de ces region
            source(file = "C:/git/epoc/04bis_functions.R")
          
          
          i <- 4 # 1ere colonne == colone des id listes
          
          while(i <= ncol(bary.1reg)-1){
            reg1.tmp <- bary.1reg[,c(1,i)] # formation du dtf regroupant id de liste et la presence/absence d'obs de la region i
            
            determination_communs_by_regions(dtf.reg1 = reg1.tmp)
            
            i <- i + 1
          }
          
          

          
          
      # Listes communes d'especes + flagging des listes dans des zones tampons de plusieurs écorégions -----
        library(reshape2)
          
        # formation d'un dtf regroupant les flaggs d'especes (communs/rares) selon les zones tampons des listes
          vec.reg <- colnames(bary.reg[5:ncol(bary.reg)-1])  # récup' des noms des polygones
          list.region <- bary.reg[,c("ID_liste",vec.reg)] # formation d'une table ID_liste / présence/absence d'une liste dans une polygone
          
          list.region_l <- melt(list.region, id.vars = "ID_liste") # formation d'un long dtf regroupant la variable nom_polygone dans une colonne (-> x5 nb ligne ==> car 5 polygones)
          colnames(list.region_l)[2:3] <- c("regions","intersect") # rename des var (+ de lisibilité)
            
            
          list.region_l <- subset(list.region_l, intersect == 1) # selection des listes ayant intersecté le polygone (-> liste realise dans zone tampon de 3 polygones ==> 3 lignes)
          
          # annexe au code --> globalisation de mes dtf communautes d'oiseaux par polygones
            oiso.reg.all <- rbind(oiso.reg.Cnmf,oiso.reg.EAmf,oiso.reg.Htsa,oiso.reg.NSaSFMf,oiso.reg.WEbf)
            oiso.reg.all <- oiso.reg.all[oiso.reg.all$communs == 1,] # selection des especes qualifiees comme communes
            
        # JOIN DES FLAGS COMMUNS/RARES SELON LE/LES POLYGONES DE LA LISTE
          list.region_l <- inner_join(list.region_l,oiso.reg.all) # tel region -> quelles especes etaient communs/rare ?
        
          list.region.espece <- unique(list.region_l[,c("ID_liste","Nom_espece","Nom_latin","communs")]) # VS flagging communs/rare d'une espece pour une liste de zone tampon

          
          
          
      # Selection des listes realisee dans les zones tampons / join avec les informations précédentes et pose des flags par listes/observateur -----
          bary.tampon.reg <- bary.reg[bary.reg$nb_intersection != 1,"ID_liste"]
          
          det.list.oiso.tampon <- epoc.oiso$ID_liste %in% bary.tampon.reg
          
          epoc.oiso.tampon <- epoc.oiso[det.list.oiso.tampon,]
          
          epoc.oiso.tampon <- left_join(x=epoc.oiso.tampon[,c("ID_liste","Observateur","Nom_espece","Nom_latin")],list.region.espece) # dtf des especes communes/rares attendue par liste (selon la zone tampon)
          
          # $communs == NA --> especes non communes observees dans la liste
            oiso.tampon.NA <- which(is.na(epoc.oiso.tampon$communs))
            epoc.oiso.tampon[oiso.tampon.NA,"communs"] <- 0
          
          
          epoc.oiso.tampon$diversite <- 1 # ajout d'une colonne pour calculer le nb d'espece vues dans une liste  
          
        # Formation du dtf regroupant les flags par listes
          list.reg.tampon <- aggregate(diversite ~ Observateur + ID_liste,
                                      data = epoc.oiso.tampon,
                                      FUN = sum)
          
          
          
        
        # PREPARATIF du flagging des listes (= calcul part d'espece communes / presence d'au moins une espece communes) -----
          epoc.oiso.tampon$communs_logical <- as.logical(epoc.oiso.tampon$communs)
        
          # flag de la part d'especes communes
            flag.prep.part.comm <- aggregate(communs ~ ID_liste, data = epoc.oiso.tampon, sum)
            colnames(flag.prep.part.comm) <- c("ID_liste","nb_communs")
          
          # flag presence d'au moins une espece commune dans la liste
            flag.prep.least_1_comm <- aggregate(communs_logical ~ ID_liste, data = epoc.oiso.tampon, any)
            colnames(flag.prep.least_1_comm) <- c("ID_liste","least_1_communs")
        
          # join au dtf (list.reg.tampon + flagging des listes)
            list.reg.tampon <- plyr::join(list.reg.tampon,flag.prep.part.comm,by="ID_liste")
            list.reg.tampon$part_communs <- list.reg.tampon$nb_communs / list.reg.tampon$diversite
            
            list.reg.tampon <- plyr::join(list.reg.tampon,flag.prep.least_1_comm,by="ID_liste")
            
            
            
            
        # FLAGGING -----
          # Flag many_rare (Liste a forte diversite [>4 especes] avec que des especes rares)
            list.reg.tampon$flag_many_rare <- 0
            
            id.list.rare <- which(list.reg.tampon$diversite >= 4 & list.reg.tampon$least_1_communs == FALSE)
            list.reg.tampon[id.list.rare,"flag_many_rare"] <- 1   
            
          # Flag only_rare_low_dic (Liste de faible diversite [< 4 especes] avec que des especes rares)
            list.reg.tampon$flag_only_rare_low_div <- 0
            
            id.list.rare.low <- which(list.reg.tampon$diversite < 4 & list.reg.tampon$least_1_communs == FALSE)
            list.reg.tampon[id.list.rare.low,"flag_only_rare_low_div"] <- 1
            
          # Flag scarce_communs (part d'especes communes moins importante que l'attendue)
            list.reg.tampon$flag_scarce_commun <- 0
            
            comp.communs <- plyr::join(list.reg.tampon[,c("ID_liste","diversite","part_communs")],tab.qt.global.part,by="diversite")
            list.emp_th <- comp.communs[which(comp.communs$part_communs <= comp.communs$borne_inf),"ID_liste"]
            
            id.list.less.comm <- list.reg.tampon$ID_liste %in% list.emp_th
            list.reg.tampon[which(id.list.less.comm == TRUE),"flag_scarce_commun"] <- 1
            
            
          # Flag premiere espece rencontree
            # ajout des categories <=> evaluation de jérémy
              epoc.oiso.tampon <- plyr::join(epoc.oiso.tampon,cate.esp2,by="Nom_espece") # join des categories de rarete experte selon le nom d'espece
              epoc.oiso.tampon[which(is.na(epoc.oiso.tampon$Decision)),"Decision"] <- 1 # cas ou une espece ne serait pas identifie (-> presummer rare)
              
            # incrementation -> detection de la 1ere observation de chaque liste
              epoc.oiso.cate_dt <- data.table(epoc.oiso.tampon)
              epoc.oiso.cate_dt <- epoc.oiso.cate_dt[, group_increment := 1:.N, by = "ID_liste"]
              epoc.oiso.tampon <- as.data.frame(epoc.oiso.cate_dt)
            
            # selection de la 1ere observation de chaque liste
              first.obs <- which(epoc.oiso.tampon$group_increment == 1)
              list.oiso.cate.tampon <- epoc.oiso.tampon[first.obs,c("ID_liste","Observateur","Nom_espece","Decision","group_increment","communs")]   
            
            # flagging moduler (espece de categorie 3 flaggees <=> non considere comme communes) 
              list.oiso.cate.tampon$flag_first_obs_unusual <- 0
              list.oiso.cate.tampon[list.oiso.cate.tampon$Decision == 2,"flag_first_obs_unusual"] <- 1
              
              list.reg.tampon <- plyr::join(list.reg.tampon,list.oiso.cate.tampon[,c("ID_liste","flag_first_obs_unusual")],by="ID_liste")
              
          
        # Rassemblement des flags ----
          # globalisation( list.flag) des liste et des flags associees  
            list.flag <- rbind(list.reg.Cnmf,list.reg.EAmf,list.reg.Htsa,list.reg.NSaSFMf,list.reg.WEbf)
            list.flag <- rbind(list.flag[,-c(grep(pattern = "nb_liste|regions",colnames(list.flag)))],list.reg.tampon)  
            
          # formation du flag meta
            list.flag$flag_meta <- rowSums(list.flag[,c("flag_many_rare","flag_only_rare_low_div","flag_scarce_commun","flag_first_obs_unusual")])
            list.flag[which(list.flag$flag_meta > 1),"flag_meta"] <- 1
            
            
        # rassemblement sur les observateurs -----
          list.flag$nb_liste <- 1 # preparation au rassemblement sur les observateurs
          observateur.flag <- aggregate(cbind(nb_liste,flag_many_rare,flag_only_rare_low_div,flag_scarce_commun,flag_first_obs_unusual) ~ 
                                          Observateur,
                                        data=list.flag,
                                        FUN = sum)
            
                      
          # calcul de la part des listes flaggées
            observateur.flag$part_many_rare <- observateur.flag$flag_many_rare / observateur.flag$nb_liste
            observateur.flag$part_only_rare <- observateur.flag$flag_only_rare / observateur.flag$nb_liste
            observateur.flag$part_scarce_commun <- observateur.flag$flag_scarce_commun / observateur.flag$nb_liste
            observateur.flag$part_first_obs_unusual <- observateur.flag$flag_first_obs_unusual / observateur.flag$nb_liste
            
            
        # graphiques repartition des flags selon le nombre d'epoc -----
          #load("C:/git/epoc/qualification_obs_initialisation4.RData") # WARNING : possible variable en conflit pour la partie de code anterieur
           # preparatif (calcul des lignes rouges + restriction de l'axe des abcisses)
            detect.champ <-observateur.flag$Observateur %in% name.champ # detection des lignes avec les champions
          
            max.champ.flag.many.rare <- max(observateur.flag[which(detect.champ == TRUE),"part_many_rare"])
            max.champ.flag.only.rare <- max(observateur.flag[which(detect.champ == TRUE),"part_only_rare"])
            max.champ.flag.scarce.communs <- max(observateur.flag[which(detect.champ == TRUE),"part_scarce_commun"])
            max.champ.flag.first.obs <- max(observateur.flag[which(detect.champ == TRUE),"part_first_obs_unusual"])

            observateur.flag.nochamp <- observateur.flag[which(detect.champ == FALSE),]            
            
          # plot graphs ----
            flag1 <- ggplot(observateur.flag.nochamp,aes(x =nb_liste,y= part_many_rare)) +
              geom_jitter() + 
              geom_hline(yintercept = max.champ.flag.many.rare,color="red") +
              ylab("Proportion de listes flaggées : Only rare sp ds listes de plus de 4 espèces") +
              xlab("Nombre d'EPOC") +
              ggtitle(paste0("Repartition du flag (0 communs, trop de rare) \nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.many.rare))
            
            flag2 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_only_rare)) +
              geom_jitter() +
              geom_hline(yintercept = max.champ.flag.only.rare,color="red") +
              ylab("Proportion de listes flaggées : Only rare sp dans listes de moins de 4 espèces") +
              xlab("Nombre d'EPOC") +
              ggtitle(paste0("Repartition du flag (0 communs, trop de rare - faible diversité)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.only.rare))
            
            flag3 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_scarce_commun)) +
              geom_jitter() + 
              geom_hline(yintercept = max.champ.flag.scarce.communs,color="red") +
              ylab("Proportion de listes flaggées : Moins d'especes communes que l'attendu") +
              xlab("Nombre d'EPOC") +
              ggtitle(paste0("Repartition du flag (moins de communs que l'attendu théorique)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.scarce.communs))
            
            flag4 <- ggplot(observateur.flag.nochamp,aes(x = nb_liste,y= part_first_obs_unusual)) +
              geom_jitter() + 
              geom_hline(yintercept = max.champ.flag.first.obs,color="red") +
              ylab("Proportion de listes flaggées") +
              xlab("Nombre d'EPOC") +
              ggtitle(paste0("Repartition du flag 1ere espece observée inhabituelle (rares ou localisées)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.meta))
            
          # plot groupe des flags
            ggpubr::ggarrange(flag1,flag2,flag3,flag4)
            
            
          # plot sommes cumulées ----
            # flag many rare
              som.cum.many.rare <- observateur.flag[,c("Observateur","nb_liste","flag_many_rare","part_many_rare")]
              som.cum.many.rare <- som.cum.many.rare[order(som.cum.many.rare$part_many_rare),]
              som.cum.many.rare.res <- cumsum(som.cum.many.rare[,c("nb_liste","part_many_rare")])
              som.cum.many.rare.res$pourcentage <- som.cum.many.rare.res$part_many_rare / max(som.cum.many.rare.res$part_many_rare)
              seuil.champ.many.rare <- max(cumsum(som.cum.many.rare[which(som.cum.many.rare$Observateur %in% name.champ == TRUE),
                                                                            c("nb_liste","part_many_rare")])[2]) / max(som.cum.many.rare$part_many_rare)
              
              sumc1 <- ggplot(som.cum.many.rare.res) +
                geom_jitter(aes(y=nb_liste,x=pourcentage)) +
                geom_vline(xintercept = seuil.champ.many.rare,color="red") +
                ggtitle("Graph : somme cumulée des parts du flag\n(bcp d'esp rare, forte diversite)\nen pourcentage")
            #ggplot(som.cum.many.rare.res) +
            #  geom_jitter(aes(y=nb_liste,x=part_many_rare))
              
              
            
            # flag only rare
              som.cum.only.rare <- observateur.flag[,c("Observateur","nb_liste","flag_only_rare_low_div","part_only_rare")]
              som.cum.only.rare <- som.cum.only.rare[order(som.cum.only.rare$part_only_rare),]
              som.cum.only.rare.res <- cumsum(som.cum.only.rare[,c("nb_liste","part_only_rare")])
              som.cum.only.rare.res$pourcentage <- som.cum.only.rare.res$part_only_rare / max(som.cum.only.rare.res$part_only_rare)
              seuil.champ.only.rare <- max(cumsum(som.cum.only.rare[which(som.cum.only.rare$Observateur %in% name.champ == TRUE),
                                                                    c("nb_liste","part_only_rare")])[2]) / max(som.cum.only.rare$part_only_rare)
              
              sumc2 <- ggplot(som.cum.only.rare.res) +
                geom_jitter(aes(y=nb_liste,x=pourcentage)) +
                geom_vline(xintercept = seuil.champ.only.rare,color="red") +
                ggtitle("Graph : somme cumulée des parts du flag\n(bcp d'esp rare, forte diversite)\nen pourcentage")
              
              
              
            # flag scarce commun
              som.cum.scarce.commun <- observateur.flag[,c("Observateur","nb_liste","flag_scarce_commun","part_scarce_commun")]
              som.cum.scarce.commun <- som.cum.scarce.commun[order(som.cum.scarce.commun$part_scarce_commun),]
              som.cum.scarce.commun.res <- cumsum(som.cum.scarce.commun[,c("nb_liste","part_scarce_commun")])
              som.cum.scarce.commun.res$pourcentage <- som.cum.scarce.commun.res$part_scarce_commun / max(som.cum.scarce.commun.res$part_scarce_commun)
              seuil.champ.scarce.commun <- max(cumsum(som.cum.scarce.commun[which(som.cum.scarce.commun$Observateur %in% name.champ == TRUE),
                                                                            c("nb_liste","part_scarce_commun")])[2]) / max(som.cum.scarce.commun.res$part_scarce_commun)
              
              
              sumc3 <- ggplot(som.cum.scarce.commun.res) +
                geom_jitter(aes(y=nb_liste,x=pourcentage)) + 
                geom_vline(xintercept = seuil.champ.scarce.commun,color="red") +
                ggtitle("Graph : somme cumulée des parts du flag\n(moins d'especes communes que l'attendue)\nen pourcentage")
              
              #ggplot(som.cum.scarce.commun.res) +
              #  geom_jitter(aes(y=nb_liste,x=part_scarce_commun)) +
              #  ggtitle("Graph : somme cumulée des parts du flag\n(moins d'especes communes que l'attendue)")
      
              
            # flag first obs unusual
              som.cum.first.obs <- observateur.flag[,c("Observateur","nb_liste","flag_first_obs_unusual","part_first_obs_unusual")]
              som.cum.first.obs <- som.cum.first.obs[order(som.cum.first.obs$part_first_obs_unusual),]
              som.cum.first.obs.res <- cumsum(som.cum.first.obs[,c("nb_liste","part_first_obs_unusual")])
              som.cum.first.obs.res$pourcentage <- som.cum.first.obs.res$part_first_obs_unusual / max(som.cum.first.obs.res$part_first_obs_unusual)
              seuil.champ.first.obs <- max(cumsum(som.cum.first.obs[which(som.cum.first.obs$Observateur %in% name.champ == TRUE),
                                                                            c("nb_liste","part_first_obs_unusual")])[2]) / max(som.cum.first.obs.res$part_first_obs_unusual)
              
              
              sumc4 <- ggplot(som.cum.first.obs.res) +
                geom_jitter(aes(y=nb_liste,x=pourcentage)) + 
                geom_vline(xintercept = seuil.champ.first.obs,color="red") +
                ggtitle("Graph : somme cumulée des parts du flag\n(moins d'especes communes que l'attendue)\nen pourcentage")
              
            # plot groupes des sommes cumulées des flags -----
              ggpubr::ggarrange(sumc1,sumc2,sumc3,sumc4)
              
              
    # Ajout des metadonnee aux barycentres ----
      bary.reg <- plyr::join(bary.reg,epoc.envi.liste[,c("ID_liste","Jour","Mois","Annee","Jour_de_l_annee","Heure_de_debut","Minute_de_debut")],
                              by="ID_liste")
      # formation d'un colone date 
        i <- 1
        while(i <= nrow(bary.reg)){
                
          # formation d'une nouvelle colonne regroupant toutes les informations temporelles
            bary.reg[i,"date"] <- paste0(bary.reg[i,"Jour"],"/",bary.reg[i,"Mois"],"/",bary.reg[i,"Annee"]," ",bary.reg[i,"Heure_de_debut"],":",bary.reg[i,"Minute_de_debut"])
                
          #cat(i,"/ ",nrow(bary.reg),"\n")
          i <- i+1
        }
        
        bary.reg$date <- as.POSIXct(bary.reg$date , format = "%d/%m/%Y %H:%M")
        
              
              
    # Sauvegarde 2 ----
      load("C:/git/epoc/04bis_save2.RData")  
    # RECUPERATION DES LISTES FLAGGEES, selon l'expérience protocole (nb listes antérieures réalisé) ----
        # calcul de l'expérience (par incrementation) ----
          # formation de la colonne experience - tri des listes par date de realisation
            exp.dtf <- epoc.envi.liste[,c("ID_liste","Observateur","Jour","Mois","Annee","Heure_de_debut","Minute_de_debut")]
                
            i <- 1
            while(i <= nrow(exp.dtf)){
                  
              # formation d'une nouvelle colonne regroupant toutes les informations temporelles
              exp.dtf[i,"date"] <- paste0(exp.dtf[i,"Jour"],"/",exp.dtf[i,"Mois"],"/",exp.dtf[i,"Annee"]," ",exp.dtf[i,"Heure_de_debut"],":",exp.dtf[i,"Minute_de_debut"])
              
              cat(i,"/ ",nrow(exp.dtf),"\n")
              i <- i+1
            }
            # formation d'un horaire pouvant etre classé    
              exp.dtf$date2 <- as.POSIXct(exp.dtf$date , format = "%d/%m/%Y %H:%M")
              
              exp.dtf <- exp.dtf[order(exp.dtf$date2),]
              
          # attribution de l'experience par incrementation selon les observateurs
            exp.dtf_dt <- data.table(exp.dtf)
            exp.dtf_dt <- exp.dtf_dt[, group.increment := 1:.N, by=Observateur]
            exp.dtf2 <- as.data.frame(exp.dtf_dt) 
            colnames(exp.dtf2)[ncol(exp.dtf2)] <- "experience_protocole"
  
          # join au dtf regroupant les flags sur les id de listes
            list.flag <- plyr::join(list.flag,exp.dtf2[,c("ID_liste","experience_protocole")],by="ID_liste")
    
        # Correlation entre les flags -----
          cor.flag <- cor(observateur.flag[,c("part_many_rare","part_only_rare","part_scarce_commun","part_first_obs_unusual")],
                           method = "spearman")
          corrplot::corrplot(cor.flag,method="number")  
            
            
        # Visualisation du comportement des flags selon l'experience protocole ----
          # GAMM
            library(mgcv)
            library(gamm4)
            library(voxel)
            
            flag.many.rare.gamm4 <- gamm4(flag_many_rare ~ s(experience_protocole), data=list.flag, random = ~ (1|Observateur),family=binomial)
            flag.only.rare.gamm4 <- gamm4(flag_only_rare_low_div ~ s(experience_protocole), data=list.flag, random = ~ (1|Observateur),family=binomial)
            flag.scarce.commun.gamm4 <- gamm4(flag_scarce_commun ~ s(experience_protocole), data=list.flag, random = ~ (1|Observateur),family=binomial)
            flag.first.obs.gamm4 <- gamm4(flag_first_obs_unusual ~ s(experience_protocole), data=list.flag, random = ~ (1|Observateur),family=binomial)
             
            g1 <- plotGAMM(flag.many.rare.gamm4,smooth.cov = "experience_protocole")
            g2 <- plotGAMM(flag.only.rare.gamm4,smooth.cov = "experience_protocole")
            g3 <- plotGAMM(flag.scarce.commun.gamm4,smooth.cov = "experience_protocole")
            g4 <- plotGAMM(flag.first.obs.gamm4,smooth.cov = "experience_protocole")
  
            # flag meta
              flag.meta.gamm4 <- gamm4(flag_meta ~ s(experience_protocole), data=list.flag, random = ~ (1|Observateur),family=binomial)
              plot(flag.meta.gamm4$gam,main="GAM : flag meta")
            
            # visualisation des 4 plots simultanée    
            library(ggpubr)          
            ggarrange(g1,g2,g3 + rremove("x.text")
                     ,g4 + rremove("x.text"))
  
            par(mfrow=c(2,2))
            plot(flag.many.rare.gamm4$gam,main="GAM : flag many rare")
            plot(flag.only.rare.gamm4$gam,main="GAM : flag only rare")
            plot(flag.scarce.commun.gamm4$gam,main="GAM : flag scarce commun")
            plot(flag.first.obs.gamm4$gam,main="GAM : flag first obs unusual")
            
  
            # sans claude falke
              list.flag2 <- list.flag[which(list.flag$Observateur %in% "Claude Falke" == FALSE),]
    
              flag.many.rare.gamm4 <- gamm4(flag_many_rare ~ s(experience_protocole), data=list.flag2, random = ~ (1|Observateur),family=binomial)
              flag.only.rare.gamm4 <- gamm4(flag_only_rare_low_div ~ s(experience_protocole), data=list.flag2, random = ~ (1|Observateur),family=binomial)
              flag.scarce.commun.gamm4 <- gamm4(flag_scarce_commun ~ s(experience_protocole), data=list.flag2, random = ~ (1|Observateur),family=binomial)
              flag.first.obs.gamm4 <- gamm4(flag_first_obs_unusual ~ s(experience_protocole), data=list.flag2, random = ~ (1|Observateur),family=binomial)
              flag.meta.gamm4 <- gamm4(flag_meta ~ s(experience_protocole), data=list.flag2, random = ~ (1|Observateur),family=binomial)
              
            
        
      # "Suspicion" ----
      # IDEE :
      # rm des listes flaggées many_rare (impossible) + detection des observateurs de ces listes
      # si ces observateurs ont des listes flaggées only_rare_low_div (-> rm, car manque de confiance)
      # formation d'un jeu de données annexes en enlevant leurs listes flaggées scarce/1st obs --> evaluation du poids
                        
        # detection des observateurs avec part des listes flaggées "many_rare" superieur au max des champions ----
          bad.observateur <- as.vector(observateur.flag[which(observateur.flag$part_many_rare > max.champ.flag.many.rare),"Observateur"])     
          
          # récuperation de l'ensemble des listes effectue par ces observateurs
            det.list.flag.bad.observateur <- list.flag$Observateur %in% bad.observateur
            list.flag.bad.observateur <- list.flag[det.list.flag.bad.observateur,]
            
          # detection liste flaggees only_rare_low_div
            det.list.only.rare.bad.observateur <- list.flag.bad.observateur[which(list.flag.bad.observateur$flag_only_rare_low_div == 1),"ID_liste"]
            
          # detection liste flaggees scarce_commun & 1st_obs_unusual
            det.list.scarce_first.bad.observateur <- list.flag.bad.observateur[which(list.flag.bad.observateur$flag_scarce_commun == 1 | list.flag.bad.observateur$flag_first_obs_unusual == 1),"ID_liste"]
            
      
        # Tagging listes flaggées many_rare (tout observateur confondu)
          list.flag$accepted <- 1
          list.flag[which(list.flag$flag_many_rare == 1),"accepted"] <- 0
          
        # Tagging des listes flaggées des observateurs suspicieux (part de liste flaggée many_are > aux champions) ----
          list.flag$strict <- 1
          # Pour le flag only_rare_low_div
            det.flagged.only.bad.observateur <- list.flag$ID_liste %in% det.list.only.rare.bad.observateur  
            list.flag[which(det.flagged.only.bad.observateur == TRUE),"accepted"] <- 0
          
          
          # Formation d'un 2eme jeu de données dans lequel on retire les listes flaggées scarce/1st_obs des observateurs suspicieux
            det.flagged.scarce_commun.bad.observateur <- list.flag$ID_liste %in% det.list.scarce_first.bad.observateur
            list.flag[which(det.flagged.scarce_commun.bad.observateur == TRUE),"strict"] <- 0
          
    # Analyse exploratoire : Etude des clusters ----
      library(sqldf)  
      library(dplyr)
            
        # determination des clusters temporels -----
              
          list.date <- loc.list[,c("ID_liste","date","Observateur")]
          list.date$date_inf <- list.date$date - 30*60
          list.date$date_sup <- list.date$date + 30*60
              
          list_date1 <- list.date      
          list_date2 <- list.date          
              
          list.date <- sqldf("SELECT * FROM list_date1 LEFT JOIN list_date2 ON
                           list_date2.date >= list_date1.date_inf AND
                           list_date2.date <= list_date1.date_sup AND
                           list_date1.ID_liste < list_date2.ID_liste")
            
          # donner plus de sens au nom de la table join 
            colnames(list.date) <- c("ID_liste_1","date_1","Observateur_1","date_inf_1","date_sup_1",
                                       "ID_liste_2","date_2","Observateur_2","date_inf_2","date_sup_2")   
            
          # flag des listes sans cluster temporelle
            list.date$L1_associated_L2 <- 1
            list.date[which(is.na(list.date$ID_liste_2)),"L1_associated_L2"] <- 0
              
              
            
          # determination des clusters spatiaux -----
          # formation d'un table spatial regroupant les barycentres des listes compris dans les clusters spatiaux
          # recuperation des listes des clusters
            list.date_v2 <- list.date[which(list.date$L1_associated_L2 == 1),]
              
            list.date_table.distance.L1 <- list.date_v2[,c("ID_liste_1","date_1")]
            list.date_table.distance.L2 <- list.date_v2[,c("ID_liste_2","date_2")]
            
            colnames(list.date_table.distance.L1)[1] <- "ID_liste"
            colnames(list.date_table.distance.L2)[1] <- "ID_liste"
            
          # jointure des listes avec leur positionnement gps
            list.date_table.distance.L1 <- plyr::join(list.date_table.distance.L1,
                                                        loc.list[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")],
                                                        by="ID_liste")
            list.date_table.distance.L2 <- plyr::join(list.date_table.distance.L2,
                                                        loc.list[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")],
                                                        by="ID_liste")
            
            
          # conversion en sf
            list.date_table.distance.L1_sf <- st_as_sf(list.date_table.distance.L1,coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                                         crs=2154)
            list.date_table.distance.L2_sf <- st_as_sf(list.date_table.distance.L2,coords = c("X_barycentre_L93","Y_barycentre_L93"),
                                                         crs=2154)
            
          # calcul des distances
            diff.spatial <- as.numeric(st_distance(list.date_table.distance.L1_sf,
                                                     list.date_table.distance.L2_sf,
                                                     by_element = TRUE))
              
            list.date_v2$Difference_spatiale_m <- diff.spatial
            
            list.date <- left_join(list.date,list.date_v2[,c("ID_liste_1","ID_liste_2","Difference_spatiale_m")],
                                     by = c("ID_liste_1","ID_liste_2"))          
            
          # flag des listes proche geographiquement
            list.date$L1_near_L2 <- 0
            det.near.list <- which(list.date$Difference_spatiale_m <= 300)   # 300m
            
            list.date[det.near.list,"L1_near_L2"] <- 1
            
            list.date.near <- list.date[which(list.date$L1_near_L2 == 1),]
      
          # Analyse cluster ----
            library(igraph)
            
            test <- list.date.near[,c("ID_liste_1","ID_liste_2")]
            
            gr.test <- graph.data.frame(test)
            links <- data.frame(id=unique(unlist(test)),group=clusters(gr.test)$membership)
            test.res <- links[order(links$group),]
            colnames(test.res) <- c("ID_liste","id_cluster")
            
            # visualisation
              test.agg <- aggregate(id ~ group,
                                    data=test.res,
                                    FUN=length) 
              hist(test.agg$id,nclass=20,
                   main="Histogramme du nombre de listes par cluster",
                   xlab="Nb listes par cluster",
                   ylab="Count")
              
            # data supplementaire
              test.agg2 <- subset(test.agg, id>5)
              sum(test.agg2$id)


            # ajout de l'id cluster au dtf regourpant les informations sur les listes ----
              list.flag <- plyr::join(list.flag,test.res,by="ID_liste") # NA -> aucun cluster
              
              
        # sauvegarde ?
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              





