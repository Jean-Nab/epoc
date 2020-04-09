# chemin
  setwd("C:/git/epoc/data")
# packages
  library(sf)
  library(ggplot2)
  library(tidyverse)


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
      
      epoc.envi.liste <- epoc.envi.liste[which(del.juin_jui1 == FALSE),]
      epoc.oiso <- epoc.oiso[which(del.juin_jui2 == FALSE),]
      
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
          champ.oiso <- aggregate(Nombre ~ ID_liste + Observateur.x + Nom_espece,data=champ.obs,FUN=sum)
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
          
      # Listes communes d'especes + flagging des listes dans des zones tampons de 2 écorégions -----
        bary.2reg <- bary.reg[bary.reg$nb_intersection == 2,]
        
        i <- 4
        while(i < ncol(bary.1reg)-1){
          
          j <- i + 1
          while(j <= ncol(bary.1reg)-1){
            
            bary.2reg.tmp <- bary.2reg[,c(1,i,j)]
            det.intersect.2reg <- which(bary.2reg.tmp[2] == 1 & bary.2reg.tmp[3] == 1)
            
            id.list.2reg.tmp <- bary.2reg.tmp[det.intersect.2reg,"ID_liste"]
            
            # récupération des observations des listes intersectant le polygone de la colonne i et celui de la colonne j
              det.list.2reg.epoc.oiso <- epoc.oiso$ID_liste %in% id.list.2reg.tmp
            
              epoc.oiso.2reg <- epoc.oiso[det.list.2reg.epoc.oiso,]
              
            # formation des dtf de synthese sur les listes et les observateurs de cette zone tampon
              list.by2region <- aggregate(diversite ~ Observateur + ID_liste,
                                         data = epoc.oiso.2reg,
                                         FUN = sum)
              
              list.by2region$nb_liste <- 1
              
              observateur.by2region <- aggregate(nb_liste ~ Observateur, data=list.by2region,sum)
              observateur.by2region$part_liste_in_region <- observateur.by2region$nb_liste / sum(observateur.by2region$nb_liste)
            
            
            j <- j + 1
          }
          
          
          
          
          cat(i,"\n")
          i <- i + 1
        }
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
          
      

  # association des ecoregions aux observations
    test <- epoc.envi.obs_sf[1:50,]
    test.res <- st_intersects(x=eco.reg,y=test,sparse = F)


























