# chemin
  setwd("C:/git/epoc/data")
# packages
  library(ggplot2)
  library(plyr)
  library(mgcv)
  library(MASS)
  library(ggExtra)
  
  
  
# upload data
  epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                     , encoding="UTF-8",quote="")
  
  epoc.oiso <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_communaute.txt"),header=T,sep="\t", dec=","
                         , encoding="UTF-8",quote="")
  epoc.envi.liste <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_liste.txt"),header=T,sep="\t", dec=","
                                , encoding="UTF-8",quote="")
  
  

  
  
  load("C:/git/epoc/qualification_obs_initialisation.RData")
  
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

# Ecart des indices d'abondance/diversite selon le departement -----
  # Idee : trier les listes qui ont un ecart trorp important avec celui de la region
  # WARNING : observations avec de fortes abondances ==> risques de tirer la moyenne de la zone vers le haut + cas des departement avec peu de donnees
  # formation des colonnes receuillant l'abondance et la diversite moyenne du departement 
    list.dep <- unique(epoc.envi.liste$Departement)
    i <- 1
      
      while(i <= length(list.dep)){
        epoc.envi.liste[epoc.envi.liste$Departement == list.dep[i],"Abondance_departement"] <- mean(epoc.envi.liste[epoc.envi.liste$Departement == list.dep[i],"Abondance_liste"])
        epoc.envi.liste[epoc.envi.liste$Departement == list.dep[i],"Diversite_departement"] <- mean(epoc.envi.liste[epoc.envi.liste$Departement == list.dep[i],"Diversite_liste"])
        
        cat(i," /",length(list.dep),"\n")
        i <- i+1
      }
  # calcul de l'ecart a la moyenne
    epoc.envi.liste$Ecart_abondance_departement <- abs(epoc.envi.liste$Abondance_liste / epoc.envi.liste$Abondance_departement)
    epoc.envi.liste$Ecart_diversite_departement <- abs(epoc.envi.liste$Diversite_liste / epoc.envi.liste$Diversite_departement)
  # visualisation
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_abondance_departement)) + xlim(-0.5,5) +
      xlab("Abondance de l'EPOC / Abondance moyenne du département") +
      ylab("Nombre d'EPOC") +
      ggtitle("Ecart d'abondance des EPOC au niveau spatial")
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_diversite_departement))
    # ecart de la diversite plus norme autour de de 1
  
  
# ecart des indices d'abondance/diversite selon le mois -----
  # Warning : attention a separer les annees
    # separation & calcul de l'abondance/diversité moyenne par mois
    test.ab <- aggregate(Abondance_liste ~ Annee + Mois, data=epoc.envi.liste, FUN=mean)
    test.dv <- aggregate(Diversite_liste ~ Annee + Mois, data=epoc.envi.liste, FUN=mean)
    
    # ajout de l'information de l'abondance/diversité moyenne aux listes
    i <- 1
    while(i <= nrow(epoc.envi.liste)){
      j <- 1
      while(j <= nrow(test.ab)){
        if(epoc.envi.liste[i,"Annee"] == test.ab[j,"Annee"] & epoc.envi.liste[i,"Mois"] == test.ab[j,"Mois"]){
          epoc.envi.liste[i,"Abondance_mois"] <- test.ab[j,"Abondance_liste"]
          epoc.envi.liste[i,"Diversite_mois"] <- test.dv[j,"Diversite_liste"]
        }
        j <- j+1
      }
      
      cat(i," /",nrow(epoc.envi.liste),"\n")
      i <- i+1
    }
      
  # calcul de l'ecart a la moyenne
    epoc.envi.liste$Ecart_abondance_mois <- abs(epoc.envi.liste$Abondance_liste / epoc.envi.liste$Abondance_mois)
    epoc.envi.liste$Ecart_diversite_mois <- abs(epoc.envi.liste$Diversite_liste / epoc.envi.liste$Diversite_mois)
    
  # visualisation
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_abondance_mois)) + xlim(-0.5,5)
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_diversite_mois))


# Travail sur la base de donnees faune-france ----
  # ==> mesure de l'implication et connaissances taxonomiques de l'observateur
  # WARNING : relie a l'anciennete de l'observateur dans la base
    id.obs <- unique(as.character(epoc.envi.liste$Observateur)) # vecteur des observateurs
    # selection de toutes les observations faune-france realisee par les observateurs du protocole EPOC
    obs.in.EPOC <- epoc$Observateur %in% id.obs 
    EPOC.obs <- epoc[which(obs.in.EPOC == TRUE),]
    
    #rm(epoc) # no need + trop encombrant
    epoc.observateur <- data.frame()
    corpus.obs <- data.frame()
    
    i <- 1
    while(i <= length(id.obs)){
      EPOC.tmp <- EPOC.obs[EPOC.obs$Observateur == id.obs[i],c("ID_liste","Observateur","Nom_espece","Nombre","Liste_complete")]
      corpus.tmp <- aggregate(Nombre ~ Nom_espece + Observateur,data=EPOC.tmp,FUN=sum) # nombre d'observation totale de toutes les especes vu par l'observateur i
      dtf.tmp <- aggregate(Nombre ~ ID_liste + Nom_espece,data=EPOC.tmp,FUN=sum) # nombre d'observation (en retirant les doublons) fait par l'observateur
      
      nb.meet.tmp <- plyr::count(dtf.tmp$Nom_espece)
      colnames(nb.meet.tmp) <- c("Nom_espece","Nb_rencontre_liste")
      nb.meet.tmp$Observateur <- c(rep(id.obs[i],nrow(nb.meet.tmp)))
      
      corpus.tmp <- join(x = corpus.tmp,y=nb.meet.tmp,by=c("Nom_espece","Observateur"))
      
      # Recuperation d'informations de mesures faune-france pour l'observateur i
      sp.range <- nrow(corpus.tmp)
      nb.obs <- nrow(dtf.tmp)
      nb.liste0 <- length(unique(EPOC.tmp[EPOC.tmp$Liste_complete == 0,"ID_liste"]))
      nb.liste1 <- length(unique(EPOC.tmp[EPOC.tmp$Liste_complete == 1,"ID_liste"]))
      nb.liste <- nb.liste0 + nb.liste1
      
      sum.obs <- data.frame(id.obs[i],as.numeric(nb.obs),as.numeric(sp.range),as.numeric(nb.liste),as.numeric(nb.liste0),
                          as.numeric(nb.liste1))
      
      # ajout des informations au sein de dtfs regroupant tous les observateurs EPOC
      epoc.observateur <- rbind(epoc.observateur,sum.obs) # A enregistrer
      corpus.obs <- rbind(corpus.obs,corpus.tmp) # A enregistrer

      
      
      cat(i," /",length(id.obs),"\n") # barre d'avancement
      i <- i+1 # iteration
    }
    
  # mise en forme du dtf  
    colnames(epoc.observateur) <- c("Observateur","Nb_observations_tot","Etendue_taxonomique","Nb_liste_tot","Nb_liste_0","Nb_liste_1")

    epoc.observateur$Ratio_liste <- epoc.observateur$Nb_liste_1 / epoc.observateur$Nb_liste_tot
    
  # idee de representation : tableau avec les grands champions d'EPOC
  #  j <- epoc.observateur[epoc.observateur$Observateur == "Jeremy Dupuy",]
  #  j <- corpus.obs[corpus.obs$Observateur == "Jeremy Dupuy",]


# Probabilite d'abondance d'observations d'especes
  # idee : en se basant sur les donnees d'observations receuillis parmis tout les observateurs du protocole EPOC
  #         calculer la probiblite d'observer une espece parmis le nombre moyens d'observations par liste (== diversite_liste)
    
  # recuperation du nombre total d'observations faites par espece
    obs.esp <- plyr::count(epoc.oiso$Nom_espece)
    colnames(obs.esp) <- c("Nom_espece","count")
    obs.esp <- obs.esp[order(obs.esp$count,decreasing = T),] # Nombre d'observations par espece selon les listes

# sauvegarde disque 1 : ----
    #save.image(file = "C:/git/epoc/qualification_obs_initialisation1.RData")
    
    load("C:/git/epoc/qualification_obs_initialisation1.RData")

# Calcul de probabilité d'observations des especes selon le protocole EPOC ----
  # Idee : apres avoir calculer le nombre de fois d'observer une espece communes dans une liste ==> calcul de la proba de l'observer par un tirage aléatoire
    # calcul du poids de l'espece dans l'ensemble des observations
      obs.esp$prob <- obs.esp$count/nrow(epoc.envi.liste)


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
                tab.qt.global.part2 <- data.frame()
                
                champ.esp$communs <- as.numeric(champ.esp$prob > 0.1)
                row.names(champ.esp) <- champ.esp$Nom_espece
                
                for(j in 1:35){
                  nb.esp <- j
                  
                  tab.qt <- data.frame() # dtf 95%
                  tab.qt2 <- data.frame() # dtf 99%
                  
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
                    tab.communs.boot.qt2 <- quantile(tab.communs.boot[,ncol(tab.communs.boot)],c(0.005,0.5,0.995)) # montre 99% des data
                    
                    min_max <- c(min(tab.communs.boot[,ncol(tab.communs.boot)]),max(tab.communs.boot[,ncol(tab.communs.boot)]))
                    
                    tab.communs.boot.qt <- append(tab.communs.boot.qt,min_max)
                    tab.communs.boot.qt2 <- append(tab.communs.boot.qt2,min_max)
                    
                    tab.qt <- rbind(tab.qt,tab.communs.boot.qt)
                    tab.qt2 <- rbind(tab.qt2,tab.communs.boot.qt2)
                    
                  }
                  
                  tab.qt.global.part <- rbind(tab.qt.global.part,apply(X = tab.qt,2,FUN=mean))
                  tab.qt.global.part2 <- rbind(tab.qt.global.part2,apply(X = tab.qt2,2,FUN=mean))
                  cat(nb.esp," /"," 35\n")
                  
                }
                
                # homogeineisation des noms de colonnes
                colnames(tab.qt.global.part) <- c("borne_inf","mediane","borne_sup","min","max")
                colnames(tab.qt.global.part2) <- c("borne_inf","mediane","borne_sup","min","max")
                
                # visualisation de la courbe
                ggplot(tab.qt.global.part) + geom_point(aes(x = c(rep(1:35)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:35)),y=mediane)) +
                  geom_ribbon(aes(x=c(rep(1:35)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Part des especes communes dans la listes (95%)") + 
                  geom_ribbon(aes(x=c(rep(1:35)),ymin=min,ymax=max),alpha=0.15)+
                  xlab("Nombre d'especes par listes") + ylab("Part en %")
                
                
                ggplot(tab.qt.global.part2) + geom_point(aes(x = c(rep(1:35)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:35)),y=mediane)) +
                  geom_ribbon(aes(x=c(rep(1:35)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Part des especes communes dans la listes (99%)") + 
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

                
 # sauvegarde disque 2 : ----
  #save.image(file = "C:/git/epoc/qualification_obs_initialisation2.RData")
                
  load("C:/git/epoc/qualification_obs_initialisation2.RData")         
                
# comparaison avec les listes des autres observateurs ------
    # idee: a partir des proba obtenus par les "champions" je vais les appliquer aux autres listes pour detecter des listes anormales
    # ex: liste de + de 7 especes sans le presence d'au moins une espece communes
                
 # formation du dtf regroupant les observations d'especes des listes "non champions" -----
      no.champ <- aggregate(Liste_complete ~ Observateur,data=epoc.envi.liste, FUN=sum)
      no.champ$part_totale <- no.champ$Liste_complete / sum(no.champ$Liste_complete)*100
                      
      no.champ.id <- no.champ[no.champ$part_totale <= 2,] # non champion = observateur ayant fait - de 2% des listes
                      
      no.champ.id1 <- merge(x = epoc.envi.liste,y=no.champ.id,by="Observateur")
      
    # verif
      length(unique(no.champ.id1$ID_liste)) == sum(no.champ.id$Liste_complete) # TRUE
  
  
    # selection de toutes les observations faites dans les listes de non champions 
    #==> permet de compter le nb de fois qu'une espece a etait observe par les non champions
      no.champ.obs <- merge(x = no.champ.id1,y=epoc.oiso,by="ID_liste")
      # verif
        length(unique(no.champ.id1$ID_liste)) == length(unique(no.champ.obs$ID_liste)) # TRUE
    
    # decompte du nb d'espece dans les listes non champions
        # formation du dtf regroupant les especes detectee + le nombre d'occurrence d'observation
        no.champ.oiso <- aggregate(Nombre ~ ID_liste + Observateur.x + Nom_espece,data=no.champ.obs,FUN=sum)                
    
  # attribution de mention (communs/rare) selon les resultats des listes champions -------
    # recuperation des listes champions
        esp.commun <- champ.esp[which(champ.esp$communs == 1),c("Nom_espece","communs")]
        esp.commun.vec <- esp.commun$Nom_espece
        esp.commun.vec <- droplevels(esp.commun.vec)
        
        
        # methode pour gerer les oiseaux rares non detecte dans les listes champions (~100 especes)
        no.champ.oiso$communs <- FALSE # tous les oiseaux sont consideres comme rare de base
        no.champ.oiso.id.commun <- no.champ.oiso$Nom_espece %in% esp.commun.vec # detection des lignes contenant des oiseaux consideres comme communs par les champions
        no.champ.oiso[which(no.champ.oiso.id.commun == TRUE),"communs"] <- TRUE # attribution de la mention commun pour les oiseaux "communs" des champions
                    
                
  # verification de la proba -------
  # idee : proceder par liste
  # regarder si les listes suivent la proba (regle generale : 8 especes rares dans une liste de 8 --> mauvaise liste)
        id.list.no.champ <- unique(no.champ.oiso$ID_liste)
        
        no.champ.prob <- data.frame()
        
        i <- 1
        while(i <= length(id.list.no.champ)){
          dtf.tmp <- no.champ.oiso[no.champ.oiso$ID_liste == id.list.no.champ[i],] # formation du dtf temporaire de la liste i (observation + oiseaux + communs/rares)
          
          # recuperation d'information de la liste
          div.tmp <- nrow(dtf.tmp) # infos sur la diversite (nb esp de la liste)
          pres.comm <- as.numeric(any(dtf.tmp$communs)) # info sur la presence d'au moins une espece dite "commune"
          part.comm <- length(which(dtf.tmp$communs == TRUE)) / nrow(dtf.tmp) # calcul de la part d'especes communes
          
          
          no.champ.prob.tmp <- c(id.list.no.champ[i],div.tmp,pres.comm,part.comm) # formation d'un vecteur rassemblant les infos en amont
          
          no.champ.prob <- rbind(no.champ.prob,no.champ.prob.tmp) # append du vecteur de la liste dans un dtf regroupant toutes les listes
          
          
          cat(i," /",length(id.list.no.champ),"\n")
          i <- i+1
        }
                
        # modification du noms des colonnes
          colnames(no.champ.prob) <- c("ID_liste","diversite","least_1_communs","part_of_communs")
          
# sauvegarde disque 3 : ----
  # save.image(file = "C:/git/epoc/qualification_obs_initialisation3.RData")
  load("C:/git/epoc/qualification_obs_initialisation3.RData")
          
  # nb listes sans oiseaux communs 
    length(which(no.champ.prob$least_1_communs == 0)) # 529 listes
    
  # flagging des listes anormales ----
    # flag : trop d'especes rare en une liste
    # d'apres courbe de proba --> impossible d'avoir une liste de plus de 4 esp sans avoir une espece commune
      no.champ.prob$flag_many_rare <- c(rep(0,nrow(no.champ.prob)))
      
    # detection des listes contenant trop d'especes rares et 0 especes communes
        id.list.rare <- which(no.champ.prob$diversite >= 4 & no.champ.prob$least_1_communs == 0)
        no.champ.prob[id.list.rare,"flag_many_rare"] <- 1 ; sum(no.champ.prob$flag_many_rare) # 313 listes flagged
        
    # flag : listes de faible div ne contenant que des especes rares
      no.champ.prob$flag_only_rare_low_div <- c(rep(0,nrow(no.champ.prob)))
      id.list.rare.low <- which(no.champ.prob$diversite < 4 & no.champ.prob$least_1_communs == 0)
      no.champ.prob[id.list.rare.low,"flag_only_rare_low_div"] <- 1 ; sum(no.champ.prob$flag_only_rare_low_div) # 216 listes flagged
                
    # verif du flag de toutes les listes sans oiseaux communs
      length(which(no.champ.prob$least_1_communs == 0)) == 
        sum(no.champ.prob$flag_many_rare) + sum(no.champ.prob$flag_only_rare_low_div) # TRUE
      
    # flag des listes ayant aperçus moins d'especes communes que l'attendu
      # selection des listes ayant aperçus au moins une espece communes (vs redondance w/ flag only rare / many rare)
        tab.qt.global.part$diversite <- c(rep(1:nrow(tab.qt.global.part))) # add d'une colonne renseignant sur la div
        no.champ.prob.comm <- no.champ.prob[which(no.champ.prob$least_1_communs==1),]
        
        # formation d'un dtf regroupant les resultats theoriques et empirique selon la diversite (nb esp observe par liste)
          no.champ.part.emp_th <- join(no.champ.prob.comm,tab.qt.global.part,by="diversite")
          
      # flagging
        id.emp_th <-  no.champ.part.emp_th[which(no.champ.part.emp_th$part_of_communs <= no.champ.part.emp_th$borne_inf),"ID_liste"]

      # rajout de l'information dans le dtf regroupant tout les listes no.champ
        no.champ.prob$flag_scarce_communs <- 0
        id.no.champ.prob <-  no.champ.prob$ID_liste %in%  id.emp_th
        
        no.champ.prob[which(id.no.champ.prob==TRUE),"flag_scarce_communs"] <- 1
      
      
    # Distribution des flags dans les listes des champions ----
    # formation des listes 
      champ.oiso$communs <- FALSE # tous les oiseaux sont consideres comme rare de base
      champ.oiso.id.commun <- champ.oiso$Nom_espece %in% esp.commun.vec # detection des lignes contenant des oiseaux consideres comme communs par les champions
      champ.oiso[which(champ.oiso.id.commun == TRUE),"communs"] <- TRUE # attribution de la mention commun pour les oiseaux "communs" des champions
      
      # attribution des probas
        id.list.champ <- unique(champ.oiso$ID_liste)
        
        champ.prob <- data.frame()
        
        i <- 1
        while(i <= length(id.list.champ)){
          dtf.tmp <- champ.oiso[champ.oiso$ID_liste == id.list.champ[i],] # formation du dtf temporaire de la liste i (observation + oiseaux + communs/rares)
          
          
          div.tmp <- nrow(dtf.tmp) # infos sur la diversite (nb esp de la liste)
          pres.comm <- as.numeric(any(dtf.tmp$communs)) # info sur la presence d'au moins une espece dite "commune"
          part.comm <- length(which(dtf.tmp$communs == TRUE)) / nrow(dtf.tmp) # calcul de la part d'especes communes
          
          champ.prob.tmp <- c(id.list.champ[i],div.tmp,pres.comm,part.comm) # formation d'un vecteur rassemblant les infos en amont
          
          champ.prob <- rbind(champ.prob,champ.prob.tmp) # append du vecteur de la liste dans un dtf regroupant toutes les listes
          
          
          cat(i," /",length(id.list.champ),"\n")
          i <- i+1
        }
        
        # modification du noms des colonnes
        colnames(champ.prob) <- c("ID_liste","diversite","least_1_communs","part_of_communs")
        
      # nb listes sans oiseaux communs 
        length(which(champ.prob$least_1_communs == 0)) # 35 listes
        
        
      # flag : trop d'especes rare en une liste
        # d'apres courbe de proba --> impossible d'avoir une liste de plus de 4 esp sans avoir une espece commune
          champ.prob$flag_many_rare <- c(rep(0,nrow(champ.prob)))
        
        # detection des listes contenant trop d'especes rares et 0 especes communes
          id.list.rare.champ <- which(champ.prob$diversite >= 4 & champ.prob$least_1_communs == 0)
          champ.prob[id.list.rare.champ,"flag_many_rare"] <- 1 ; sum(champ.prob$flag_many_rare) # 36 listes flagded
        
        # flag : listes de faible div ne contenant que des especes rares
          champ.prob$flag_only_rare_low_div <- c(rep(0,nrow(champ.prob)))
          id.list.rare.low.champ <- which(champ.prob$diversite < 4 & champ.prob$least_1_communs == 0)
          champ.prob[id.list.rare.low.champ,"flag_only_rare_low_div"] <- 1 ; sum(champ.prob$flag_only_rare_low_div) # 14 listes flagged
      
        # flag des listes ayant aperçus moins d'especes communes que l'attendu
          # selection des listes ayant aperçus au moins une espece communes (vs redondance w/ flag only rare / many rare)
            tab.qt.global.part$diversite <- c(rep(1:nrow(tab.qt.global.part))) # add d'une colonne renseignant sur la div
            champ.prob.comm <- champ.prob[which(champ.prob$least_1_communs==1),]
            
          # formation d'un dtf regroupant les resultats theoriques et empirique selon la diversite (nb esp observe par liste)
            champ.part.emp_th <- join(champ.prob.comm,tab.qt.global.part,by="diversite")
          
          # flagging
            id.emp_th <-  champ.part.emp_th[which(champ.part.emp_th$part_of_communs <= champ.part.emp_th$borne_inf),"ID_liste"]
          
          # rajout de l'information dans le dtf regroupant tout les listes champ
            champ.prob$flag_scarce_communs <- 0
            id.champ.prob <-  champ.prob$ID_liste %in%  id.emp_th
          
            champ.prob[which(id.champ.prob==TRUE),"flag_scarce_communs"] <- 1
          
          
          
# check vis-a-vis des donnees faune-france ------
# idee rassembler les informations liees aux proba et les infos de faune-france
# formation de dtf synthetiques sur les listes / observateurs
      list.obs.quali <- as.data.frame(epoc.envi.liste[,c("ID_liste","Observateur")])

      all.prob <- rbind(champ.prob,no.champ.prob)
      list.obs.quali <- plyr::join(list.obs.quali,all.prob,by="ID_liste") # dtf rassemblant les qualites des listes
      
  # rajout du nombre d'epoc realise par l'observateur
      nb.epoc.obs <- aggregate(Liste_complete ~ Observateur,data=epoc.envi.liste,sum)
      colnames(nb.epoc.obs) <- c("Observateur","Nb_epoc")
      list.obs.quali <- plyr::join(list.obs.quali,nb.epoc.obs,by="Observateur")
      
  # ajout du metaflag --> activation d'au moins un flag par liste
      list.obs.quali$flag_meta <- 0
      list.obs.quali[list.obs.quali$flag_many_rare == 1,"flag_meta"] <- 1
      list.obs.quali[list.obs.quali$flag_only_rare_low_div == 1,"flag_meta"] <- 1
      list.obs.quali[list.obs.quali$flag_scarce_communs == 1,"flag_meta"] <- 1
      
  # need de rassembler les infos par observateur
      aggr.comm <- aggregate(least_1_communs ~ Observateur,data=list.obs.quali,sum)
      aggr.flag.rare <- aggregate(flag_many_rare ~ Observateur,data=list.obs.quali,sum)
      aggr.flag.rare.low <- aggregate(flag_only_rare_low_div ~ Observateur,data=list.obs.quali,sum)
      aggr.flag.comm.scarc <- aggregate(flag_scarce_communs ~ Observateur,data=list.obs.quali,sum)
      aggr.flag.meta <- aggregate(flag_meta ~ Observateur, data=list.obs.quali,sum)

      
      # join des dtfs
      aggr.all <- plyr::join(aggr.comm,aggr.flag.rare,by="Observateur")
      aggr.all <- plyr::join(aggr.all,aggr.flag.rare.low,by="Observateur")
      aggr.all <- plyr::join(aggr.all,aggr.flag.comm.scarc,by="Observateur")
      aggr.all <- plyr::join(aggr.all,aggr.flag.meta,by="Observateur")

      
  # add des infos de qualites observateurs aux donnees faune-france
      epoc.observateur <- plyr::join(epoc.observateur,nb.epoc.obs,by="Observateur")
      epoc.observateur <- plyr::join(epoc.observateur,aggr.all,by="Observateur")
      
      # representation interressante head(epoc.observateur ; flag many_rare ordered)
      
  # calcul de la part des epoc de qualite / epoc avec des flags
  # determination du "comportement d'observation" de l'observateur
      epoc.observateur$part_epoc_least_1_communs <- epoc.observateur$least_1_communs / epoc.observateur$Nb_epoc
      epoc.observateur$part_flag_many_rare <- epoc.observateur$flag_many_rare / epoc.observateur$Nb_epoc
      epoc.observateur$part_flag_only_rare_low_div <- epoc.observateur$flag_only_rare_low_div / epoc.observateur$Nb_epoc
      epoc.observateur$part_flag_scarce_communs <- epoc.observateur$flag_scarce_communs / epoc.observateur$Nb_epoc
      epoc.observateur$part_flag_meta <- epoc.observateur$flag_meta / epoc.observateur$Nb_epoc
      

        
      
# sauvegarde disque 4 : ----
 # save.image(file = "C:/git/epoc/qualification_obs_initialisation4.RData")
 load("C:/git/epoc/qualification_obs_initialisation4.RData")       
      
      
      
      
# Calcul des residus liees a l'observateur, plus ajout dans la table epoc.observateur ----
  # idee : faire une boucle sur l'ensemble des EPOC , rassembler les residus, calculer moyenne/mediane/ecart-type pour chaque obs (=> regroupement des residus d'EPOC realisee par les differents obs)
    # moyenne / mediane / ecart-type ==> calcul coeff de variation  (ecart-type/moyenne)
  # Modeles :-----
    load("C:/git/epoc/qualification_obs_initialisation1.RData")
    library(speedglm)
    library(MASS)
    rm(epoc) ; rm(EPOC.obs)
    
    
    te <- sample(c(rep(1:nrow(epoc.envi.liste))),size=4000)
    epoc.env <- epoc.envi.liste[te,]
    
    te1 <- sample(c(rep(1:nrow(epoc.envi.liste))),size=8000)
    epoc.env1 <- epoc.envi.liste[te1,]
    
    
    # modele sur sample 4000
    start_glm <- proc.time()
    mod.ab.liste.nb.samp <- glm.nb(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=epoc.env)
    end_glm <- proc.time() # 22min
    save(mod.ab.liste.nb.samp,file="C:/git/epoc/output/result_models_nb_by_id_sampled.RData")
    rm(mod.ab.liste.nb.samp)
    
    
    start_spe.glm <- proc.time()
    mod.ab.liste.qp.samp <-glm(Abondance_liste ~ Mois + Annee + as.factor(ID_liste), data=epoc.env,family = "quasipoisson")
    end_spe.glm <- proc.time()
    save(mod.ab.liste.qp.samp,file="C:/git/epoc/output/result_models_qp_by_id_sampled.RData")
    rm(mod.ab.liste.qp.samp)
    
    mod.ab.liste.poi.samp <- glm(Abondance_liste ~ Mois + Annee + as.factor(ID_liste), data=epoc.env,family = "poisson")
    save(mod.ab.liste.poi.samp,file="C:/git/epoc/output/result_models_poi_by_id_sampled.RData")
    rm(mod.ab.liste.poi.samp)
    

    # modele sur sample 8000
    start1_glm <- proc.time()
    mod.ab.liste.nb.samp1 <- glm.nb(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=epoc.env1)
    end1_glm <- proc.time() # 
    save(mod.ab.liste.nb.samp1,file="C:/git/epoc/output/result_models_nb_by_id_sampled8000.RData")
    rm(mod.ab.liste.nb.samp1)
    
    
    start1_spe.glm <- proc.time()
    mod.ab.liste.qp.samp1 <-glm(Abondance_liste ~ Mois + Annee + as.factor(ID_liste), data=epoc.env1,family = "quasipoisson")
    end1_spe.glm <- proc.time()
    save(mod.ab.liste.qp.samp1,file="C:/git/epoc/output/result_models_qp_by_id_sampled8000.RData")
    rm(mod.ab.liste.qp.samp1)
    
    mod.ab.liste.poi.samp1 <- glm(Abondance_liste ~ Mois + Annee + as.factor(ID_liste), data=epoc.env1,family = "poisson")
    save(mod.ab.liste.poi.samp1,file="C:/git/epoc/output/result_models_poi_by_id_sampled8000.RData")
    rm(mod.ab.liste.poi.samp1)
    
    
    
    
    start_glm <- proc.time()
    mod.ab.liste.nb <- glm.nb(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=epoc.envi.liste)
    end_glm <- proc.time()
    save(mod.ab.liste.nb,file="C:/git/epoc/output/result_models_nb_by_id.RData")
    
    end_glm - start_glm
    rm(mod.ab.liste.nb)
    
    mod <- speedglm(Abondance_liste ~ Mois + Annee + as.factor(ID_liste), data=epoc.envi.liste,family = "quasipoisson")
    save(mod,file="C:/git/epoc/output/result_models_by_id.RData")
    
    mod.ab.liste.qp <- glm(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=epoc.envi.liste,family = "quasipoisson")
    save(mod.ab.liste.qp,file="C:/git/epoc/output/result_models_by_id.RData")
    
    
    

  # association residus des listes aux observateurs
    load("C:/git/epoc/output/result_models_by_id.RData")
    # residus du modele = ranger en ordre croissant 
      resi.list <- mod.ab.liste$residuals
      list.obs.quali <- list.obs.quali[order(list.obs.quali$ID_liste),]

      list.obs.quali$residus_liste <- resi.list
    
    # calcul moyenne / ecart-type / mediane pour chaque observateur
      # moyenne
        resi.obs.mean <- aggregate(residus_liste ~ Observateur,data=list.obs.quali,mean) # moyenne des residus par observateurs
        colnames(resi.obs.mean) <- c("Observateur","residus_mean")
      
      # ecart-type
        resi.obs.sd <- aggregate(residus_liste ~ Observateur,data=list.obs.quali,sd)
        colnames(resi.obs.sd) <- c("Observateur","residus_sd")
          # NA --> observateurs ayant fait qu'une liste
        resi.obs.sd[which(is.na(resi.obs.sd$residus_sd)),"residus_sd"] <- 0

      # mediane
        resi.obs.med <- aggregate(residus_liste ~ Observateur,data=list.obs.quali,median)
        colnames(resi.obs.med) <- c("Observateur","residus_median")
    
    
    # join des informations sur le dtf epoc.observateur (dtf de la qualite des observateurs)
      epoc.observateur <- join(epoc.observateur,resi.obs.mean,by="Observateur")
      epoc.observateur <- join(epoc.observateur,resi.obs.sd, by="Observateur")
      epoc.observateur <- join(epoc.observateur,resi.obs.med, by="Observateur")
      epoc.observateur$residus_coeff_var <- epoc.observateur$residus_sd / epoc.observateur$residus_mean 
    
      
    # indicateur 1 : flags sur especes rare
      indic1 <- epoc.observateur[,c("Observateur","Nb_epoc","least_1_communs","flag_many_rare","flag_only_rare_low_div","part_epoc_least_1_communs",
                                    "part_flag_many_rare","part_flag_only_rare_low_div")]

    # indicateur 2 : residus par observateurs
      indic2 <- epoc.observateur[,c("Observateur","Nb_epoc","residus_mean","residus_sd","residus_median","residus_coeff_var")]
    
    
# Zoom sur les indicateurs : interactions / cmt ils structurent le jeu de donnees  ----- 
    # structuration du jeu de donnees par les flags -----
      # plot x : flag / y : flag -------
        flag.plot <- ggplot(epoc.observateur,aes(x=part_flag_only_rare_low_div,
                                                 y=part_flag_many_rare,color=Nb_epoc,
                                                 size=Nb_epoc, # modif : add des proportions (warn : (1 - proportion) * 100 better ?)
                                                 alpha=Nb_epoc)) + 
          geom_jitter() +
          scale_color_gradient(low="blue",high="red") +
          #scale_x_log10() + scale_y_log10()+
          xlab("Proportion de listes flaggées : Only rare sp dans listes de moins de 4 espèces") +
          ylab("Proportion de listes flaggées : Only rare sp ds listes de plus de 4 espèces") +
          ggtitle("Structuration du jeu de données par l'indicateur des flags")
        plot(flag.plot)
        ggExtra::ggMarginal(flag.plot, type = "histogram")
        
        # 2eme plot des flags
        flag.plot1 <- ggplot(epoc.observateur,aes(x=part_flag_only_rare_low_div,
                                                 y=part_flag_scarce_communs,color=Nb_epoc,
                                                 size=Nb_epoc, # modif : add des proportions (warn : (1 - proportion) * 100 better ?)
                                                 alpha=Nb_epoc)) + 
          geom_jitter() +
          scale_color_gradient(low="blue",high="red") +
          #scale_x_log10() + scale_y_log10()+
          xlab("Proportion de listes flaggées : Only rare sp dans listes de moins de 4 espèces") +
          ylab("Proportion de listes flaggées : Moins d'especes communes que l'attendu") +
          ggtitle("Structuration du jeu de données par l'indicateur des flags")
        plot(flag.plot1)
        ggExtra::ggMarginal(flag.plot1, type = "histogram")
        
        #3eme plot des dlags
        flag.plot2 <- ggplot(epoc.observateur,aes(x=part_flag_many_rare,
                                                 y=part_flag_scarce_communs,color=Nb_epoc,
                                                 size=Nb_epoc, # modif : add des proportions (warn : (1 - proportion) * 100 better ?)
                                                 alpha=Nb_epoc)) + 
          geom_jitter() +
          scale_color_gradient(low="blue",high="red") +
          #scale_x_log10() + scale_y_log10()+
          xlab("Proportion de listes flaggées : Only rare sp ds listes de plus de 4 espèces") +
          ylab("Proportion de listes flaggées : Moins d'especes communes que l'attendu") +
          ggtitle("Structuration du jeu de données par l'indicateur des flags")
        plot(flag.plot2)
        ggExtra::ggMarginal(flag.plot2, type = "histogram")
        
      # plot (x : flag / y : nb_epoc) ----------
        ggplot(epoc.observateur,aes(x =Nb_epoc,y= part_flag_many_rare)) +
          geom_jitter() + ggtitle("Repartition du flag (0 communs, trop de rare) par observateurs") +
          ylab("Proportion de listes flaggées : Only rare sp ds listes de plus de 4 espèces") +
          xlab("Nombre d'EPOC")
        
        ggplot(epoc.observateur,aes(x = Nb_epoc,y= part_flag_only_rare_low_div)) +
          geom_jitter() + ggtitle("Repartition du flag (0 communs, trop de rare - faible diversité)\npar observateurs") +
          ylab("Proportion de listes flaggées : Only rare sp dans listes de moins de 4 espèces") +
          xlab("Nombre d'EPOC")
        
        ggplot(epoc.observateur,aes(x = Nb_epoc,y= part_flag_scarce_communs)) +
          geom_jitter() + ggtitle("Repartition du flag (moins de communs que l'attendu théorique)\npar observateurs") +
          ylab("Proportion de listes flaggées : Moins d'especes communes que l'attendu") +
          xlab("Nombre d'EPOC")
        
        ggplot(epoc.observateur,aes(x = Nb_epoc,y= part_flag_meta)) +
          geom_jitter() + ggtitle("Repartition du flag meta (au moins un flag activé)") +
          ylab("Proportion de listes flaggées") +
          xlab("Nombre d'EPOC")
        
      #  plot (x : flag / y : nb_epoc)  / retrait champions plus amelioration visu ------
        # initialisation : detection champion (recup nom champ puis rmv ds epoc.observateur en sauvant les maximum par part_flags)
          name.champ <- unique(as.character(champ.oiso$Observateur.x)) # recup' des noms des champions
          detect.champ <-epoc.observateur$Observateur %in% name.champ # detection des lignes avec les champions
          
          # recup' des valeurs maximals par flags
            max.champ.flag.many.rare <- max(epoc.observateur[which(detect.champ == TRUE),"part_flag_many_rare"])
            max.champ.flag.only.rare <- max(epoc.observateur[which(detect.champ == TRUE),"part_flag_only_rare_low_div"])
            max.champ.flag.scarce.communs <- max(epoc.observateur[which(detect.champ == TRUE),"part_flag_scarce_communs"])
            max.champ.flag.meta <- max(epoc.observateur[which(detect.champ == TRUE),"part_flag_meta"])
            
          epoc.observateur.no.champ <- epoc.observateur[which(detect.champ == FALSE),] # dtf epoc.observateur sans champions
          
        # plot graphs
          ggplot(epoc.observateur.no.champ,aes(x =Nb_epoc,y= part_flag_many_rare)) +
            geom_jitter() + 
            geom_line(aes(x=c(rep(1:nrow(epoc.observateur.no.champ))),y=max.champ.flag.many.rare,color="red"),show.legend=FALSE) +
            ylab("Proportion de listes flaggées : Only rare sp ds listes de plus de 4 espèces") +
            xlab("Nombre d'EPOC") +
            ggtitle(paste0("Repartition du flag (0 communs, trop de rare) \nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.many.rare))
            
          ggplot(epoc.observateur.no.champ,aes(x = Nb_epoc,y= part_flag_only_rare_low_div)) +
            geom_jitter() + ggtitle("Repartition du flag (0 communs, trop de rare - faible diversité)\npar observateurs") +
            geom_line(aes(x=c(rep(1:nrow(epoc.observateur.no.champ))),y=max.champ.flag.only.rare,color="red"),show.legend=FALSE) +
            ylab("Proportion de listes flaggées : Only rare sp dans listes de moins de 4 espèces") +
            xlab("Nombre d'EPOC") +
            ggtitle(paste0("Repartition du flag (0 communs, trop de rare - faible diversité)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.only.rare))
          
          ggplot(epoc.observateur.no.champ,aes(x = Nb_epoc,y= part_flag_scarce_communs)) +
            geom_jitter() + ggtitle("Repartition du flag (moins de communs que l'attendu théorique)\npar observateurs") +
            geom_line(aes(x=c(rep(1:nrow(epoc.observateur.no.champ))),y=max.champ.flag.scarce.communs,color="red"),show.legend=FALSE) +
            ylab("Proportion de listes flaggées : Moins d'especes communes que l'attendu") +
            xlab("Nombre d'EPOC") +
            ggtitle(paste0("Repartition du flag (moins de communs que l'attendu théorique)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.scarce.communs))
          
          ggplot(epoc.observateur.no.champ,aes(x = Nb_epoc,y= part_flag_meta)) +
            geom_jitter() + ggtitle("Repartition du flag meta (au moins un flag activé)") +
            geom_line(aes(x=c(rep(1:nrow(epoc.observateur.no.champ))),y=max.champ.flag.meta,color="red"),show.legend=FALSE) +
            ylab("Proportion de listes flaggées") +
            xlab("Nombre d'EPOC") +
            ggtitle(paste0("Repartition du flag meta (au moins un flag activé)\nPar observateurs (Sans champions)\n","Maximum champions : ",max.champ.flag.meta))
          
          
    # structuration du jeu de donnees par les residus -----
      resi.plot <- ggplot(indic2) +
        geom_jitter(aes(x=residus_coeff_var
                        ,y=residus_mean,color=Nb_epoc,
                        size=Nb_epoc,
                        alpha=Nb_epoc)) +
        geom_smooth(aes(x=residus_coeff_var,
                        y=residus_mean), method="lm") +
        xlab("Coefficient de variation des résidus") +
        ylab("Moyenne des résidus") +
        ggtitle("Structuration du jeu de données par les résidus")
      plot(resi.plot)    
    
    
      ggExtra::ggMarginal(resi.plot, type = "histogram")
      
      
      # retrait des residus outliner
        indic2.1 <- indic2[indic2$residus_coeff_var > -2,]
        
        resi1.plot <- ggplot(indic2.1) +
          geom_jitter(aes(x=residus_coeff_var
                          ,y=residus_mean,color=Nb_epoc,
                          size=Nb_epoc,
                          alpha=Nb_epoc)) +
          geom_smooth(aes(x=residus_coeff_var,
                          y=residus_mean), method="lm") +
          xlab("Coefficient de variation des résidus") +
          ylab("Moyenne des résidus") +
          ggtitle("Structuration du jeu de données par les résidus \nRetrait des outliners")
        plot(resi1.plot)  
        
        ggExtra::ggMarginal(resi1.plot, type = "histogram")
        
      # detection des observateurs outliners de residus / cb d'epoc retirer
        dif.indic2 <- indic2$Observateur %in% indic2.1$Observateur
        head(indic2[which(dif.indic2 == FALSE),])
    
    # table de correlation
      cor.indic <- epoc.observateur[,c("residus_mean","residus_coeff_var","flag_many_rare","flag_only_rare_low_div","flag_scarce_communs")]
      
      corrplot::corrplot(cor(cor.indic,method="spearman"),method="number") # 0 correlation entre les indicateurs
    
    
# sauvegarde disque 5 (special plot / rm(epoc/EPOC.obs/mod.ab.liste)) : ----
# save.image(file = "C:/git/epoc/qualification_obs_initialisation5.RData")
load("C:/git/epoc/qualification_obs_initialisation5.RData")    
    
    
    
# Ajout de l'experience de l'observateur selon la region observée ----------
# en attente d'un shp biomes  
# idée: ajout d'une experience nuance par la region (contenant des especes auxquels l'observateur peut ne pas connaitre)
  library(sf)
  library(data.table)
  # Pas d'information sur la region ds le jeu de donnees
  # besoin d'importer les infos de regions a partir d'un shp
      fra.adm.reg <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm1.shp")
      epoc.envi.liste.sf <- st_as_sf(x = epoc.envi.liste,coords=c("Lon_WGS84","Lat_WGS84"),crs=4326)

 
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      















      ggplot() + geom_sf(data=fra.adm.reg) + geom_sf(data=epoc.envi.liste.sf)