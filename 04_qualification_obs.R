# chemin
  setwd("C:/git/epoc/data")
# packages
  library(ggplot2)
  library(plyr)
  library(mgcv)
  library(MASS)
  
  
  
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
      
      nb.meet.tmp <- count(dtf.tmp$Nom_espece)
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
    obs.esp <- count(epoc.oiso$Nom_espece)
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
                tir.31esp <- c()
                
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
                
          
            # bouclage general (part especes communes dans listes) ----
                
                tab.qt.global.part <- data.frame() # pour stack les 31 miyennes des quantiles
                champ.esp$communs <- as.numeric(champ.esp$prob > 0.1)
                row.names(champ.esp) <- champ.esp$Nom_espece
                
                for(j in 1:31){
                  nb.esp <- j
                  
                  tab.qt <- data.frame()
                  
                  vec.tir <- as.vector(get(paste0("tir.",nb.esp,"esp")))
                  
                  vec.communs <- champ.esp[vec.tir,"communs"]
                  tab.communs <- matrix(vec.communs,nrow = 10000)
                  
                  for(i in 1:100){
                    test <- sample(x = row(tab.communs),size=1000)
                    # comm = part en bootstrap
                    #tab.communs.boot <- tab.communs[test,]
                    
                    tab.communs.boot <- as.matrix(tab.communs[test,])
                    tab.communs.boot <- cbind(tab.communs.boot,rowSums(tab.communs.boot/ncol(tab.communs.boot))) # part des especes communes dans une liste
                    
                    tab.communs.boot.qt <- quantile(tab.communs.boot[,ncol(tab.communs.boot)],c(0.025,0.5,0.975))
                    
                    tab.qt <- rbind(tab.qt,tab.communs.boot.qt)
                    
                  }
                  
                  tab.qt.global.part <- rbind(tab.qt.global.part,apply(X = tab.qt,2,FUN=mean))
                  cat(nb.esp," /"," 31\n")
                  
                }
                
                # homogeineisation des noms de colonnes
                colnames(tab.qt.global.part) <- c("borne_inf","mediane","borne_sup")
                # visualisation de la courbe
                ggplot(tab.qt.global.part) + geom_point(aes(x = c(rep(1:31)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:31)),y=mediane)) +
                  geom_ribbon(aes(x=c(rep(1:31)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Part des especes communes dans la listes") + 
                  xlab("Nombre d'especes par listes") + ylab("Part en %")
                
                
              # globalisation du bootstrap (proba de trouver au moins une espece rare) ----
              
                tab.qt.global.prob <- data.frame() # pour stack les 31 miyennes des quantiles
                champ.esp$communs <- as.numeric(champ.esp$prob > 0.1)
                row.names(champ.esp) <- champ.esp$Nom_espece
                
                for(j in 1:31){
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
                  cat(nb.esp," /"," 31\n")
                  
                }
              
                # homogeineisation des noms de colonnes
                colnames(tab.qt.global.prob) <- c("borne_inf","mediane","borne_sup")
                # visualisation de la courbe
                ggplot(tab.qt.global.prob) + geom_point(aes(x = c(rep(1:31)),y=mediane,ymin=0.75)) + geom_line(aes(x = c(rep(1:31)),y=mediane)) +
                  geom_ribbon(aes(x=c(rep(1:31)),ymin=borne_inf,ymax=borne_sup),alpha=0.5) + ggtitle("Proba d'avoir au moins une espece communes dans les listes") + 
                  xlab("Nombre d'especes par listes") + ylab("Probabilité")
                
          
        
# Calcul des residus liees a l'observateur, plus ajout dans la table epoc.observateur ----
  # idee : faire une boucle sur l'ensemble des EPOC , rassembler les residus, calculer moyenne/mediane/ecart-type pour chaque obs (=> regroupement des residus d'EPOC realisee par les differents obs)
    
  # preparation
    # upload du fichier d'observations
      epoc.envi.observ <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_environnement_observation.txt"),header=T,sep="\t", dec=","
                                   , encoding="UTF-8",quote="")
    # retrait des listes non pris en compte (cf debut du script)
    del.juin_jui3 <- epoc.envi.observ$ID_liste %in% del.juin_jui
    epoc.envi.observ <- epoc.envi.observ[which(del.juin_jui3 == FALSE),]
    
    del.alt3 <- epoc.envi.observ$ID_liste %in% del.alt
    epoc.envi.observ <- epoc.envi.observ[which(del.alt3 == FALSE),]
    
    del.hour3 <- epoc.envi.observ$ID_liste %in% del.hour
    epoc.envi.observ <- epoc.envi.observ[which(del.hour3 == FALSE),]
    
    
    
    
    # moyenne / mediane / ecart-type ==> calcul coeff de variation  (ecart-type/moyenne)
    # need plus de memoire
    mod.ab.liste <- glm(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=epoc.envi.liste,family = "poisson")


    # test sur donnee restreinte
      champ.id2 <- champ.id1[1:15,]
      mod.ab.liste <- glm(Abondance_liste ~ Mois + Annee  + as.factor(ID_liste) , data=champ.id2,family = "poisson")








    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    















