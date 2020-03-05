# chemin
  setwd("C:/git/epoc/data")
# packages
  library(ggplot2)
  library(plyr)
  
  
  
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
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_abondance_departement)) + xlim(-0.5,5)
    ggplot(epoc.envi.liste) + geom_histogram(aes(x = Ecart_diversite_departement))
    # ecart de la diversite plus norme autour de de 1
  
  
# ecart des indices d'abondance/diversite selon le mois -----
  # Warning : attention a separer les annees
    test.ab <- aggregate(Abondance_liste ~ Annee + Mois, data=epoc.envi.liste, FUN=mean)
    test.dv <- aggregate(Diversite_liste ~ Annee + Mois, data=epoc.envi.liste, FUN=mean)
    
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

  # idee de representation : tableau avec les grands champions d'EPOC
    j <- epoc.observateur[epoc.observateur$Observateur == "Jeremy Dupuy",]
    j <- corpus.obs[corpus.obs$Observateur == "Jeremy Dupuy",]


# Probabilite d'abondance d'observations d'especes
  # idee : en se basant sur les donnees d'observations receuillis parmis tout les observateurs du protocole EPOC
  #         calculer la probiblite d'observer une espece parmis le nombre moyens d'observations par liste (== diversite_liste)
    
  # recuperation du nombre total d'observations faites par espece
    obs.esp <- count(epoc.oiso$Nom_espece)
    colnames(obs.esp) <- c("Nom_espece","count")
    obs.esp <- obs.esp[order(obs.esp$count,decreasing = T),] # Nombre d'observations par espece selon les listes
    
    



























