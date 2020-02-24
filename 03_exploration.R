# chemin
setwd("C:/git/epoc/data")

# packages
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(mgcv)
library(maptools)
library(raster)
library(sf)
library(classInt)
library(tmap) ; library(tmaptools)


# import des data/intialisation ----
    # tableau court dans la periode 1/03 - 31/07
        epoc.court.in <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_in_period.txt"),header=T,sep="\t", dec=","
                                    , encoding="UTF-8",quote="")
        
        epoc.court.in$Departement <- gsub("\\","",epoc.court.in$Departement,fixed=TRUE) # 3 departements avec des \\ inclus
        
        time_to_decimal <- function(x) {
          x <- hm(x, quiet = TRUE)
          hour(x) + minute(x) / 60
        }
        
        length_unique <- function(x){
          u <- unique(x)
          o <- length(u)
          return(o)
        }
        
        epoc.court.in$Heure_debut <- gsub("\\.","\\:",epoc.court.in$Heure_debut)
        epoc.court.in$Heure_debut <- time_to_decimal(epoc.court.in$Heure_debut)
        # gestion d'un probleme de conversion avec la foncction
          o <- which(is.na(epoc.court.in$Heure_debut))
          epoc.court.in[o,"Heure_debut"] <- as.numeric(epoc.court.in[o,"Heure_de_debut"])


# EXPLORATION ----
    # variable d'interets
          hist(epoc.court.in$Abondance_liste)
          ggplot(epoc.court.in) + geom_histogram(aes(x=Abondance_liste))
          hist(epoc.court.in$Diversite_liste)
          
          cor.test(epoc.court.in$Abondance_liste,epoc.court.in$Diversite_liste,method = "spearman")
    # Biais d'observation -----
        # Liees au temps et a la periode -----
            hist(epoc.court.in$Jour_de_l_annee, xlab="Jour de l'année",ylab="Nombre d'observations",
                 main="Histogramme de la répartition des observations selon les jours de l'année") # mois de juin/juillet : effort d'observation plus faible
            hist(epoc.court.in$Mois,xlab="Mois",ylab="Nombre d'observations",main="Histogramme de la répartition des observations par mois")
                mars <- epoc.court.in[epoc.court.in$Mois == 3,] # 66 107 obs
                avril <- epoc.court.in[epoc.court.in$Mois == 4,] # 85 907 obs
                mai <- epoc.court.in[epoc.court.in$Mois == 5,] # 80 152 obs
                juin <- epoc.court.in[epoc.court.in$Mois == 6,] # 32 659 obs
                juillet <- epoc.court.in[epoc.court.in$Mois == 7,] # 15 709 obs
            hist(epoc.court.in$Heure_debut) # Grande majorite des observations faites entre 6h et 12-13h
            hist(epoc.court.in$Tps_ecoute) # Grande majorite des observations dure 4-6 minutes
            hist(epoc.court.in$Altitude) # Majorite des observations entre 0-1000m + surrepresentation des basses altitudes
                plot(epoc.court.in$Altitude,epoc.court.in$Abondance_liste,ylim=c(0,500))
                    ggplot(epoc.court.in,aes(x=Altitude,y=Abondance_liste)) + geom_smooth(method = "glm") # TENDANCE a une diminution d'abondance aux altitudes eleve
                    ggplot(epoc.court.in,aes(x=Altitude,y=Diversite_liste)) + geom_smooth(method = "glm") # IDEM mais abondance et div correlee
                
            # GAM jour
                mod.jour.ab <- mgcv::gam(Abondance_liste ~ s(Jour_de_l_annee), data=epoc.court.in, family="nb",method="REML")
                plot(mod.jour.ab)
                
                mod.jour.dv <- mgcv::gam(Diversite_liste ~ s(Jour_de_l_annee), data=epoc.court.in, family="nb",method="REML")
                plot(mod.jour.dv)
            
            # GAM heure de début
                mod.tp.ab <- mgcv::gam(Abondance_liste ~ s(Heure_debut), data=epoc.court.in, family="nb",method="REML")
                plot(mod.tp.ab)
                
                mod.tp.dv <- mgcv::gam(Diversite_liste ~ s(Heure_debut), data=epoc.court.in, family="nb",method="REML")
                plot(mod.tp.dv)
            
            # GAM temps d'écoute
                mod.tpec.ab <- mgcv::gam(Abondance_liste ~ s(Tps_ecoute), data=epoc.court.in, family="nb",method="REML")
                plot(mod.tpec.ab)
                
                mod.tpec.dv <- mgcv::gam(Diversite_liste ~ s(Tps_ecoute), data=epoc.court.in, family="nb",method="REML")
                plot(mod.tpec.dv)
                    
                ggplot(epoc.court.in,aes(x=Mois,y=Altitude)) + geom_boxplot(aes(group=Mois),notch=TRUE) 
                  # altitude ne suit pas la loi normale
                    kruskal.test(epoc.court.in$Altitude,epoc.court.in$Mois) # Repartition de l'altitude varie significativement en fonction des mois
            
            mod.ab.alt <- mgcv::gam(Abondance_liste ~ s(Altitude), data=epoc.court.in, family="nb",method="REML")
            plot(mod.ab.alt)
            # verification des residus des modeles
                par(mfrow = c(2,2))
                gam.check(mod.ab_alt)
                
            mod.div.alt <- mgcv::gam(Diversite_liste ~ s(Altitude), data=epoc.court.in, family="nb")
            plot(mod.div.alt)
      
      # Effort echantillonnage ----
          # Observateur -----
            ggplot(epoc.court.in,aes(x=Observateur)) + geom_histogram(stat="count") + # gros biais d'effort d'echantillonnage
                xlab("Observateur") + ylab("Nombre d'observations") + ggtitle("Nombres d'observations selon les observateurs")
                
                # 5 observateurs regroupe 80 000 observations 
                    # zoom sur un observateur rassemblant 40 000 observations
                      o <- grep("Jean-Pierre Matérac|Claude Falke|Romain Riols|François Bouzendorf|Thibault Brugerolle",epoc.court.in$Observateur)
                    
                    # representation graphique de la repartition des observations -----
                          big.ops <- epoc.court.in[o,c("Lon_WGS84","Lat_WGS84","Observateur")]
                          big.ops[big.ops$Observateur == "Jean-Pierre Matérac","col"] <- "red"
                          big.ops[big.ops$Observateur == "Claude Falke","col"] <- "green"
                          big.ops[big.ops$Observateur == "Romain Riols","col"] <- "darkcyan"
                          big.ops[big.ops$Observateur == "François Bouzendorf","col"] <- "purple"
                          big.ops[big.ops$Observateur == "Thibault Brugerolle","col"] <- "deeppink2"
                          
                          data(wrld_simpl)
                          plot(wrld_simpl,xlim=c(-10,15),ylim=c(40,50))
                          big.ops.SP <- SpatialPointsDataFrame(coords = big.ops[,c("Lon_WGS84","Lat_WGS84")], data = big.ops, proj4string = raster::crs(wrld_simpl))
                          plot(big.ops.SP,add=TRUE,col=big.ops.SP$col)
                          
                    
                    # Comparaison div/abon entre les observateurs ----
                          mgcv::gam(Abondance_liste ~ Observateur, data=epoc.court.in, family="nb")
                        
                    # comparaison div/abond entre 5 gros obs vs le reste ------
                        # import data
                        big.obs <- epoc.court.in[o,]
                        small.obs <- epoc.court.in[-o,]
                        
                        # comp moyenne
                        # div et abon --> donnees de comptage ==> loi poisson ==> test de wilcoxon
                        div.big.obs <- big.obs$Diversite_liste
                        div.small.obs <- small.obs$Diversite_liste
                        
                        abon.big.obs <- big.obs$Abondance_liste
                        abon.small.obs <- small.obs$Abondance_liste
                        
                        # tests et visualisation
                        epoc.obs <- epoc.court.in
                        epoc.obs$Qualification_obs <- 0
                        epoc.obs[o,"Qualification_obs"] <- "Grande_contribution"
                        epoc.obs[-o,"Qualification_obs"] <- "Contribution_normale"
                        
                        ggplot(epoc.obs) + geom_boxplot(aes(x=Qualification_obs,y=Diversite_liste),notch=TRUE) +
                          xlab("Qualification des observateurs") + ylab("Diversité")
                        wilcox.test(div.big.obs,div.small.obs) # grand contributeur trouve plus de diversite
                        
                        ggplot(epoc.obs,aes(x=Qualification_obs,y=Abondance_liste)) + geom_boxplot(outlier.shape = NA,notch=TRUE) +
                          scale_y_continuous(limits = quantile(epoc.obs$Abondance_liste, c(0.1,0.9)))
                        wilcox.test(abon.big.obs,abon.small.obs)            

          # Nb_observateur ----
            ggplot(epoc.court.in,aes(x=Nb_observateur)) + geom_histogram(stat="count") 
             which(epoc.court.in$Nb_observateur == 3)  #62 observations realise avec 2 observateurs utilite de les garde ?
             
          # Nombre d'EPOC par observateur ----
             # IDEE : Lecture du jeu de donnee sur les observateurs et prelevement du nombre d'epoc realise (ID_liste differentes faites par le meme observateur)
                 id.obs <- unique(as.character(epoc.court.in$Observateur)) # formation d'un vecteur regroupant les observateurs
                 i <- 1
                 while(i <= length(id.obs)){ # lecture d'une boucle sur le vecteur des observateurs
                   y <- unique(epoc.court.in[epoc.court.in$Observateur == id.obs[i],"ID_liste"]) # detection des ID_liste (=EPOC) realise par l'observateur i
                   epoc.court.in[epoc.court.in$Observateur == id.obs[i],"Nb_EPOC_par_observateur"] <- length(y) # ajout du nombre d'EPOC realise par l'observateur dans une nouvelle colonne du jeu de donnees EPOC
                   
                   
                   cat(i,"/",length(id.obs),"\n") # etat d'avancement de la boucle (rapide)
                   i  <- i+1
                 }
                 
                 nb.epoc_obs <- epoc.court.in[,c("Observateur","Nb_EPOC_par_observateur")] # formation d'un dtf contenant des informations sur les observateurs et leur nombre d'epoc realise
                 l <- duplicated(nb.epoc_obs$Observateur) # detection des lignes dupliquees
                 nb.epoc_obs <- nb.epoc_obs[which(l == FALSE),] # selection des lignes n'apparaissant qu'une fois
                 nb.epoc_obs <- nb.epoc_obs[order(nb.epoc_obs$Nb_EPOC_par_observateur),] # optionnel : trie du dtf de facon decroissante
              # IDEE de romain : tirer 4/5 EPOC par observateur pour les utiliser comme calibrage des outputs de modeles
                  # Realisable ? ==> Cb d'observateur ont - de 5 EPOC
                      obs.less5 <- which(nb.epoc_obs[,"Nb_EPOC_par_observateur"] <= 5)
                      length(obs.less5) # 219 observateurs ont - de 5 EPOC
                      cat("soit",round(length(obs.less5)/length(id.obs)*100,3),"% des observateurs")
             
          # Range spatiale ----
             ggplot(epoc.court.in,aes(x=Departement)) + geom_histogram(stat="count") # presence de departement sur-echantillonne
             dep <- which(summary(epoc.court.in$Departement) > 10000)
             dep <- summary(epoc.court.in$Departement)[dep] # Haute-Savoie / Puy-de-Dôme / Loire / Bouches-du-Rhône / Charente-Maritime / Yonne
             
             kruskal.test(epoc.court.in$Abondance_liste,epoc.court.in$Departement) # difference signi d'abondance selon les departements
             
         
             
             # visualisation de l'effort d'echantillonnage ----
                    carte.dep <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm2.shp")
                    dep <- st_as_sf(epoc.court.in[,c("Nom_espece","Jour_de_l_annee","Mois","Heure_debut","ID_liste","Lon_WGS84","Lat_WGS84","Diversite_liste","Abondance_liste","Tps_ecoute","Altitude","Estimation","Nombre","Observateur")], coords=c("Lon_WGS84","Lat_WGS84"),crs = 4326)
                     
                    carte.dep$Obs_count <- lengths(st_intersects(carte.dep,dep)) # Decompte du nombre de points d'observation par departement
                    nb.obs_dep <- st_drop_geometry(carte.dep[,c("NAME_2","Obs_count")]) # tableau regroupant les observations par departement
                    # Trie des departement par ordre croissant
                      nb.obs_dep <- nb.obs_dep[order(nb.obs_dep$Obs_count),]
                      head(nb.obs_dep) # Tarn et Garonne & Aisne n'ont pas de donnees / Corse du sud possede 5 observation ==> 1 EPOC
                     
                     
                    plot(carte.dep["Obs_count"]
                          ,breaks = c(0,1,20,250,1500,6000,24000,48000,80000))
                    # preparation visualisation sur carte
                      breaks_qt <- classIntervals(c(min(carte.dep$Obs_count)- .00001, carte.dep$Obs_count), n = 9, style = "quantile")
                      carte.dep <- mutate(carte.dep, Obs_count_cat = cut(Obs_count, breaks_qt$brks)) 
    
                # ggplot version
                 ggplot(carte.dep) + geom_sf(aes(fill=Obs_count_cat)) +
                   scale_fill_brewer(palette = "PuBuGn") +
                   theme(panel.grid.major = element_line(color = gray(.5)
                                                         , linetype = "dashed", size = 0.5)
                         , panel.background = element_rect(fill ="aliceblue"))
                 
                # tmap version
                 tm_shape(carte.dep) +
                   tm_polygons("Obs_count",
                               style="fixed",breaks = c(0,1,20,150,300,750,1000,1500,2000,10000,30000,75000),
                               palette="BuGn",
                               title="Nombre \nd'observations \npar département",
                               lwd=0.05) 
                 tmap_mode("view")
                 tmap_last()
                 
            # Nb EPOC par departement/communes ----
                # Par departement : -----
                     list.dep <- unique(epoc.court.in$Departement)
                     nb.epoc_dep <- data.frame()
                     i <- 1
                     while(i <= length(list.dep)){
                       dtf.tmp <- epoc.court.in[epoc.court.in$Departement == list.dep[i],c("Departement","ID_liste")]
                       l <- duplicated(dtf.tmp$ID_liste) # detection des lignes dupliquees
                       dtf.tmp <- dtf.tmp[which(l == FALSE),]
                       
                       dtf.tmp$Nb_EPOC_by_departement <- nrow(dtf.tmp)
                       
                       nb.epoc_dep <- rbind(nb.epoc_dep,dtf.tmp)
                       
                       
                       cat(i,"/",length(list.dep),"\n")
                       i  <- i+1
                     }
                     
                     nb.epoc_dep2 <- nb.epoc_dep[,c("Departement","Nb_EPOC_by_departement")]
                     y <- duplicated(nb.epoc_dep2$Departement)
                     nb.epoc_dep2 <- nb.epoc_dep2[which(y == FALSE),]
                     
                     
                     
                     # version sans boucle
                     nb.epoc_dep <- epoc.court.in[,c("Nom_espece","Jour_de_l_annee","Mois","Heure_debut","ID_liste","Departement","Lon_WGS84","Lat_WGS84","Diversite_liste","Abondance_liste","Tps_ecoute","Altitude","Estimation","Nombre","Observateur")]
                     
                     epoc_dep <- aggregate(ID_liste ~ Departement, data=nb.epoc_dep, FUN=length_unique)
                     colnames(epoc_dep)[2] <- "Nb_EPOC_dep"
                     
                     colnames(carte.dep)[7] <- "Departement"
                     epoc_dep$Departement <- gsub("\\","",epoc_dep$Departement,fixed=TRUE)
                     carte.dep <- merge(carte.dep,epoc_dep,by="Departement",all.x=TRUE) # Ajout des donnees de comptage d'epoc par departement dans l'objet sf
                     
                     qt <- quantile(carte.dep$Nb_EPOC_dep,na.rm = TRUE)
                     
                     # 
                     cat("Nombre de département :\n - Ayant entre 1 et 10 EPOC :",length(which(carte.dep$Nb_EPOC_dep <= 10)),
                         "\n - Ayant entre 10 et 25 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 10 & carte.dep$Nb_EPOC_dep <= 25)),
                         "\n - Ayant entre 25 et 50 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 25 & carte.dep$Nb_EPOC_dep <= 50)),
                         "\n - Ayant entre 50 et 75 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 50 & carte.dep$Nb_EPOC_dep <= 75)),
                         "\n - Ayant entre 75 et 300 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 75 & carte.dep$Nb_EPOC_dep <= 300)),
                         "\n - Ayant entre 300 et 1500 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 300 & carte.dep$Nb_EPOC_dep <= 1500)),
                         "\n - Ayant entre 1500 et 3000 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 1500 & carte.dep$Nb_EPOC_dep <= 3000)),
                         "\n - Ayant entre 3000 et 5453 EPOC :",length(which(carte.dep$Nb_EPOC_dep > 3000 & carte.dep$Nb_EPOC_dep <= 5453)),
                         "\n - Sans information : ",length(which(is.na(carte.dep$Nb_EPOC_dep))))
                     
                     # Plot de la carte
                         tm_shape(carte.dep) +
                           tm_fill(col="Nb_EPOC_dep",
                                   style="fixed", breaks =c(1,10,25,50,75,300,1500,3000,5453),
                                   palette="BuGn",
                                   title = "Nombre \nd'observation \npar departements",
                                   colorNA = "burlywood2") +
                           tm_borders(col=NA,lwd=0,alpha=0.50)
                     
                     
                     # mapping du nb epoc par departement
                        carte.dep <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm2.shp")
                        # homogeneisation
                            colnames(carte.dep)[7] <- "Departement"
                            nb.epoc_dep2$Departement <- gsub("\\\\","",nb.epoc_dep2$Departement)
                            # ajout des departement sans aucune observations/EPOC
                            nb.epoc_dep2 <- rbind(nb.epoc_dep2,c("Tarn-et-Garonne",0))
                            nb.epoc_dep2 <- rbind(nb.epoc_dep2,c("Aisne",0))
                            
                      
                        carte.dep <- merge(carte.dep,nb.epoc_dep2,by="Departement")
                        carte.dep$Nb_EPOC_by_departement <- as.numeric(carte.dep$Nb_EPOC_by_departement)
                      
                      
                      ggplot(carte.dep) + geom_sf(aes(fill=Nb_EPOC_by_departement))
                      tm_shape(carte.dep) +
                        tm_polygons("Nb_EPOC_by_departement",
                                    style="fixed",breaks=c(0,1,15,40,120,250,2000,5500),
                                    palette=viridis::viridis(8,direction = -1),
                                    title="Nombre \nd'EPOC \npar département")
                # Par communes : ----
                      # Nb observations
                        carte.commun <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm5.shp")
                        colnames(carte.commun)[13] <- "Commune"
                        commun <- st_as_sf(epoc.court.in[,c("Nom_espece","Jour_de_l_annee","Mois","Heure_debut","ID_liste","Lon_WGS84","Lat_WGS84","Diversite_liste","Abondance_liste","Tps_ecoute","Altitude","Estimation","Nombre","Observateur")], coords=c("Lon_WGS84","Lat_WGS84"),crs = 4326)
                        
                        carte.commun$Obs_count <- lengths(st_intersects(carte.commun,commun)) # Decompte du nombre de points d'observation par departement
                       
                        #carte.commun$Obs_count <- log(carte.commun$Obs_count+1)
                        # carte w/ echelle log (besoin de bien calibrer)
                            tm_shape(carte.commun) +
                              tm_fill(col="Obs_count",
                                      style="fixed",breaks=c(0,0.5,1,3,5,9),
                                      palette="-Greys",
                                      legend.hist.title = "Nombre d'observation par communes")+ # Greys ou -Greys ?
                              tm_borders(col=NA,lwd=0,alpha=0.05)
                        
                        tm_shape(carte.commun) +
                          tm_fill(col="Obs_count",
                                  style="fixed",breaks=c(0,1,20,50,200,500,1500,3500,7000,12000,15500),
                                  palette="-Greys",
                                  legend.hist.title = "Nombre d'observation par communes")+ # Greys ou -Greys ?
                          tm_borders(col=NA,lwd=0,alpha=0.05)
                          
                      # Nombre d'epoc
                        nb.epoc_commun <- epoc.court.in[,c("Nom_espece","Jour_de_l_annee","Mois","Heure_debut","ID_liste","Commune","Lon_WGS84","Lat_WGS84","Diversite_liste","Abondance_liste","Tps_ecoute","Altitude","Estimation","Nombre","Observateur")]
                        # Somme des EPOC par communes
                          epoc_comm <- aggregate(ID_liste ~ Commune, data=nb.epoc_commun, FUN=length_unique)
                          colnames(epoc_comm)[2] <- "Nb_EPOC_commune"
                         
                          carte.commun <- merge(carte.commun,epoc_comm,by="Commune",all.x=TRUE) # Ajout des donnees de comptage d'epoc par commune dans l'objet sf
                        
                          qt <- quantile(carte.commun$Nb_EPOC_commune,na.rm = TRUE) ; qt
                          cat("Nombre de communes :\n - Ayant entre 1 et 2 EPOC :",length(which(carte.commun$Nb_EPOC_commune <= 2)),
                              "\n - Ayant entre 2 et 5 EPOC :",length(which(carte.commun$Nb_EPOC_commune > 2 & carte.commun$Nb_EPOC_commune <= 5)),
                              "\n - Ayant entre 5 et 25 EPOC :",length(which(carte.commun$Nb_EPOC_commune > 5 & carte.commun$Nb_EPOC_commune <= 25)),
                              "\n - Ayant entre 25 et 100 EPOC :",length(which(carte.commun$Nb_EPOC_commune > 25 & carte.commun$Nb_EPOC_commune <= 100)),
                              "\n - Ayant entre 100 et 350 EPOC :",length(which(carte.commun$Nb_EPOC_commune > 100 & carte.commun$Nb_EPOC_commune <= 350)),
                              "\n - Ayant entre 350 et 750 EPOC :",length(which(carte.commun$Nb_EPOC_commune > 350 & carte.commun$Nb_EPOC_commune <= 750)),
                              "\n - Sans information : ",length(which(is.na(carte.commun$Nb_EPOC_commune))))
                          
                        # Plot de la carte
                          tm_shape(carte.commun) +
                            tm_fill(col="Nb_EPOC_commune",
                                    style="fixed", breaks =c(1,2,5,25,100,350,750),
                                    palette="BuGn",
                                    title = "Nombre \nd'observation \npar communes",
                                    colorNA = "burlywood2") +
                            tm_borders(col=NA,lwd=0,alpha=0.05)
             
             
            # Tps d'ecoute (ab/div augmente selon le tps d'ecoute ?) ----
              mod <- mgcv::gam(Abondance_liste ~ s(Tps_ecoute), data=epoc.court.in,family="nb") ; plot(mod, main="GAM Abondance selon temps d'ecoute")
              mod <- mgcv::gam(Abondance_liste ~ s(Tps_ecoute), data=epoc.court.in,family="nb") ; plot(mod, main= "GAM Diversite selon temps d'ecoute")
             
            # Efficacite EPOC -----
              # regarder le nombre d'observation par epoc (diversite) ----
                low.div <- which(epoc.court.in$Diversite_liste <= 4)
                low.div.liste <- unique(epoc.court.in[low.div,"ID_liste"]) # 1615 EPOC avec moins de 4 listes repertorie ( = 6751 observations, 2,406%)
                
                
                # EPOC de mauvaise qualite ou EPOC caracterisant un milieu pauvre en espece ? ----
                  # IDEE : comparaison des especes trouver dans ces EPOC avec des especes communes (retrouve dans la majorite des EPOC du jeu de donnees)
                      # Formation de la liste "d'espece commune" (Warning : Biais spatiale + effort d'echantillonnage peut influencer cet indice)
                        # table de contingence espece / EPOC (ID_liste)
                            esp_liste.table <- table(epoc.court.in$ID_liste,epoc.court.in$Nom_espece)
                            nb.epoc <- nrow(esp_liste.table)
                            esp_div.by.liste <- colSums(esp_liste.table)
                            esp_div.by.liste <- sort(esp_div.by.liste,decreasing = T)
                            
                            plot(esp_div.by.liste)
                            
                            esp_communes <- which(esp_div.by.liste >= nb.epoc*0.25) # selection des especes vu dans 75% des EPOCs
                            esp_communes <- esp_div.by.liste[esp_communes]
                            cat("listes des especes :\n",names(esp_communes))
                            # Ex: Fauvette a tete noire presentes dans 20 088 EPOC
                            
                # Detection des EPOC de mauvaise qualite (=absence d'especes "communes") ----
                        epoc.low.div <- epoc.court.in[low.div,]
                        
                        esp.temp <- grepl(pattern=names(esp_communes[1]),epoc.low.div[,"Nom_espece"]) # recherche du nom de la 1ere espece communes dans toutes les observations d'EPOC de faible diversite
                        esp.com.obs <- which(esp.temp == TRUE) # formation d'un vecteur regroupant l'ensemble des lignes contenant les especes communes
                        
                        i <- 2 # indice de lecture de boucle 
                        while(i <= length(esp_communes)){ # boucle de lecture du vecteur contenant le nom d'especes communes
                          
                          esp.temp <- grepl(pattern=names(esp_communes[i]),epoc.low.div[,"Nom_espece"])
                          esp.temp1 <- which(esp.temp == TRUE)
                          esp.com.obs <- append(esp.temp1,esp.com.obs) # ajout des observations ayant apercu la i eme especes communes
                          
                          i <- i+1
                        }
                        
                        esp.com.obs <- unique(esp.com.obs) # toutes les lignes possedant au moins une espece communes
                        
                        # detection des ID_liste de faible diversite contenant au moins une observation d'especes communes
                            j <- epoc.low.div[esp.com.obs,"ID_liste"]
                            j <- unique(j)
                        # Besoin de retrouver l'ensemble des observations de ces EPOC (faible diversite, mais presence d'au moins une espece communes)
                            u.tmp <- which(epoc.low.div$ID_liste == j[1])
                            u.end <- u.tmp
                            
                              i <- 2
                              while(i <= length(j)){
                                u.tmp <- which(epoc.low.div$ID_liste == j[i])
                                u.end <- append(u.end,u.tmp) # toutes les observations dont les EPOC contiennent au moins une espece communes
                                
                                i <- i+1
                              }
                        # detection des ID_liste ne contenant aucune observation d'espece communes
                            bad.in.low.div<- epoc.low.div[-u.end,] # dtf contenant les EPOC de moins de 4 observations ne prennant pas en compte les especes "communes" du jeu de donnees
                            # = 526 observations
                            length(unique(bad.in.low.div$ID_liste)) # = 166 EPOC 
                            length(bad.in.low.div$ID_liste)/length(epoc.court.in$ID_liste)*100 # (soit 0.187% des EPOC)
                            
                            # EPOC de faible diversite sans especes communes ==> mauvaise EPOC ou EPOC dans des milieux specifiques ?
                                # listes des especes trouvees par ces EPOC :
                                      bad.in.low.div_esp <- unique(bad.in.low.div$Nom_espece)
                                      bad.in.low.div_esp <- droplevels(bad.in.low.div_esp) # 120 especes
                                      
                                # cartographie des EPOC faible diversite sans presence d'especes communes
                                      carte.dep <- st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm2.shp")
                                      bad.in.low.div.sf <- st_as_sf(bad.in.low.div[,c("Nom_espece","Jour_de_l_annee","Mois","Heure_debut","ID_liste","Lon_WGS84","Lat_WGS84","Diversite_liste","Abondance_liste","Tps_ecoute","Altitude","Estimation","Nombre","Observateur")], coords=c("Lon_WGS84","Lat_WGS84"),crs = 4326)
                                      
                                      ggplot(carte.dep) + 
                                        geom_sf(color="black") +
                                        geom_sf(data=bad.in.low.div.sf,color="red",shape=20,size=2.5) +
                                        ggtitle("Localisation des EPOC d'espèces 'rares'")
                                      
                                        
                                        
                        
                        
            # Nb d'EPOC realise par observateur -----
                
                plot(as.factor(epoc.court.in$ID_liste),epoc.court.in$Diversite_liste)
                ggplot(epoc.court.in) + geom_boxplot(aes(x=ID_liste,y=Diversite_liste,group=ID_liste))
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

      # Table de contingence (espece/observateur) ----
            dtf.esp_obs <- epoc.court.in[,c("Observateur","Nom_espece")]
            dtf.esp_obs$Presence <- c(rep(1,nrow(dtf.esp_obs)))
            #dtf.esp_obs$ID_sample <- c(rep(1:nrow(dtf.esp_obs)))
                        
            # conversion du tableau presence --> presence/absence d'obs par observateur
                dtf1 <- dcast(dtf.esp_obs, Observateur ~ Nom_espece) # donne une table d'abondance
                      
                dtf1[dtf1>0] <- 1 # conversion en table de presence/absence
                #dtf1$ID_sample <- c(rep(1:nrow(dtf1))) # ajout d'une colonne ID
                        
            # conversion format court --> format long
                dtf2 <- melt(dtf1, id.vars = "Observateur",
                             variable.name = "Nom_espece",
                             value.name = "Presence")
                        
            # perte du nb d'obs --> besoin de le recalculer
            #    dtf2$Nb_observateur <- 1 + str_count(as.character(gsub(",| et ","\\+",dtf2$Observateur)), pattern="\\+")
                        
            # aggregation des donnees
                #dtf3 <- aggregate(Presence ~ Nb_observateur + Nom_espece, data=dtf2, FUN = quantile,c(0.025,0.5,0.975))
                dtf3.obs_sp <- aggregate(Presence ~ Observateur + Nom_espece, data=dtf2, FUN = mean)
                        
            # gestion du dtf de la fonction aggregate
                #dtf4 <- data.frame(dtf3[,1:ncol(dtf3)],dtf3[,ncol(dtf3)])
                #colnames(dtf4)[4:6] <- c("Borne_inf","Mediane","Borne_sup")
                
                dtf4.obs_sp <- data.frame(dtf3.obs_sp[,1:ncol(dtf3.obs_sp)],dtf3.obs_sp[,ncol(dtf3.obs_sp)])
                colnames(dtf4.obs_sp)[4:6] <- c("Borne_inf","Mediane","Borne_sup")
                
                summary(dtf4.obs_sp)





































