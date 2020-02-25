# chemin
  setwd("C:/git/epoc/data")

library(lubridate)
  library(ggplot2)
  library(dplyr)
  library(sf)


epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                   , encoding="UTF-8",quote="")

# function to convert time observation to minutes since midnight
    time_to_decimal <- function(x) {
      x <- hm(x, quiet = TRUE)
      hour(x)*60 + minute(x)
    }
    # time_to_decimal("8:13")

# BUT du script : 
#     - Compter l'effort (tps d'ecoute) de toutes les listes completes (= effort total / effort moyen)
#     - Compter le nombre de listes completes mensuelles selon la periode 2017/2019

# filtrage ----
    # filtrage selon listes completes :
      epoc.filt1 <- epoc[epoc$Liste_complete == 1,]

    # filtrage selon les protocoles
      search.prot <- grep("SHOC|STOC_EPS|STOC_MONTAGNE|STOC_ONF|STOC_SITES|WATERBIRDS",epoc.filt1$Protocole) # Selection des lignes contenant les protocoles a retirer
      epoc.filt2 <- epoc.filt1[-search.prot,] # formation d'un dtf sans les lignes selectionnees

      
# Repartition mensuelle : calcul du nombre de temps d'ecoute pour chaque liste ----
  epoc3 <- epoc.filt2
  # formation d'une colonne temps d'ecoute
    # passer le temps comme une variable continue (Heure:minute --> minutes)
      epoc3$Heure_debut <- time_to_decimal(epoc3$Heure_debut) 
      epoc3$Heure_fin <- time_to_decimal(epoc3$Heure_fin)
      
    # tps ecoute = tps fin - tps debut
      epoc3[,"Tps_ecoute"] <- abs(epoc3[,"Heure_fin"] - epoc3[,"Heure_debut"]) # valeur absolue 
      
  # selection des listes (prend uniquement 1 observation par liste == retrait des doublons)
      
      id.liste <- duplicated(epoc3$ID_liste)
      id.liste <- which(id.liste == FALSE)
      
      epoc.liste <- epoc3[id.liste,] # dtf contenant une ligne par liste
      
  # repartition du temps moyen
      j <- which(epoc.liste$Tps_ecoute > 600) # tps d'ecoute superieur a 10 heures (erreur dans la saisie de donnees ?)
      j <- length(j) # 466 observations avec des duree d'ecoute au-dela des 10 heures
      
  # Calcul
    # Tps d'ecoute moyen
      tps.mean <- mean(epoc.liste$Tps_ecoute) # 30.18826
      tps.sd <- sd(epoc.liste$Tps_ecoute)
      tps.var <- var(epoc.liste$Tps_ecoute)
      
    # Tps d'ecoute total
      tps.tot <- sum(epoc.liste$Tps_ecoute)
      tps.tot.heur <- tps.tot/60
      tps.tot.jour <- (tps.tot/60)/24
      tps.tot.week <- ((tps.tot/60)/24)/7
      tps.tot.ans <- (((tps.tot/60)/24)/7)/52
      
# Repartition selon ans 2017/2018/2019 -----
  # 2017 ----
    dtf_2017 <- epoc.liste[epoc.liste$Annee == 2017,]
    obs_2017 <- epoc[epoc$Annee == 2017,]
      # Nb listes
        liste_2017 <- nrow(dtf_2017)
      # Tps d'ecoute
        # Tps d'ecoute moyen
            tps.mean.2017 <- mean(dtf_2017$Tps_ecoute) # 30.18826
            tps.sd.2017 <- sd(dtf_2017$Tps_ecoute)
            tps.var.2017 <- var(dtf_2017$Tps_ecoute)
            
          # Tps d'ecoute total
            tps.tot.2017 <- sum(dtf_2017$Tps_ecoute)
            tps.tot.heur.2017 <- tps.tot.2017/60
            tps.tot.jour.2017 <- (tps.tot.2017/60)/24
            tps.tot.week.2017 <- ((tps.tot.2017/60)/24)/7
            tps.tot.ans.2017 <- (((tps.tot.2017/60)/24)/7)/52 
            
    # 2018 ----
      dtf_2018 <- epoc.liste[epoc.liste$Annee == 2018,]
      obs_2018 <- epoc[epoc$Annee == 2018,]
        # Nb listes
          liste_2018 <- nrow(dtf_2018)
        # Tps d'ecoute
          # Tps d'ecoute moyen
            tps.mean.2018 <- mean(dtf_2018$Tps_ecoute) # 30.18826
            tps.sd.2018 <- sd(dtf_2018$Tps_ecoute)
            tps.var.2018 <- var(dtf_2018$Tps_ecoute)
            
          # Tps d'ecoute total
            tps.tot.2018 <- sum(dtf_2018$Tps_ecoute)
            tps.tot.heur.2018 <- tps.tot.2018/60
            tps.tot.jour.2018 <- (tps.tot.2018/60)/24
            tps.tot.week.2018 <- ((tps.tot.2018/60)/24)/7
            tps.tot.ans.2018 <- (((tps.tot.2018/60)/24)/7)/52 
      
      # 2019 ----
        dtf_2019 <- epoc.liste[epoc.liste$Annee == 2019,]
        obs_2019 <- epoc[epoc$Annee == 2019,]
          # Nb listes
            liste_2019 <- nrow(dtf_2019)
          # Tps d'ecoute
            # Tps d'ecoute moyen
            tps.mean.2019 <- mean(dtf_2019$Tps_ecoute) # 30.18826
            tps.sd.2019 <- sd(dtf_2019$Tps_ecoute)
            tps.var.2019 <- var(dtf_2019$Tps_ecoute)
            
            # Tps d'ecoute total
            tps.tot.2019 <- sum(dtf_2019$Tps_ecoute)
            tps.tot.heur.2019 <- tps.tot.2019/60
            tps.tot.jour.2019 <- (tps.tot.2019/60)/24
            tps.tot.week.2019 <- ((tps.tot.2019/60)/24)/7
            tps.tot.ans.2019 <- (((tps.tot.2019/60)/24)/7)/52    
      
      
      
            
# Plot des resultats      
      cat("Informations sur les données traités :\n -\tNombre d'observations :",nrow(epoc.filt2),
          "\n -\tNombre de listes :",nrow(epoc.liste))
      
      cat("L'effort d'échantillonnage de tous les protocoles de la base de données faune-france (en dehors des STOC, SHOC et WATERBIRDS) sur la période 2017-2019 est de :\n",
          round(tps.tot,2)," minutes\n soit :",round(tps.tot.heur,2)," heures\n soit :",round(tps.tot.jour,2)," jours\n soit :",
          round(tps.tot.week,2)," semaines\n soit :",round(tps.tot.ans,2),"ans")
      cat("Temps d'écoute moyen par liste :",round(tps.mean,2))
      
      cat("Pour l'année 2017, nous avons :\n -\t",nrow(obs_2017)," Observations\n -\t",
          liste_2017," Listes réalisées\n -\t",round(tps.mean.2017)," minutes de temps d'écoute moyen\n -\t",
          tps.tot.2017," minutes de temps d'écoute total soit :\n \t-\t",round(tps.tot.heur.2017,2)," Heures\n \t-\t",
          tps.tot.jour.2017," Jours\n \t-\t",tps.tot.week.2017," Semaines\n \t-\t",tps.tot.ans.2017," Années")
      
      cat("Pour l'année 2018, nous avons :\n -\t",nrow(obs_2018)," Observations\n -\t",
          liste_2018," Listes réalisées\n -\t",round(tps.mean.2018)," minutes de temps d'écoute moyen\n -\t",
          tps.tot.2018," minutes de temps d'écoute total soit :\n \t-\t",round(tps.tot.heur.2018,2)," Heures\n \t-\t",
          tps.tot.jour.2018," Jours\n \t-\t",tps.tot.week.2018," Semaines\n \t-\t",tps.tot.ans.2018," Années")
      
      cat("Pour l'année 2019, nous avons :\n -\t",nrow(obs_2019)," Observations\n -\t",
          liste_2019," Listes réalisées\n -\t",round(tps.mean.2019)," minutes de temps d'écoute moyen\n -\t",
          tps.tot.2019," minutes de temps d'écoute total soit :\n \t-\t",round(tps.tot.heur.2019,2)," Heures\n \t-\t",
          tps.tot.jour.2019," Jours\n \t-\t",tps.tot.week.2019," Semaines\n \t-\t",tps.tot.ans.2019," Années")
      
      
  # boxplot des tps d'ecoute selon les mois/annees
      epoc3 %>%
        mutate(Mois2 = as.Date(paste0("2015-", Mois,"-01"),"%Y-%m-%d")) %>%
        ggplot(aes(x = Mois2, y = Tps_ecoute)) +
        geom_boxplot(aes(group=Mois), fill = "darkolivegreen3",outlier.shape = NA) +
        scale_y_continuous(limits = quantile(epoc.liste$Tps_ecoute, c(0.1,0.90)))  +
        facet_wrap(~ Annee, ncol = 3) +
        labs(title = "Repartition annuel du temps d'écoute d'observations",
             subtitle = "Listes completes faune-france",
             y = "Temps d'ecoute",
             x = "Mois") + theme_bw(base_size = 15) +
        scale_x_date(date_labels = "%b")
  
  # plot variation tps d'ecoute selon le temps
      # preparation data
        sum.data <- matrix(c(2017,2018,2019,))













