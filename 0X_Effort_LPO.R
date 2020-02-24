# chemin
  setwd("C:/git/epoc/data")

library(lubridate)
  library(ggplot2)
  library(dplyr)


epoc <- read.table(file = paste0(sub("/data","/output",getwd()),"/export_2017_2019.txt"),header=T,sep="\t", dec=","
                   , encoding="UTF-8",quote="")

# function to convert time observation to hours since midnight
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

# Prerequis pour la repartition mensuelle ----
  # manipulation mois -> distinction selon les annees (= Rajout d'un suffixe indiquant l'annee)
      epoc.filt2$Mois <- gsub("10","Oct",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("11","Nov",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("12","Dec",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("1","Jan",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("2","Fev",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("3","Mar",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("4","Avr",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("5","Mai",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("6","Juin",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("7","Juil",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("8","Aout",epoc.filt2$Mois)
      epoc.filt2$Mois <- gsub("9","Sep",epoc.filt2$Mois)


      # 2017
        u <- which(epoc.filt2$Annee == 2017)
        epoc.filt2[u,"Mois"] <- gsub("([a-zA-Z]{1,})","\\1_2017",epoc.filt2[u,"Mois"])
  
        
      # 2018
        u <- which(epoc.filt2$Annee == 2018)
        epoc.filt2[u,"Mois"] <- gsub("([a-zA-Z]{1,})","\\1_2018",epoc.filt2[u,"Mois"])
        
      
      # 2019
        u <- which(epoc.filt2$Annee == 2019)
        epoc.filt2[u,"Mois"] <- gsub("([a-zA-Z]{1,})","\\1_2019",epoc.filt2[u,"Mois"])
   


# Verification
  y <- duplicated(epoc.filt2$Mois)
  y2 <- which(y == FALSE)
  head(epoc.filt2[y2,"Mois"],36)


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
      
      epoc.liste <- epoc3[id.liste,]
      
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
      
      cat("L'effort d'échantillonnage de tous les protocoles de la base de données faune-france (en dehors des STOC, SHOC et WATERBIRDS est de :\n",
          round(tps.tot,2)," minutes\n soit :",round(tps.tot.heur,2)," heures\n soit :",round(tps.tot.jour,2)," jours\n soit :",
          round(tps.tot.week,2)," semaines\n soit :",round(tps.tot.ans,2),"ans")
  
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
  
  
  
  
  
  













