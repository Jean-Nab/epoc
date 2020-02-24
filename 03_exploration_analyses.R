# chemin
setwd("C:/git/epoc/data")

# packages
  library(reshape2)
  library(stringr)
  #library(data.table)

# import des data
  # tableau court dans la periode 1/03 - 31/06
      epoc.court.in <- read.table(file = paste0(sub("/data","/output",getwd()),"/epoc_filtre_7_court_in_period.txt"),header=T,sep="\t", dec=","
                                  , encoding="UTF-8",quote="")
  
      
      
      
      
      
# Exploration ------
      # Table des especes apercu par observateur
          dtf.esp_obs <- epoc.court.in[,c("Observateur","Nb_observateur","Nom_espece")]
          dtf.esp_obs$Presence <- c(rep(1,nrow(dtf.esp_obs)))
          dtf.esp_obs$ID_sample <- c(rep(1:nrow(dtf.esp_obs)))
          
      # conversion du tableau presence --> presence/absence d'obs par observateur
          dtf1 <- dcast(dtf.esp_obs, Observateur ~ Nom_espece) # donne une table d'abondance
          
          dtf1[dtf1>0] <- 1 # conversion en table de presence/absence
          dtf1$ID_sample <- c(rep(1:nrow(dtf1))) # ajout d'une colonne ID
            
      # conversion format court --> format long
            dtf2 <- melt(dtf1, id.vars = "Observateur",
                         variable.name = "Nom_espece",
                         value.name = "Presence")
            
      # perte du nb d'obs --> besoin de le recalculer
            dtf2$Nb_observateur <- 1 + str_count(as.character(gsub(",| et ","\\+",dtf2$Observateur)), pattern="\\+")
          
      # aggregation des donnees
            dtf3 <- aggregate(Presence ~ Nb_observateur + Nom_espece, data=dtf2, FUN = quantile,c(0.025,0.5,0.975))
            dtf3.obs_sp <- aggregate(Presence ~ Observateur + Nom_espece, data=dtf2, FUN = quantile,c(0.025,0.5,0.975))
            
      # gestion du dtf de la fonction aggregate
            dtf4 <- data.frame(dtf3[,1:ncol(dtf3)],dtf3[,ncol(dtf3)])
            colnames(dtf4)[4:6] <- c("Borne_inf","Mediane","Borne_sup")
            
# testing ground (not run) -----
 
  
  
  
  
  
  
  
  
  
  
  