

# input : reg1.tmp (cf 04bis_qualification_by_ecoregions.R) ==> dtf des listes et de leur presence/absence dans l'écorégion de la boucle
# output : dtf champion / dtf des flags
determination_communs_by_regions <- function(dtf.reg1){
  
  library(data.table)
  
  id.list.reg <- reg1.tmp[reg1.tmp[2] == 1,"ID_liste"] # detection des id de liste detectee dans cette region  
  
  # recuperation des observation liee aux id de liste ----
    det.list.byreg.epoc_oiso <- epoc.oiso$ID_liste %in% id.list.reg # detection des lignes
    epoc.oiso.region <- epoc.oiso[which(det.list.byreg.epoc_oiso == TRUE),] # formation du dtf associee aux lignes detectee
  
  # formation du dtf permettant de detecter les champions de la region ----
    list.byregion <- aggregate(diversite ~ Observateur + ID_liste,
                           data = epoc.oiso.region,
                           FUN = sum)
    
    list.byregion$nb_liste <- 1
    
    observateur.byregion <- aggregate(nb_liste ~ Observateur, data=list.byregion,sum)
    observateur.byregion$part_liste_in_region <- observateur.byregion$nb_liste / sum(observateur.byregion$nb_liste)
  
  # selection des champions (+ de 30 EPOCs des plus fortes participation)
    select.champ <- as.vector(observateur.byregion[which(observateur.byregion$nb_liste >= 30),"Observateur"])
    
    
    
    
  
  # determination des especes communes a partir des champions -----
    det.champ.epoc.oiso.region <- epoc.oiso.region$Observateur %in% select.champ # detection des observations realiser par les champions
    
    champ.oiso.region <- epoc.oiso.region[which(det.champ.epoc.oiso.region == TRUE),] # formation du dtf d'observation des champions
    champ.oiso.region <- plyr::count(df=champ.oiso.region,
                                     vars=c("Nom_espece","Nom_latin"))
    
    # obtention du nombre d'epoc realiser par les champions dans cette region
      nb.list.champ <- sum(observateur.byregion[which(observateur.byregion$nb_liste >= 30),"nb_liste"])
  
    # Determination de l'etat commun / rare d'une espece
      champ.oiso.region$part_total_in_list_byregion <- champ.oiso.region$freq / nb.list.champ # calcul de la part d'observation de l'espece dans toute les listes "champions"
      
      det.oiso.comm.byregion <- as.numeric(quantile(champ.oiso.region$part_total_in_list_byregion,probs=0.85))
      
      champ.oiso.region$communs <- 0
      champ.oiso.region[which(champ.oiso.region$part_total_in_list_byregion >= det.oiso.comm.byregion),"communs"] <- 1
      
      champ.oiso.region.communs <- as.vector(champ.oiso.region[champ.oiso.region$communs == 1,"Nom_espece"]) # vecteur regroupant les noms d'especes communes
    
      
      
      
      
    
  # Application du flag especes communes sur toutes les observations de la regions (epoc.oiso.region) -----
    det.obs.oiso.communs.byregion <- epoc.oiso.region$Nom_espece %in% champ.oiso.region.communs
      
    epoc.oiso.region$communs <- 0 # Vs cas ou une espece n'est pas detecte par les champions (=> ici, on la considere comme rare)
    
    epoc.oiso.region[which(det.obs.oiso.communs.byregion == TRUE),"communs"] <- 1 # flag des oiseaux consideres comme communs par les champions
    
    
    
    
    
  # PREPARATIF du flagging des listes (= calcul part d'espece communes / presence d'au moins une espece communes) -----
    epoc.oiso.region$communs_logical <- as.logical(epoc.oiso.region$communs) # preparatif pour l'aggregate en any
    
    # flag de la part d'espece communes
      flag.prep.part.comm <- aggregate(communs ~ ID_liste, data = epoc.oiso.region, sum)
      colnames(flag.prep.part.comm) <- c("ID_liste","nb_communs")

    # flag presence d'au moins une espece commune dans la liste
      flag.prep.least_1_comm <- aggregate(communs_logical ~ ID_liste, data = epoc.oiso.region, any)
      colnames(flag.prep.least_1_comm) <- c("ID_liste","least_1_communs")
      
  # join au dtf (list.byregion + flagging des listes)
      list.byregion <- plyr::join(list.byregion,flag.prep.part.comm,by="ID_liste")
      list.byregion$part_communs <- list.byregion$nb_communs / list.byregion$diversite
      
      list.byregion <- plyr::join(list.byregion,flag.prep.least_1_comm,by="ID_liste")
      
      
      
      
      
  # FLAGGING -----
    # Flag many_rare (Liste a forte diversite [>4 especes] avec que des especes rares)
      list.byregion$flag_many_rare <- 0
      
      id.list.rare <- which(list.byregion$diversite >= 4 & list.byregion$least_1_communs == FALSE)
      list.byregion[id.list.rare,"flag_many_rare"] <- 1
      
    # Flag only_rare_low_dic (Liste de faible diversite [< 4 especes] avec que des especes rares)
      list.byregion$flag_only_rare_low_div <- 0
    
      id.list.rare.low <- which(list.byregion$diversite < 4 & list.byregion$least_1_communs == FALSE)
      list.byregion[id.list.rare.low,"flag_only_rare_low_div"] <- 1
      
    # Flag scarce_communs (part d'especes communes moins importante que l'attendue)
      list.byregion$flag_scarce_commun <- 0
      
      comp.communs <- plyr::join(list.byregion[,c("ID_liste","diversite","part_communs")],tab.qt.global.part,by="diversite")
      list.emp_th <- comp.communs[which(comp.communs$part_communs <= comp.communs$borne_inf),"ID_liste"]
      
      id.list.less.comm <- list.byregion$ID_liste %in% list.emp_th
      list.byregion[which(id.list.less.comm == TRUE),"flag_scarce_commun"] <- 1
      
    # Flag premiere espece rencontree
      # ajout des categories <=> evaluation de jérémy
        epoc.oiso.region <- plyr::join(epoc.oiso.region,cate.esp2,by="Nom_espece") # join des categories de rarete experte selon le nom d'espece
        epoc.oiso.region[which(is.na(epoc.oiso.region$Decision)),"Decision"] <- 1 # cas ou une espece ne serait pas identifie (-> presummer rare)
        
        # incrementation -> detection de la 1ere observation de chaque liste
        epoc.oiso.cate_dt <- data.table(epoc.oiso.region)
        epoc.oiso.cate_dt <- epoc.oiso.cate_dt[, group_increment := 1:.N, by = "ID_liste"]
        epoc.oiso.region <- as.data.frame(epoc.oiso.cate_dt)
      
      # selection de la 1ere observation de chaque liste
        first.obs <- which(epoc.oiso.region$group_increment == 1)
        list.oiso.cate.byregion <- epoc.oiso.region[first.obs,c("ID_liste","Observateur","Nom_espece","Decision","group_increment")]   
      
      # add flag communs/rare d'une espece
        det.list.oiso.cate.communs.byregion <- list.oiso.cate.byregion$Nom_espece %in% champ.oiso.region.communs
        
        list.oiso.cate.byregion$communs <- 0
        list.oiso.cate.byregion[which(det.list.oiso.cate.communs.byregion == TRUE),"communs"] <- 1
        
        list.oiso.cate.byregion$flag_first_obs_unusual <- 0
        list.oiso.cate.byregion[list.oiso.cate.byregion$Decision == 1 | list.oiso.cate.byregion$Decision == 2,"flag_first_obs_unusual"] <- 1
        list.oiso.cate.byregion[list.oiso.cate.byregion$Decision == 3 & list.oiso.cate.byregion$communs == 0
                                ,"flag_first_obs_unusual"] <- 1
        
        list.byregion <- plyr::join(list.byregion,list.oiso.cate.byregion[,c("ID_liste","flag_first_obs_unusual")],by="ID_liste")
        
      
  # Regroupement des flags (fait par listes) sur les observateurs -----
    observateur.flag.many.byregion <- aggregate(flag_many_rare ~ Observateur,
                                           data = list.byregion,
                                           FUN = sum)
        
    observateur.flag.low.byregion <- aggregate(flag_only_rare_low_div ~ Observateur,
                                                data = list.byregion,
                                                FUN = sum)
    
    observateur.flag.scarce.byregion <- aggregate(flag_scarce_commun ~ Observateur,
                                                data = list.byregion,
                                                FUN = sum)
    
    observateur.flag.first.byregion <- aggregate(flag_first_obs_unusual ~ Observateur,
                                                 data = list.oiso.cate.byregion,
                                                 FUN = sum)
    
    # ajout de ces informations sur le dtf des observateurs
      observateur.byregion <- plyr::join(observateur.byregion,observateur.flag.many.byregion,by="Observateur")
      observateur.byregion <- plyr::join(observateur.byregion,observateur.flag.low.byregion,by="Observateur")
      observateur.byregion <- plyr::join(observateur.byregion,observateur.flag.scarce.byregion,by="Observateur")
      observateur.byregion <- plyr::join(observateur.byregion,observateur.flag.first.byregion,by="Observateur")
      
      
      
      
      
  # Return des dtf dans l'environnement global ----
      
      # indexation selon la region (nouvelle colonne $regions dans chaque dtf de sortie de fonction)
      
      observateur.byregion$regions <- colnames(reg1.tmp[2])
      list.byregion$regions <- colnames(reg1.tmp[2])
      champ.oiso.region$regions <- colnames(reg1.tmp[2])
      
      # enregistrement des variables dans l'environnement global
      var.reg <- paste0("observateur.reg.",abbreviate(colnames(reg1.tmp[2])))
      assign(x = var.reg, value = observateur.byregion,
             envir = globalenv())
      
      var.reg2 <- paste0("list.reg.",abbreviate(colnames(reg1.tmp[2])))
      assign(x = var.reg2, value = list.byregion,
             envir = globalenv())
      
      var.reg3 <- paste0("oiso.reg.",abbreviate(colnames(reg1.tmp[2])))
      assign(x = var.reg3, value = champ.oiso.region[,c("Nom_espece","Nom_latin","freq","communs","regions")],
             envir = globalenv())
    

    
    
}












































































