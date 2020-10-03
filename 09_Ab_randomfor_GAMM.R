# chemin
  setwd("C:/git/epoc/data")

# packages
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(scam)
  library(sf)
  
  library(tibble)
  library(stringr)
  
  #library(randomForest)
  library(ranger)
  library(pdp)
  library(caret)
  #library(mgcv)
  library(MASS)
  library(PresenceAbsence)
  library(dggridR)


# import data -----
  epoc.envi.obs <- read.csv(file = paste0(sub("/data","/DS.v2",getwd()),"/epoc_environnement_observation_DS.csv"))  
  bary.list <- read.csv("C:/git/epoc/DS.v2/epoc_barycentre_liste_density_add.csv")
  tabl.commu <- read.csv("C:/git/epoc/DS.v2/epoc_table_communaute_PA_DS.csv")
  epoc.oiso <- read.csv(file = paste0(sub("/data","/DS.v2",getwd()),"/epoc_communaute_DS.csv"))
  list.all.var <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_Coordonnee_listes_EPOC_cleaned_v2.1.csv")
  tabl.pheno <- read.csv2(file = paste0(sub("/data","/DS.v2",getwd()),"/Especes_communes_phenologies.csv"))
  grid.predict <- read.csv("C:/git/epoc/data/Donnees_Yves/GI_SysGrid__3e+05_cleaned_v2.1.csv")
  


# formation du dataset pour randomForest (data.rF.sp) -----
  # choix espece
   sp <- "Fringilla coelebs"
  
  # selection pres/abs de l'espece 
    tabl.PA.sp <- tabl.commu[,c(1,grep(pattern = gsub(" ",".",sp),colnames(tabl.commu)))]
    colnames(tabl.PA.sp)[2] <- "sp_observee"
    
  # formation du dataset
    data.rF.sp <- left_join(tabl.PA.sp,bary.list[,c("ID_liste","X_barycentre_L93","Y_barycentre_L93")])
    
    data.rF.sp <- left_join(data.rF.sp,epoc.oiso[epoc.oiso$Nom_latin == sp,c("ID_liste","Abondance")])
    data.rF.sp[which(is.na(data.rF.sp$Abondance)),"Abondance"] <- 0
    
    data.rF.sp <- left_join(data.rF.sp,unique(epoc.envi.obs[,c("ID_liste","Heure_de_debut","Tps_ecoute","Jour_de_l_annee","Annee")]))
    data.rF.sp <- data.rF.sp[which(duplicated(data.rF.sp$ID_liste) == FALSE),]
  
    #data.rF.sp <- left_join(data.rF.sp,var.envi.liste[,c(-1,-2)])
  
    data.rF.sp <- left_join(data.rF.sp,bary.list[,c("ID_liste","densite","Jour","Mois")])
  
  # ecremage des donnees d'abondances 
    ab.max <- as.numeric(quantile(data.rF.sp[data.rF.sp$Abondance != 0,"Abondance"],0.98)) # Abondance max relevée en excluant 2% de données extreme
    
    data.rF.sp$Ab_ecretee <- data.rF.sp$Abondance
    data.rF.sp[which(data.rF.sp$Ab_ecretee > ab.max),"Ab_ecretee"] <- ab.max
    
  # ajout variables environnementales (Yves) ----
    data.rF.sp <- right_join(data.rF.sp,list.all.var,by= "ID_liste")
    
    data.rF.sp <- data.rF.sp[-which(is.na(data.rF.sp$sp_observee)),]
    
    # nettoyage
      data.rF.sp[,grep("[.]y",colnames(data.rF.sp))] <- NULL
      colnames(data.rF.sp) <- gsub("[.]x","",colnames(data.rF.sp))
   
  # filtrage phenologiques ------
    # recup des dates de jour/fin selon la table phenologique (tabl.pheno) ----
      jD <- tabl.pheno[tabl.pheno$Nom_latin == sp,"debut_jour"]
      mD <- tabl.pheno[tabl.pheno$Nom_latin == sp,"debut_mois"]
      
      jF <- tabl.pheno[tabl.pheno$Nom_latin == sp,"fin_jour"]
      mF <- tabl.pheno[tabl.pheno$Nom_latin == sp,"fin_mois"]
    
    # conversion en jour de lannee
      dateD <- paste(c(jD,mD),collapse = "/")
      dateD <- as.Date(dateD,format = "%d/%m")
      dateD <- strftime(dateD,format = "%j")
      
      dateF <- paste(c(jF,mF),collapse = "/")
      dateF <- as.Date(dateF,format = "%d/%m")
      dateF <- strftime(dateF,format = "%j")
    
    # filtrage du jeu de donnees -----
      data.rF.sp2 <- data.rF.sp[data.rF.sp$Jour_de_l_annee %in% c(dateD:dateF),]


# Preparation jeu de données (traitement de sous-echantillonnement) -----
  # split du jeu de donnees entre train et test (-> choix aleatoire) ----
    data.rF.sp_split <- data.rF.sp2  
      
    data.rF.sp_split <- data.rF.sp_split %>%
      split(if_else(runif(nrow(.)) <= 0.8, "train","test"))
    
  # calcul frequence de detection de l'espece sp dans la partie train du jeu de données --> selection plus homogene dans la formation des arbres
    freq.detect <- mean(data.rF.sp_split$train$sp_observee)
  
    
    
  # Sous-echantillonnement spatial ------
    dggs <- dgconstruct(spacing = 1) # formation d'une grille hexagonale avec un espacement de ~ 1km
    
    data_cell <- data.rF.sp2 %>%
      mutate(cell = dgGEO_to_SEQNUM(dggs, Lon_WGS84_bary, Lat_WGS84_bary)$seqnum) # assignation d'un identifiant d'hexagone selon la position des listes
    
    data_sample <- data_cell %>%     # Tirage aléatoire des listes (1 liste par cellule) / presence et absence traite de facon independantes
      group_by(sp_observee,cell) %>% 
      sample_n(size = 1) %>% 
      ungroup() %>%
      dplyr::select(-cell)
    
    # gestion du jeu de donnees a posteriori du sous-echantillonnement spatial ------
      # detection et suppression de l'habitat CLC majoritaire -----
        max.hab <- colSums(data_sample[,grep("CLCM",colnames(data_sample))])
        max.hab <- names(which.max(max.hab))
        
        data_sample[,grep(max.hab,colnames(data_sample))] <- NULL
    
    
    
  # Division du jeu de data en train et test (pour GAM) -----
    # splitting de la data en train/test 
      data_split <- data_sample %>%
          split(if_else(runif(nrow(.)) <= 0.8,"train","test"))
    freq.detect.post.subsampl <- mean(data_split$train$sp_observee) # frequence de detection des especes apres sous-echantillonnement
    
    
  # formation du la formule du modele (-> retrait de prise en compte des pres/abs) -----
    det.CLC <- grep("CLC",colnames(data_split$train))
    
    var.model <- data_split$train %>%
      dplyr::select(-sp_observee,-Abondance,-densite,-Ab_ecretee,-Jour,
                    -Mois,-ID_liste,-all_of(det.CLC),-Annee,-starts_with("BioClim_ACP")) %>%
      names()
    
    formul.model <- str_glue("{var}", var = var.model) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("sp_observee"," ~ ", .) %>%
      as.formula()

# modele randomForest -----
  rf.1_0 <- ranger(formula = formul.model,
                   data = data_split$train,
                   importance = "impurity",
                   probability = TRUE,
                   replace = TRUE,
                   #sample.fraction = freq.detect,
                   case.weights = 1/sqrt(data_split$train$densite),
                   #classification = T,
                   #mtry = 3,
                   num.trees = 4000)


# Calibration du modele -----
  # formation de la table de comparaison observees/predites
    occ.pred <- rf.1_0$predictions[,grep("1",colnames(rf.1_0$predictions))]
    occ.obs <- data_split$train$sp_observee
    
    rf.1_0.pred.train <- tibble(obs= occ.obs, pred=occ.pred) %>%
      drop_na()
  
  # formation d'un GAM comparant donnees observe vs donnees predites
    #calibration.model <- scam(obs ~ s(pred, k=5),
    #                        gamma=1.4,
    #                        data=rf.1_0.pred.train)
    
    calibration.model <- mgcv::gam(obs ~ s(pred),
                             family = nb,
                             data=rf.1_0.pred.train) # ; gam.check(calibration.model)

  # table des categories d'abondances estimee
    average.encounter <- rf.1_0.pred.train %>%
      mutate(pred_cat = cut(rf.1_0.pred.train$pred, breaks = seq(0,1, by=0.02))) %>%
      group_by(pred_cat) %>%
      summarise(pred = mean(pred), obs = mean(obs), Nb_liste = n()) %>%
      ungroup()

  # visualisation Calibaration modele -----
    cal.pred <- tibble(pred = seq(0,1, length.out = 100))
    cal.pred <- predict(calibration.model, cal.pred, type = "response") %>%
      bind_cols(cal.pred, calibrated = .)
    
    plot.cali.mod.1_0 <- ggplot(cal.pred) +
      aes(x = pred, y =calibrated) +
      geom_line() +
      geom_point(data = average.encounter,
                 aes( x= pred, y= obs, size = sqrt(Nb_liste)),
                 show.legend = FALSE, shape= 1) +
      geom_abline(slope = 1,intercept = 0,colour="grey",linetype="dashed") +
      labs(x = "Taux de rencontre estimé",
           y = "Taux de rencontre observé (moyenne)",
           title = paste("Calibration model {1/0 ~ .} :", sp,collapse = " "))

# Evaluation du modele ----
  # Preparatifs -----
  p_fitted <- predict(rf.1_0, data= data_split$test, type="response")
  p_fitted <- p_fitted$predictions[,2]
  
  # calibration des data test predites 
    p_calibre <- predict(calibration.model,
                         newdata = tibble(pred = p_fitted),
                         type = "response")
    
    rf.1_0.pred.test <- data.frame(id = seq_along(p_calibre),
                                   obs = data_split$test$sp_observee,
                                   pred_non_calibre = p_fitted,
                                   pred_calibre = p_calibre) %>%
      mutate(pred_calibre = pmin(pmax(pred_calibre,0),1)) %>%
      drop_na()
  
  
  # MSE (Mean Squared Error) - Erreur quadratique moyenne [estimation precision modele] -----
    mse_fit <- mean((rf.1_0.pred.test$obs - rf.1_0.pred.test$pred_non_calibre)^2, na.rm = TRUE)
    mse_cal <- mean((rf.1_0.pred.test$obs - rf.1_0.pred.test$pred_calibre)^2, na.rm = TRUE)
    
  
  # Indices de precision (AUC, Kappa, Sensibilité, Specificité) -----
    opt_thresh <- optimal.thresholds(rf.1_0.pred.test, opt.methods = "MaxKappa") # Seuil de determination 0.53 -> 1 & 0.5295 -> 0
  
  # modele non calibre (fit du rF)
    indics.fit <- rf.1_0.pred.test %>% 
      dplyr::select(id, obs, pred_non_calibre) %>% 
      presence.absence.accuracy(threshold = opt_thresh$pred_non_calibre, 
                                na.rm = TRUE, 
                                st.dev = FALSE)
  
  # modele calibre (GAM des resultats du rF)
    indics.cal <- rf.1_0.pred.test %>% 
      dplyr::select(id, obs, pred_calibre) %>% 
      presence.absence.accuracy(threshold = opt_thresh$pred_calibre, 
                                na.rm = TRUE, 
                                st.dev = FALSE)
  
  # Table de résumé ----
    rf_assessment <- tibble(
      model = c("RF", "Calibrated RF"),
      mse = c(mse_fit, mse_cal),
      sensitivity = c(indics.fit$sensitivity, indics.cal$sensitivity),
      specificity = c(indics.fit$specificity, indics.cal$specificity),
      auc = c(indics.fit$AUC, indics.cal$AUC),
      kappa = c(indics.fit$Kappa, indics.cal$Kappa))
    
    knitr::kable(rf_assessment, digits = 3)


# Prediction ------
  # pre-requis de detection des valeurs opti -----
    calculate_pd <- function(predictor, model, data, 
                             x_res = 25, n = round(nrow(data_split$train)/4)) {
      # create prediction grid
      rng <- range(data[[predictor]], na.rm = TRUE)
      x_grid <- seq(rng[1], rng[2], length.out = x_res)
      grid <- data.frame(covariate = predictor, x = x_grid, 
                         stringsAsFactors = FALSE)
      names(grid) <- c("covariate", predictor)
      
      # subsample training data
      n <- min(n, nrow(data))
      s <- sample(seq.int(nrow(data)), size = n, replace = FALSE)
      data <- data[s, ]
      
      # drop focal predictor from data
      data <- data[names(data) != predictor]
      grid <- merge(grid, data, all = TRUE)
      
      # predict
      p <- predict(model, data = grid)
      
      # summarize
      pd <- grid[, c("covariate", predictor)]
      names(pd) <- c("covariate", "x")
      pd$pred <- p$predictions
      pd <- dplyr::group_by(pd, covariate, x) %>% 
        dplyr::summarise(pred = mean(pred, na.rm = TRUE)) %>% 
        dplyr::ungroup()
      
      return(pd)
    }
  
  
  
  # detection de la date/jour de l'annee optimal ----
    dp_date <- calculate_pd("Jour_de_l_annee",
                            model = rf.1_0,
                            data = data_split$train,
                            x_res = 6*30, n = round(nrow(data_split$train)/4)) %>% 
      transmute(Jour_de_l_annee = x, taux_rencontre = pred)
    
    date.opti <- dp_date %>% 
      top_n(1, wt = taux_rencontre) %>%
      pull(Jour_de_l_annee)
    date.opti <- round(mean(date.opti))
    
    dp_heure <- calculate_pd("Heure_de_debut",
                             model = rf.1_0,
                             data = data_split$train,
                             x_res = 2*(17-5), n = round(nrow(data_split$train)/4)) %>% 
      transmute(Heure_de_debut = x, taux_rencontre = pred)
    
    heure.opti <- dp_heure %>% 
      top_n(1, wt = taux_rencontre) %>%
      pull(Heure_de_debut)
    heure.opti <- round(mean(heure.opti))


  # formation de la grille de prediction ----
    grid.predict.sp <- grid.predict
    grid.predict.sp$Jour_de_l_annee <- date.opti
    grid.predict.sp$Heure_de_debut <- heure.opti
    grid.predict.sp$Tps_ecoute <- 0.05


# Prediction ----
  grid.predict.sp$id_prediction <- c(rep(1:nrow(grid.predict.sp)))
  
  pred.model <- predict(rf.1_0, grid.predict.sp, type="response")
  grid.predict.sp$Prob_presence <- pred.model$predictions[,2]
  
  grid.predict.sp[which(grid.predict.sp$Prob_presence >= 0.9),"Abondance"] <- 1
  

# Visualisation ------
  grid.predict.sp_sf <- st_as_sf(grid.predict.sp, coords = c("Lon_WGS84_bary","Lat_WGS84_bary"),crs=4326)

# Proba : Presence/Absence
  ggplot() + 
    geom_sf(data = grid.predict.sp_sf, aes(colour= Prob_presence)) +
    scale_colour_viridis_c(option = "B") +
    ggtitle(sp)


# Modelisation de l'abondance w/ GAM avec info de prediction de la presence/absence -----
  # sous-echantillonnement spatial plus ferme -----
    dggs <- dgconstruct(spacing = 10) # formation d'une grille hexagonale avec un espacement de ~ 10km
    
    data_cell.GAM <- data_sample %>%
      mutate(cell = dgGEO_to_SEQNUM(dggs, Lon_WGS84_bary, Lat_WGS84_bary)$seqnum) # assignation d'un identifiant d'hexagone selon la position des listes
    
    data_sample.GAM <- data_cell.GAM %>%     # Tirage aléatoire des listes (1 liste par cellule) / presence et absence traite de facon independantes
      group_by(sp_observee,cell) %>% 
      sample_n(size = 1) %>% 
      ungroup() %>%
      dplyr::select(-cell)
    
    data_sample.GAM$Annee <- as.factor(data_sample.GAM$Annee)

  # Division du jeu de data en train et test (pour GAM) -----
    # splitting de la data en train/test 
    data_split.GAM <- data_sample.GAM %>%
      split(if_else(runif(nrow(.)) <= 0.8,"train","test"))
    freq.detect.post.subsampl.GAM <- mean(data_split.GAM$train$sp_observee) 
  
  
  # Modele GAM ------
    data_split$train$Annee <- as.factor(data_split$train$Annee) 
    
  # Formation des formules de modeles (<=> OSO + bio-alti / CLC + bio-alti / OSO + CLC + bio-alti) -----
    # AVANT : Selection des variables environnementales pour le GAM -> 3 modeles [Bioclim + OSO / Bioclim + CLC / Bioclim + OSO + CLC] -----
      # variables bioclimatiques  
        var.bio <- colnames(data_sample)[grep("BioClim_ACP",colnames(data_sample))]
  
      # variables environnementales OSO
        var.hab.OSO <- colnames(data_sample)[grep("HO",colnames(data_sample))]
        var.hab.OSO <- var.hab.OSO[-grep("M",var.hab.OSO)] # retrait des buffers de 5000m dans les variables du GAM
        var.hab.OSO <- var.hab.OSO[-grep("HO3S",var.hab.OSO)]
        
      # variables environnementales CLC niveau 2
        var.hab.CLC <- colnames(data_sample)[grep("CLCM",colnames(data_sample))]
        #var.hab.CLC <- var.hab.CLC[grep("Batis|Terres|Forets|Surfaces",var.hab.CLC)]
        
        var.hab.CLC.TEST <- colnames(data_sample)[grep("CLCM",colnames(data_sample))]
        var.hab.CLC.TEST <- var.hab.CLC.TEST[grep("ouvert|hetero|Forets|arable",var.hab.CLC.TEST)]
        
      # variables annexees a la prise de mesure
        var.mesure <- c("Heure_de_debut","Jour_de_l_annee")
      
        
  # Exploration de la colinéarité des variables -------
    #corrplot::corrplot(cor(data_sample[,which(colnames(data_sample) %in% union(union(var.hab.CLC,var.bio),var.mesure) == TRUE)],method="spearman"),
      #                 method="number")
    #corrplot::corrplot(cor(data_sample[,which(colnames(data_sample) %in% var.hab.OSO == TRUE)],method="spearman"),
     #                 method="number")
  
  # parametre d'ondulation du GAM ----
    k <- -1
    k1 <- 20
  
  # formation des formules ----
    str.hab <- str_glue('s({var}, k={k1},bs="cr")',var=union(var.hab.CLC,var.bio),k1=k1) %>%
      str_flatten(collapse = " + ")
    
    str.loc <- str_glue('s(Lat_WGS84_bary,Lon_WGS84_bary, bs="sos") + Annee')
    
    
    formul.GAM.OSO <- str_glue("s({var}, k = {k})", 
                               var = union(var.hab.OSO,var.mesure), k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.) %>%
      as.formula()
    
    formul.GAM.CLC <- str_glue('s({var}, k = {k},bs="cr")', 
                               var = var.mesure, k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.," + ",str.hab) %>%
      str_glue(.," + ",str.loc) %>%
      as.formula()
    
    '
    formul.GAMM.CLC <- str_glue("s({var}, k = {k})", 
                               var = var.mesure, k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.," + ",str.hab, " + Annee") %>%
      as.formula()
    '
    
    
    str.hab.GLS <- str_glue("{var}",var=var.hab.CLC) %>%
      str_flatten(collapse = " + ")
    
    formul.GLS.CLC <- str_glue("{var}", 
                               var = var.mesure) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.," + ",str.hab.GLS, " + Annee") %>%
      as.formula()
    
    #formul.GAM.CLC.OSO <- str_glue("s({var}, k = {k})", 
     #                              var = union(union(union(var.hab.CLC,var.hab.OSO),var.bio),var.mesure), k = k) %>%
      #str_flatten(collapse = " + ") %>%
      #str_glue("Abondance ~ ",.) %>%
      #as.formula()
    
    formul.GAM.bio_mesure <- str_glue("s({var}, k = {k})", 
                                      var = union(var.bio,var.mesure), k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.) %>%
      as.formula()
    
    formul.GAM.bio <- str_glue("s({var}, k = {k})", 
                               var = var.bio, k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.) %>%
      as.formula()
    
    formul.GAM.mesure <- str_glue("s({var}, k = {k})", 
                                  var = var.mesure, k = k) %>%
      str_flatten(collapse = " + ") %>%
      str_glue("Abondance ~ ",.) %>%
      as.formula()
  
  
  # Modeles GAM (w/ differentes loi de distribution [Zero inflated-Poisson / Negative binomial / Tweedie?]) -----
    start_gam_nb <- Sys.time()
    mod.GAM.nb.mesure <- mgcv::gam(formul.GAM.mesure, 
                             data =data_split$train,
                             family= "nb",
                             method = "REML")
    end_gam_nb <- Sys.time() ; end_gam_nb - start_gam_nb
    
    
    start_gam_nb <- Sys.time()
    mod.GAM.nb.bio <- mgcv::gam(formul.GAM.bio, 
                      data =data_split$train,
                      family= "nb",
                      method = "REML")
    end_gam_nb <- Sys.time() ; end_gam_nb - start_gam_nb
    
    
    start_gam_nb <- Sys.time()
    mod.GAM.nb.bio_mesure <- mgcv::gam(formul.GAM.bio_mesure, 
                             data =data_split$train,
                             family= "nb",
                             method = "REML")
    end_gam_nb <- Sys.time() ; end_gam_nb - start_gam_nb
    
    
    start_gam_nb <- Sys.time()
    mod.GAM.nb <- mgcv::gam(formul.GAM.CLC, 
                      data =data_split$train,
                      family= "nb",
                      method = "REML",select=TRUE)#,
                      #correlation = corSpher(form = ~ Lon_WGS84_bary + Lat_WGS84_bary))
    end_gam_nb <- Sys.time() ; end_gam_nb - start_gam_nb
    
    '
    start_gam_nb <- Sys.time()
    mod.GAM.nb <- gamm(formul.GAMM.CLC, 
                      data =data_split.GAM$train,
                      family= "nb",
                      method = "REML",
    correlation = corSpher(form = ~ Lon_WGS84_bary + Lat_WGS84_bary))
    end_gam_nb <- Sys.time() ; end_gam_nb - start_gam_nb
    '
    
    
    '
    start <- Sys.time()
    m.GLS <- gls(formul.GLS.CLC, 
                 data =data_split.GAM$train,
                 correlation = corSpher(form = ~ Lon_WGS84_bary + Lat_WGS84_bary))
    end <- Sys.time() ; end - start
    
    variogram <- Variogram(m.GLS, form = ~ Lon_WGS84_bary + Lat_WGS84_bary)
    plot(variogram)
    '
    
    
    start_gam_tw <- Sys.time()
    mod.GAM.tw <- mgcv::gam(formul.GAM.CLC, 
                      data =data_split$train,
                      family= "tw",
                      method = "REML",select = TRUE)#,
                      #correlation = corSpher(form = ~ Lon_WGS84_bary + Lat_WGS84_bary))
    end_gam_tw <- Sys.time() ; end_gam_tw - start_gam_tw
  
    
    
    # tentative de selection de modele (step-wise) ----
      library(gam)
    
      # formation des formules -----
        str.hab.test <-  str_glue('s({var})',var=union(var.hab.CLC,var.bio),k1=k1) %>%
          str_flatten(collapse = " + ")
    
        str.loc.test <- str_glue('s(Lon_WGS84_bary,Lat_WGS84_bary) + Annee')
        
        formul.GAM.CLC.test <- str_glue('s({var})', 
                                   var = var.mesure, k = k) %>%
          str_flatten(collapse = " + ") %>%
          str_glue("Abondance ~ ",.," + ",str.hab.test) %>%
          str_glue(.," + ",str.loc.test) %>%
          as.formula()
        
        
        # etablissement de la scope (l'ensemble des possibilite de combinaison) -----
          tabl.var <- data_split.GAM$train[,c(grep("Abondance|CLCM|BioClim_Axis|Tps_ecoute|Annee|Jour_de|WGS84",colnames(data_split.GAM$train)))]
    
          mod.test <- gam::gam(formul.GAM.CLC.test,family = "nb",data = data_split$train)
    
    
  
  # Evaluation des modeles -----
    # determination du meilleur modele (prediction sur donnees de test ----
    ab_count <- dplyr::select(data_split$test, obs = Abondance) # recuperation des abondances observee
    
    # prediction des abondances selon les 2 modeles ----
      # Negative binomial version simplifiee
        m_nb_pred_bio <- predict(mod.GAM.nb.bio, data_split$test, type = "response") %>%
          tibble(family = "Negative binomial BIO", pred = .) %>%
          bind_cols(ab_count)
    
        m_nb_pred_mesure <- predict(mod.GAM.nb.mesure, data_split$test, type = "response") %>%
          tibble(family = "Negative binomial MESURE", pred = .) %>%
          bind_cols(ab_count)
        
        m_nb_pred_bio_mesure <- predict(mod.GAM.nb.bio_mesure, data_split$test, type = "response") %>%
          tibble(family = "Negative binomial BIO + MESURE", pred = .) %>%
          bind_cols(ab_count)
    
    
      # Negative binomial 
        m_nb_pred <- predict(mod.GAM.nb, data_split$test, type = "response") %>%
          tibble(family = "Negative binomial", pred = .) %>%
          bind_cols(ab_count)
      
      # Tweedie 
        m_tw_pred <- predict(mod.GAM.tw, data_split$test, type = "response") %>%
          tibble(family = "Tweedie", pred = .) %>%
          bind_cols(ab_count)
        
    # Regroupement des tableaux
    test_pred <- bind_rows(m_nb_pred,m_tw_pred,m_nb_pred_bio,m_nb_pred_mesure,m_nb_pred_bio_mesure) %>%
      mutate(family=as.factor(family))
    
    
    # évaluation de la classification des modeles : Ranking ------
      test_pred %>% 
        group_by(family) %>% 
        summarise(rank_cor = cor.test(obs, pred, 
                                      method = "spearman", 
                                      exact = FALSE)$estimate) %>% 
        ungroup()
    
    # Evaluation de l'amplitude des modeles -----
      # visualisation ----
        ticks <- c(0, 1, 10, 100, 1000)
        mx <- round(max(test_pred$obs))
        ggplot(test_pred) +
          aes(x = log10(obs + 1), 
              y = log10(pred + 1)) +
          geom_jitter(alpha = 0.2, height = 0) +
          # y = x line
          geom_abline(slope = 1, intercept = 0, alpha = 0.5) +
          # area where counts off by a factor of 10
          geom_area(data = tibble(x = log10(seq(0, mx - 1) + 1), 
                                  y = log10(seq(0, mx - 1) / 10 + 1)),
                    mapping = aes(x = x, y = y),
                    fill = "red", alpha = 0.2) +
          # loess fit
          geom_smooth(method = "loess", 
                      method.args = list(span = 2 / 3, degree = 1)) +
          scale_x_continuous(breaks = log10(ticks + 1), labels = ticks) +
          scale_y_continuous(breaks = log10(ticks + 1), labels = ticks) +
          labs(x = "Abondance observée",
               y = "Abondance prédite") +
          facet_wrap(~ family, nrow = 1)
        
      # table de comparaison ----
        test_pred %>% 
        group_by(family) %>% 
          summarize(n = sum(obs / pred > 10),
                    pct = mean(obs / pred > 10))
        
        
    # MAE (Mean Absolute Error) ----
        test_pred %>% 
          group_by(family) %>% 
          summarise(MAE = mean(abs(obs - pred), na.rm = TRUE)) %>% 
          ungroup()
        
# Selection du modele le plus performant -----
  lower.AIC <- min(AIC(mod.GAM.nb),AIC(mod.GAM.tw))
  
  ifelse(test = lower.AIC == AIC(mod.GAM.nb), mod.GAM.4.pred <- mod.GAM.nb,
         mod.GAM.4.pred <- mod.GAM.tw)
      

# Check effet des variables (effet ecologique concret ?) ----
  # fonction de plot effet des covariables -----
    plot_gam <- function(m, title = NULL, ziplss = c("presence", "abundance")) {
          # capture plot
          tmp <- tempfile()
          png(tmp)
          p <- plot(m, pages = 5)
          dev.off()
          unlink(tmp)
          
          p <- p[-length(p)]
         
          # extract data
          p_df <- purrr::map_df(p, ~ tibble(cov = rep(.$xlab, length(.$x)),
                                     x = .$x, fit = .$fit, se = .$se))
          
          # plot
          g <- ggplot(p_df) +
            aes(x = x, y = fit,
                ymin = fit - se, ymax = fit + se) +
            geom_ribbon(fill = "grey80") +
            geom_line(col = "blue") +
            facet_wrap(~ cov, scales = "free") +
            labs(x = NULL,
                 y = "Smooth function",
                 title = title)
          print(g)
          invisible(p_df)
        }

    plot.gam.title <- paste0("Negative Binomial GAM : ",sp)    
    plot.gam.title1 <- paste0("Tweedie GAM : ",sp)   
    
    plot_gam(mod.GAM.nb, title = plot.gam.title)
    plot_gam(mod.GAM.tw, title = plot.gam.title1)
    
    
# Prediction ----
  # detection l'heure de debut optimal -----
    seq_tod <- seq(5, 17, length.out = 300)
    tod_df <- data_split$train %>% 
      dplyr::select(starts_with("CLCM"),starts_with("BioClim"),starts_with("SpBio"),"Jour_de_l_annee","Lon_WGS84_bary","Lat_WGS84_bary") %>% 
      summarize_all(mean, na.rm = TRUE) %>% 
      ungroup() %>% 
      # use standard checklist
      mutate(Tps_ecoute = 0.05) %>% 
      cbind(Heure_de_debut = seq_tod,
            Annee = 2019)


    # Prediction selon differentes heure de debut 
      pred_tod <- predict.gam(mod.GAM.nb, newdata = tod_df, 
                          type = "link", 
                          se.fit = TRUE) %>% 
        as_tibble() %>% 
        # Calcul de l'interval de confiance
        transmute(Heure_de_debut = seq_tod,
                  pred = mod.GAM.nb$family$linkinv(fit),
                  pred_lcl = mod.GAM.nb$family$linkinv(fit - 1.96 * se.fit),
                  pred_ucl = mod.GAM.nb$family$linkinv(fit + 1.96 * se.fit))

    # Heure optimal par le GAM
      heure.opti.GAM <- pred_tod$Heure_de_debut[which.max(pred_tod$pred_lcl)] ; heure.opti.GAM

    # Visualisation ----
      ggplot(pred_tod) +
        aes(x = Heure_de_debut, y = pred,
            ymin = pred_lcl, ymax = pred_ucl) +
        geom_ribbon(fill = "grey80", alpha = 0.5) +
        geom_line() +
        geom_vline(xintercept = heure.opti.GAM, color = "blue", linetype = "dashed")
      
      
  # detection la date optimale -----
      seq_tod <- seq(dateD, dateF, length.out = (as.numeric(dateF)-as.numeric(dateD))*2)
      tod_df <- data_split$train %>% 
        # find average pland habitat covariates
        dplyr::select(starts_with("CLCM"),starts_with("BioClim"),starts_with("SpBio"),"Jour_de_l_annee","Lon_WGS84_bary","Lat_WGS84_bary") %>% 
        summarize_all(mean, na.rm = TRUE) %>% 
        ungroup() %>% 
        # use standard checklist
        mutate(Tps_ecoute = 0.05,
               Heure_de_debut = heure.opti.GAM) %>% 
        cbind(Jour_de_l_annee = seq_tod,
              Annee = 2019)
      
      
      # Prediction selon differentes heure de debut 
      pred_tod <- predict(mod.GAM.nb, newdata = tod_df, 
                          type = "link", 
                          se.fit = TRUE) %>% 
        as_tibble() %>% 
        # Calcul de l'interval de confiance
        transmute(Jour_de_l_annee = seq_tod,
                  pred = mod.GAM.nb$family$linkinv(fit),
                  pred_lcl = mod.GAM.nb$family$linkinv(fit - 1.96 * se.fit),
                  pred_ucl = mod.GAM.nb$family$linkinv(fit + 1.96 * se.fit))
      
      # Heure optimal par le GAM
      date.opti.GAM <- pred_tod$Jour_de_l_annee[which.max(pred_tod$pred_lcl)] ; date.opti.GAM
      
    # Visualisation ----
      ggplot(pred_tod) +
        aes(x = Jour_de_l_annee, y = pred,
            ymin = pred_lcl, ymax = pred_ucl) +
        geom_ribbon(fill = "grey80", alpha = 0.5) +
        geom_line() +
        geom_vline(xintercept = date.opti.GAM, color = "blue", linetype = "dashed")

      
  # modification de la grille de prediction ----
      grid.predict.sp$Heure_de_debut <- heure.opti.GAM
      grid.predict.sp$Jour_de_l_annee <- date.opti.GAM
      grid.predict.sp$Annee <- 2019
      
      
      
      pred.GAM <- predict.gam(mod.GAM.4.pred, newdata = grid.predict.sp, 
                      type = "link", 
                      se.fit = TRUE) %>% 
        as_tibble() %>% 
        # Calcul de l'interval de confiance + conversion (exp() pour negative binomial)
        transmute(Abondance = mod.GAM.4.pred$family$linkinv(fit),
                  Abondance_se = mod.GAM.4.pred$family$linkinv(se.fit),
                  Abondance_bornes_inf_interval = mod.GAM.4.pred$family$linkinv(fit - 1.96 * se.fit),
                  Abondance_bornes_sup_interval = mod.GAM.4.pred$family$linkinv(fit + 1.96 * se.fit)) %>%
        # Ajout a la table de prediction
        bind_cols(grid.predict.sp, .)
      
      #j <- pred.GAM[,328:333]
      
    # preparation visualisation ------
      pred.GAM[which(pred.GAM$Abondance1 < 0.1),"Abondance1"] <- 0
      
      pred.GAM[which(pred.GAM$Abondance_se > quantile(pred.GAM$Abondance_se,0.9)),"Abondance_se"] <- quantile(pred.GAM$Abondance_se,0.9)
      
      '
      pred.GAM$Abondance_cat <- ifelse(test = pred.GAM$Abondance >= 60, "+60",
                                       ifelse(test = pred.GAM$Abondance >= 40,  "60-40",
                                              ifelse(test = pred.GAM$Abondance >= 30, "40-30",
                                                     ifelse(test = pred.GAM$Abondance >=20, "30-20",
                                                            ifelse(test = pred.GAM$Abondance >= 10, "20-10",
                                                                   ifelse(test = pred.GAM$Abondance >= 5, "10-5",
                                                                          ifelse(test = pred.GAM$Abondance >= 4, "5-4",
                                                                                 ifelse(test = pred.GAM$Abondance >= 3, "4-3",
                                                                                        ifelse(test = pred.GAM$Abondance >= 2, "3-2",
                                                                                               ifelse(test = pred.GAM$Abondance >= 1, "2-1",
                                                                                                      ifelse(test = pred.GAM$Abondance >= 0.05, "1-0.05",
                                                                                                             "0")))))))))))
      '
      
      grid.predict.sp_sf <- st_as_sf(pred.GAM, coords = c("Lon_WGS84_bary","Lat_WGS84_bary"),crs=4326)
      grid.predict.sp_sp <- SpatialPointsDataFrame(coords = pred.GAM[,c("Lon_WGS84_bary","Lat_WGS84_bary")],data = pred.GAM,
                                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      
  # Visualisation -----
    ggplot() + 
      geom_sf(data = grid.predict.sp_sf, aes(colour= log(Abondance1))) +
      #scale_colour_gradientn(colors = hcl.colors(6,palette = "Green-Orange",rev=T)) +
      labs(colour = "Log(Abondance)") +
      scale_colour_viridis_c(option="B") +
      ggtitle(sp)
      
    ggplot() + 
      geom_sf(data = grid.predict.sp_sf, aes(colour= log(Abondance_se))) +
      scale_colour_gradient(low = "black",high = "white") +
      labs(colour = "Log(Variance)") + 
      ggtitle(sp,"GAM : Carte de la variance")

    
    
  # merge des 2 prédictions -----
    grid.predict.sp_sf$Abondance_corrige <- grid.predict.sp_sf$Abondance1 * grid.predict.sp_sf$Prob_presence
    grid.predict.sp_sf[which(grid.predict.sp_sf$Abondance_corrige < 0.1),"Abondance_corrige"] <- 0
    
    grid.predict.sp_sf$Abondance_corrige2 <- grid.predict.sp_sf$Abondance_corrige * (551695/(227753*(pi*0.2^2)))
    #grid.predict.sp_sf$Abondance_non_corrige <- grid.predict.sp_sf$Abondance * (551695/(227753*(pi*0.2^2)))
    
    #grid.predict.sp_sf$Abondance_bornes_sup_interval1 <- grid.predict.sp_sf$Abondance * grid.predict.sp_sf$Prob_presence
    
    grid.predict.sp_sf$Abondance_corrige2_cat <- ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 400, "+400",
                                     ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 300,  "400-300",
                                            ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 150, "300-150",
                                                   ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >=50, "150-50",
                                                          ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 25, "50-25",
                                                                 ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 10, "25-10",
                                                                        ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 7, "10-7",
                                                                               ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 4, "7-4",
                                                                                      ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 2, "4-2",
                                                                                             ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 1, "2-1",
                                                                                                    ifelse(test = grid.predict.sp_sf$Abondance_corrige2 >= 0.05, "1-0.05",
                                                                                                           "0")))))))))))
    
    
    
    
    
    
    # Visualisation -----
      ggplot() + 
        geom_sf(data = grid.predict.sp_sf, aes(colour= log(Abondance_corrige))) +
        #scale_colour_gradientn(colors = hcl.colors(6,palette = "Green-Orange",rev=T)) +
        scale_colour_viridis_c(option="B") +
        labs(colour = "Log(Abondance corrigée)") +
        ggtitle(sp)
    
    
  # Calcul des abondances sur le territoire francais ------
    ab_tot_moy <- sum(grid.predict.sp_sf$Abondance1 * 
                        grid.predict.sp_sf$Prob_presence * 
                        (551695/(227753*(pi*0.2^2))))
    
    ab_tot_inf <- sum(grid.predict.sp_sf$Abondance_bornes_inf_interval * 
                        grid.predict.sp_sf$Prob_presence * 
                        (551695/(227753*(pi*0.2^2))))
    
    ab_tot_sup <- sum(grid.predict.sp_sf$Abondance_bornes_sup_interval * 
                        grid.predict.sp_sf$Prob_presence * 
                        (551695/(227753*(pi*0.2^2))))

    
  # plot des listes de presence -------
    carte_presence <- data_split$train
    
    carte_presence$sp_observee[carte_presence$sp_observee>1] <- 1
    carte_presence$color <- ifelse(carte_presence$sp_observee == 1,"green","red")
    
    carte_presence_sf <- st_as_sf(carte_presence,coords = c("X_barycentre_L93","Y_barycentre_L93"),crs=2154)
    
    fra.adm.l93 <- st_transform(st_read(dsn = "C:/Users/Travail/Desktop/Ressource QGis/france/adm/FRA_adm2.shp"),crs=2154)
    
    
    ggplot() +
      geom_sf(data = fra.adm.l93,alpha = 0.5) +
      geom_sf(data = carte_presence_sf,aes(colour = as.factor(sp_observee)),alpha = 0.5)+
      scale_colour_manual(values=c("red","green")) +
      labs(colour = "Presence de l'espèce") +
      ggtitle(sp, "Carte des EPOC avec données de présence")

    
  # chek-up de la prediction (comparaison avec la partie du jeu de données non utilisé pour former le modèle) ------
    m_nb_pred$pred_corrigee <- m_nb_pred$pred * p_fitted # ajout de la prediction corr
    m_nb_pred$coefficient_pred_obs <- m_nb_pred$pred / (m_nb_pred$obs + 1) 
    m_nb_pred$coefficient_pred_corrigee_obs <- m_nb_pred$pred_corrigee / (m_nb_pred$obs + 1)
    
    
    
    
    m_tw_pred$pred_corrigee <- m_tw_pred$pred * p_fitted
    m_tw_pred$coefficient_pred_obs <- m_tw_pred$pred / m_tw_pred$obs 
    m_tw_pred$coefficient_pred_corrigee_obs <- m_tw_pred$pred_corrigee / m_tw_pred$obs
    
    
  # visualisation du check-up ------
    # grpah check-up de la presence
      ggplot(data =rf.1_0.pred.test,aes (x=obs,y=pred_non_calibre)) +
        geom_jitter() +
        geom_abline(slope = 1)+
        geom_smooth(method=lm)+
      xlab("Observations") + ylab("Valeur prédite") + ggtitle(sp,"randomForest")
    
    # grpah check-up de l'abondance
    ggplot(data =m_nb_pred,aes (x=obs,y=pred)) +
      geom_jitter() +
      geom_abline(slope = 1) +
      geom_smooth(method=lm) +
      ylab("Valeur prédite") + xlab("Observations") + ggtitle(sp,"GAM")
    
    # grpah check-up de l'abondance X presence
    ggplot(data =m_nb_pred,aes (x=obs,y=pred_corrigee)) +
      geom_jitter() +
      geom_abline(slope = 1)+
      geom_smooth(method=lm) +
      ylab("Valeur prédite") + xlab("Observations") + ggtitle(sp,"randomForest x GAM")
    
    
      
# NOT RUN -----
  # evaluation de l'autocorr des residus ----
    library(spdep)
      
      
      








