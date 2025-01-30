#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
# --------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                   -------------
#--------------------                    Loading and cleaning all data                     -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                               Version 1                              -----------------------------------#  
#--------------------                               June 2024                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#

##### Load Data and clean #####%


#---- LoadSource ----
# Original baseline dataset with the number of refusals
BaselineFull <- read_csv(here("Data","BaselineWithRefusals.csv")) 



# Import the main database with both baseline and endline information for all participants
MainDB <- read_csv(here("Data","MainDatabaseWithoutIds.csv")) %>%   mutate(
                            NormsBaseline=factor(NormsBaseline,
                                                 levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"), ordered = TRUE # how many mothers in their surroundings use childcare 
                            ), 
                            Dep = as.factor(Dep) 
)

# Between the submission and first round of reviews, the authors spotted two typos in the initial coding of 
# - Mother's baseline income
# - Family's baseline income
# We correct here this typo by replacing the initial variables with the corrected ones

# Import the file with the corrected variables

BaselineCorrected <- read_csv(here("Data","BaselinePremiersPas_corrected.csv")) %>% 
  select(ResponseId, Income, FmlyIncome, NormsOpposedN) # we select the variables we want


# Import the variables of interest in the dataframe
MainDB <- MainDB %>% left_join(BaselineCorrected, by = "ResponseId") %>% 
  select(-c(FmlyIncomeBaseline,IncomeBaseline, FmilyEarnLessThan2500 )) %>%  # we remove the variables with mistakes
  mutate(
    FmilyEarnLessThan2500 = ifelse((FmlyIncome == "+8000" | FmlyIncome == "+8000" | FmlyIncome == "2500-3000"  
                                    | FmlyIncome == "3000-4500" | FmlyIncome == "4500-5000" | FmlyIncome == "5000-6000"
                                    |  FmlyIncome == "6000-7000" | FmlyIncome == "7000-8000" | FmlyIncome == "Ne veut pas répondre"), 
                                   "No",
                                   "Yes"), 
    FmilyEarnLessThan2500 = as.factor(FmilyEarnLessThan2500), 
    NumberOfChildren3 = ifelse(NumberChildren > 1, # recode for the balance table
                               "2 or more", 
                               as.character(NumberChildren)
                               ), 
    NumberOfChildren3 = as.factor(NumberOfChildren3), 
  NormsOpposedYes = ifelse(NormsOpposedN != 0, 
                           "Yes", 
                           "No"),
  NormsOpposedYes = replace_na(NormsOpposedYes, "No"),
  DescriptiveNorms = ifelse((NormsBaseline == "Toutes" | NormsBaseline == "La plupart"),  
                            "Yes", 
                            "No"
  )
  
  
  )
Endline <- read_csv(here("Data","PremiersPasWithNonRespondents112024.csv")) %>% 
  select(BabyBirthDate, ResponseId, TrustCreche1or0) %>% 
  mutate(BabyBornJanuary = ifelse(BabyBirthDate > "2022-12-31", 
                "After January", 
                "Before January"
                ), 
         BabyBornJanuary =  replace_na(BabyBornJanuary, "Don't know")
  )

MainDB <- MainDB %>% left_join(Endline, by = "ResponseId") %>% 
  mutate(
    
    LikertReturnHK1or0 = ifelse(
      (LikertReturnHKBaseline == "Assez d’accord" | LikertReturnHKBaseline == "Très d’accord"), 
     1, 
      0
    ),
    # create a score of positive attitudes 
    AttitudesScore = (TrustCreche1or0 + LikertReturnHK1or0) / 2,
    AttitudeScoreMoreThanMedian = ifelse(AttitudesScore > median(AttitudesScore, na.rm = TRUE), 
                                         "Yes", 
                                         "No"
    ),

      TrustCreche1or0 = ifelse(TrustCreche1or0 == 1, 
                               "Yes", 
                               "No"
      ), 
    LikertReturnHK1or0 = ifelse(LikertReturnHK1or0 == 1, 
                               "Yes", 
                               "No"
      )

  )

# Additional items due to reviewers questions

# Import the dataset from *Endline*: Endline.csv
Endline_data <- read_csv(here("Data","Endline.csv"))
# Import the second data collection  RCT set that has to be created due to a bug in qualtrics
Endline_extra <- read_csv(here("Data","Endline_extra.csv"))

## Both Endline_data and Endline_extra contain observations from the RCT sample. We want to merge them into 1 dataset
#Get the data ready for merging
##create a df with the first two rows that contains the full questions, in case it is needed
details_values <- Endline_data[1:2,]


##remove these rows from the main df
Endline_data<- Endline_data[-c(1:2),]
Endline_extra <- Endline_extra[-c(1:2),]

# Merge the two datasets to keep only Endline_data
Endline_data <- bind_rows(Endline_data, Endline_extra)

Endline_add <- Endline_data %>% 
  select(c(ExternalReference, ECSNoECSAppAnswer, ECSIdealNotReason, ECSIdealNotReason_5_TEXT))

# Some obervations are duplicated, we keep only the last observation
Endline_add <- Endline_add %>%
  group_by(ExternalReference) %>%
  slice(which.max(!is.na(ECSNoECSAppAnswer))) %>%
  ungroup()
  ## For exploratory data analysis, we add more variables

MainDB <- MainDB %>% left_join(Endline_add, by = c("ResponseId" = "ExternalReference")) %>%
  mutate(ECSNoECSAppAnswer_simple = case_when( #
    is.na(ECSNoECSAppAnswer) ~ NA_character_,  # Keep NAs as NAs
    ECSNoECSAppAnswer == "Non, on attend encore" ~ "Still waiting/No Answer",
    ECSNoECSAppAnswer == "Oui, on a été rejetés" ~ "Rejected",
    ECSNoECSAppAnswer == "Oui, on est sur la liste d’attente" ~ "Waiting list",
    ECSNoECSAppAnswer %in% c(
      "Oui,  on été acceptés dans au moins 1 mais il y a eu un problème",
      "Oui,  on été acceptés dans au moins 1 mais on a refusé (à cause d'un problème, etc.)",
      "Oui,  on été acceptés mais on a dû repousser à cause de problèmes (de santé)",
      "Oui, on été acceptés, mais on a déménagé"
    ) ~ "Other"
  ), 
  ECSNoECSAppAnswer_long = case_when(
    is.na(ECSNoECSAppAnswer) ~ NA_character_,  # Keep NAs as NAs
    ECSNoECSAppAnswer == "Oui, on est sur la liste d’attente" ~ "Waiting list",
    ECSNoECSAppAnswer == "Non, on attend encore" ~ "Still waiting/No Answer",
    ECSNoECSAppAnswer == "Oui, on a été rejetés" ~ "Rejected",
    ECSNoECSAppAnswer == "Oui,  on été acceptés mais on a dû repousser à cause de problèmes (de santé)" ~ "Health issues",
    ECSNoECSAppAnswer == "Oui, on été acceptés, mais on a déménagé" ~ "Moved out",
    ECSNoECSAppAnswer == "Oui,  on été acceptés dans au moins 1 mais il y a eu un problème" ~ "Issue with the application",
    ECSNoECSAppAnswer == "Oui,  on été acceptés dans au moins 1 mais on a refusé (à cause d'un problème, etc.)" ~ "Declined because inadequate"
  ), 
  ECSIdealNotReason_recoded = case_when(
    (str_detect(ECSIdealNotReason_5_TEXT, "correspondaient pas") ==TRUE | 
      str_detect(ECSIdealNotReason_5_TEXT,"J'ai eu la place maiq que pour un de mes enfants" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"mais pas à temps plein" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"dispo que pour qq mois" ) == TRUE | 
      str_detect(ECSIdealNotReason_5_TEXT,"ne sont pas agrées" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"Mauvais contact" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"même crèche municipale" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"pas aimé la crèche" ) == TRUE |
      str_detect(ECSIdealNotReason_5_TEXT,"séparer ses bébés" ) == TRUE 
      ) ~ "Inadequate",
    (str_detect(ECSIdealNotReason_5_TEXT,"mais on lui donnait obligatoirement 2 jours" ) == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"pour 3j/sem" ) == TRUE 
     
) ~ "Incompatible working hours",
    (str_detect(ECSIdealNotReason_5_TEXT, "déménagé") ==TRUE | 
       str_detect(ECSNoECSAppAnswer, "déménagé") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "déménagement") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "Déménagement") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Pour la nouvelle") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "a déménagé") ==TRUE 
     ) ~ "Moved out",
    (str_detect(ECSIdealNotReason_5_TEXT, "santé") ==TRUE | 
       str_detect(ECSNoECSAppAnswer, "santé") ==TRUE 
     ) ~ "Health issues",
    (str_detect(ECSIdealNotReason_5_TEXT, "trop tard") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "lui a été donnée") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT,  "peur de ne rien trouver" )  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,  "la mairie leur a donné" )  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,  "bien avec l'ass mat" )  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "la crèche beaucoup plus tôt") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "marche acquise")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "date limite")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "reprendre le travail")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "déjà chez une assmat")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "un peu plus longtemps")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"elle ne marche pas encore")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"trop petit")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"surtout au cours de l'annéé")== TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"septembre était déjà terminée") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"j'ai été engagé auprès de l'ass mat") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "a eu la place pour Avy") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"la place en retard") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "après l'adaptation à préférer rester" ) ==TRUE|
       str_detect(ECSIdealNotReason_5_TEXT,"la place en retard") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "période ou a besoin")== TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Trop tot")== TRUE 
       ) ~ "Timing",
    (str_detect(ECSIdealNotReason_5_TEXT, "loin du domicile") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "près du lieu de résidence") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "mais trop loin") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "proche du domicile")== TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "géographique")== TRUE 
     ) ~ "Too far away",
    (str_detect(ECSIdealNotReason_5_TEXT, "Administrativement compliqué") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"dossier non actualisé") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"encore fait les démarches") == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "s'est trompée")  == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Dossier non traité") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "encore fait les démarches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pas fait les inscriptions") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pas terminé mes recherches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pour pouvoir candidater") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "faire les démarches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "fait la demande avec l'assistant social") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pas candidaté") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pas encore fait les démarches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Problème d'inscription") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "joindre les crèches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "situation") ==TRUE 
     ) ~ "Paperwork too heavy",
    (str_detect(ECSIdealNotReason_5_TEXT, "les inscriptions étaient passées") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "délai trop court") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "vient de faire la demande") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "la garder avec moi") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "auprès de moi au début") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "pas prévu de reprendre") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "trop tard") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "encore entrepris les recherches") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "rester avec bébé") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "garder par quelqu'un d'autre") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "à la mairie") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "a été annulé") ==TRUE 
     ) ~ "Applied too late",
    (str_detect(ECSIdealNotReason_5_TEXT, "Attend") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "Attente") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "d'attente") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "attend une place") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "en attente que place") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT,"j'attends une réponse du mode de garde") ==TRUE  |
       str_detect(ECSIdealNotReason_5_TEXT, "pas eu de retour") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "PAS DE NOUVELLES") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Pas beaucoup d'assmat") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Pas de réponse") ==TRUE 
       ) ~ "Still waiting/No Answer", 
    (str_detect(ECSIdealNotReason_5_TEXT, "j'avais d'autres plans") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "2 refus") ==TRUE 
       )~ "Rejected",
    (str_detect(ECSIdealNotReason_5_TEXT,"pas droit vu qu'elle ne travaille pas ") ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "pas éligible") ==TRUE 
     ) ~ "Perceived Not Eligible",
    (str_detect(ECSIdealNotReason_5_TEXT, "la crèche serait trop chère") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT,"perte d'argent") ==TRUE
     ) ~ "Too expensive",
    (str_detect(ECSIdealNotReason_5_TEXT, "de m'informer") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Je ne savais pas") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "pas renseignée")  ==TRUE | 
       str_detect(ECSIdealNotReason_5_TEXT, "pas connaissance") ==TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "Pas le temps pour se renseigne") ==TRUE
    ) ~ "Lack of information",
    (str_detect( ECSIdealNotReason_5_TEXT, "père" ) == TRUE |
       str_detect(ECSIdealNotReason_5_TEXT, "partenaire" ) == TRUE 
       ) ~ "Father's disagreed",
     is.na(ECSIdealNotReason_5_TEXT) ~ NA_character_,  # Keep NAs as NAs
    TRUE ~ "Other"
  ), 
  ECSIdealNotReasonClean = case_when(
    is.na(ECSIdealNotReason) & is.na(ECSNoECSAppAnswer_long) ~ NA_character_,
    is.na(ECSIdealNotReason) & !is.na(ECSNoECSAppAnswer_long) ~ ECSNoECSAppAnswer_long,
    str_detect(ECSIdealNotReason, "Autre raison") & !is.na(ECSNoECSAppAnswer_long) ~ paste0(
      str_replace(
        ECSIdealNotReason,
        "Autre raison.",
        as.character(ECSIdealNotReason_recoded)
      ),
      ",",
      ECSNoECSAppAnswer_long
    ),
    str_detect(ECSIdealNotReason, "Autre raison") & is.na(ECSNoECSAppAnswer_long) ~ str_replace(
      ECSIdealNotReason,
      "Autre raison.",
      as.character(ECSIdealNotReason_recoded)
    ),
    !is.na(ECSNoECSAppAnswer_long) ~ paste0(ECSIdealNotReason, ",", ECSNoECSAppAnswer_long),
    TRUE ~ ECSIdealNotReason
  )
  )
  
    

#confession juive","pas en France ", "Hésitait", "idée qu'elle considère", 
#"pas le temps", "convenait pas finalement", "regroupement familial"
# Descrtiptive statistics 

## For descriptive stats of the coverage rates in 2021 we used data from https://data.caf.fr/explore/dataset/txcouv_pe_com/information/?disjunctive.annee&refine.annee=2021
## We import the data for the coverage rates in 2021 outside from the cities in the sample
### Import the files
txcouv_pe_com_EAJE_assmat <- read.csv(here("Data", "Spatial data","txcouv_pe_com_EAJE_assmat.csv"),sep=";")%>% filter(Date.référence == "2021") %>% mutate(NationalAvgTauxCouv2021 = round(mean(Taux.de.couv.global, na.rm = TRUE), 1)) 

# Import  shape data for the map at the city level

# Note: here they have data on PMI and EAJE in SSD https://data.iledefrance.fr/explore/?sort=modified&refine.theme=Logement+-+sant%C3%A9+-+social

# Import our map at the city level
gp_shape <- st_read(file.path(here("Data", "Spatial data", 
                                   "RECENSEMENT_COMMUNE_POPULATION.shp")))

#---- MainDBPrep ----


PredVars <- c(#"Age",
  #"SingleMum",
  #"Couple",
  #"CoupleCohabiting",
  #"PregMonth",
  #"Educ",
  #"Act3",
  #"FrenchYNBaseline",
  #"FmlyIncomeBaseline",
  #"IncomeBaseline",
  #"ECSUsed",
  #"ECSNeedBaseline",
  #"NumberChildren",
  #"Primipare",
  #"WorkBefore",
  #"WorkPlanTo",
  #"WorkPlanNotTo",
  #"WorkNoPlan",
  #  "ComputerYN",
  #"HighLowearly childcareBaseline",
  #"Dep",
  "Assignment",
  "StrataWave"
  # "KnowsCrecheOnly",
  #  "SmokePreg",
  #  "Primipare",
  #  "WorkPlanTo"#,
  # "LikertReturnHK1or0Discount501or0"
  #,
  #  "Assignment",
  # "StrataWave"
)



# Convert character variables to factor
char_vars <- lapply(MainDB, class) %in% c('character','logical')
MainDB[, char_vars] <- lapply(MainDB[, char_vars], as.factor)

DBResponse <- MainDB %>% select(Responded, one_of(PredVars))%>% as.data.frame()

ps_fit <- ps(Responded ~ . ,## Use all variables
             data=DBResponse, ## Select just the response and specified predictors
             estimand='ATE', ## ATE: Generalize to all those invited for the survey, not just responders
             verbose=FALSE
)


# get the weights
DBResponse$weight <- get.weights(ps_fit, stop.method="es.mean")
DBResponse$PsRX <- ps_fit$ps$es.mean.ATE

survey_trick <- rbind(
  data.frame(DBResponse, trt_trick=1, weight_trick=1) # full sample
  , data.frame(DBResponse, trt_trick=0, weight_trick=DBResponse$weight) %>%
    filter(Responded==1) # Responders, weighted
) # Trick dx.wts function to summarize this data set to compare if weighted responders are similar to the full sample (ATT since full sample has trt_trick=1 as trick treatment)



MainDB$WeightBalance <- DBResponse$weight

###### Compute propensity scores of treatment assignments
# I use a multinomial logit of assignment over blocks within each wave (i.e. the exact randomisation design)
# multinom is taken from the package glmnet
(fit_basic <- multinom(Assignment ~ StrataWave, data = MainDB))
#%>% invisible()


# Then retrieve the predicted probability

MainDB$predT1 <- predict(fit_basic,MainDB,"probs")[,"T1"]
MainDB$predT2 <- predict(fit_basic,MainDB,"probs")[,"T2"]

# From there, we generate centred treatment dummies

MainDB <- MainDB %>% mutate(Z1=ifelse(Assignment=="T1",1,0),
                            Z2=ifelse(Assignment=="T2",1,0),
                            Z1.c=Z1-predT1,
                            Z2.c=Z2-predT2
)




# Ultimately, we want to test all pairwise comparison of attrition rate across groups. So Thats how we do it
SampleT1C <- MainDB %>%filter( Assignment %in% c("Control","T1")) %>%
  mutate(SubSample="T1-C") %>% mutate(Z=ifelse(Assignment=="T1",1,0), 
                                      Z.c = Z-predT1, 
                                      WeightPS=Z/predT1+(1-Z)/(1-predT1))

SampleT2C <- MainDB %>%filter( Assignment %in% c("Control","T2"))%>% 
  mutate(SubSample="T2-C") %>% mutate(Z=ifelse(Assignment=="T2",1,0),
                                      Z.c=Z-predT2,
                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))
SampleT2T1 <- MainDB %>%filter( Assignment %in% c("T2","T1"))%>% mutate(SubSample="T2-T1") %>% mutate(Z=ifelse(Assignment=="T2",1,0), 
                                                                                                      Z.c=Z-predT2,
                                                                                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))                                                                          

# bind them

StackedDB <- bind_rows(SampleT1C,SampleT2C,SampleT2T1) %>% mutate(Treat=paste("Z=",Z,":",SubSample,sep=""),
                                                                  SubSampleStrata=paste(SubSample,StrataWave,sep=":")
)

# Propensity score for each pairwise comparison: simple probit
fit_basicStack <- feglm(Z ~ 1|SubSampleStrata, data = StackedDB,"probit",cluster = ~StrataWave)
#retrieve the prediction
StackedDB$psscore=predict(fit_basicStack,StackedDB)

StackedDB <- StackedDB %>% mutate(
  Z.c=Z-psscore,
  WeightPS=Z/psscore+(1-Z)/(1-psscore),
  ZT2C.c =Z.c*as.numeric(SubSample=="T2-C"),
  ZT1C.c =Z.c*as.numeric(SubSample=="T1-C"),
  ZT2T1.c=Z.c*as.numeric(SubSample=="T2-T1")
)



# Prepare all the covariates

#------ PrepCovariates ------
#Here, we build the "\dot{X}" matrix and the interaction with the treatment the we will use in all models for the Lasso
# The idea is to take each variable we consider, to generate dummies, and even centre them by block to mimic the fixed effect regression. We center every variable by block essentially.
# then, for the lasso, all covariates are centred, we can interact them with treatment and therefore keep and ATE interpretation of the coefficient.

## MaternityWardBaseline
matid <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,MaternityWardBaseline) 
# everything as dummies
mat <- model.matrix(~0+MaternityWardBaseline,matid) %>% as.data.frame() 
# centred
mat.c <-  mat %>% bind_cols(.,matid %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata) %>% mutate_at(all_of(names(mat)),~.x-mean(.))%>% ungroup()

## AgeChild1:AgeChild8,Primipare,Age, PregMonth
AgeChildenId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,Primipare,AgeChild1:AgeChild8,Age, PregMonth) %>% 
  mutate_at(vars(Primipare:AgeChild8,Age, PregMonth),~ifelse(is.na(.),0,.)) 
AgeChilden <-  model.matrix(~0+.,AgeChildenId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 


AgeChilden.c <- AgeChilden %>% bind_cols(.,AgeChildenId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata) %>% mutate_at(all_of(names(AgeChilden)),~.x-mean(.))%>% ungroup()

#KnowsCreche:KnowsNothing
KnowsId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,KnowsCreche:KnowsCrecheOnly) 
Knows <- model.matrix(~0+.,KnowsId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 
Knows.c <- Knows   %>% bind_cols(.,KnowsId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Knows)),~.x-mean(.))%>% ungroup()


# Relationship LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline

SocDemoId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  Relationship,LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline,Dep,
                                  "WorkBefore","WorkPlanTo" ,
                                  "WorkPlanNotTo","WorkNoPlan" 
) %>% mutate(Dep=as.factor(Dep))
SocDemo <-  model.matrix(~0+.,SocDemoId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame() 
SocDemo.c <- SocDemo %>% bind_cols(.,SocDemoId %>% select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(SocDemo)),~.x-mean(.))%>% ungroup()

# FmlyIncome, Income at Baseline
# the authors corrected a typo between the initial submission and the second version of the manuscript

IncomesId <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  FmlyIncome, Income) %>% 
  mutate_at(vars(FmlyIncome, Income),~as.character(.)) %>% 
  replace_na(list(FmlyIncome="NA", Income="NA"))

Incomes <-   model.matrix(~0+.,IncomesId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Incomes.c <- Incomes %>% bind_cols(.,IncomesId %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Incomes)),~.x-mean(.))%>% ungroup()

SubjectiveId <-  StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                      "AccessEasyBaseline", "AccessHoursBaseline" ,"AccessInformalCareBaseline",
                                      "LikertReturnHKBaseline","LikertAccessInfoBaseline"  ,"LikertReturnHK1or0",
                                      "LikertAccessSoc","NormsBaseline" )

Subjective <- model.matrix(~0+.,SubjectiveId %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Subjective.c <- Subjective %>% bind_cols(.,SubjectiveId %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Subjective)),~.x-mean(.))%>% ungroup()


Depriveid <- StackedDB %>% select(ResponseId,SubSampleStrata,SubSample,
                                  "DeprivClothesIrrelevant" ,"DeprivFoodIrrelevant"  ,
                                  "DeprivBillsIrrelevant"   ,"DeprivHolidaysIrrelevant" ,
                                  "DeprivHousingIrrelevant" ,"DeprivClothes" ,
                                  "DeprivFood"              ,"DeprivBills"    ,
                                  "DeprivHolidays"          ,"DeprivHousing"  ,"LevelComprehensionExPost"  ) %>% mutate_at(vars(DeprivClothesIrrelevant:LevelComprehensionExPost),~ifelse(is.na(.),"NA",.))

Deprive <- model.matrix(~0+.,Depriveid %>% select(-c(ResponseId,SubSampleStrata,SubSample))) %>% as.data.frame()

Deprive.c <- Deprive %>% bind_cols(.,Depriveid %>%  select(ResponseId,SubSampleStrata,SubSample)) %>% 
  group_by(SubSampleStrata)  %>% mutate_at(all_of(names(Deprive)),~.x-mean(.)) %>% ungroup()


# Vector of outcome names
Outcomes <- c("ECSApp","ECSUseYes","UseCreche","AppCreche","AppAssMat","Suivi_administratif1_0")
# Vector of secondary outcomes' names
sec.Outcomes <- c("ECSIdealYes","UsePrivée","ECSAppTiming","KnowsAssMatEndline")

# Databases to use
PostDB <- StackedDB %>% filter(Responded==1) %>% ungroup()

# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT2 <- PostDB %>% filter(str_detect(SubSample,"T2")) %>% mutate(D=Suivi_administratif1_0) %>% ungroup()


# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT1C <- PostDB %>% filter(SubSample == "T1-C") %>% ungroup()

# When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
PostDBT2C <- PostDBT2 %>% filter(SubSample == "T2-C") %>% ungroup()
# Matrix of centred potential covariates
X.c <- left_join(mat.c,AgeChilden.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>%
  left_join(.,Knows.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,SocDemo.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Incomes.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Subjective.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,Deprive.c,by = join_by(ResponseId, SubSampleStrata, SubSample)) %>% 
  left_join(.,StackedDB %>% select(ResponseId, SubSampleStrata, SubSample,Responded))# %>% mutate(Sub)


## For logit models


reg_MainDB <- MainDB %>%
  dplyr::select(ResponseId, ECSApp, ECSUseYes, AppCreche, 
                UseCreche, Assignment, StrataWave, Educ2, FrenchYNBaseline, HighLowECECBaseline, 
                InfoBaseline, Discount501or0,HigherThanMeadianISEIMother,HigherThanMeadianSESIndex, 
                HighLowCoverageGlobal, UsedECEC, Responded, ResponseStatus, T1, T2, High_SES,
                NoMigrationBackground, HighCoverageBaseline, High_knowledge, HighCoverage_total, PresentOrientated) %>% filter(Responded==1) %>% 
  mutate(StrataWave=as.factor(StrataWave)) 


# remove unecessary datasets
rm(SocDemo, SocDemo.c, SocDemoId, 
         Subjective, SubjectiveId, Subjective.c,
         AgeChilden, AgeChildenId, AgeChilden.c,
         Deprive, Depriveid, Deprive.c,
         Incomes, IncomesId, Incomes.c,
         Knows, KnowsId, Knows.c,
         mat, matid, mat.c,
         SampleT1C, SampleT2C, SampleT2T1,
         DBResponse, PredVars, fit_basic, fit_basicStack,
         survey_trick, ps_fit
         ) 

