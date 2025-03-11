# Import and clean the data to update the database with new columns needed for the R&R
# Source databases are not available due to confidentiality issues

# Laudine : if you need to go back to the previous import code, see Sauvegarde or on Github


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

# Write the file
write.csv(MainDB, here("Data","MainDatabase.csv"), row.names = FALSE)
