#---------------------- DiffEduc --------------------


tabVarEduc <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Yes", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    BabyFemale = ifelse(BabyFemale == TRUE, 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0), 
    NumberOfChildren = as.numeric(NumberChildren), 
    InfoBaseline1or0 = ifelse(InfoBaseline == "Low knowledge", 1, 0)
) %>% 
  select(
    "High-SES" = Educ1or0,
    "Single-parent family" = SingleMum1or0,
    "Age of the mother" = Age,
    "The houshold is primiparous" = Primipare1or0,
    "Number of children in the household" = NumberOfChildren,
    "The mother is born in France" = BornFr1or0,
   # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present orientated" = Discount501or0,
    "The mother has low knowledge about early childcare" =InfoBaseline1or0,
    "The mother is active at baseline" = Active1or0,
    "The mother wants to work after maternity leaves" = WorkPlanTo1or0,
    #"The mother did not smoke" = DidNotSmoke1or0,
    #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
    "The household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "The mother wants to use early childcare" = PlanToUseECS1or0,  
    "The household has access to a computer" = ComputerYes1or0,
    # Intend to use, block variable
    #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
    #  "Knows only daycare" = KnowsCrecheOnly1or0,
    #    "Value socialization" = ValSocialisation,
    #"Believe in returns to early childcare" = LikertReturnHK1or0,
    #"The mother trusts early childcare" = TrustCreche1or0,
    "The mother lives in Paris" = DepParis1or0,
    "Early childcare coverage is high" = HighECSCov1or0,
    "Child is a girl" = BabyFemale,
    #  StrataWave
   "Applied to any early childcare facility at endline" = ECSApp, 
   "Applied to any daycare center at endline" = AppCreche,
   "Accessed any early childcare facility at endline" = ECSUseYes, 
   "Accessed any daycare center at endline" = UseCreche
  )


summary_baseline_variables_SES <- tabVarEduc %>%
  tbl_summary(
    by = "High-SES",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference()%>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Low-SES",
                stat_2 ~ "High-SES") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Socio-Economic Status**") %>% 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_SES %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by socio-economic status") %>% 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.") 


#---------------------- DiffMig --------------------
summary_baseline_variables_Migration <- tabVarEduc %>%
  tbl_summary(
    by = "The mother is born in France",  # 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Born abroad",  # 
                stat_2 ~ "Born in France") %>%  # 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Migration Background**") %>%  # 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_Migration %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by migration background") %>%  # 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#-------------------- DiffInfo --------------------

summary_baseline_variables_Info <- tabVarEduc %>%
  tbl_summary(
    by = "The mother has low knowledge about early childcare",  # 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "High knowledge",  # Changé ici
                stat_2 ~ "Low knowledge") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Initial Level of Knowledge**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_Info %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by initial level of knowledge") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffPresentBias --------------------
summary_baseline_variables_TimeOrientation <- tabVarEduc %>%
  tbl_summary(
    by = "The mother is present orientated",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Future oriented",  # Changé ici
                stat_2 ~ "Present oriented") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Temporal Orientation**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_TimeOrientation %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by temporal orientation") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#---------------------- DiffUse --------------------

summary_baseline_variables_PreviousUse <- tabVarEduc %>%
  tbl_summary(
    by = "The household has ever used early childcare",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Never used",  # Changé ici
                stat_2 ~ "Previous user") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Previous Childcare Usage**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_PreviousUse %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by previous childcare usage") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffActive --------------------

summary_baseline_variables_Activity <- tabVarEduc %>%
  tbl_summary(
    by = "The mother is active at baseline",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Inactive",  # Changé ici
                stat_2 ~ "Active") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Activity Status**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_Activity %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by activity status") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")
#---------------------- DiffParis --------------------


tabVarDep <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Yes", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    BabyFemale = ifelse(BabyFemale == TRUE, 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0), 
    NumberOfChildren = as.numeric(NumberChildren), 
    InfoBaseline1or0 = ifelse(InfoBaseline == "Low knowledge", 1, 0)
  ) %>% 
  select(Dep,
         "High-SES" = Educ1or0,
         "Single-parent family" = SingleMum1or0,
         "Age of the mother" = Age,
         "The houshold is primiparous" = Primipare1or0,
         "Number of children in the household" = NumberOfChildren,
         "The mother is born in France" = BornFr1or0,
         # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
         #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
         "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
         "The mother is present orientated" = Discount501or0,
         "The mother has low knowledge about early childcare" =InfoBaseline1or0,
         "The mother is active at baseline" = Active1or0,
         "The mother wants to work after maternity leaves" = WorkPlanTo1or0,
         #"The mother did not smoke" = DidNotSmoke1or0,
         #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
         "The household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
         "The mother wants to use early childcare" = PlanToUseECS1or0,  
         "The household has access to a computer" = ComputerYes1or0,
         # Intend to use, block variable
         #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
         #  "Knows only daycare" = KnowsCrecheOnly1or0,
         #    "Value socialization" = ValSocialisation,
         #"Believe in returns to early childcare" = LikertReturnHK1or0,
         #"The mother trusts early childcare" = TrustCreche1or0,
         "The mother lives in Paris" = DepParis1or0,
         "Early childcare coverage is high" = HighECSCov1or0,
         "Child is a girl" = BabyFemale,
         #  StrataWave
         "Applied to any early childcare facility at endline" = ECSApp, 
         "Applied to any daycare center at endline" = AppCreche,
         "Accessed any early childcare facility at endline" = ECSUseYes, 
         "Accessed any daycare center at endline" = UseCreche
  )

summary_baseline_variables_District_Paris <- tabVarDep %>%
  mutate(Paris_vs_Others = ifelse(Dep == "75", "Paris", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "Paris_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Paris",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Paris comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_District_Paris %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by district") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffSSD --------------------



# Second: Seine-Saint-Denis vs Others
summary_baseline_variables_District_SSD <- tabVarDep %>%
  mutate(SSD_vs_Others = ifelse(Dep == "93", "Seine-Saint-Denis", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "SSD_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Seine-Saint-Denis",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Seine-Saint-Denis comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))

summary_baseline_variables_District_SSD %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:6), unit = "cm", width=2) %>%  # Modifié pour inclure une colonne supplémentaire
  set_caption(caption = "Differences by district") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#---------------------- DiffVDM --------------------

# Third: Val-de-Marne vs Others
summary_baseline_variables_District_VDM <- tabVarDep %>%
  mutate(VDM_vs_Others = ifelse(Dep == "94", "Val-de-Marne", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "VDM_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Val-de-Marne",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Val-de-Marne comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))


summary_baseline_variables_District_VDM %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:6), unit = "cm", width=2) %>%  # Modifié pour inclure une colonne supplémentaire
  set_caption(caption = "Differences by district") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#---------------------- Correlation --------------------


p_load(psych, corrplot)

# Select the relevant variables
variables <- MainDB[, c("Educ2", "FrenchYNBaseline", "InfoBaseline", 
                        "PresentOrientated", "UsedECEC", "ActiveBaseline")]

# Convert them into numerical variables
variables_num <- data.frame(lapply(variables, function(x) {
  if(is.factor(x)) as.numeric(x) - 1 else x
}))

# Remove variables with no variance (none but for some reason it creates a bug)
vars_with_variance <- names(which(sapply(variables_num, var, na.rm=TRUE) > 0))
variables_num_clean <- variables_num[, vars_with_variance]

tetrachoric_matrix <- tetrachoric(variables_num_clean)$rho

# Change the names so that they are prettier
nouveaux_noms <- c("SES", "Migration background", "Knowledge", 
                   "Temporal Orientation", "Previous early childcare use", "Activity")

# Renames rows and cols
colnames(tetrachoric_matrix) <- nouveaux_noms
rownames(tetrachoric_matrix) <- nouveaux_noms

# Plot the correlation matrix
corrplot(tetrachoric_matrix, method="color", 
         type="upper", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,  # tl.srt : orientation
         tl.cex = 0.8,               # font size
         diag=FALSE)

# Then add the title
title(main="Tetrachoric Correlation Coefficients of the Main Variables", 
      line=1,    # Adjust the vertical position of the title
      cex.main=1)  # Adjust the size of the title

#----------- InteractionSESActive -----------
Het.ITT.App.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.App.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ITT.Use.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.Use.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

# Separate the interaction terms
Het.ITT.App.ActiveEduc$ModelSummary$tidy <- Het.ITT.App.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ITT.App.ActiveEduc$ModelSummary0$tidy <- Het.ITT.App.ActiveEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ATT.App.ActiveEduc$ModelSummary$tidy <- Het.ATT.App.ActiveEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Activity","SES"))

Het.ITT.Use.ActiveEduc$ModelSummary$tidy <- Het.ITT.Use.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ITT.Use.ActiveEduc$ModelSummary0$tidy <- Het.ITT.Use.ActiveEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ATT.Use.ActiveEduc$ModelSummary$tidy <- Het.ATT.Use.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table with the new interaction terms
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.ActiveEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.ActiveEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.ActiveEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.ActiveEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.ActiveEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.ActiveEduc$ModelSummary),
             shape = term + Activity + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and employment status at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")

#---------------------- Norms --------------------