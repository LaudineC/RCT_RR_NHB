
#----------------------- Descriptive -------------
# D'abord créer un dataframe avec les variables originales
tabDes_temp <- MainDB  %>% 
  mutate(
    # Variables catégorielles maintenues comme facteurs
    SingleMum = as.factor(ifelse(SingleMum == TRUE, "Yes", "No")),
    Active = as.factor(ifelse(Act3 == "Active", "Yes", "No")),
    Educ = as.factor(ifelse(Educ == "Sup", "Yes", "No")),
    BornFr = as.factor(ifelse(FrenchYNBaseline == "France", "No", "Yes")),
    EverUsedECS = as.factor(ifelse(UsedECEC == "Yes", "Yes", "No")),
    PlanToUseECS = as.factor(ifelse(ECSPlanToBaseline == TRUE, "Yes", "No")),
    HighECSCov = as.factor(ifelse(HighLowECECBaseline == "High ECEC covering", "Yes", "No")),
    DepParis = as.factor(ifelse(Dep == "75", "Yes", "No")),
    WorkPlanTo = as.factor(ifelse(WorkPlanTo == TRUE, "Yes", "No")),
    BabyFemale = as.factor(ifelse(BabyFemale == TRUE, "Yes", "No")),
    Primipare = as.factor(ifelse(Primipare == TRUE, "Yes", "No")),
    ComputerAccess = as.factor(ifelse(ComputerYN == "Oui", "Yes", "No")),
    Age = Age,  # Garde numérique
    NumberOfChildren = as.numeric(NumberChildren),  # Garde numérique
    InfoBaseline = as.factor(ifelse(InfoBaseline == "Low knowledge", "Yes", "No")),
    FmilyEarnLessThan2500 = as.factor(ifelse(FmilyEarnLessThan2500 == "Yes", "Yes", "No")),
    DescriptiveNorms = as.factor(ifelse(DescriptiveNorms == "Yes", "Yes", "No")),
    NormsOpposedYes = as.factor(ifelse(NormsOpposedYes == "Yes", "Yes", "No")),
    LikertReturnHK = as.factor(ifelse(LikertReturnHK1or0 == "Yes", "Yes", "No")),
    TrustCreche = as.factor(ifelse(TrustCreche1or0 == "Yes", "Yes", "No")),
    Discount50 = as.factor(ifelse(Discount501or0 == 1, "Yes", "No")),
    ECSApp = as.factor(ifelse(ECSApp == 1, "Yes", "No")),
    AppCreche = as.factor(ifelse(AppCreche == 1, "Yes", "No")),
    ECSUseYes = as.factor(ifelse(ECSUseYes == 1, "Yes", "No")),
    UseCreche = as.factor(ifelse(UseCreche == 1, "Yes", "No"))
  ) %>%
  select(
    Educ, SingleMum, Age, Primipare, NumberOfChildren, BornFr,
    FmilyEarnLessThan2500, Discount50, InfoBaseline, Active,
    WorkPlanTo, EverUsedECS, PlanToUseECS, ComputerAccess,
    LikertReturnHK, TrustCreche, DescriptiveNorms, NormsOpposedYes,
    DepParis, HighECSCov, BabyFemale, ECSApp, AppCreche,
    ECSUseYes, UseCreche
  )

# Création du tableau descriptif
tbl_summary <- tabDes_temp  %>%
  tbl_summary(
    type = list(
      Age ~ "continuous2",
      NumberOfChildren ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
      all_categorical() ~ c("{n} ({p}%)")
    ),
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    label = list(
      Educ ~ "The mother is high-SES (Post-secondary education)",
      SingleMum ~ "Single-parent family",
      Age ~ "Age of the mother",
      Primipare ~ "The household is primiparous",
      NumberOfChildren ~ "Number of children in the household",
      BornFr ~ "The mother has a migration background",
      FmilyEarnLessThan2500 ~ "The household earns less than €2,500 per month",
      Discount50 ~ "The mother is present orientated",
      InfoBaseline ~ "The mother has low knowledge about early childcare",
      Active ~ "The mother is active at baseline",
      WorkPlanTo ~ "The mother wants to work after maternity leaves",
      EverUsedECS ~ "The household already accessed early childcare in the past",
      PlanToUseECS ~ "The mother wants to use early childcare",
      ComputerAccess ~ "The household has access to a computer",
      LikertReturnHK ~ "The mother believe in early childcare benefits",
      TrustCreche ~ "The mother trusts early childcare",
      DescriptiveNorms ~ "The majority of friends and relatives use early childcare",
      NormsOpposedYes ~ "The mother perceives social approval for using early childcare",
      DepParis ~ "The mother lives in Paris",
      HighECSCov ~ "Early childcare coverage is high",
      BabyFemale ~ "Child is a girl",
      ECSApp ~ "Applied to any early childcare facility at endline",
      AppCreche ~ "Applied to any daycare center at endline",
      ECSUseYes ~ "Accessed any early childcare facility at endline",
      UseCreche ~ "Accessed any daycare center at endline"
    )
  ) %>%
  modify_header(label = "**Variable**",
                stat_0 = "**n = {N}**") %>%
  modify_caption("Table 1: Descriptive Statistics") %>%
  bold_labels()

# Convert to flextable for Rmd output
ft <- tbl_summary %>%
  as_flex_table() %>%
  theme_booktabs() %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding = 3, part = "all") %>%
  align(align = "left", part = "all") %>%
  autofit()

ft
#---------------------- DiffEduc --------------------


tabVarEduc <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France", 0, 1),
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
    "The household is primiparous" = Primipare1or0,
    "Number of children in the household" = NumberOfChildren,
    "The mother has a migration background" = BornFr1or0,
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
    by = "The mother has a migration background",  # 
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
                stat_1 ~ "Future orientated",  # Changé ici
                stat_2 ~ "Present orientated") %>%  # Changé ici
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
    BornFr1or0  = ifelse(FrenchYNBaseline == "France",0, 1),
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
         "The household is primiparous" = Primipare1or0,
         "Number of children in the household" = NumberOfChildren,
         "The mother has a migration background" = BornFr1or0,
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
                        "PresentOrientated", "UsedECEC", "ActiveBaseline", 
                        "LikertReturnHK1or0", "TrustCreche1or0", "DescriptiveNorms",
                        "NormsOpposedYes"
                        )]

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
                   "Temporal Orientation", "Previous early childcare use", 
                   "Activity", "Believe in early childcare benefits ", "Trust in early childcare", 
                   "Descriptive norms", "Prescriptive norms"
                   )

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
# title( main="Tetrachoric Correlation Coefficients of the Main Variables", 
 #     line=1,    # Adjust the vertical position of the title
  #    cex.main=1)  # Adjust the size of the title

#corr
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


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "NormsOpposedYes", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

# App Att
Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


cm <- c('T2-C'    = 'Information + Support vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms$ModelSummary$tidy= Het.ATT.App.Norms$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms$ModelSummary$tidy= Het.ATT.Use.Norms$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary,
                  "Early childcare access_ATT"                   =Het.ATT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#------------- NormsT1-----------------------


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "NormsOpposedYes", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


cmInfo <- c('T1-C'    = 'Information only vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cmInfo,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#----------- InteractionSESNorms -----------


# For Early Childcare
Het.ITT.App.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.App.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ITT.Use.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.Use.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

# Separate the interaction terms for Early Childcare
Het.ITT.App.NormsEduc$ModelSummary$tidy <- Het.ITT.App.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ITT.App.NormsEduc$ModelSummary0$tidy <- Het.ITT.App.NormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ATT.App.NormsEduc$ModelSummary$tidy <- Het.ATT.App.NormsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Norms","SES"))

Het.ITT.Use.NormsEduc$ModelSummary$tidy <- Het.ITT.Use.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ITT.Use.NormsEduc$ModelSummary0$tidy <- Het.ITT.Use.NormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ATT.Use.NormsEduc$ModelSummary$tidy <- Het.ATT.Use.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.NormsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.NormsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.NormsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.NormsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.NormsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.NormsEduc$ModelSummary),
             shape = term + Norms + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and perceived prescriptive norms",
             notes = paste("
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

#---------------------- NormsDaycare --------------------



Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ATT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
Het.ATT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms.Daycare$ModelSummary$tidy= Het.ATT.App.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms.Daycare$ModelSummary$tidy= Het.ATT.Use.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects of the information only treatment on application and access to daycare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#---------------------- NormsDaycareT1 -------------------


Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cmInfo <- c('T1-C'    = 'Information only vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cmInfo,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects of the information only treatment on application and access to daycare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")
#---------------------- NormsDecriptive --------------------


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")



Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


# App Att
Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")



cm <- c('T2-C'    = 'Information + Support vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms$ModelSummary$tidy= Het.ATT.App.Norms$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms$ModelSummary$tidy= Het.ATT.Use.Norms$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary,
                  "Early childcare access_ATT"                   =Het.ATT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#---------------------- NormsDecriptiveT1 --------------------


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")



Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


cm <- c('T1-C'    = 'Information only vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#---------------------- NormsDaycareDescriptive --------------------
Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
Het.ATT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ATT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cm <- c('T2-C'    = 'Information + Support vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms.Daycare$ModelSummary$tidy= Het.ATT.App.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms.Daycare$ModelSummary$tidy= Het.ATT.Use.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by  whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

#----------------------- DescriptiveDaycareT1 ------------------------
Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cm <- c('T1-C'    = 'Information only vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by  whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#----------- InteractionSESDescriptiveNorms -----------

# For Early Childcare
Het.ITT.App.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSApp",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = TRUE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ATT.App.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSApp",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = FALSE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ITT.Use.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSUseYes",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = TRUE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ATT.Use.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSUseYes",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = FALSE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

# Separate the interaction terms for Early Childcare
Het.ITT.App.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ITT.App.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.App.DescriptiveNormsEduc$ModelSummary0$tidy <- Het.ITT.App.DescriptiveNormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ATT.App.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ATT.App.DescriptiveNormsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.Use.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ITT.Use.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0$tidy <- Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ATT.Use.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ATT.Use.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.DescriptiveNormsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.DescriptiveNormsEduc$ModelSummary),
             shape = term + DescriptiveNorms + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and descriptive social norms",
             notes = paste("
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
#---------------------- BirthDate --------------------

##Baby birth date
# App itt              
Het.ITT.App.Birth <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "BabyBornJanuary", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Birth.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "BabyBornJanuary",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Birth <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "BabyBornJanuary",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Birth.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "BabyBornJanuary",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

# App Att
Het.ATT.App.Birth <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "BabyBornJanuary",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.App.Birth.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "BabyBornJanuary",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ATT.Use.Birth <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "BabyBornJanuary",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.Use.Birth.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "BabyBornJanuary",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
cm <- c('T2-C'    = 'Information + Support vs Control')

# Early childcare table
Het.ITT.App.Birth$ModelSummary0$tidy= Het.ITT.App.Birth$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Birth$ModelSummary$tidy= Het.ATT.App.Birth$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Birth$ModelSummary0$tidy= Het.ITT.Use.Birth$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Birth$ModelSummary$tidy= Het.ATT.Use.Birth$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Birth$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Birth$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Birth$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Birth$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Birth$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Birth$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether the baby was born before or after January",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

# Daycare table 
Het.ITT.App.Birth.Daycare$ModelSummary0$tidy= Het.ITT.App.Birth.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Birth.Daycare$ModelSummary$tidy= Het.ATT.App.Birth.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Birth.Daycare$ModelSummary0$tidy= Het.ITT.Use.Birth.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Birth.Daycare$ModelSummary$tidy= Het.ATT.Use.Birth.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
cm <- c('T2-C'    = 'Information + Support vs Control')

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Birth.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Birth.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Birth.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Birth.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Birth.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Birth.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by whether the baby was born before or after January",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

#----------  LikertReturnHK1or0 -------------------


# App itt              
Het.ITT.App.Dev <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                           Outcome = "ECSApp",
                                           Heterogeneity = "LikertReturnHK1or0", 
                                           ITT = TRUE,
                                           Weights = "WeightPS",
                                           clusters = "StrataWave")



Het.ITT.Use.Dev <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                           Outcome = "ECSUseYes",
                                           Heterogeneity = "LikertReturnHK1or0",
                                           ITT = TRUE,
                                           Weights = "WeightPS",
                                           clusters = "StrataWave")


# App Att
Het.ATT.App.Dev <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                           Outcome = "ECSApp",
                                           Heterogeneity = "LikertReturnHK1or0",
                                           ITT = FALSE,
                                           Weights = "WeightPS",
                                           clusters = "StrataWave")


Het.ATT.Use.Dev <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                           Outcome = "ECSUseYes",
                                           Heterogeneity = "LikertReturnHK1or0",
                                           ITT = FALSE,
                                           Weights = "WeightPS",
                                           clusters = "StrataWave")

# Early childcare table
Het.ITT.App.Dev$ModelSummary0$tidy= Het.ITT.App.Dev$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Dev$ModelSummary$tidy= Het.ATT.App.Dev$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Dev$ModelSummary0$tidy= Het.ITT.Use.Dev$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Dev$ModelSummary$tidy= Het.ATT.Use.Dev$ModelSummary$tidy %>% filter(term == "T2-C")

cm <- c('T2-C'    = 'Information + Support vs Control')

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Dev$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Dev$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Dev$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Dev$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Dev$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Dev$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                        "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by beliefs in early childcare benefits for child development",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")



#-------------- BeliefsReturnDaycare --------------------------

Het.ATT.App.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "LikertReturnHK1or0",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ITT.App.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "LikertReturnHK1or0",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")
Het.ITT.Use.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "LikertReturnHK1or0",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")


Het.ATT.Use.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "LikertReturnHK1or0",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Daycare table 
Het.ITT.App.Dev.Daycare$ModelSummary0$tidy= Het.ITT.App.Dev.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Dev.Daycare$ModelSummary$tidy= Het.ATT.App.Dev.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Dev.Daycare$ModelSummary0$tidy= Het.ITT.Use.Dev.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Dev.Daycare$ModelSummary$tidy= Het.ATT.Use.Dev.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

cm <- c('T2-C'    = 'Information + Support vs Control')


modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Dev.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Dev.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Dev.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Dev.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Dev.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Dev.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                        "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by beliefs in early childcare benefits for child development",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

#----------------  TrustCreche1or0 ----------------

# App itt              
Het.ITT.App.Trust <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                             Outcome = "ECSApp",
                                             Heterogeneity = "TrustCreche1or0", 
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")


Het.ITT.Use.Trust <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "TrustCreche1or0",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")



# App Att
Het.ATT.App.Trust <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                             Outcome = "ECSApp",
                                             Heterogeneity = "TrustCreche1or0",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")



Het.ATT.Use.Trust <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "TrustCreche1or0",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")



# Early childcare table
Het.ITT.App.Trust$ModelSummary0$tidy= Het.ITT.App.Trust$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Trust$ModelSummary$tidy= Het.ATT.App.Trust$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Trust$ModelSummary0$tidy= Het.ITT.Use.Trust$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Trust$ModelSummary$tidy= Het.ATT.Use.Trust$ModelSummary$tidy %>% filter(term == "T2-C")

cm <- c('T2-C'    = 'Information + Support vs Control')


modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Trust$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Trust$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Trust$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Trust$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Trust$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Trust$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by trust in early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

#--------------------- TrustDaycare -----------------

Het.ITT.App.Trust.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "TrustCreche1or0",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ITT.Use.Trust.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "TrustCreche1or0",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ATT.App.Trust.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "TrustCreche1or0",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ATT.Use.Trust.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "TrustCreche1or0",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
# Daycare table 
Het.ITT.App.Trust.Daycare$ModelSummary0$tidy= Het.ITT.App.Trust.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Trust.Daycare$ModelSummary$tidy= Het.ATT.App.Trust.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Trust.Daycare$ModelSummary0$tidy= Het.ITT.Use.Trust.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Trust.Daycare$ModelSummary$tidy= Het.ATT.Use.Trust.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Trust.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Trust.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Trust.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Trust.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Trust.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Trust.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by trust in early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#---------------- Attitudes ----------------
#################### For AttitudeScoreMoreThanMedian ####################
# App itt              
Het.ITT.App.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian", 
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ITT.App.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = TRUE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.Use.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ITT.Use.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = TRUE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# App Att
Het.ATT.App.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ATT.App.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ATT.Use.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ATT.Use.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Early childcare table
Het.ITT.App.Attitude$ModelSummary0$tidy= Het.ITT.App.Attitude$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Attitude$ModelSummary$tidy= Het.ATT.App.Attitude$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Attitude$ModelSummary0$tidy= Het.ITT.Use.Attitude$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Attitude$ModelSummary$tidy= Het.ATT.Use.Attitude$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Attitude$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Attitude$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Attitude$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Attitude$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Attitude$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Attitude$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by attitudes towards early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

# Daycare table 
Het.ITT.App.Attitude.Daycare$ModelSummary0$tidy= Het.ITT.App.Attitude.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Attitude.Daycare$ModelSummary$tidy= Het.ATT.App.Attitude.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Attitude.Daycare$ModelSummary0$tidy= Het.ITT.Use.Attitude.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Attitude.Daycare$ModelSummary$tidy= Het.ATT.Use.Attitude.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Attitude.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Attitude.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Attitude.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Attitude.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Attitude.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Attitude.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by attitudes towards early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#----------- InteractionSESBeliefs -----------
# For Early Childcare
Het.ITT.App.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                       mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                     Outcome = "ECSApp",
                                                     Heterogeneity = "BeliefsEduc",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.App.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                       mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                     Outcome = "ECSApp",
                                                     Heterogeneity = "BeliefsEduc",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.Use.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                       mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                     Outcome = "ECSUseYes",
                                                     Heterogeneity = "BeliefsEduc",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.Use.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                       mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                     Outcome = "ECSUseYes",
                                                     Heterogeneity = "BeliefsEduc",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")


# Separate the interaction terms for Early Childcare
Het.ITT.App.BeliefsEduc$ModelSummary$tidy <- Het.ITT.App.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.App.BeliefsEduc$ModelSummary0$tidy <- Het.ITT.App.BeliefsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ATT.App.BeliefsEduc$ModelSummary$tidy <- Het.ATT.App.BeliefsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.Use.BeliefsEduc$ModelSummary$tidy <- Het.ITT.Use.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.Use.BeliefsEduc$ModelSummary0$tidy <- Het.ITT.Use.BeliefsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ATT.Use.BeliefsEduc$ModelSummary$tidy <- Het.ATT.Use.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))


# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.BeliefsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.BeliefsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.BeliefsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.BeliefsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.BeliefsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.BeliefsEduc$ModelSummary),
             shape = term + Beliefs + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and beliefs in the benefits of early childcare for child development",
             notes = paste("
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

#------------ InteractionBeliefEducDaycare ------------

# For Daycare
Het.ITT.App.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "AppCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = TRUE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ATT.App.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "AppCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = FALSE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ITT.Use.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "UseCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = TRUE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ATT.Use.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "UseCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = FALSE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

# Separate the interaction terms for Daycare
Het.ITT.App.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ITT.App.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0$tidy <- Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ATT.App.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ATT.App.BeliefsEduc.Daycare$ModelSummary$tidy %>%
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0$tidy <- Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Beliefs","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Daycare
modelsummary(list("Daycare application_Control mean" = Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.App.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare application_ATT" = Het.ATT.App.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare access_Control mean" = Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare access_ATT" = Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary),
             shape = term + Beliefs + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by level of education and Beliefss towards early childcare",
             notes = paste("
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

#----------- InteractionSESTrust -----------
# For Early Childcare
Het.ITT.App.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                       mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                     Outcome = "ECSApp",
                                                     Heterogeneity = "TrustEduc",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.App.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                       mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                     Outcome = "ECSApp",
                                                     Heterogeneity = "TrustEduc",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.Use.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                       mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                     Outcome = "ECSUseYes",
                                                     Heterogeneity = "TrustEduc",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.Use.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                       mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                     Outcome = "ECSUseYes",
                                                     Heterogeneity = "TrustEduc",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")



# Separate the interaction terms for Early Childcare
Het.ITT.App.TrustEduc$ModelSummary$tidy <- Het.ITT.App.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.App.TrustEduc$ModelSummary0$tidy <- Het.ITT.App.TrustEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.App.TrustEduc$ModelSummary$tidy <- Het.ATT.App.TrustEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc$ModelSummary$tidy <- Het.ITT.Use.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc$ModelSummary0$tidy <- Het.ITT.Use.TrustEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.Use.TrustEduc$ModelSummary$tidy <- Het.ATT.Use.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))


# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.TrustEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.TrustEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.TrustEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.TrustEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.TrustEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.TrustEduc$ModelSummary),
             shape = term + Trust + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and Trusts towards early childcare",
             notes = paste("
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


#------------ InteractionTrustEducDaycare ------------

# For Daycare
Het.ITT.App.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                            mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                          Outcome = "AppCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = TRUE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ATT.App.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                            mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                          Outcome = "AppCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = FALSE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ITT.Use.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                            mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                          Outcome = "UseCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = TRUE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ATT.Use.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                            mutate(TrustEduc=interaction(TrustCreche1or0,Educ2)),
                                                          Outcome = "UseCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = FALSE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

# Separate the interaction terms for Daycare
Het.ITT.App.TrustEduc.Daycare$ModelSummary$tidy <- Het.ITT.App.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.App.TrustEduc.Daycare$ModelSummary0$tidy <- Het.ITT.App.TrustEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.App.TrustEduc.Daycare$ModelSummary$tidy <- Het.ATT.App.TrustEduc.Daycare$ModelSummary$tidy %>%
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc.Daycare$ModelSummary$tidy <- Het.ITT.Use.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc.Daycare$ModelSummary0$tidy <- Het.ITT.Use.TrustEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.Use.TrustEduc.Daycare$ModelSummary$tidy <- Het.ATT.Use.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Daycare
modelsummary(list("Daycare application_Control mean" = Het.ITT.App.TrustEduc.Daycare$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.App.TrustEduc.Daycare$ModelSummary,
                  "Daycare application_ATT" = Het.ATT.App.TrustEduc.Daycare$ModelSummary,
                  "Daycare access_Control mean" = Het.ITT.Use.TrustEduc.Daycare$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.Use.TrustEduc.Daycare$ModelSummary,
                  "Daycare access_ATT" = Het.ATT.Use.TrustEduc.Daycare$ModelSummary),
             shape = term + Trust + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by level of education and Trusts towards early childcare",
             notes = paste("
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

#--------------- GraphReasonsNoECS ----------------------------

# create the data for the plot by SES
ecs_reasons_simple <- MainDB %>%
  filter(!is.na(ECSNoECSAppAnswer_long)) %>%
  group_by(ECSNoECSAppAnswer_long) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count) * 100, 
         total = sum(count)) %>%
  arrange(desc(count))


# Create the plot
ggplot(ecs_reasons_simple, 
       aes(x = reorder(ECSNoECSAppAnswer_long, percentage), 
           y = percentage, 
           fill = ECSNoECSAppAnswer_long)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Percentage (%)") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2,
            size = 4)  # Increase percentage labels size

#---------- ReasonsSES --------
ecs_reasons_simple <- MainDB %>%
  filter(!is.na(ECSNoECSAppAnswer_long)) %>%
  group_by(ECSNoECSAppAnswer_long, Educ2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count) * 100, 
         total = sum(count)) %>%
  arrange(desc(count))

# Create the plot
ggplot(ecs_reasons_simple, 
       aes(x = reorder(ECSNoECSAppAnswer_long, total), 
           y = total, 
           fill = Educ2)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Count") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  )# +
  #geom_text(aes(label = sprintf("%.1f%%", percentage)), 
          #  hjust = -0.2,
            size = 4)  # Increase percentage labels size


#--------------- GraphReasonIdealSES ----------------------------

# Convert to character
MainDB$ECSIdealNotReasonClean <- as.character(MainDB$ECSIdealNotReasonClean)

# Calculate results
# By education level
results_educ <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$Educ2)

# Create plots
# For education level
create_reasons_plot(results_educ, "Reasons for Not Getting Preferred Childcare by SES")


#--------------- GraphReasonIdealMigration ----------------------------
# By migration background
results_migr <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$FrenchYNBaseline)

# For migration background
create_reasons_plot(results_migr, "Reasons for Not Getting Preferred Childcare by Migration Background")


#--------------- GraphReasonIdealAll ----------------------------


results_educ <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$Educ2)

results <- results_educ %>% 
  group_by(reason) %>%
  summarise(count_tot = sum(count)) %>%
  mutate(   total = sum(count_tot),
    percentage = count_tot/sum(count_tot)*100) %>%
  arrange(desc(count_tot))

# Create the plot
ggplot(results, 
       aes(x = reorder(reason, percentage), 
           y = percentage, 
           fill = reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  #scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Percentage (%)") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2,
            size = 4)  # Increase percentage labels size
