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
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0)
  ) %>% 
  select(
    "SES" = Educ2,
    "Single-parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children in the household" = NumberOfChildren3,
    # "The mother has no child" = Primipare1or0,
    "The mother is born in France" = BornFr1or0,
   # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present orientated" = Discount501or0,
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
    by = "SES",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference(include = -matches("Number of children in the household"))%>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "High-SES",
                stat_2 ~ "Low-SES") %>%
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
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0)
  ) %>% 
  select(
    "SES" = Educ2,
    "Single-parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children in the household" = NumberOfChildren3,
    # "The mother has no child" = Primipare1or0,
    "The mother is born in France" = BornFr1or0,
    # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present orientated" = Discount501or0,
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
    by = "SES",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference(include = -matches("Number of children in the household"))%>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "High-SES",
                stat_2 ~ "Low-SES") %>%
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


