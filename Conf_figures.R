## Conference Script

#------------ Intention Action gap ------------
#------ AttentionAction --------
# The idea is to plot the average difference between high/low SES for 3 outcomes
# outcomes <- c("ECSPlanToBaseline", "ECSApp", "ECSUseYes")
# Simplest, fastest way of doing that: OLS 
DiffControls <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~Educ2,
                      MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                      #weights = ~WeightBalance,
                      se="hetero")
DiffControlsFr <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~i(MigrationBackground,ref="No"),
                        MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                        #weights = ~WeightBalance,
                        se="hetero")
res <-  DiffControls %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Baseline education")%>% 
  mutate(Outcome=str_remove(model,'lhs:'),term=ifelse(str_detect(term,'Low-SES'),'Gap by SES','Mean High SES'))
resb <-  DiffControlsFr %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Migration background")%>% mutate(Outcome=str_remove(model,'lhs:'),
                                                                                                              term=ifelse(str_detect(term,'Yes'),'Gap by migration background','Mean French'))
res2 <- bind_rows(res,resb) %>% mutate(OutcomeLabel=factor(case_when(str_detect(Outcome,"ECSPlanToBaseline")~"Intend to use",
                                                                     str_detect(Outcome,"ECSApp")~"Apply",
                                                                     str_detect(Outcome,"ECSUseYes")~"Access"),
                                                           levels=c("Intend to use","Apply","Access")
))
# stack the two results and use "Heterogeneity" for facets
Stack.Intend.Gap <- res2 %>% 
  mutate(termPlot=factor(term,
                         levels=c('Gap by SES','Gap by migration background')))

# Définir un thème pour les présentations
presentation_theme <- theme_minimal() +
  theme(
    # Agrandir tous les textes
    text = element_text(size = 18),
    # Titres de facettes plus visibles
    strip.text = element_text(size = 20, face = "bold"),
    # Texte des axes plus grand
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    # Légende plus visible
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    # Plus d'espace pour la lisibilité
    plot.margin = margin(20, 20, 20, 20),
    # Notes de bas de page plus lisibles
    plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 20)),
    # Grille moins visible pour ne pas interférer avec les barres
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# Version améliorée du graphique
Intend.Gap.plot = 
  ggplot(Stack.Intend.Gap %>% filter(str_detect(term,"Gap"))) +
  geom_bar(aes(x = OutcomeLabel, y = estimate, fill = OutcomeLabel), 
           stat = 'identity', alpha = 0.6, width = 0.7) +
  geom_errorbar(aes(x = OutcomeLabel, ymin = conf.low, ymax = conf.high, 
                    color = OutcomeLabel), width = 0.4, size = 1) +
  facet_wrap(~termPlot, scales = "free_y") +
  scale_x_discrete(name = "Outcomes") +
  scale_fill_viridis_d("Outcomes", alpha = 0.9, option = "A", end = 0.8) +
  scale_color_viridis_d("Outcomes", alpha = 1, option = "A", end = 0.8) +
  labs(
    title = 'Intention-to-access gaps across subgroups',
    y = "Estimate",
    caption = "Sources: Control group only. Intention (Q4 2022); application and access (Q4 2023).\n Coefficients of OLS regressions of the outcomes on dummies for the group variable of interest. Error bars indicate 
pointwise 95% CI based on heteroskedasticity-robust standard errors (HC1). Intention-to-action gap in early childcare application 
and access gap in the control group across SES and migration background:
- Gap by SES compares households (HH) in which the mother did not attend any kind of post-secondary education 
with those in which the mother did.
- Gap by migration background compares HH in which the mother was born abroad with HH in which the mother was born in France."
  ) +
  presentation_theme

# Afficher le graphique
Intend.Gap.plot

# Pour sauvegarder en haute résolution pour présentations
# ggsave("gap_plot_presentation.png", Intend.Gap.plot, width = 12, height = 8, dpi = 300)

#------------ T2Effect ------------

# [Code initial précédent maintenu...]

#### Code modifié pour le graphique final avec police plus grande ####

Data.Het.EducMig <- bind_rows(DataPlot,DataPlotUse)

# Définir un thème pour améliorer la lisibilité tout en gardant le style original
vis_theme <- theme(
  # Agrandir tous les textes
  text = element_text(size = 16),
  # Titres de facettes plus grands
  strip.text = element_text(size = 18, face = "bold"),
  # Texte des axes plus grand
  axis.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 16),
  # Légende plus visible
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 16),
  # Notes de bas de page plus lisibles
  plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15)),
  # Plus d'espace global
  plot.margin = margin(15, 15, 15, 15)
)

# Graphique avec style original mais polices plus grandes
ggplot(Data.Het.EducMig)+
  geom_pointrange(aes(
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group), 
    position = position_dodge(.4),
    size = 0.8) +  # Augmenter légèrement la taille des points et lignes
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    y = "Estimate",  # Ajout d'un titre d'axe y plus clair
    caption = paste("Sources:", SourcesStacked,
                    "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block fixed effects")
  ) +
  vis_theme  # Appliquer le thème pour améliorer la visibilité

# Pour sauvegarder en haute résolution pour présentations
# ggsave("heterogeneity_plots_presentation.png", width = 12, height = 8, dpi = 300)

#------------- T1Graph ------------------------

# [Code initial maintenu jusqu'au graphique final]

#### Modification du graphique final avec police plus grande ####

Data.Het.EducMig <- bind_rows(DataPlot,DataPlotUse)

# Thème pour améliorer la visibilité tout en gardant le style original
vis_theme <- theme(
  # Agrandir tous les textes
  text = element_text(size = 16),
  # Titres de facettes plus grands
  strip.text = element_text(size = 18, face = "bold"),
  # Texte des axes plus grand
  axis.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 16),
  # Légende plus visible
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 16),
  # Notes de bas de page plus lisibles
  plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15)),
  # Plus d'espace global
  plot.margin = margin(15, 15, 15, 15)
)

# Graphique avec style original mais polices plus grandes
ggplot(Data.Het.EducMig)+
  geom_pointrange(aes(
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),
    position = position_dodge(.4),
    size = 0.8) +  # Augmenter légèrement la taille des points et lignes
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    y = "Estimate",  # Ajout d'un titre d'axe y plus clair
    caption = paste("Sources:", SourcesStacked,
                    "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Intervals (CI).",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block fixed effects")
  ) +
  vis_theme  # Appliquer le thème pour améliorer la visibilité

# Pour sauvegarder en haute résolution pour présentations
# ggsave("heterogeneity_t1_plots_presentation.png", width = 12, height = 8, dpi = 300)

#------------- InfoMechanisms ------------------------

# [Code initial maintenu jusqu'au premier graphique]

# Pour sauvegarder en haute résolution pour présentations
# ggsave("daycare_plots_presentation.png", daycare_plot, width = 12, height = 8, dpi = 300)

# [Code intermédiaire maintenu...]

#### Modification du deuxième graphique - Information Frictions ####

# Graphique 2: Information Frictions avec style original mais polices plus grandes
info_frictions_plot <- ggplot(Data.Het.InfoFriction)+
  geom_pointrange(aes(
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),
    position = position_dodge(.4),
    size = 0.8) +  # Augmenter légèrement la taille des points et lignes
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.InfoFriction %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    y = "Estimate",  # Ajout d'un titre d'axe y plus clair
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block fixed effects")
  ) +
  vis_theme  # Appliquer le thème pour améliorer la visibilité

# Afficher le graphique des frictions d'information
info_frictions_plot

# Pour sauvegarder en haute résolution pour présentations
# ggsave("info_frictions_plots_presentation.png", info_frictions_plot, width = 14, height = 10, dpi = 300)

#------------- PsyMechanisms ------------------------

# [Code initial maintenu jusqu'au graphique final]

#### Modification du graphique des mécanismes psychologiques ####

Data.Het.Psy <- bind_rows(DataPlot,DataPlotUse)

# Thème pour améliorer la visibilité tout en gardant le style original
vis_theme <- theme(
  # Agrandir tous les textes
  text = element_text(size = 16),
  # Titres de facettes plus grands
  strip.text = element_text(size = 18, face = "bold"),
  # Texte des axes plus grand
  axis.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 16),
  # Légende plus visible
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 16),
  # Notes de bas de page plus lisibles
  plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15)),
  # Plus d'espace global
  plot.margin = margin(15, 15, 15, 15)
)

# Graphique des mécanismes psychologiques avec style original mais polices plus grandes
psych_mechanism_plot <- ggplot(Data.Het.Psy)+
  geom_pointrange(aes(
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),
    position = position_dodge(.4),
    size = 0.8) +  # Augmenter légèrement la taille des points et lignes
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.Psy %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    y = "Estimate",  # Ajout d'un titre d'axe y plus clair
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) +
  vis_theme  # Appliquer le thème pour améliorer la visibilité

# Afficher le graphique
psych_mechanism_plot

# Pour sauvegarder en haute résolution pour présentations
# ggsave("psych_mechanism_plots_presentation.png", psych_mechanism_plot, width = 14, height = 9, dpi = 300)
#------------- Daycare ------------------------


# [Code initial maintenu jusqu'au graphique final]

#### Modification du graphique pour meilleure visibilité sur slides ####

Data.Het.Daycare.EducMig <- bind_rows(DataPlot,DataPlotUse)

# Thème pour améliorer la visibilité tout en gardant le style original
vis_theme <- theme(
  # Agrandir tous les textes
  text = element_text(size = 16),
  # Titres de facettes plus grands
  strip.text = element_text(size = 18, face = "bold"),
  # Texte des axes plus grand
  axis.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 16),
  # Légende plus visible
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 16),
  # Notes de bas de page plus lisibles
  plot.caption = element_text(size = 14, hjust = 0, margin = margin(t = 15)),
  # Plus d'espace global
  plot.margin = margin(15, 15, 15, 15)
)

# Graphique avec style original mais polices plus grandes
daycare_educmig_plot <- ggplot(Data.Het.Daycare.EducMig)+
  geom_pointrange(aes(
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),
    position = position_dodge(.4),
    size = 0.8) +  # Augmenter légèrement la taille des points et lignes
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.Daycare.EducMig %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    y = "Estimate",  # Ajout d'un titre d'axe y plus clair
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block fixed effects")
  ) +
  vis_theme  # Appliquer le thème pour améliorer la visibilité

# Afficher le graphique
daycare_educmig_plot

# Pour sauvegarder en haute résolution pour présentations
# ggsave("daycare_educmig_plots_presentation.png", daycare_educmig_plot, width = 12, height = 8, dpi = 300)