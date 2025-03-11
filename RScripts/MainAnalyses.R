#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
#---------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                -------------
#--------------------                            Main analysis                             -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                               Version 1                              -----------------------------------#  
#--------------------                               June 2024                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


# Instructions and comments for replication
# This file is built with chunk labels than are called from the main Rmarkdown document.
# The file should be run from the R project "Analysis_Notebooks".
# These chunks are to be launched after running the package loading and data wrangling scripts.



#------ AttentionAction --------
# The idea is to plot the average difference between high/low SES for 3 outcomes
# outcomes <- c("ECSPlanToBaseline", "ECSApp", "ECSUseYes")

# Simplest, fastest way of doing that: OLS 
DiffControls <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~Educ2,
                      MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                      #weights = ~WeightBalance,
                      se="hetero")

DiffControlsFr <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~i(MigrationBackground,ref="France"),
                        MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                        #weights = ~WeightBalance,
                        se="hetero")

res <-  DiffControls %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Baseline education")%>% 
  mutate(Outcome=str_remove(model,'lhs:'),term=ifelse(str_detect(term,'Low-SES'),'Gap by SES','Mean High SES'))

resb <-  DiffControlsFr %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Country of birth")%>% mutate(Outcome=str_remove(model,'lhs:'),
                                                                                                          term=ifelse(str_detect(term,'Abroad'),'Gap by migration background','Mean French'))

res2 <- bind_rows(res,resb) %>% mutate(OutcomeLabel=factor(case_when(str_detect(Outcome,"ECSPlanToBaseline")~"Intend to use",
                                                                     str_detect(Outcome,"ECSApp")~"Apply",
                                                                     str_detect(Outcome,"ECSUseYes")~"Access"),
                                                           levels=c("Intend to use","Apply","Access")
))


# stack the two results and use "Heterogeneity" for facets
Stack.Intend.Gap <- res2 %>% 
  mutate(termPlot=factor(term,
                         levels=c('Gap by SES','Gap by migration background')))


Intend.Gap.plot = 
  ggplot(Stack.Intend.Gap %>% filter(str_detect(term,"Gap")))+geom_bar(aes(x=OutcomeLabel,y=estimate,fill=OutcomeLabel),stat='identity',alpha=.4)+
  geom_errorbar(aes(x=OutcomeLabel,ymin=conf.low,
                    ymax =conf.high,
                    ,color=OutcomeLabel),width=.3)+
  facet_wrap(~termPlot)+#,scales = "free_y")+
  scale_x_discrete(name = "Outcomes")+
  scale_fill_viridis_d("Outcomes",alpha=.8,option="A",end=.6)+
  scale_color_viridis_d("Outcomes",alpha=.8,option="A",end=.6)+
  labs(#title='Intention-to-access gaps across sub-groups',
       caption=
"Sources: Control group only. Intention are measured at the baseline survey during pregnancy (Q4 2022); application and access 
are measured at the endline survey one year after (Q4 2023).
Notes: Coefficients of OLS regressions of the outcomes on dummies for the group variable of interest. Error bars indicate 
pointwise 95% CI based on heteroskedasticity-robust standard errors (HC1). Intention-to-action gap in early childcare application 
and access gap in the control group across four dimensions of heterogeneity:
- Gap by SES compares households (HH) in which the mother did not attend any kind of post-secondary education 
with those in which the mother did.
- Gap by migration background compares HH in which the mother was born abroad with HH in which the mother was born in France."
  )

Intend.Gap.plot




#----- TableITT -----

set.seed(999)


ITT.UseCreche <- ITTSimultaneous(Y="UseCreche",
                                 treat="Z",
                                 DB=PostDB,
                                 Correction="Westfall",
                                 weights="WeightPS")
ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes",
                                 treat="Z",
                                 DB=PostDB,
                                 Correction="Westfall",
                                 weights="WeightPS")
ITT.ECSApp <- ITTSimultaneous(Y="ECSApp",
                              treat="Z",
                              DB=PostDB,
                              Correction="Westfall",
                              weights="WeightPS")
ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche",
                                    treat="Z",
                                    DB=PostDB,
                                    Correction="Westfall",
                                    weights="WeightPS")

#Coef Map for clear labels
cm <- c('T1-C'    = 'Information-only vs Control ',
        'T2-C'    = 'Information + Support vs Control',
        'T2-T1'   = 'Information + support vs Information-only',
        "Control mean" = "Mean control group")

#empty <- list(tidy=as.data.frame())

MainResultTable =
      modelsummary(list(
        "Application_Early childcare"=ITT.ECSApp$ModelSummary,
        "Application_Daycare"=ITT.ECSAppCreche$ModelSummary,
        "Access_Early childcare"=ITT.ECSUseYes$ModelSummary,
        "Access_Daycare"=ITT.UseCreche$ModelSummary
      ),
      coef_map = cm,
      fmt=fmt_statistic(estimate=2, 
                        adj.p.value=3,
                        std.error=2,
                        conf.int=2,
                        "Chi 2"=2,
                        "P-value"=3), 
      estimate = '{estimate}{stars} ({std.error})',
      statistic = c("conf.int",
                    "adj.p.val. = {adj.p.value}"),
      #stars=FALSE,
      stars = c('*' = .1,'**' = .05, '***' = .01),
      gof_map = c(
        "Covariates","Fixed effects","Chi 2","P-value",
        "nobs", "r.squared","adj.r.squared"),
      title="Intention-to-treat effects on the main outcomes",
      notes=paste("Sources:", SourcesStacked,
                  "
      *= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
      Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
      Adjusted p-value and confidence intervals account for simultaneous inference using the",ITT.UseCreche$Correction, "method. 
      Each column estimates jointly the effects of the program using fully-saturated stacked regressions. Control means estimated separately by OLS.
      Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
      output = 'flextable') 


MainResultTable %>% 
  merge_at(j=1,i=c(1:3),part="body")|>   
  merge_at(j=1,i=c(4:6),part="body")|>   
  merge_at(j=1,i=c(7:9),part="body") |>   
  merge_at(j=1,i=c(10:11),part="body") |>   
  #theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=1,i=c(1:2),part="header") %>% 
  #merge_v(j = 1,part="header") %>% 
  italic(i = c(2),  part = "header") %>% 
  bold(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3,4,5),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  hline(c(9,11),part="body")



#----- HetT2ITTATT ------------


## First etimate the ITT
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            ,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

## Estimate the ATT
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


## Estimate the ATT
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")
# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.EducMig <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.EducMig)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 


#----- HetT2Daycare ------------


## First etimate the ITT
Het.ITT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")


Het.ITT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

## Estimate the ATT
Het.ATT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")
Het.ATT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")



# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.AppCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")


## Estimate the ATT
Het.ATT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")
# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.UseCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access early daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$Tidy %>% mutate(Y = "Access early daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access early daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.UseCreche.Mig$Tidy %>% mutate(Y = "Access early daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.Daycare.EducMig <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.Daycare.EducMig)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.Daycare.EducMig %>% filter( panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 


#----- MechanismsInfo ------------

#Knowledge, previous ecs use and DescriptiveNorms as proxy for information costs

#UsedECEC 

## ITT App


Het.ITT.App.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDB ,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  DescriptiveNorms=ifelse(DescriptiveNorms == "Yes","Majority","Minority")),
  Outcome = "ECSApp",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")




## ATT


Het.ATT.App.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ATT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                             Outcome = "ECSApp",
                                             Heterogeneity = "InfoBaseline",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")




Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% mutate(
  DescriptiveNorms=ifelse(DescriptiveNorms == "Yes","Majority","Minority")),
  Outcome = "ECSApp",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")




# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("Level of \nknowledge", "Used Early \nChildcare before","Share of people \nusing Early Childcare \naround")
panel_levels <- c("Control group", "ITT", "ATT")


# Het.ITT.App.UsedECEC
# Het.ITT.App.Info
# Het.ITT.App.Norms

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Info$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.UsedECEC$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.UsedECEC$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Norms$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Norms$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels)
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.Info$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Level of \nknowledge", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.UsedECEC$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Used Early \nChildcare before", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.Norms$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )


## ECS Use


Het.ITT.Use.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDB ,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ITT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  DescriptiveNorms=ifelse(DescriptiveNorms == "Yes","Majority","Minority")),
  Outcome = "ECSUseYes",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")




## ATT


Het.ATT.Use.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ATT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "InfoBaseline",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")




Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  DescriptiveNorms=ifelse(DescriptiveNorms == "Yes","Majority","Minority")),
  Outcome = "ECSUseYes",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Info$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Info$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Norms$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Norms$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.UsedECEC$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.UsedECEC$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels)
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.Info$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Level of \nknowledge", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.Norms$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.UsedECEC$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Used Early \nChildcare before", Type = "ATT") %>% filter(term %in% term_levels)
  
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


Data.Het.InfoFriction <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.InfoFriction)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.InfoFriction %>% filter( panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 

#----- MechanismsPsych ------------


#Present biased trust et active pour psychological costs


#PresentOrientated 

## ITT App


Het.ITT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.App.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  TrustCreche1or0=ifelse(TrustCreche1or0 == "Yes","High trust","Low trust")),
  Outcome = "ECSApp",
  Heterogeneity = "TrustCreche1or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "ECSApp",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")




## ATT


Het.ATT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.App.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(TrustCreche1or0=ifelse(TrustCreche1or0 == "Yes","High trust","Low trust")),
                                                       Outcome = "ECSApp",
                                                       Heterogeneity = "TrustCreche1or0",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")




Het.ATT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "ECSApp",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")




# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("Present biased", "Trust","Activity")
panel_levels <- c("Control group", "ITT", "ATT")


# Het.ITT.App.PresentOrientated
# Het.ITT.App.Info
# Het.ITT.App.ActiveBaseline

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.TrustCreche1or0$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.TrustCreche1or0$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.PresentOrientated$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.PresentOrientated$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.ActiveBaseline$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.ActiveBaseline$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels)
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.TrustCreche1or0$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Trust", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.PresentOrientated$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Present biased", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.ActiveBaseline$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Activity", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )


## ECS Use


Het.ITT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.Use.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  TrustCreche1or0=ifelse(TrustCreche1or0 == "Yes","High trust","Low trust")),
  Outcome = "ECSUseYes",
  Heterogeneity = "TrustCreche1or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "ECSUseYes",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")




## ATT


Het.ATT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1, "Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.Use.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(TrustCreche1or0=ifelse(TrustCreche1or0 == "Yes","High trust","Low trust")),
                                                       Outcome = "ECSUseYes",
                                                       Heterogeneity = "TrustCreche1or0",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")




Het.ATT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "ECSUseYes",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")

# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Trust", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.TrustCreche1or0$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.TrustCreche1or0$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.ActiveBaseline$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.ActiveBaseline$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.PresentOrientated$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.PresentOrientated$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels)
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.TrustCreche1or0$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Trust", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.ActiveBaseline$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Activity", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.PresentOrientated$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Present biased", Type = "ATT") %>% filter(term %in% term_levels)
  
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


Data.Het.Psy <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.Psy)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.Psy %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 


#------ BalanceTable ----------

## This chunk construct the balance table of covariates at baseline for the appendix
# We add more covariates for the second round, following the coments
tabVar <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(MigrationBackground == "France", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Oui", 1, 0),
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
    "Assignment" = Assignment,
    "Single-parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children in the household" = NumberOfChildren3,
    # "The mother has no child" = Primipare1or0,
    "The mother is born in France" = BornFr1or0,
    "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
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
  )

summary_baseline_variables_endline <- tabVar %>%
  tbl_summary(
    by = Assignment,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall() %>% #add_difference() %>% 
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Information only",
                stat_2 ~ "Information + support",
                stat_3 ~ "Control") %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3") ~ "**Assignment group**") %>% 
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>% 
  add_q(method = "fdr",
        pvalue_fun = ~ style_pvalue(.x, digits = 3))%>% 
  add_significance_stars(  thresholds = c(0.01, 0.05, 0.1))


summary_baseline_variables_endline %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_v(part = "header", j = 3) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1,part="body")|>  
  width(j=c(1),unit = "cm",width=3)|>
  width(j=c(2:7),unit = "cm",width=2)|>
  set_caption(caption = "Baseline balance by treatment groups") %>% 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test. Q-value control for the false discovery rate.")  %>%  fontsize(i=1,size=10,part="footer") #%>% print()


#-------- MAP --------------------

# Create a sf file with the Endline Data
rct_shapebaseline <- MainDB %>% # tauxcouv_com: endline from 2021 caf data, TAUXCOUV_COM: Baseline based on 2019 data
  #filter(!duplicated(NUMCOMBaseline)) %>%
  select(NUMCOMBaseline, CodePostalBaseline, latitude, longitude, HighLowECECBaseline, 
         TAUXCOUV_COM, tauxcouv_com, nbr_city_baseline, 
         Nom.Département, Dep, Taux.de.couv.global) %>% 
  filter(!is.na(longitude)) %>% 
  mutate(Nom.Département = case_when(
    Nom.Département == "PARIS" ~ "Paris",
    Nom.Département == "SEINE SAINT DENIS" ~ "Seine-Saint-Denis",
    Nom.Département == "VAL DE MARNE" ~ "Val-de-Marne"
  )
  ) %>% 
  mutate(NUMCOMBaseline = as.character(NUMCOMBaseline))
# %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(gp_shape)) # Some zipcodes are not right, so I remove them for now


# Perform a spatial join based on zip codes to have a file with the info we need
joined_sf_baseline <- gp_shape %>% 
  mutate(c_cainsee.ch = as.character(c_cainsee)) %>% 
  left_join(rct_shapebaseline, by = c("c_cainsee.ch" = "NUMCOMBaseline")) 

# Do the intersection of the 2 files to have only the cities where Endline participants live  
common_zipcodes <- intersect(joined_sf_baseline$c_cainsee, rct_shapebaseline$NUMCOMBaseline)

common_zipcodes_sf <- joined_sf_baseline[joined_sf_baseline$c_cainsee %in% common_zipcodes, ]


#smallsample  <- common_zipcodes_sf %>% filter(nbr_city_baseline > 40)

# D'abord, créer un objet pour les contours des départements
departements_contours <- common_zipcodes_sf %>% 
  group_by(Dep) %>% 
  summarise(geometry = st_union(geometry) ,
            Nom.Département) %>%
  ungroup()

departements_labels <- departements_contours %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])  # Ajoute un décalage vertical de 2000 unités

library("ggrepel")


# Créer la carte avec les étiquettes
ggplot() + 
  geom_sf(data = gp_shape) +   
  geom_sf(data = common_zipcodes_sf, aes(fill = as.numeric(Taux.de.couv.global))) +
  geom_sf(data = departements_contours, 
          fill = NA, 
          color = "black", 
          linewidth = 1) +
  stat_sf_coordinates(data = common_zipcodes_sf, aes(size = nbr_city_baseline)) +
  # Ajouter les étiquettes des départements
  geom_label(data = departements_labels,
             aes(x = lon, y = lat, label = Nom.Département),
             fontface = "bold",  # texte en gras
             size = 3,          # taille du texte
             color = "black") +  # couleur du texte
  scale_fill_gradientn(
    colors = c( "#A84268","#FCB97D","#9DBF9E" , "limegreen"))+
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(
    fill = "Early childcare coverage rate",
    size = "Number of participants by city"
  ) +
  theme(
    legend.box = "vertical", 
    legend.position = "bottom",
    axis.text = element_blank()
  )


#------- EXTENDED DATA ------------


#------- PostLassoMainTables----------------------

# First, run the regression without Lasso
ITT.UseCreche <- ITTSimultaneous(Y="UseCreche")
ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes")
ITT.ECSApp <- ITTSimultaneous(Y="ECSApp")
ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche")


## Bind them together 
MainITTResults <- bind_rows(ITT.ECSApp$Tidy %>% mutate(Outcome="Early childcare application",Y=Outcome),
                            ITT.ECSUseYes$Tidy %>% mutate(Outcome="Early childcare access",Y=Outcome),
                            ITT.UseCreche$Tidy %>% mutate(Outcome="Daycare access",Y=Outcome),
                            ITT.ECSAppCreche$Tidy   %>% mutate(Outcome="Daycare application",Y=Outcome),
                            #  ITT.ECSAppAssmat$Tidy   %>% mutate(Outcome="Apply for childminder")
) %>% rename("SubSample"="Var") %>% mutate(Model="ITT")


# Then Post Lasso on each outcome

# Run lasso on access to early childcare in general
ECSUseYesT1C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T1-C")
ECSUseYesT2C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-C")
ECSUseYesT2T1 <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSUseYesLasso <- bind_rows(ECSUseYesT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                ECSUseYesT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                ECSUseYesT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Early childcare access")

# Run lasso on application for early childcare in general
ECSAppT1C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T1-C")
ECSAppT2C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-C")
ECSAppT2T1 <- EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSAppLasso <- bind_rows(   ECSAppT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                ECSAppT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                ECSAppT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Early childcare application")



# Run lasso on access to daycare
USeCrecheT1C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T1-C")
USeCrecheT2C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-C")
USeCrecheT2T1 <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-T1")

#EstPostLasso()
## Stack the results
TidyUseCrecheLasso <- bind_rows(USeCrecheT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                USeCrecheT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                USeCrecheT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Daycare access")

# Run lasso on appliation for daycare
ECSAppCrecheT1C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T1-C")
ECSAppCrecheT2C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-C")
ECSAppCrecheT2T1 <- EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSAppCrechepLasso <- bind_rows(   ECSAppCrecheT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                       ECSAppCrecheT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                       ECSAppCrecheT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Daycare application")

# on récupère tous les résultats dans un dataframe et on garde que le coefficient des itt donc on filtre sur Z

ITT.PostLasso <- bind_rows(TidyUseCrecheLasso,
                           TidyECSAppCrechepLasso,
                           TidyECSAppLasso,
                           TidyECSUseYesLasso
) %>% filter(term=="Z")


# ITT.UseCreche <- ITTSimultaneous(Y="UseCreche")
# ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes")
# ITT.ECSApp <- ITTSimultaneous(Y="ECSApp")
# ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche")


# Early childcare application
# Do the tables

modelsummary(list("Information + support vs. Control_Basic"=ECSAppT2C$`Basic Model`, "Information + support vs. Control_Post-Lasso"=ECSAppT2C$`ITT Post lasso`,
                  "Information + support vs. Information-only_Basic"=ECSAppT2T1$`Basic Model`, "Information + support vs. Information-only_Post-Lasso"=ECSAppT2T1$`ITT Post lasso`,
                  "Information-only vs. Control_Basic"=ECSAppT1C$`Basic Model`, "Information-only vs. Control_Post-Lasso"=ECSAppT1C$`ITT Post lasso`
),
coef_map="Z",
title="Early childcare application - Intention-to-treat estimates",
fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int"),
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c("Covariates","Fixed effects",
            "nobs", "r.squared","adj.r.squared"),
notes=paste(" 
The dependent variable equals 1 if the household applied for at least one early childcare facility at endline.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in brackets"), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:7),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  hline(2,part="body")


# Early childcare access

# Early childcare application
# Do the tables
#
modelsummary(list("Information + support vs. Control_Basic"=ECSUseYesT2C$`Basic Model`, "Information + support vs. Control_Post-Lasso"=ECSUseYesT2C$`ITT Post lasso`,
                  "Information + support vs. Information-only_Basic"=ECSUseYesT2T1$`Basic Model`, "Information + support vs. Information-only_Post-Lasso"=ECSUseYesT2T1$`ITT Post lasso`,
                  "Information-only vs. Control_Basic"=ECSUseYesT1C$`Basic Model`, "Information-only vs. Control_Post-Lasso"=ECSUseYesT1C$`ITT Post lasso`
),
coef_map="Z",
title="Early childcare access- Intention-to-treat estimates",
fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int"),
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c("Covariates","Fixed effects",
            "nobs", "r.squared","adj.r.squared"),
notes=paste("
The dependent variable equals 1 if the household accessed early childcare at endline.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in brackets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:7),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  hline(2,part="body")


# Daycare application

modelsummary(list("Information + support vs. Control_Basic"=ECSAppCrecheT2C$`Basic Model`, "Information + support vs. Control_Post-Lasso"=ECSAppCrecheT2C$`ITT Post lasso`,
                  "Information + support vs. Information-only_Basic"=ECSAppCrecheT2T1$`Basic Model`, "Information + support vs. Information-only_Post-Lasso"=ECSAppCrecheT2T1$`ITT Post lasso`,
                  "Information-only vs. Control_Basic"=ECSAppCrecheT1C$`Basic Model`, "Information-only vs. Control_Post-Lasso"=ECSAppCrecheT1C$`ITT Post lasso`
),
coef_map="Z",
title="Daycare application - Intention-to-treat estimates",
fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int"),
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c("Covariates","Fixed effects",
            "nobs", "r.squared","adj.r.squared"),
notes=paste("
The dependent variable equals 1 if the household applied to at least one daycare center at endline.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the  level in parenthesis ; point-wise 95% confidence intervals in brackets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:7),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  hline(2,part="body")


# Daycare access
modelsummary(list("Information + support vs. Control_Basic"=USeCrecheT2C$`Basic Model`, "Information + support vs. Control_Post-Lasso"=USeCrecheT2C$`ITT Post lasso`,
                  "Information + support vs. Information-only_Basic"=USeCrecheT2T1$`Basic Model`, "Information + support vs. Information-only_Post-Lasso"=USeCrecheT2T1$`ITT Post lasso`,
                  "Information-only vs. Control_Basic"=USeCrecheT1C$`Basic Model`, "Information-only vs. Control_Post-Lasso"=USeCrecheT1C$`ITT Post lasso`
),
coef_map="Z",
title="Daycare access - Intention-to-treat estimates",
fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int"),
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c("Covariates","Fixed effects",
            "nobs", "r.squared","adj.r.squared"),
notes=paste("
The dependent variable equals 1 if the household accessed early childcare at endline.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in brackets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:7),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  hline(2,part="body")



#------- LATE ------------
## Estimate the LATE for the 4 main outcomes and make the same table as with the 1st stage
FirstStage <- ITTSimNoControl(Y="D",DB=PostDBT2)
LATE.UseCreche   <- LATESimultaneous(Y="UseCreche")
LATE.ECSUseYes   <- LATESimultaneous(Y="ECSUseYes")
LATE.ECSApp      <- LATESimultaneous(Y="ECSApp")
LATE.ECSAppCreche <-LATESimultaneous(Y="AppCreche")


#Coef Map for clear labels
cm <- c( 'T2-C'    = 'Information + Support vs Control',
         'T2-T1'   = 'Information + support vs Information-only', 
         "Avg. cfct."= "Average counterfactual")



modelsummary(list(
  "_First Stage"=FirstStage$ModelSummary,
  "Application_Early childcare"  =LATE.ECSApp$ModelSummary,
  "Application_Daycare"          =LATE.ECSAppCreche$ModelSummary,
  "Access_Early childcare"       =LATE.ECSUseYes$ModelSummary,
  "Access_Daycare"               =LATE.UseCreche$ModelSummary
),
fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int",
              "adj.p.val. = {adj.p.value}"),
#stars=FALSE,
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c(#"Mean of DV",
  "Covariates","Fixed effects","Chi 2","P-value",
  "nobs", "r.squared","adj.r.squared"), coef_map=cm,
title="Average treatment effect on the treated on the main outcomes",
notes=paste("Sources:", SourcesStacked,
            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the",LATE.UseCreche$Correction, "method. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table.
First stage reports OLS estimates of offering support on actual support on both comparison groups.
Average treatment effects on the treated estimated jointly for both comparison by instrumenting administrative support in each comparison sample by assignment to T2 (centred by the pairwise instrument propensity score) interacted with the comparison sample dummy and block x wave x comparison fixed effects instrumenting themselved.
Avg. Cfct. indicates the untreated compliers' average and is estimated by TSLS with (1-D)*Y as an outcome, (1-D) as the treatment variable instrumented by the centred assignment.
"),
output = 'flextable') %>% 
 # theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3,4,5,6),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  merge_at(j=1,i=c(1,2),part="header") %>% 
  #merge_v(j=2,i=c(1,2),part="header") %>% 
  border_inner_h(border = NULL,part="header") %>% 
  hline(i=2,border=NULL,part="header") %>% 
  hline(i=1,j=c(3:6),border=NULL,part="header") %>% 
  #merge_v(i = c(6,7),j=1,part="body") %>% 
  hline(c(6,8),part="body")


#----- HetT1ITTATT ------------


## First etimate the ITT
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            ,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

## Estimate the ATT
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



# Define the factors
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


## Estimate the ATT
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")
# Define the factors
term_levels <- c("T1-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.EducMig <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.EducMig)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 


#------------ HetT2table ------------

# Heterogeneous effects of the information + support treatment on early childcare applications 
# Only including SES and Migration background dimensions

# Step 1 : estimate the conditional ITTs of interest using the function
## First estimate the ITT for applications
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# We get many results in each function:
# - ModelSummary0 - prepare a modelsummary table for the control group, basic table : modelsummary(NAME$ModelSummary0,shape=term+Group~model)
# - `Model 0` is the raw model presented in ModelSummary0
# - Estimation - gets the main estimates (ITTs or LATEs)
# - modelsummary prepare a modelsummary table for the main estimates, basic table : modelsummary(NAME$ModelSummary,shape=term+Group~model)
# - Tidy : the tidy version with both models

# Stack control for application - only SES and Migration background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - only SES and Migration background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - only SES and Migration background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - only SES and Migration background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class


# Step 2 : estimate the conditional ATTs of interest using the function
## First estimate the ATT for applications - only SES and Migration background
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use - only SES and Migration background
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Stack ATT for application - only SES and Migration background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ATT.App.Educ2C$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for use - only SES and Migration background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ATT.Use.Educ2C$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <- list(StackedControlApp,
                     StackedITTApp,
                     StackedATTApp,
                     StackedControlUse,
                     StackedITTUse,
                     StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C' = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects by SES and migration background (Yes = Migration background, No = Without migration background)"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape=Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Models are jointly estimating conditional averages in each pair of treatment arm.
Adjusted p-value and confidence intervals account for simultaneous inference across treatment arms.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  #merge_v(j=1,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(c(3*c(1:4)),c(3:8),part="body") %>% 
  hline(c(6*c(1:2)),c(1:8),part="body") 
#hline(c(3*c(1:24)),part="body")

ModelT2C


#------ DaycareApplicationAccessT1Graph ------------


## First etimate the ITT
Het.ITT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")


Het.ITT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

## Estimate the ATT
Het.ATT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")
Het.ATT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")



# Define the factors
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.AppCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")


## Estimate the ATT
Het.ATT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")
# Define the factors
term_levels <- c("T1-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.UseCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access early daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$Tidy %>% mutate(Y = "Access early daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access early daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.UseCreche.Mig$Tidy %>% mutate(Y = "Access early daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.Daycare.EducMig <- bind_rows(DataPlot,DataPlotUse)

ggplot(Data.Het.Daycare.EducMig)+
  geom_pointrange(aes(#x=interaction(Y,Het,Heterogeneity,sep="!"),
    x=interaction(Het,Heterogeneity,sep="!"),
    y=estimate,
    ymin=point.conf.low,
    ymax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    y = estimate, x = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel),scale="free_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  coord_flip()+
  geom_hline(data=Data.Het.Daycare.EducMig %>% filter( panel!="Control group"),
             aes(yintercept = 0),linetype=c(2))+
  xlab("")+
  #scale_vline(aes(Yintercept=0))+
  scale_x_discrete(guide = guide_axis_nested(delim = "!"))+
  scale_fill_brewer("Heterogeneity", palette = "Dark2" 
  ) +
  scale_color_brewer("Heterogeneity", palette = "Dark2")+
  scale_shape("Heterogeneity")+
  labs(
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
                    "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
of pairwise comparisons and subgroups using the Westfall-Young method.",
                    "\nAll models include block x wave fixed effects")
  ) 




#------------ HetT2tableDaycare ------------

# Heterogeneous effects of the information + support treatment on daycare applications 
# Only including SES and Migration background dimensions

# Step 1 : estimate the conditional ITTs of interest using the function
## First estimate the ITT for applications
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "AppCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "AppCreche",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "UseCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# We get many results in each function:
# - ModelSummary0 - prepare a modelsummary table for the control group, basic table : modelsummary(NAME$ModelSummary0,shape=term+Group~model)
# - `Model 0` is the raw model presented in ModelSummary0
# - Estimation - gets the main estimates (ITTs or LATEs)
# - modelsummary prepare a modelsummary table for the main estimates, basic table : modelsummary(NAME$ModelSummary,shape=term+Group~model)
# - Tidy : the tidy version with both models

# Stack control for application - only SES and Migration background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - only SES and Migration background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - only SES and Migration background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - only SES and Migration background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class


# Step 2 : estimate the conditional ATTs of interest using the function
## First estimate the ATT for applications - only SES and Migration background
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "AppCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "AppCreche",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use - only SES and Migration background
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "UseCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Stack ATT for application - only SES and Migration background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ATT.App.Educ2C$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for use - only SES and Migration background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Var="Migration background")),
  glance = Het.ATT.Use.Educ2C$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <- list(StackedControlApp,
                     StackedITTApp,
                     StackedATTApp,
                     StackedControlUse,
                     StackedITTUse,
                     StackedATTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C' = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects by SES and migration background (Yes = Migration background, No = Without migration background)"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape=Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Models are jointly estimating conditional averages in each pair of treatment arm.
Adjusted p-value and confidence intervals account for simultaneous inference across treatment arms.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  #merge_v(j=1,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(c(3*c(1:4)),c(3:8),part="body") %>% 
  hline(c(6*c(1:2)),c(1:8),part="body") 
#hline(c(3*c(1:24)),part="body")

ModelT2C
#------ ActivityReduction ------------


Activity.ITT <-  ITTSimultaneous(Y="WorkMotherReduced", DB = PostDB %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))))

Activity.LATE <-  LATESimultaneous(Y="WorkMotherReduced", DB = PostDBT2 %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))))

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.

activity.ITT <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))),
                                         Outcome = "WorkMotherReduced",
                                         Heterogeneity = "Educ2",
                                         ITT = TRUE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

activity.ATT <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))),
                                         Outcome = "WorkMotherReduced",
                                         Heterogeneity = "Educ2",
                                         ITT = FALSE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

# Select the relevant coef
activity.ITT$ModelSummary0$tidy <- activity.ITT$ModelSummary0$tidy  %>% filter(term == "T2-C")
activity.ITT$ModelSummary$tidy <- activity.ITT$ModelSummary$tidy  %>% filter(term == "T2-C")
activity.ATT$ModelSummary0$tidy <- activity.ATT$ModelSummary0$tidy  %>% filter(term == "T2-C")

cm <- c('T2-C' = 'Information + Support vs Control')


modelsummary(list("Reduced their activity_Control mean"  =activity.ITT$ModelSummary0,
                  "Reduced their activity_ITT"           =activity.ITT$ModelSummary,
                  "Reduced their activity_ATT"           =activity.ATT$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects of the information + support treatment on the probability that the mother reduces her activity by SES",
             notes=paste("Sources:", SourcesStacked,
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm") %>% 
  hline(c(6,3),part="body")





#------ MechanismsNewcomers ------------

##ever used early childcare
# App itt              

Het.ITT.App.Use <- GroupHeterogeneityFnCTRL(DB = PostDB%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                            Outcome = "ECSApp",
                                            Heterogeneity = "UsedECEC",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ITT.Use.Use <- GroupHeterogeneityFnCTRL(DB = PostDB%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "UsedECEC",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")



# App Att

Het.ATT.App.Use <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                            Outcome = "ECSApp",
                                            Heterogeneity = "UsedECEC",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.App.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ATT.Use.Use <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "UsedECEC",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.Use.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>%  mutate(UsedECEC=ifelse(UsedECEC == "Oui","Yes","No")),
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")




cm <- c('T2-C'    = 'Information + Support vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Use$ModelSummary0$tidy= Het.ITT.App.Use$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Use$ModelSummary$tidy= Het.ATT.App.Use$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Use$ModelSummary0$tidy= Het.ITT.Use.Use$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Use$ModelSummary$tidy= Het.ATT.Use.Use$ModelSummary$tidy %>% filter(term == "T2-C")


modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Use$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Use$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Use$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Use$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Use$ModelSummary,
                  "Early childcare access_ATT"                   =Het.ATT.Use.Use$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by past early childcare usage",
             notes=paste("Sources:", SourcesStacked,
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Use.Daycare$ModelSummary0$tidy= Het.ITT.App.Use.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Use.Daycare$ModelSummary$tidy = Het.ATT.App.Use.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Use.Daycare$ModelSummary0$tidy= Het.ITT.Use.Use.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Use.Daycare$ModelSummary$tidy = Het.ATT.Use.Use.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")


modelsummary(list("Daycare application_Control mean"     =Het.ITT.App.Use.Daycare$ModelSummary0,
                  "Daycare application_ITT"              =Het.ITT.App.Use.Daycare$ModelSummary,
                  "Daycare application_ATT"              =Het.ATT.App.Use.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Use.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Use.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Use.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by past early childcare usage",
             notes=paste("Sources:", SourcesStacked,
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#------ MechanismsActivexSES------------

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.
# Update model and variable names from MigrationBackground to ActiveSES and from FrenchEduc to ActiveEduc

# Create interaction variable and estimate the models for ActiveSES
Het.ITT.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, Educ2)),
                                                        Outcome = "AppCreche",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, Educ2)),
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, Educ2)),
                                                        Outcome = "UseCreche",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, Educ2)),
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Intersectional estimation, adding another variable for heterogeneity

Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy %>%
  separate(Group, into = c("Activity", "SES"))

Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))



# change the name
cm <- c('T2-C'    = 'Information + Support vs Control')


# Summary table of results
modelsummary(list("Daycare application_Control mean"  = Het.ITT.AppCreche.ActiveSES$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.AppCreche.ActiveSES$ModelSummary,
                  "Daycare application_ATT" = Het.LATE.AppCreche.ActiveSES$ModelSummary,
                  "Daycare access_Control mean"  = Het.ITT.UseCreche.ActiveSES$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.UseCreche.ActiveSES$ModelSummary,
                  "Daycare access_ATT" = Het.LATE.UseCreche.ActiveSES$ModelSummary),
             shape = term + Activity + SES ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(# "Mean of DV",
               "Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
               "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by SES and mothers' activity at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the method. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%   # Separate headers
  bold(i = 1,  part = "header") %>%  # Variable labels bold
  merge_at(j = 3, part = "header") %>%
  merge_at(j = 2, part = "header") %>%
  merge_at(j = 1, part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  merge_v(j = 2, part = "body") %>%
  merge_v(j = 3, part = "body") %>%
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%  # center
  align(part = "body", align = "center") %>%  # center
  width(j = c(5,6,8,9), width = 2.4, unit = "cm") %>%
  width(j = c(2,3,4,7), width = 2.2, unit = "cm") %>%
  hline(c(6,12), part = "body") 


#----- ATTRITIONTable ----


##### Tableau attrition

#### Attrition model by comparison arm ####
M.Response.Stack <- feols(Responded~i(Z,SubSample,ref="0")|SubSampleStrata,StackedDB,weights = ~WeightPS,cluster = ~StrataWave)

# joint significance test
Glht.Response.Stacked <- glht(M.Response.Stack) 

# get the results in a tidy data frame
tidy.Glht.Response.Stacked <- left_join(tidy(Glht.Response.Stacked),tidy(confint(Glht.Response.Stacked)))

## Get the results in a tidy table for export with flextable and let's do a plot too.

tidy.Response.Stacked <- tidy(M.Response.Stack) %>% bind_cols(.,confint(M.Response.Stack)[1],
                                                              confint(M.Response.Stack)[2]) %>% 
  rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.Response.Stacked,by=c("term"="contrast","estimate","std.error")) %>% 
  mutate(term=str_remove_all(term,"Z::1:|\\(|\\)|SubSample::"))


# Also run a F test of joint null effect
ChisQTest <- glht(M.Response.Stack) %>% summary(.,test=Chisqtest())
# 
# as.data.frame(bind_cols(c("P-value"=  ChisQTest$test$pvalue,
#                           "Chi 2"= ChisQTest$test$SSH,
#                           "DF" = ChisQTest$test$df[1]
# )))


# prepare for modelsummary: baseline
modelAttritionStacked <- list(tidy=tidy.Response.Stacked, # list with tidy containing the dataframe with the estimates
                              glance=get_gof(M.Response.Stack) %>%  # statistics of the model
                                bind_cols(.,"Fixed effects"="X") %>% 
                                bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                                "P-value"=  ChisQTest$test$pvalue)
                                )))
class(modelAttritionStacked) <- "modelsummary_list"   # define the class



# Compute response rate in the comparison group (trick with OLS ;) )
ControlMean <- feols(ZO*Responded~i(ZO,SubSample,ref=0)|StrataWave^SubSample,StackedDB %>% mutate(ZO=1-Z),cluster = ~StrataWave)

# joint significance test
Glht.ControlMean <- glht(ControlMean) 

# get the results in a tidy data frame
tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))

# tidy the model 
tidy.ControlMean <- tidy(ControlMean) %>% bind_cols(.,confint(ControlMean)[1],
                                                    confint(ControlMean)[2]) %>% 
  rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
  mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample::"))

# prepare for modelsummary: baseline
AttritionControlMeans <- list(tidy=tidy.ControlMean, # list with tidy containing the dataframe with the estimates
                              glance=get_gof(ControlMean) %>% # statistics of the model
                                bind_cols(.,"Fixed effects"="X") 
)
class(AttritionControlMeans) <- "modelsummary_list"   # define the class


#Coef Map for clear labels
cm <- c('T1-C'    = 'Information-only vs Control ',
        'T2-C'    = 'Information + Support vs Control',
        'T2-T1'   = 'Information + support vs Information-only')

#### Modelsummary Attrition ####
modelsummary(list("Reference \nmean"=AttritionControlMeans,"Differential \nAttrition"=modelAttritionStacked),
             coef_map = cm,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars=FALSE,
             #stars = c('*' = .1,'**' = .05, '***' = .01),
             gof_map = c("Covariates","Fixed effects","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Model of the probability of responding to the follow-up survey",
             notes="* p <  0.1, ** p <  0.05,*** p < 0.01 using point-wise p-value. Adjusted p-value and confidence intervals account for simultaneous inference using the Holm–Bonferroni correction. Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Notes: ",
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  hline(9,part="body")




# ------ JunkAttritionTable ------
## This chunk constructs the balance table for baseline and endline samples with q-values.

CompareSamples <- bind_rows(MainDB %>% mutate(Sample = "Baseline"),
                            MainDB %>% filter(Responded == 1) %>% mutate(Sample = "Endline")) %>%
  mutate(
    T1 = ifelse(Assignment == "T1", 1, 0),
    T2 = ifelse(Assignment == "T2", 1, 0),
    Control = ifelse(Assignment == "Control", 1, 0),
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    CoupleCohabiting1or0 = ifelse(CoupleCohabiting == TRUE, 1, 0),
    AgeSup321or0 = ifelse(AgeSup32 == "Yes", 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(MigrationBackground == "France", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Oui", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    DidNotSmoke1or0 = ifelse(SmokePreg == "Did Not Smoke", 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    DeprivHolidays1or0 = ifelse(DeprivHolidays != "Pas du tout", 1, 0), NumberChildren = as.numeric(NumberChildren), 
    BabyFemale = ifelse(BabyFemale == TRUE, 1, 0)
  ) %>%  filter(NumberChildren <2) %>%
  select(
    T1, T2, Control,
    "Single parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children" = NumberChildren,
    # "The mother has no child" = Primipare1or0,
    "Mother is born in France" = BornFr1or0,
    "Mother has attained post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The mother is active at baseline" = Active1or0,
    "Household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    #  "The family couldn't afford holidays" = DeprivHolidays1or0,
    #  "Has a computer" = ComputerYes1or0,
    "Mother is present oriented" = Discount501or0,
    "Mother wants to work after maternity leaves" = WorkPlanTo1or0,
    #"The mother did not smoke" = DidNotSmoke1or0,
    #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
    "Household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "Mother wants to use early childcare" = PlanToUseECS1or0,                 # Intend to use, block variable
    #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
    #  "Knows only daycare" = KnowsCrecheOnly1or0,
    #    "Value socialization" = ValSocialisation,
    #"Believe in returns to early childcare" = LikertReturnHK1or0,
    #"The mother trusts early childcare" = TrustCreche1or0,
    "Mother lives in Paris" = DepParis1or0,
    "Early childcare coverage is high" = HighECSCov1or0,
    "Child is a girl" = BabyFemale, Sample
  )


# Create balance table using tbl_summary
summary_baseline_variables_endline <- CompareSamples %>%
  tbl_summary(
    by = Sample,  # Comparison by baseline and endline
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% 
  add_q(method = "fdr", pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1)) %>%
  modify_header(label ~ "**Variable**", stat_0 ~ "Overall", stat_1 ~ "Baseline", stat_2 ~ "Endline") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sample**")

# Convert to a flextable for formatting
summary_baseline_variables_endline %>%
  as_flex_table() %>%
  merge_v(part = "header", j = 1) %>%
  merge_h(part = "header", i = 1) %>%
  italic(j = 1, part = "body") |>  
  width(j = c(1), unit = "cm", width = 3) |>
  width(j = c(2:5), unit = "cm", width = 2) |>
  set_caption(caption = "Balance test of baseline and endline characteristics across samples") %>%
  add_footer_lines(
    "Proportions and number of observations in parentheses for categorical and dichotomous variables. 
    Averages and standard deviations in parentheses for continuous variables. 
    Q-value control for the false discovery rate using FDR correction."
  ) %>%
  fontsize(i = 1, size = 10, part = "footer")



#------ JUNKT1AccessApplicationGraph ------------
## First etimate the ITT
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.Discount <- GroupHeterogeneityFnCTRL(DB = PostDB%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSApp",
  Heterogeneity = "Discount501or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
panel_levels <- c("Control mean", "ITT Application")

# Fusionner les données en un seul DataFrame avec les niveaux de facteur appropriés
DataApp <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Early childcare application", panel = "Control mean", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Early childcare application", panel = "ITT Application", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Early childcare application", panel = "Control mean", Heterogeneity = "Migration \nbackground") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Early childcare application", panel = "ITT Application", Heterogeneity = "Migration \nbackground") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$ModelSummary0$tidy %>% mutate(Y = "Early childcare application", panel = "Control mean", Heterogeneity = "Level of \nknowledge") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$Tidy %>% mutate(Y = "Early childcare application", panel = "ITT Application", Heterogeneity = "Level of \nknowledge") %>% filter(term %in% term_levels),
  Het.ITT.App.Discount$ModelSummary0$tidy %>% mutate(Y = "Early childcare application", panel = "Control mean", Heterogeneity = "Temporal \norientation") %>% filter(term %in% term_levels),
  Het.ITT.App.Discount$Tidy %>% mutate(Y = "Early childcare application", panel = "ITT Application", Heterogeneity = "Temporal \norientation") %>% filter(term %in% term_levels)
) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T1-C!Early childcare application!SES" = "SES",
  "T1-C!Early childcare application!Migration background" = "Migration \nbackground",
  "T1-C!Early childcare application!Level of knowledge" = "Level of \nknowledge",
  "T1-C!Early childcare application!Temporal orientation" = "Temporal \norientation"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlotApp <- DataApp %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free", space = "free_y") +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("High-SES", "Low-SES", "France", "Abroad", "Low knowledge", "High knowledge", "Present Orientated", "Future Orientated")) +
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("High-SES", "Low-SES", "France", "Abroad", "Low knowledge", "High knowledge", "Present Orientated", "Future Orientated")) +
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) +  # Adjust the number of legend columns
  geom_hline(data=DataApp %>%filter(panel!="Control mean"),
             aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 4),
    fill = guide_legend(ncol = 4)
  ) +
  theme(
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_blank(),   # Remove the y-axis labels
    axis.ticks.y = element_blank()   # Remove the y-axis ticks
  )




Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ITT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.Use.Discount <- GroupHeterogeneityFnCTRL(DB = PostDB%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSUseYes",
  Heterogeneity = "Discount501or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
panel_levels <- c("Control mean", "ITT Access")

# Fusionner les données en un seul DataFrame avec les niveaux de facteur appropriés
DataAccess <- bind_rows(
  Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Use early childcare", panel = "Control mean", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.Use.Educ2C$Tidy %>% mutate(Y = "Use early childcare", panel = "ITT Access", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$ModelSummary0$tidy %>% mutate(Y = "Use early childcare", panel = "Control mean", Heterogeneity = "Migration \nbackground") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$Tidy %>% mutate(Y = "Use early childcare", panel = "ITT Access", Heterogeneity = "Migration \nbackground") %>% filter(term %in% term_levels),
  Het.ITT.Use.Info$ModelSummary0$tidy %>% mutate(Y = "Use early childcare", panel = "Control mean", Heterogeneity = "Level of \nknowledge") %>% filter(term %in% term_levels),
  Het.ITT.Use.Info$Tidy %>% mutate(Y = "Use early childcare", panel = "ITT Access", Heterogeneity = "Level of \nknowledge") %>% filter(term %in% term_levels),
  Het.ITT.Use.Discount$ModelSummary0$tidy %>% mutate(Y = "Use early childcare", panel = "Control mean", Heterogeneity = "Temporal \norientation") %>% filter(term %in% term_levels),
  Het.ITT.Use.Discount$Tidy %>% mutate(Y = "Use early childcare", panel = "ITT Access", Heterogeneity = "Temporal \norientation") %>% filter(term %in% term_levels)
) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


# Create a named vector for the x-axis labels
x_labels <- c(
  "T1-C!Use early childcare!SES" = "SES",
  "T1-C!Use early childcare!Migration background" = "Migration \nbackground",
  "T1-C!Use early childcare!Level of knowledge" = "Level of \nknowledge",
  "T1-C!Use early childcare!Temporal orientation" = "Temporal \norientation"
)
# Tracer le graphique avec les facteurs ordonnés
DatPlotAccess <- DataAccess  %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free", space = "free_y") +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("High-SES", "Low-SES", "France", "Abroad", "Low knowledge", "High knowledge", "Present Orientated", "Future Orientated")) +
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("High-SES", "Low-SES", "France", "Abroad", "Low knowledge", "High knowledge", "Present Orientated", "Future Orientated")) +
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) +  # Adjust the number of legend columns
  geom_hline(data=DataAccess %>%filter(panel!="Control mean"),
             aes(yintercept = 0), linetype = c(2)) + 
  ylab("Estimates") +
  guides(
    col  = guide_legend(ncol = 4),
    fill = guide_legend(ncol = 4)
  ) +
  theme(
    axis.title.y = element_blank(),  # Remove the y-axis title
    axis.text.y = element_blank(),   # Remove the y-axis labels
    axis.ticks.y = element_blank()   # Remove the y-axis ticks
  )
# Plots in a panel with ggarrange
CombineAppAccess <- ggarrange("Application"=DataPlotApp,
                              "Access"=DatPlotAccess,
                              common.legend = TRUE,
                              ncol = 2,#align = "hv",
                              legend = "bottom"
)

# Annotate figure             
CombineAppAccess <- annotate_figure(CombineAppAccess,
                                    bottom = text_grob(  label = paste("Sources:", SourcesStacked,
                                                                       "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                                                                       "\nPoint indicates the ITT and the error bars indicate pointwise 95% CI.",
                                                                       "\nBoxes around estimates indicate simultaneous 95% CI accounting for multiple testing.",
                                                                       "\nThe Fixed effect models are estimated with block x subsample fixed effects and inverse probability weighting."),
                                                         hjust = 0, x = 0, size = 10))

CombineAppAccess


#---------JUNKTableT2HET ----------
#------ TableInfoSupportITTATT -------------
# Heterogeneous effects of the information + support treatment on early childcare applications 


# Step 1 : estimate the conditional ITTs of interest using the function
## First etimate the ITT for applications
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.App.Discount <- GroupHeterogeneityFnCTRL(DB = PostDB%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSApp",
  Heterogeneity = "Discount501or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


Het.ITT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.Use.Discount <- GroupHeterogeneityFnCTRL(DB = PostDB%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSUseYes",
  Heterogeneity = "Discount501or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# We get many results in each function:
# - ModelSummary0 - prepare a modelsummary table for the control group, basic table : modelsummary(NAME$ModelSummary0,shape=term+Group~model)
# - `Model 0` is the raw model presented in ModelSummary0
# - Estimation - gets the main estimates (ITTs or LATEs)
# - modelsummary prepare a modelsummary table for the main estimates, basic table : modelsummary(NAME$ModelSummary,shape=term+Group~model)
# - Tidy : the tidy version with both models


#From there, we want to :
# Keep only one comparison arm (à voir)
# make a list of 
# 1) stacked control group means over all pair of heterogeneity dimensions for applications
# 2) stacked ITTs over all pair of heterogeneity dimensions for use
# 3) stacked control group means over all pair of heterogeneity dimensions for use
# 4) stacked ITTs over all pair of heterogeneity dimensions for use

# make a nice modelsummary out of this.

# Stack control for application
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background"),
                   Het.ITT.App.Info$ModelSummary0$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ITT.App.Discount$ModelSummary0$tidy) %>% select(-model)%>% mutate(Var="Temporal orientation"),
  glance = Het.ITT.App.Educ2C$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class



# Stack Itt for application
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Migration background"),
                   Het.ITT.App.Info$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ITT.App.Discount$ModelSummary$tidy %>% select(-model))%>% mutate(Var="Temporal orientation"),
  glance =Het.ITT.App.Educ2C$ModelSummary$glance)

class(StackedITTApp) <- "modelsummary_list"   # define the class


# Stack control for use
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Var="Migration background"),
                   Het.ITT.Use.Info$ModelSummary0$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ITT.Use.Discount$ModelSummary0$tidy) %>% select(-model)%>% mutate(Var="Temporal orientation"),
  glance = Het.ITT.Use.Educ2C$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Migration background"),
                   Het.ITT.Use.Info$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ITT.Use.Discount$ModelSummary$tidy %>% select(-model))%>% mutate(Var="Temporal orientation"),
  glance =Het.ITT.Use.Educ2C$ModelSummary$glance)

class(StackedITTUse) <- "modelsummary_list"   # define the class




# Step 2 : estimate the conditional ATTs of interest using the function
## First etimate the ITT for applications
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ATT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.App.Discount <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSApp",
  Heterogeneity = "Discount501or0",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")
Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


Het.ATT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge")),
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ATT.Use.Discount <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSUseYes",
  Heterogeneity = "Discount501or0",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")




# Stack Itt for application
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Migration background"),
                   Het.ATT.App.Info$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ATT.App.Discount$ModelSummary$tidy %>% select(-model))%>% mutate(Var="Temporal orientation"),
  glance =Het.ATT.App.Educ2C$ModelSummary$glance)

class(StackedATTApp) <- "modelsummary_list"   # define the class


# Stack ITT for use
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Var="SES"),
                   Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Migration background"),
                   Het.ATT.Use.Info$ModelSummary$tidy %>% select(-model)%>% mutate(Var="Level of knowledge"),
                   Het.ATT.Use.Discount$ModelSummary$tidy %>% select(-model))%>% mutate(Var="Temporal orientation"),
  glance =Het.ATT.Use.Educ2C$ModelSummary$glance)

class(StackedATTUse) <- "modelsummary_list"   # define the class




# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))



# Now T2 angainst C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape=Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Models are jointly estimating conditional averages in each pair of treatment arm.
Adjusted p-value and confidence intervals account for simultaneous inference across treatment arms.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  #merge_v(j=1,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(c(3*c(1:8)),c(3:8),part="body") %>% 
  hline(c(6*c(1:4)),c(1:8),part="body") 
#hline(c(3*c(1:24)),part="body")




ModelT2C



