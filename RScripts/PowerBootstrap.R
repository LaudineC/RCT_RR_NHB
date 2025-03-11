



p_load(foreach)
p_load(doParallel)




# Take the control group only

Ctrl <- MainDB %>% filter(Assignment=="Control")

s.size <- nrow(MainDB)

# get number of cores
numCores <- parallel::detectCores() # Requires library(parallel)

#print(numCores)
registerDoParallel(numCores-2)

BootPlaceboPar <- function(data=Ctrl,seed=666, sample.size=1849, n.Bootstraps=100){

# This function takes a database, loop in parallel to:
# - reample with remplacement to reach the required sample size,  
# - reassign a placebo treatment within block x waves just like the true experiment.
# - prepare the database just like in the main analysis and run the estimates for the 3 outcomes
# It returns a dataframe with all estimates, standard errors, p.values, etc. per bootstrap sample.
  
# The standard deviation of the estimates variable in the returned function corresponds to the standard errors.  
# the quantile .025 and .975 of estimates give the 95% confidence interval

  
    
  
  set.seed({{seed}})
  foreach(n.boot= 1:{{n.Bootstraps}},.combine=rbind) %dopar% {
    
  #for (n.boot in 1:{{n.Bootstraps}}){
    # sample the size of the mainDB 
    
    Boot.sample <- sample_n({{data}},nrow(MainDB),replace = TRUE)
    
    # Assign placebo treatment.
    
    Boot.sample <- Boot.sample %>% mutate(Assignment=block_ra(blocks = c(StrataWave), prob_each = c(.34, .33, .33),conditions = c("Control","T1","T2")))
    
    ###### Compute propensity scores of treatment assignments
    # I use a multinomial logit of assignment over blocks within each wave (i.e. the exact randomisation design)
    # multinom is taken from the package glmnet
    (fit_basic <- multinom(Assignment ~ StrataWave, data = Boot.sample))
    #%>% invisible()
    
    
    # Then retrieve the predicted probability
    
    MainDB$predT1 <- predict(fit_basic,Boot.sample,"probs")[,"T1"]
    MainDB$predT2 <- predict(fit_basic,Boot.sample,"probs")[,"T2"]
    
    # From there, we generate centred treatment dummies
    
    Boot.sample <- Boot.sample %>% mutate(Z1=ifelse(Assignment=="T1",1,0),
                                          Z2=ifelse(Assignment=="T2",1,0),
                                          Z1.c=Z1-predT1,
                                          Z2.c=Z2-predT2
    )
    
    
    
    
    # Ultimately, we want to test all pairwise comparison of attrition rate across groups. So Thats how we do it
    SampleT1C <- Boot.sample %>%filter( Assignment %in% c("Control","T1")) %>%
      mutate(SubSample="T1-C") %>% mutate(Z=ifelse(Assignment=="T1",1,0), 
                                          Z.c = Z-predT1, 
                                          WeightPS=Z/predT1+(1-Z)/(1-predT1))
    
    SampleT2C <- Boot.sample %>%filter( Assignment %in% c("Control","T2"))%>% 
      mutate(SubSample="T2-C") %>% mutate(Z=ifelse(Assignment=="T2",1,0),
                                          Z.c=Z-predT2,
                                          WeightPS=Z/predT2+(1-Z)/(1-predT2))
    SampleT2T1 <- Boot.sample %>%filter( Assignment %in% c("T2","T1"))%>% mutate(SubSample="T2-T1") %>% mutate(Z=ifelse(Assignment=="T2",1,0), 
                                                                                                               Z.c=Z-predT2,
                                                                                                               WeightPS=Z/predT2+(1-Z)/(1-predT2))                                                                          
    
    # bind them
    
    StackedDB <- bind_rows(SampleT1C,SampleT2C,SampleT2T1) %>% mutate(Treat=paste("Z=",Z,":",SubSample,sep=""),
                                                                      SubSampleStrata=paste(SubSample,StrataWave,sep=":")
    )
    
    # Databases to use
    PostDB <- StackedDB %>% filter(Responded==1) %>% ungroup()
    
    # When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
    PostDBT2 <- PostDB %>% filter(str_detect(SubSample,"T2")) %>% mutate(D=Suivi_administratif1_0) %>% ungroup()
    
    
    # When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
    PostDBT1C <- PostDB %>% filter(SubSample == "T1-C") %>% ungroup()
    
    # When we want to compare T2 with either T1 or C ; and we generate D for suivi administratif
    PostDBT2C <- PostDBT2 %>% filter(SubSample == "T2-C") %>% ungroup()
    
    
    # Estimate main models
    
    
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
    
    
    # Collect results 
    
    DB <- bind_rows(ITT.UseCreche$Tidy %>% mutate(Outcome="UseCreche"),
                    ITT.ECSApp$Tidy %>% mutate(Outcome="ECSApp"),
                    ITT.ECSUseYes$Tidy %>% mutate(Outcome="ECSUseYes"),
                    ITT.ECSAppCreche$Tidy %>% mutate(Outcome="AppCreche"),
    ) %>% mutate(BootSample = n.boot)
    # 
    # if (n.boot==1){
    #   All.results <- DB
    # }else{
    #   All.results <- bind_rows(All.results,DB)  
    # }
    
  } -> All.results
  
  return(All.results)
  
}


testboot <- BootPlaceboPar(sample.size=1849, n.Bootstraps = 500)



#BootPlaceboPar(n.Bootstraps = 500)

Summary.boot <- testboot %>% group_by(term,Outcome) %>% 
  summarise(m.estimate=mean(estimate),
            sd.estimate=sd(estimate),
            qlb=quantile(estimate,.025),qub=quantile(estimate,.025))

library(ggpubr)

ggarrange(ggdensity(testboot %>% filter(Outcome=="ECSApp"), 
                    x = "estimate",
          add = "mean", rug = TRUE,
          color = "term", fill = "term",
          palette = palette_lgbtq("bisexual")),
          ggdensity(testboot %>% filter(Outcome=="ECSUseYes"), 
                    x = "estimate",
                    add = "mean", rug = TRUE,
                    color = "term", fill = "term",
                    palette = palette_lgbtq("bisexual")),
          labels=c("Applied any childcare", "Use any childcare"),
          #vjust=1,
          common.legend = TRUE,
          legend = "bottom"
          )


# improved correlation matrix
library(corrplot)

# pivote testboot

wide.testboot <- testboot %>% select(term,Outcome,estimate,BootSample) %>% pivot_wider(id_cols = c(BootSample,term),
                                                                                       names_from = Outcome,
                                                                                       values_from = estimate
)

corrplot(cor(wide.testboot%>% filter(term=="T1-C") %>% select(-c(BootSample,term)) ),
         method = "number",
        # type = "upper" # show only upper side
)



ggplot(wide.testboot %>% mutate(SigCreche=(AppCreche>=quantile(AppCreche,.025) & UseCreche<=quantile(AppCreche,.975)),
                                SigECSUseYes=(ECSUseYes>=quantile(ECSUseYes,.025) & ECSUseYes<=quantile(ECSUseYes,.975)),
))+
  geom_point(aes(x=AppCreche,y=ECSUseYes,
                 color=interaction(SigCreche,SigECSUseYes)))



ggplot(wide.testboot %>% mutate(SigCreche=(UseCreche>=quantile(UseCreche,.025) & UseCreche<=quantile(UseCreche,.975)),
                                SigECSUseYes=(ECSUseYes>=quantile(ECSUseYes,.025) & ECSUseYes<=quantile(ECSUseYes,.975)),
                                ))+
  geom_point(aes(x=AppCreche,y=ECSUseYes,
                 color=interaction(SigCreche,SigECSUseYes)))
#geom_density_2d_filled(aes(x=UseCreche,y=ECSUseYes),bins=100)


#ggplot(testboot)+geom_poiTRUE#ggplot(testboot)+geom_pointrange(aes(x=BootSample,y=estimate,ymin=point.conf.low,ymax = point.conf.high,color=term),size=.1)+facet_wrap(~Outcome)




#Coef Map for clear labels
cm <- c('T1-C'    = 'Information-only vs Control ',
        'T2-C'    = 'Information + Support vs Control',
        'T2-T1'   = 'Information + support vs Information-only',
        "Control mean" = "Mean control group")




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



