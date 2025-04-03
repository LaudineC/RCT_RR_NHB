
rescale_01 <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}


BootHet <- function(CATE_matrix,nboot=500,N=1850,seed=666){
  
  results <- c()
  N = {{N}}
  
  set.seed({{seed}})
  foreach(n.boot= 1:{{nboot}},.combine=rbind) %dopar% {
    
    Educ2 = rbinom(N, size = 1, prob = HighSES$coefficients[1])
    
    #Define intention to use as a binary variable with conditional probability different by education, drawn from the regression tables
    Intend = rbinom(N,size=1,
                    prob=Educ2*intention$coefficients["Educ2High-SES"]+
                      (1-Educ2)*intention$coefficients["Educ2Low-SES"])
    
    #define Blocks with the intersection of the two variables
    block <- Educ2 * 2 + Intend  # Encodes blocks as 0,1,2,3
    # block=case_when(
    #   (Educ2==0 & Intend==0)~0,
    #   (Educ2==0 & Intend==1)~1,
    #   (Educ2==1 & Intend==0)~2,
    #   (Educ2==1 & Intend==1)~3
    # )
    
    # We can define the potential outcome in the absence of the program similarly:
    
    Y0.star =
      case_when(
        (Educ2==1 & Intend==1) ~ rnorm( N, High.Intend,  C.means$se["Intend::1:Educ2::High-SES"]),
        (Educ2==0 & Intend==1) ~ rnorm( N, Low.Intend,   C.means$se["Intend::1:Educ2::Low-SES"]),
        (Educ2==1 & Intend==0) ~ rnorm( N, High.NoIntend,C.means$se["Intend::0:Educ2::High-SES"]),
        (Educ2==0 & Intend==0) ~ rnorm( N, Low.NoIntend, C.means$se["Intend::0:Educ2::Low-SES"])
      )
    
    Y0=rbinom(N,size=1,prob=rescale_01(Y0.star))
    
    # We use block random assignment
    Z = block_ra(blocks=block, prob_each = c(.34,.33,.33), conditions = c("0", "1", "2"))
    
    
    # Assign treatment effects using the CATE matrix
    idx <- block + 1
    ATE1 <- CATE_matrix[idx, 1]  # Treatment effect for T1
    ATE2 <- CATE_matrix[idx, 2]  # Treatment effect for T2
    
    # Simulate treated outcomes
    Y1 <- rbinom(N, 1, prob = rescale_01(Y0 + rnorm(N, ATE1, 0.02)))
    Y2 <- rbinom(N, 1, prob = rescale_01(Y0 + rnorm(N, ATE2, 0.02)))
    
    Y.obs=case_when(Z==0~Y0,Z==1~Y1,Z==2~Y2)
    
    
    db0 <- data.frame(Educ2,Intend,block,Y.obs,Y1,Y2,Z,Y0,Y0.star) %>% mutate(TE.21=Y2-Y1,TE.10=Y1-Y0,TE.20=Y2-Y0)
    
    # like in our analysis, we build pair of comparison
    db0 <- db0 %>% mutate(id=cur_group_rows()) 
    
    T2T1 <- db0 %>% filter(Z %in% c(1,2)) %>% mutate(SubSample="T2-T1",Z1=ifelse(Z==2,1,0))
    T2T0 <- db0 %>% filter(Z %in% c(0,2)) %>% mutate(SubSample="T2-C",Z1=ifelse(Z==2,1,0))
    T1T0 <- db0 %>% filter(Z %in% c(0,1)) %>% mutate(SubSample="T1-C",Z1=ifelse(Z==1,1,0))
    
    StackedDB <- bind_rows(T2T1,T2T0,T1T0) %>% mutate(SubSampleStrata=interaction(SubSample,block),
                                                      StrataWave=block,
                                                      weights=1
    )
    # Propensity score for each pairwise comparison: simple probit
    fit_basicStack <- feglm(Z1 ~ 1|SubSampleStrata, data = StackedDB,"probit",cluster = ~StrataWave)
    #retrieve the prediction
    StackedDB$psscore=predict(fit_basicStack,StackedDB)
    
    StackedDB <- StackedDB %>% mutate(
      Z.c=Z1-psscore,
      WeightPS=Z1/psscore+(1-Z1)/(1-psscore),
      ZT2C.c =Z.c*as.numeric(SubSample=="T2-C"),
      ZT1C.c =Z.c*as.numeric(SubSample=="T1-C"),
      ZT2T1.c=Z.c*as.numeric(SubSample=="T2-T1")
    )
    
    
    estimates <- ITTSimultaneous(Y="Y.obs",treat = "Z1",DB=StackedDB,weights = "WeightPS")
    
    results <- estimates$Tidy %>% mutate(Boot=n.boot)
    
    estimand <- StackedDB %>% group_by(SubSample) %>% summarise(TE.21=mean(TE.21,na.rm=T),TE.20=mean(TE.20,na.rm=T),TE.10=mean(TE.10,na.rm=T))
    
    results <- results %>% left_join(estimand,by=c("term"="SubSample"))
    
    #results %>% left_join(estimand,by=c("term"="SubSample")) #%>% mutate(bias=estimate-estimand)
    
    
  } -> All.results
  
  return(All.results)
  
}


CATE_matrix <- matrix(c(
  0.10, 0.2,  # Low-SES, No Intention
  0.05, 0.1,  # Low-SES, Intention
  0.05, 0.1,  # High-SES, No Intention
  0.0, 0.05   # High-SES, Intention
), nrow = 4, byrow = TRUE)

#The first column (CATE for T1) gives the expected increase in outcome probability when moving from the control group to T1 (Information only).
#The second column (CATE for T2) gives the expected increase in outcome probability when moving from the control group to T2 (Information + Support).

#For example:

#Low-SES, No Intention ([1,1]):
#  10pp effect under T1 (.10).
# 10 percentage point (pp) increase under T2 (0.1).


set.seed(123)

testBootHet <- BootHet(CATE_matrix,nboot=100,N=1850,seed=666)


testBootHet %>% group_by(term) %>% mutate(estimand=case_when(term=="T2-T1"~TE.21,
                                                          term=="T2-C"~TE.20,
                                                          term=="T1-C"~TE.10)) %>% 
  summarise(bias=mean(estimate-estimand),
            rmse=sqrt(mean((estimate - estimand)^2)),
            power = mean(p.value <= 0.05),
            FWER.power  = mean(adj.p.value<.05),
            coverage = mean(estimand <= point.conf.high & estimand >= point.conf.low),
            FWER.coverage = mean(estimand <= conf.high & estimand >= conf.low)
  ) %>% flextable()


view(testBootHet)


testBootHet <- testBootHet%>% mutate(estimand=case_when(term=="T2-T1"~TE.21,
                                         term=="T2-C"~TE.20,
                                         term=="T1-C"~TE.10))
ggdensity(data=testBootHet,x="estimate",fill="term",rug=TRUE)+geom_density(aes(x=estimand,color=term))


hist(testBootHet$estimate)
