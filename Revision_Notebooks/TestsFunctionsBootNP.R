

install.packages("localIV")

#And the development version from GitHub with:
  
  # install.packages("devtools")
 # devtools::install_github("xiangzhou09/localIV")

Example

#Below is a toy example illustrating the use of mte() to fit an MTE model using the local IV method.

library(localIV)

mod <- mte(selection = D ~ Age+AtiveBaseline+Educ2 * IntendUse*HighLowECECBaseline* Z, outcome = ECSApp ~  Age+AtiveBaseline+Educ2 * IntendUse*HighLowECECBaseline, data = PostDBT2 %>% mutate(StrataWave = factor(StrataWave)), bw = 0.1)

# fitted propensity score model
summary(mod$ps_model)


mte_vals <- mte_at(u = seq(0.05, 0.95, 0.1), model = mod)

# install.packages("ggplot2")
library(ggplot2)

ggplot(mte_vals, aes(x = u, y = value)) +
  geom_line(size = 1) +
  xlab("Latent Resistance U") +
  ylab("Estimates of MTE at Average values of X") +
  theme_minimal(base_size = 14)

u <- p <- seq(0.05, 0.95, 0.1)
mte_tilde <- mte_tilde_at(p, u, model = mod)

# heatmap showing MTE_tilde(p, u)
ggplot(mte_tilde$df, aes(x = u, y = p, fill = value)) +
  geom_tile() +
  scale_fill_gradient(name = expression(widetilde(MTE)(p, u)), low = "yellow", high = "blue") +
  xlab("Latent Resistance U") +
  ylab("Propensity Score p(Z)") +
  theme_minimal(base_size = 14)


#When u = p, the “MTE tilde” corresponds to the marginal policy relevant treatment effect (MPRTE) as a function of p.


mprte_tilde_df <- subset(mte_tilde$df, p == u)

# heatmap showing MPRTE_tilde(p)
ggplot(mprte_tilde_df, aes(x = u, y = p, fill = value)) +
  geom_tile() +
  scale_fill_gradient(name = expression(widetilde(MPRTE)(p)), low = "yellow", high = "blue") +
  xlab("Latent Resistance U") +
  ylab("Propensity Score p(Z)") +
  theme_minimal(base_size = 14)

# decomposition of MPRTE_tilde(p) into the p-component and the u-component

# install.packages(c("dplyr", "tidyr"))

mprte_tilde_df %>%
  pivot_longer(cols = c(u_comp, p_comp, value)) %>%
  mutate(name = recode_factor(name,
                              `value` = "MPRTE(p)",
                              `p_comp` = "p(Z) component",
                              `u_comp` = "U component")) %>%
  ggplot(aes(x = p, y = value)) +
  geom_line(aes(linetype = name), size = 1) +
  scale_linetype("") +
  xlab("Propensity Score p(Z)") +
  ylab("Treatment Effect") +
  theme_minimal(base_size = 14)



ate <- ace(mod, "ate")
att <- ace(mod, "att")
atu <- ace(mod, "atu")
mprte1 <- ace(mod, "mprte")
mprte2 <- ace(mod, "mprte", policy = p)
mprte3 <- ace(mod, "mprte", policy = 1-p)
mprte4 <- ace(mod, "mprte", policy = I(p<0.25))

c(ate, att, atu, mprte1, mprte2, mprte3, mprte4)


#### A Declare design approach to POSAJE

Test <- MainDB %>%   mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
                            LowSES = ifelse(Educ2=="Low-SES",1,0))

TestC <- Test %>% filter(Assignment=="Control")


BootControlGroup <- resample_data(TestC, N = c(nrow(MainDB)))

#Quick Check

original <-lm_lin(ECSPlanToBaseline~LowSES,~PresentOrientated+High_knowledge+NoMigrationBackground+Dep,Test)
Boot <-lm_lin(ECSPlanToBaseline~LowSES,~PresentOrientated+High_knowledge+NoMigrationBackground+Dep,BootControlGroup)

modelsummary(list("original"=original,"Boot"=Boot))

# Pretty good fit on this simple model on both database



N <- nrow(MainDB)
r_sq <- 0

TestCov <-
  declare_model(N = N,
                draw_multivariate(c(U, X) ~ MASS::mvrnorm(
                  n = N,
                  mu = c(0, 0),
                  Sigma = matrix(c(1, sqrt(r_sq), sqrt(r_sq), 1), 2, 2)
                )), 
                potential_outcomes(Y ~ 0.1 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z, covariates = ~X, .method = lm_lin, inquiry = "ATE"
  )







CoverRate <- MainDB$HighCoverageBaseline %>% mean()

Test <- MainDB %>%   mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
                        LowSES = ifelse(Educ2=="Low-SES",1,0))

TestC <- Test %>% filter(Assignment=="Control")


baseline <- lm_lin(ECSPlanToBaseline~LowSES,~PresentOrientated+High_knowledge+NoMigrationBackground+Dep,TestC)

baselineApp <- lm_lin(ECSApp~ECSPlanToBaseline,~LowSES+(PresentOrientated+High_knowledge+NoMigrationBackground+Dep),TestC)


baseline <- lm_lin(ECSPlanToBaseline~LowSES,~PresentOrientated*High_knowledge,TestC)

baseline %>% modelsummary(stars = T)

predict(baseline,TestC)

hist(baseline$fitted.values)




# Function to generate new binary outcome datasets based on model coefficients
generate_similar_binary_data <- function(
    coef_vector,      # Vector of coefficients from your model
    vcov_matrix,      # Variance-covariance matrix of the coefficients
    n_samples = 1000, # Number of new observations to generate
    X_ranges = NULL   # List with min/max values for each covariate
) {
  require(MASS)
  
  # Generate new coefficient sets from multivariate normal distribution
  new_coefs <- mvrnorm(
    n = n_samples,
    mu = coef_vector,
    Sigma = vcov_matrix
  )
  
  # Number of predictors (excluding intercept)
  n_predictors <- length(coef_vector) - 1
  
  # Generate new X values
  new_X <- matrix(nrow = n_samples, ncol = n_predictors)
  
  # If ranges are provided, use them; otherwise use standard normal
  for(i in 1:n_predictors) {
    if(!is.null(X_ranges) && !is.null(X_ranges[[i]])) {
      new_X[,i] <- runif(
        n = n_samples,
        min = X_ranges[[i]][1],
        max = X_ranges[[i]][2]
      )
    } else {
      new_X[,i] <- rnorm(n_samples)
    }
  }
  
  # Add column of 1s for intercept
  X_matrix <- cbind(1, new_X)
  
  # Calculate linear predictors
  linear_predictors <- X_matrix %*% t(new_coefs)
  
  # Transform to probabilities using logistic function
  probs <- 1 / (1 + exp(-linear_predictors))
  
  # Generate binary outcomes using these probabilities
  y <- matrix(nrow = n_samples, ncol = ncol(probs))
  for(i in 1:ncol(probs)) {
    y[,i] <- rbinom(n_samples, size = 1, prob = probs[,i])
  }
  
  # Take the mean across samples to get final binary outcomes
  final_y <- apply(y, 1, mean) > 0.5
  
  # Create output dataframe
  output_df <- data.frame(
    y = as.numeric(final_y),
    X = new_X
  )
  names(output_df) <- c("y", paste0("X", 1:n_predictors))
  
  # Add predicted probabilities as an attribute
  attr(output_df, "predicted_probs") <- rowMeans(probs)
  
  return(output_df)
}

# Example usage:
# Assuming you have logistic regression coefficients

example_coef <- c(-0.5, 1.2, -0.8)  # Intercept and two predictors
example_vcov <- matrix(
  c(0.04, 0.01, -0.01,
    0.01, 0.03, 0.005,
    -0.01, 0.005, 0.02),
  nrow = 3
)

# Generate new dataset
new_data <- generate_similar_binary_data(
  coef_vector = example_coef,
  vcov_matrix = example_vcov,
  n_samples = 1000,
  X_ranges = list(
    c(-2, 2),  # Range for X1
    c(-3, 3)   # Range for X2
  )
)

# Diagnostic plots and checks
# Check proportion of 1s
mean(new_dataset$y)

# Plot predicted probabilities
hist(attr(new_dataset, "predicted_probs"), 
     main = "Distribution of Predicted Probabilities",
     xlab = "Probability")

# Check relationship between predictors and outcome
par(mfrow = c(1, 2))
for(i in 2:ncol(new_dataset)) {
  boxplot(new_dataset[,i] ~ new_dataset$y,
          main = paste("Distribution of", names(new_dataset)[i]),
          ylab = names(new_dataset)[i],
          xlab = "Outcome")
}


# If your model is called 'model'
my_coef <- coef(baselineApp)
my_vcov <- vcov(baselineApp)

new_dataset <- generate_similar_binary_data(
  coef_vector = my_coef,
  vcov_matrix = my_vcov,
  n_samples = 1850
)







Model <- declare_model(
                      N=nrow(MainDB),
                      Cover=rbinom(N,1,prob=CoverRate)  
                      #SES=rbinom(N,)
                      #IntendToUse =
                      #Cover  
                      )


X = rbinom(N, 1, prob = pnorm(U)),
Y = rbinom(N, 1, prob = pnorm(U + X)))

declaration_18.4 <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_assignment(
    Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
    probs =
      obtain_condition_probabilities(assignment = Z, 
                                     blocks = X, 
                                     block_prob = c(0.2, 0.5)),
    ipw = 1 / probs
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    covariates = ~ X,
    .method = lm_lin,
    weights = ipw,
    label = "Lin"
  )





















datasummary_crosstab(Which_wave*Assignment ~ Educ2 * IntendUse*HighLowECECBaseline, data = MainDB)

feols(UseCreche~Z|StrataWave,PostDBT2C)
# Problem of blocking.
SimpleBoot <- function(data=PostDBT2C,
                       Y="ECSApp",
                       blocks="StrataWave",
                       Z="Z",
                       NBoot = 499,
                       seed=666
                       ){
  results <- c()
  data$Y <- data[[{{Y}}]]
  data$Z <- data[[{{Z}}]]
  data$blocks <- factor(data[[{{blocks}}]])
  
  set.seed({{seed}})
  
  lightdb <- data %>% ungroup() %>% select(Y,Z,blocks)

  lightdb1 <- lightdb %>% group_by(blocks) %>% mutate(n=n(),NZ=sum(Z)) %>% filter(NZ!=n & NZ!=0)
  
  MainFE <- feols(Y~Z|blocks,lightdb)

  results <- tidy(MainFE,conf.int = T)
  results$N = MainFE$nobs
  results$Boot = 0
  

for (b in 1:{{NBoot}}) {
          
          db <-    lightdb %>%
          group_by(blocks) %>%
            sample_n(.,n=nrows(lightdb),replace=T)
            
            slice_sample(lightdb, n=1, replace = TRUE,by=blocks)
          db <- db %>% group_by(blocks) %>% mutate(n=n(),NZ=sum(Z)) %>% filter(NZ!=n & NZ!=0)
          
          feols <- feols(Y~Z|blocks,db)
          thisResult <- tidy(feols) 
          thisResult$Boot = b
          thisResult$N <- feols$nobs
          
          results <- bind_rows(results,thisResult)
          # 
          # 
          # dim <- estimatr::difference_in_means(Y~Z,data=db,blocks = blocks,se_type = "none")
          
    
}
  
results0 <- results %>% filter(Boot==0)  %>% mutate(Method="Main",lb=conf.low,ub=conf.high,se=std.error)

Summary.table <- results %>% filter(Boot>0) %>% summarise(se=sd(estimate,na.rm=TRUE),estimate=mean(estimate),lb=quantile(estimate,.025),ub=quantile(estimate,.975)) %>% 
  mutate(Method = "Bootstrap") %>% bind_rows(results0) %>% select(Method,estimate,se,lb,ub)
  
return (list("Summary.table"=Summary.table,"estimates"=results %>% select(Boot,estimate,std.error,N)))

}


test <- SimpleBoot()

testCreche <- SimpleBoot(Y="UseCreche")


ggplot(test$estimates)+geom_histogram(aes(x=estimate,fill=(estimate>quantile(estimate,.025) & estimate<quantile(estimate,.975))),bins = 100)
ggplot(testCreche$estimates)+geom_histogram(aes(x=estimate,fill=(estimate>quantile(estimate,.025) & estimate<quantile(estimate,.975))),bins = 100)



SimpleBootmean <- function(data=PostDBT2C,
                       Y="ECSApp",
                       blocks="StrataWave",
                       Z="Z",
                       NBoot = 499,
                       seed=666
){
  results <- c()
  data$Y <- data[[{{Y}}]]
  data$Z <- data[[{{Z}}]]
  data$blocks <- factor(data[[{{blocks}}]])
  
  set.seed({{seed}})
  
  lightdb <- data %>% ungroup() %>% select(Y,Z,blocks)
  
  lightdb1 <- lightdb %>% group_by(blocks) %>% mutate(n=n(),NZ=sum(Z)) %>% filter(NZ!=n & NZ!=0)
  
  MainFE <- feols(Y~Z|blocks,lightdb)
  
  results <- tidy(MainFE,conf.int = T)
  results$N = MainFE$nobs
  results$Boot = 0
  
  
  for (b in 1:{{NBoot}}) {
    
    db <-    sample_n(lightdb, nrow(lightdb), replace = TRUE,by=blocks)
    db <- db %>% group_by(blocks) %>% mutate(n=n(),NZ=sum(Z)) %>% filter(NZ!=n & NZ!=0)
    
    feols <- feols(Y~Z|blocks,db)
    thisResult <- tidy(feols) 
    thisResult$Boot = b
    thisResult$N <- feols$nobs
    
    results <- bind_rows(results,thisResult)
    # 
    # 
    # dim <- estimatr::difference_in_means(Y~Z,data=db,blocks = blocks,se_type = "none")
    
    
  }
  
  results0 <- results %>% filter(Boot==0)  %>% mutate(Method="Main",lb=conf.low,ub=conf.high,se=std.error)
  
  Summary.table <- results %>% filter(Boot>0) %>% summarise(se=sd(estimate,na.rm=TRUE),estimate=mean(estimate),lb=quantile(estimate,.025),ub=quantile(estimate,.975)) %>% 
    mutate(Method = "Bootstrap") %>% bind_rows(results0) %>% select(Method,estimate,se,lb,ub)
  
  return (list(Summary=summary.table,estimates=results %>% select(Boot,estimate,std.error,N)))
  
}












ggplot(results)+geom_histogram(aes(x=estimate,fill=(estimate>quantile(estimate,.025) & estimate<quantile(estimate,.975))))

BootstrapDIM <- function(data=MainDB,Y="ECSApp"){
data$Y <- data[[{{Y}}]]

# get a summary table of average outcome in each block and treatment arms, with N and sd.
table <- data %>% group_by(Which_wave,Educ2,IntendUse,HighLowECECBaseline,StrataWave,Assignment) %>% summarise(N=sum(!is.na(Y)),Mean=mean(Y,na.rm=TRUE),SD=sd(Y,na.rm=TRUE))

# remove empty cells
table0 <- table %>% filter(N>0 & !is.na(N))

# Table 0 pivoted to compute differences

WideTable0 <- table0 %>% pivot_wider(id_cols = c(Which_wave,Educ2,IntendUse,HighLowECECBaseline,StrataWave),names_from = Assignment,values_from = c(N,Mean,SD))

# Imbens Rubin (2015) section 9.5.2 page 205

WideTable0 <- WideTable0 %>% mutate(YT2T1 = Mean_T2 - Mean_T1,
                                    YT2C= Mean_T2 - Mean_Control,
                                    YT1C = Mean_T1 - Mean_Control,
                                    NT2T1=N_T2+N_T1,
                                    PT2T1=N_T2/NT2T1,
                                    NT2C=N_T2+N_Control,
                                    PT2C=N_T2/NT2C,
                                    NT1C=N_Control+N_T1,
                                    PT1=N_T1/NT1C,
                                    VARYT2T1 = SD_T2^2/N_T2+SD_T1^2/N_T1,
                                    VARYT2C = SD_T2^2/N_T2+SD_Control^2/N_Control,
                                    VARYT1C = SD_T1^2/N_T1+SD_Control^2/N_Control
)


PATE <- WideTable0 %>% ungroup() %>% summarise(ITT.T2T1 = sum(NT2T1*YT2T1,na.rm = T)/sum(NT2T1,na.rm = T),
                                               ITT.T2C = sum(NT2C*YT2C,na.rm = T)/sum(NT2C,na.rm = T),
                                               ITT.T1C = sum(NT1C*YT1C,na.rm = T)/sum(NT1C,na.rm = T),
                                               SE.T2T1 = sqrt(sum((NT2T1/sum(NT2T1,na.rm = T))^2*VARYT2T1,na.rm = T)),
                                               SE.T2C  = sqrt(sum((NT2C/sum(NT2C,na.rm = T))^2*VARYT2C,na.rm = T)),
                                               SE.T1C  = sqrt(sum((NT1C/sum(NT1C,na.rm = T))^2*VARYT1C,na.rm = T)),
                                               LB.T2T1=ITT.T2T1-qnorm(0.975)*SE.T2T1,
                                               UB.T2T1=ITT.T2T1+qnorm(0.975)*SE.T2T1,
                                               LB.T2C=ITT.T2C-qnorm(0.975)*SE.T2C,
                                               UB.T2C=ITT.T2C+qnorm(0.975)*SE.T2C,
                                               LB.T1C=ITT.T1C-qnorm(0.975)*SE.T1C,
                                               UB.T1C=ITT.T1C+qnorm(0.975)*SE.T1C,
) %>% 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c(".value", "Group"),  # Separate into values and group
    names_sep = "\\."  # The separator in column names
  )

summary.table <- PATE
estimates = WideTable0

return (list(Summary=summary.table,estimates=estimates))
}







test <- BootstrapDIM()

# take an outcome in MainDB
data <- MainDB %>% mutate(Y=ECSApp)

# get a summary table of average outcome in each block and treatment arms, with N and sd.
table <- data %>% group_by(Which_wave,Educ2,IntendUse,HighLowECECBaseline,StrataWave,Assignment) %>% summarise(N=sum(!is.na(Y)),Mean=mean(Y,na.rm=TRUE),SD=sd(Y,na.rm=TRUE))

# remove empty cells
table0 <- table %>% filter(N>0 & !is.na(N))

# Effective N: 1453
sum(table0$N)

# Table 0 pivoted to compute differences

WideTable0 <- table0 %>% pivot_wider(id_cols = c(Which_wave,Educ2,IntendUse,HighLowECECBaseline,StrataWave),names_from = Assignment,values_from = c(N,Mean,SD))

# Imbens Rubin (2015) section 9.5.2 page 205

WideTable0 <- WideTable0 %>% mutate(YT2T1 = Mean_T2 - Mean_T1,
                                    YT2C= Mean_T2 - Mean_Control,
                                    YT1C = Mean_T1 - Mean_Control,
                                    NT2T1=N_T2+N_T1,
                                    PT2T1=N_T2/NT2T1,
                                    NT2C=N_T2+N_Control,
                                    PT2C=N_T2/NT2C,
                                    NT1C=N_Control+N_T1,
                                    PT1=N_T1/NT1C,
                                    VARYT2T1 = SD_T2^2/N_T2+SD_T1^2/N_T1,
                                    VARYT2C = SD_T2^2/N_T2+SD_Control^2/N_Control,
                                    VARYT1C = SD_T1^2/N_T1+SD_Control^2/N_Control
                                    )

# Reweighted means by inverse share of treated 

#HorwitzDIM <- WideTable0 %>% mutate(YT2T1 =1/PT2T1*Mean_T2-1/(1-PT2T1)*Mean_T1)
# 
# HorwitzDIM <- WideTable0 %>% mutate(YT2T1 = N_T2/(N_T1+N_T2)*Mean_T2-N_T1/(N_T1+N_T2)*Mean_T1,
#                                     YT2C = N_T2/(N_Control+N_T2)*Mean_T2-N_Control/(N_Control+N_T2)*Mean_Control,
#                                     YT1C = N_T1/(N_T1+N_Control)*Mean_T1-N_Control/(N_Control+N_T1)*Mean_Control,
#                                     )

# PATE & Variance

PATE <- WideTable0 %>% ungroup() %>% summarise(ITT.T2T1 = sum(NT2T1*YT2T1,na.rm = T)/sum(NT2T1,na.rm = T),
                                               ITT.T2C = sum(NT2C*YT2C,na.rm = T)/sum(NT2C,na.rm = T),
                                               ITT.T1C = sum(NT1C*YT1C,na.rm = T)/sum(NT1C,na.rm = T),
                                               SE.T2T1 = sqrt(sum((NT2T1/sum(NT2T1,na.rm = T))^2*VARYT2T1,na.rm = T)),
                                               SE.T2C  = sqrt(sum((NT2C/sum(NT2C,na.rm = T))^2*VARYT2C,na.rm = T)),
                                               SE.T1C  = sqrt(sum((NT1C/sum(NT1C,na.rm = T))^2*VARYT1C,na.rm = T)),
                                               LB.T2T1=ITT.T2T1-qnorm(0.975)*SE.T2T1,
                                               UB.T2T1=ITT.T2T1+qnorm(0.975)*SE.T2T1,
                                               LB.T2C=ITT.T2C-qnorm(0.975)*SE.T2C,
                                               UB.T2C=ITT.T2C+qnorm(0.975)*SE.T2C,
                                               LB.T1C=ITT.T1C-qnorm(0.975)*SE.T1C,
                                               UB.T1C=ITT.T1C+qnorm(0.975)*SE.T1C,
                                               ) %>% 
         pivot_longer(
          cols = everything(),  # Pivot all columns
          names_to = c(".value", "Group"),  # Separate into values and group
          names_sep = "\\."  # The separator in column names
        )
                   

PATEC <- WideTable0 %>% ungroup() %>% group_by(Educ2) %>% 
                                      summarise(ITT.T2T1 = sum(NT2T1*YT2T1,na.rm = T)/sum(NT2T1,na.rm = T),
                                                ITT.T2C = sum(NT2C*YT2C,na.rm = T)/sum(NT2C,na.rm = T),
                                                ITT.T1C = sum(NT1C*YT1C,na.rm = T)/sum(NT1C,na.rm = T),
                                                SE.T2T1 = sqrt(sum((NT2T1/sum(NT2T1,na.rm = T))^2*VARYT2T1,na.rm = T)),
                                                SE.T2C  = sqrt(sum((NT2C/sum(NT2C,na.rm = T))^2*VARYT2C,na.rm = T)),
                                                SE.T1C  = sqrt(sum((NT1C/sum(NT1C,na.rm = T))^2*VARYT1C,na.rm = T)),
                                                LB.T2T1 =ITT.T2T1-qnorm(0.975)*SE.T2T1,
                                                UB.T2T1 =ITT.T2T1+qnorm(0.975)*SE.T2T1,
                                                LB.T2C  =ITT.T2C-qnorm(0.975)*SE.T2C,
                                                UB.T2C  =ITT.T2C+qnorm(0.975)*SE.T2C,
                                                LB.T1C  =ITT.T1C-qnorm(0.975)*SE.T1C,
                                                UB.T1C  =ITT.T1C+qnorm(0.975)*SE.T1C,
) %>% 
  pivot_longer(cols=c(ITT.T2T1,
               ITT.T2C ,
               ITT.T1C ,
               SE.T2T1 ,
               SE.T2C  ,
               SE.T1C  ,
               LB.T2T1 ,
               UB.T2T1 ,
               LB.T2C  ,
               UB.T2C  ,
               LB.T1C  ,
               UB.T1C  ),
     # Pivot all columns
    names_to = c(".value", "Group"),  # Separate into values and group
    names_sep = "\\."  # The separator in column names
  )

data <- PostDBT2C %>% group_by(StrataWave) %>% mutate(n=n(),NZ=sum(Z)) %>% filter(NZ!=n & NZ!=0)

estimatr::difference_in_means(ECSApp~Z,data=data,blocks = StrataWave)

lm_lin(ECSApp~Z,~factor(StrataWave),data)



library(dplyr)
library(tidyr)

PATE_long <- PATE %>%
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c(".value", "Group"),  # Separate into values and group
    names_sep = "\\."  # The separator in column names
  )

# View the result
print(PATE_long)

                                                 



Wmeans <- table %>% group_by(Assignment) %>% summarise(W.mean = sum(Mean*N,na.rm=TRUE)/sum(N,na.rm=TRUE),N=sum(N,na.rm=TRUE))




block_summary <- function(df, outcome, treatment, block) {
  library(dplyr)
  library(broom)
  
  # Compute block-level summary statistics
  summary_df <- df %>%
    group_by(!!sym(block), !!sym(treatment)) %>%
    summarise(
      count = n(),
      mean_outcome = mean(!!sym(outcome), na.rm = TRUE),
      sd_outcome = sd(!!sym(outcome), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = !!sym(treatment), 
      values_from = c(count, mean_outcome, sd_outcome),
      names_glue = "{.value}_{!!sym(treatment)}"
    ) %>%
    left_join(df %>% count(!!sym(block)) %>% rename(block_size = n), by = block)
  
  # Compute weighted average difference between treatment arms
  lm_model <- lm(as.formula(paste(outcome, "~", treatment, "+ factor(", block, ")")), data = df)
  
  treatment_coef <- tidy(lm_model) %>%
    filter(grepl(paste0("^", treatment), term))
  
  return(list(summary_table = summary_df, treatment_effects = treatment_coef))
}

# Example usage:
# result <- block_summary(mydata, "outcome_var", "treatment_var", "block_var")
# result$summary_table # View summary table
# result$treatment_effects # View treatment effect estimates

test <- block_summary(MainDB,outcome="ECSApp",treatment="Assignment",block="StrataWave")





sims <- 500
p.values <- rep(NA, sims)

for(i in 1:sims){
  db <- sample_n(PostDBT2C %>% filter(Z==0),by="StrataWave",size = nrow(PostDBT2C),replace = T)
  db <-db %>% mutate(Z=block_ra(prob = c(.5),blocks="StrataWave"),condition=c("Control","Treatment"))
  db$Z <- block_ra(blocks = blocks, block_m_each = block_m_each,
                conditions = c("control", "placebo", "treatment"))
  U <- rnorm(100)
  Y <- 0.2 * Z + U
  p.values[i] <- summary(lm(Y ~ Z))$coefficients[2, 4]
}

power <- mean(p.values <= 0.05)
power

group_by(HighLowECEC,Educ2,IntendUse) %>%
  mutate(n=n(),
         strata=cur_group_id(),
         Assignment=block_ra(blocks = c(strata), prob_each = c(.34, .33, .33),conditions = c("Control","T1","T2"))) %>% 
  arrange(strata,Assignment)



M <- declare_model(N = 1850,
                   U = rnorm(N),
                   X = rbinom(N, 1, prob = pnorm(U)),
                   Y = rbinom(N, 1, prob = pnorm(U + X)))
m <- M()


I <- declare_inquiry(Ybar = mean(Y[X==1]))

a_m <- I(m)


mstar <- fabricate(N = 1000,
                   U = rnorm(N),
                   X = rbinom(N, 1, prob = pnorm(U)),
                   Y = rbinom(N, 1, prob = pnorm(U)))




declaration_18.4 <-
  declare_model(
    N = 500,
    X = rep(c(0, 1), each = N / 2),
    U = rnorm(N, sd = 0.25),
    potential_outcomes(Y ~ 0.2 * Z + X + U)
  ) +
  declare_assignment(
    Z = block_ra(blocks = X, block_prob = c(0.2, 0.5)),
    probs =
      obtain_condition_probabilities(assignment = Z, 
                                     blocks = X, 
                                     block_prob = c(0.2, 0.5)),
    ipw = 1 / probs
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(
    Y ~ Z,
    covariates = ~ X,
    .method = lm_lin,
    weights = ipw,
    label = "Lin"
  )