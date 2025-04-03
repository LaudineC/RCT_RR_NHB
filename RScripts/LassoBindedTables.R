

## ---- StackedPostLassoTable ----

gm <- c("nobs", "r.squared","adj.r.squared")
panels <- list(
  "Outcome: Early childcare application" = list(
         "Information + support vs. Control_Basic"=ECSAppT2C$`Basic Model`, 
         "Information + support vs. Control_Post-Lasso"=ECSAppT2C$`ITT Post lasso`,
         "Information + support vs. Information-only_Basic"=ECSAppT2T1$`Basic Model`, 
         "Information + support vs. Information-only_Post-Lasso"=ECSAppT2T1$`ITT Post lasso`,
         "Information-only vs. Control_Basic"=ECSAppT1C$`Basic Model`, 
         "Information-only vs. Control_Post-Lasso"=ECSAppT1C$`ITT Post lasso`
  ),
  "Outcome: Early childcare access" = list(
         "Information + support vs. Control_Basic"=ECSUseYesT2C$`Basic Model`, 
         "Information + support vs. Control_Post-Lasso"=ECSUseYesT2C$`ITT Post lasso`,
         "Information + support vs. Information-only_Basic"=ECSUseYesT2T1$`Basic Model`, 
         "Information + support vs. Information-only_Post-Lasso"=ECSUseYesT2T1$`ITT Post lasso`,
         "Information-only vs. Control_Basic"=ECSUseYesT1C$`Basic Model`, 
         "Information-only vs. Control_Post-Lasso"=ECSUseYesT1C$`ITT Post lasso`
  ),
  "Outcome: Early daycare application"=list(
         "Information + support vs. Control_Basic"=ECSAppCrecheT2C$`Basic Model`, 
         "Information + support vs. Control_Post-Lasso"=ECSAppCrecheT2C$`ITT Post lasso`,
         "Information + support vs. Information-only_Basic"=ECSAppCrecheT2T1$`Basic Model`, 
         "Information + support vs. Information-only_Post-Lasso"=ECSAppCrecheT2T1$`ITT Post lasso`,
         "Information-only vs. Control_Basic"=ECSAppCrecheT1C$`Basic Model`, 
         "Information-only vs. Control_Post-Lasso"=ECSAppCrecheT1C$`ITT Post lasso`
  ),
  "Outcome: Early daycare access"=list(
         "Information + support vs. Control_Basic"=USeCrecheT2C$`Basic Model`, 
         "Information + support vs. Control_Post-Lasso"=USeCrecheT2C$`ITT Post lasso`,
         "Information + support vs. Information-only_Basic"=USeCrecheT2T1$`Basic Model`, 
         "Information + support vs. Information-only_Post-Lasso"=USeCrecheT2T1$`ITT Post lasso`,
         "Information-only vs. Control_Basic"=USeCrecheT1C$`Basic Model`, 
         "Information-only vs. Control_Post-Lasso"=USeCrecheT1C$`ITT Post lasso`
))

modelsummary(
  panels,
  shape = "rbind",
  coef_map=c("Z"="ITT"),
  fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
  estimate = '{estimate}{stars} ({std.error})',
  statistic = c("conf.int"),
  stars = c('*' = .1,'**' = .05, '***' = .01),
  gof_map = gm,
  notes=paste(" 
Basic specifications run OLS on a treatment dummy and block fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets.")) 


#%>% 
  #theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) #|>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:7),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  hline(2,part="body")







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
Basic specification run OLS on a treatment dummy and block fixed effects.
Post-lasso use coefficients of an OLS regression of the outcome on a treatment dummy, the de-meaned covariates and interactions. Covariates were selected by a lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation. 
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets."), output = 'flextable') %>% 
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
