### Time Series Analysis ###
### Fixed/Random Effects ###

#load packages
library(plm)
library(plyr)
library(stargazer)

#import data
panel=read.csv(file.choose())
panel=read.csv("panel-for-R.csv")
#I want to investigate the relationship between people's opinions on government should help the sick and financial pressure of paying bills.

vars = c("helpsick", "finan4", "idnum", "panelwave", "realinc", "age", "sex", "attend")
data = panel[,vars] #subset

#In general, some people think that it is the responsibility of the government in Washington to see to it that people have help in paying for doctors and hospital bills. Others think that these matters are not the responsibility of the federal government and that people should take care of these things themselves. A. Where would you place yourself on this scale, or haven't you made up your mind on this? 
#1: Government should help 3: Agree with both 5: People help selves

data$help = 6-data$helpsick # reverse code so that a score of 1 means people help themselves; 
#a score of 5 means government should help; 
##data$finsat = 4-data$satfin # reverse code satisfaction with financial situation so that a score of 1 means not satisfied at all; 
#a score of 2 means more or less satisfied; a score of 3 means pretty well satisfied.
data$bills = ifelse(data$finan4==1, 1,0) # dummy for Did any of the following financial matters happen to you during the last year... 4. Pressured to pay bills by stores, creditors, or bill collectors, where NOW 1 means yes, that is an issue and 0 means, no, not an issue


#firstD command from QMSS package
firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}

data <- ddply(data, "idnum", mutate, d.help = firstD(help), d.finsat = firstD(finsat), 
              d.realinc = firstD(realinc), d.bills = firstD(bills)) ## create first differences ##

#changes of opinions on government should help the sick
table(data$d.help)
prop.table(table(data$d.help))

data$helpup = ifelse(data$d.help>=1,1,0) #people who increased their opinions on government should help the sick
data$helpdown = ifelse(data$d.help<=-1,1,0) #people who decreased their opinions on government should help the sick

table(data$helpup)
table(data$helpdown)
prop.table(table(data$helpup, data$helpdown))

#changes of bills pressure
table(data$d.bills)
prop.table(table(data$d.bills))

data$inn = ifelse(data$d.bills==1,1,0) #entering bills struggles 
data$out = ifelse(data$d.bills==-1,1,0) #leaving bills struggles

table(data$out)
table (data$inn)
prop.table(table(data$out, data$inn))

#(a) Run an OLS regression, including at least one independent variable and a time variable (as dummies).  
#Explain how you think your independent variable relates to your dependent variable.  
#Interpret your results. Did you find what you expected to find? 

#Research question:
#Will you agree more on the opinion that government should help the sick if you're pressured to pay your bills?

#summary(lm(judge ~ bills + as.factor(panelwave), data))

#summary(lm(judge ~ finsat + as.factor(panelwave), data))

help.pool <- plm(help ~ bills + as.factor(panelwave), # model formula
               index = c("idnum", "panelwave"), # id & time variables
               model = "pooling", 
               data = data) 

summary(help.pool)

#The OLS regression result indicates that being pressured to pay bills increases people's opinion on helping the sick by 0.46 on average, which is statistically significant at 0.1% significance level.
#People in wave 3, relative to wave 1, decreased their opinion on helping the sick by 0.34, which is also statistically significant at 0.1% significance level.
#It is what I expected.

## first difference model 
help.fd <- plm(help ~ bills + as.factor(panelwave), # model formula
             index = c("idnum", "panelwave"), # id & time variables
             model = "fd", #first difference model
             data = data) 

summary(help.fd)
#Now the coefficient on bills is -0.08, meaning that changing from no bill pressure to having bill pressure decreases people's opinion on helping the sick by 0.08 on average, for the same person, net of waves.
#The negative sign is not what I expected. However, it is statistically insignificant. The changes in bill pressure do NOT have significant effect on changes in opinion on helping the sick.
#The coefficients on panel dummies are insignificant, too.
#It seems like a story of individual heterogeneity.

#(b) Then run a fixed effect model version of that OLS model. 
#Interpret your results. Did you find what you expected to find? Why? Why not? 
  
help.fe <- plm(help ~ bills + as.factor(panelwave), # model formula
                index = c("idnum", "panelwave"), # id & time variables
                model = "within", #fixed effect model
                data = data) 

summary(help.fe)

#The fixed effect model gives essentially the same result as the first differences model.
#Changing from no bill pressure to having bill pressure decreases people's opinion on helping the sick by 0.08 on average, for the same person, net of waves.
#But the estimate is NOT statistically significant, which is consistent with the FD model indicating individual heterogeneity.

## (c) Then include an additional predictor in your fixed effects model that you think might account for 
#the initial relationship you found between your X and your Y.  
#What effect does that new independent variable have in your new regression?

#I add controls for sex, age, family income (you may need more help if having low income) and 
#attending religious services (maybe you think the government should help the sick if you attend religious services more often).

help.fe2 <- plm(help ~ bills +sex + age + attend + realinc + as.factor(panelwave), # model formula
                index = c("idnum", "panelwave"), # id & time variables
                model = "within", 
                data = data) 

summary(help.fe2)

#The estimates of coefficients are quite small. NONE of them is statistically significant, except for the dummy wave 3.

## (d) Then run a random effects model equivalent to your fixed effects model in step (b).  
#Interpret the results.

help.re <- plm(help ~ bills + as.factor(panelwave), # model formula
                index = c("idnum", "panelwave"), # id & time variables
                model = "random", #random effects model
                data = data) 

summary(help.re)

#Changing from no bill pressure to having bill pressure increases people's opinion on helping the sick by 0.35 on average, net of waves, adjusting for the fact that the same person is repeatedly answering the same survey.
#It is statistically significant at 0.1% significance level.
#People in wave 3, relative to wave 1, decreased their opinion on helping the sick by 0.33, adjusting for the fact that the same person is repeatedly answering the same survey.
#It is also statistically significant at 0.1% significance level.

## (e) Run a Hausman test to compare your fixed effects and your random effects models. What do you conclude? 

phtest(help.fe, help.re) #Hausman test comparing FE and RE 

#p-value is quite small, we can reject the null hypothesis that they are essentially the same coefficients in favor of the alternative hypothesis.
#We'd better choose the fixed effects model.

#summary table
stargazer(help.pool, help.fd, help.fe, help.re,
          title="Regression Results", 
          align=TRUE, 
          dep.var.labels=c("Help the sick"), 
          covariate.labels=c("Bill pressure","2010"),  
          no.space=TRUE, 
          column.labels=c("Pooled", "First Diff", "Fixed Effects", "Random Effects"), 
          dep.var.caption="", 
          model.numbers=FALSE,
          type = "text", omit = "Constant")




