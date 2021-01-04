### Introduction ### 

#The researcher is interested in what is the 

library(psych)
library(dplyr)
library(tidyverse)
attach(College)

# Data Cleaning 
str(College)
summary(College)

### Pearson's correlation S.F.Ratio <-> GradRate ###
####################################################
###         Research Question 1:                 ###
####################################################

# The research question is whether the correlation between graduation rate 
# and students and faculty ratio were negatively correlated.
qt(0.05, 775)
plot(Grad.Rate, S.F.Ratio, main = "Scatterplot", las = 1)
cor.test(Grad.Rate, S.F.Ratio, method = "pearson")


# Pearson's product-moment correlation

# data:  Grad.Rate and S.F.Ratio
# t = -8.9708, df = 775, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.3690817 -0.2415888
# sample estimates:
#        cor 
# -0.3067104

# Conclusion: 
# Reject the Null
# There is a significant negative relationship between the students and faculty ratio and graduation rate. 
# [r = -0.307, t(775)= -0.-8.971 p < 0.05] 

### Dependent correlation S.F.Ratio <-> GradRate ###
###             Private and Public               ###
####################################################
###         Research Question 2:                 ###
####################################################

# The research question is whether the correlation between student-faculty ratio and graduation rate 
# was stronger for the private university than for the public.

College%>% group_by(Private) %>% summarize(cor(S.F.Ratio,Grad.Rate))
# Public  1       0                     -0.0820
# Private 2       1                     -0.209 


College%>% group_by(Private) %>% summarize(mean(S.F.Ratio),mean(Grad.Rate))
# Private `mean(S.F.Ratio)` `mean(Grad.Rate)`
# <dbl>             <dbl>             <dbl>
#  1  Public          17.1              56.0
#  2  Private         12.9              69.0

n.Pri <- sum(Private) # = 565
n.Pub <- 777-sum(Private) # = 212

library(psych)
r.test(n = n.Pri, n2 = n.Pub, -0.209, -0.082, twotailed = FALSE)

# Correlation tests 
# Call:r.test(n = n.Pri, r12 = -0.209, r34 = -0.082, n2 = n.Pub, twotailed = FALSE)
# Test of difference between two independent correlations 
# z value 1.6    with probability  0.05

# Conclusion: 
# Fail to reject the null.
# The correlation between student-faculty ratio and graduation rate 
# was not stronger for the private university than for the public. 
# [r-public = -0.082, n-public = 212, r-private = -0.209, n-private = 565, Z = - 1.6, p > 0.05]

###       Simple Linear Regression (SLR)         ###
###          S.F.Ratio <-> GradRate              ###
####################################################
###         Research Question 3:                 ###
####################################################

# The research question was whether the student-faculty ratio was a predictor of the graduation rate of a college.

# H1: The student-faculty ratio is a negative predictor of the graduation rate of this college. 

# H0: The student-faculty ratio is not a negative predictor of the graduation rate of this college.


# critical value
qt(0.05, 775)

# Sample test analysis
summary(lm(formula = Grad.Rate ~ S.F.Ratio))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  84.2168     2.1713  38.786   <2e-16 ***
#  S.F.Ratio    -1.3310     0.1484  -8.971   <2e-16 ***
# Residual standard error: 16.36 on 775 degrees of freedom
# Multiple R-squared:  0.09407,	Adjusted R-squared:  0.0929 
# F-statistic: 80.48 on 1 and 775 DF,  p-value: < 2.2e-16

### So the sample test statistics t=-8.971, the unstandardized slope is -1.331

lm(formula = scale(Grad.Rate) ~ scale(S.F.Ratio))

# Coefficients:
#  (Intercept)  scale(S.F.Ratio)  
#  2.377e-16        -3.067e-01  

### So the standardized slope is -0.307

### Conclusion:
# Reject the null.
# The student-faculty ratio is a negative predictor of the graduation rate of this college.
# [ B = -1.331, ?? = -0.307, t(775)= -8.971,p <0.05 ]

# The result indicates that for two universities which differ by 1 point in student-faculty ratio
# is predicted to have 1.331 percentage decrease in the graduation rate. 
# Also, the university with I standard deviation higher in 1 point in student-faculty ratio
# is predicted to have 0.307 standard deviation decrease in the graduation rate. 

###      Multiple Linear Regression (MLR)        ###
###      S.F.Ratio + Top 10% <-> GradRate        ###
####################################################
###         Research Question 4:                 ###
####################################################

# The research question was whether the percentage of new students from top 10 of high school class
# and the student and faculty ratio were significant predictors of the graduation rate of a college.

# Critical value
# F critical for H0A = 3.007357
qf(0.95, 2, 774)
# t critical for H0B = -1.646825
qt(0.05, 774)
# t critical for H0C = 1.646825
qt(0.05, 774)

# 3. Sample test result

# 3-1. mean-center each of the two predictors:

Top10perc.ctrd <-  scale(Top10perc, center = TRUE,scale = FALSE)
S.F.Ratio.ctrd <-  scale(S.F.Ratio, center = TRUE, scale = FALSE)

# 3-2. unstandardized multiple linear regression model:

summary(lm(Grad.Rate ~ Top10perc + S.F.Ratio))

# Residuals:
#   Min      1Q    Median      3Q     Max 
# -47.619  -9.990   0.121   9.343  60.658 

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  61.9293     2.5876  23.933  < 2e-16 ***
#   Top10perc     0.4309     0.0326  13.216  < 2e-16 ***
#   S.F.Ratio    -0.5920     0.1453  -4.074  5.1e-05 ***

# Residual standard error: 14.79 on 774 degrees of freedom
# Multiple R-squared:  0.2609,	Adjusted R-squared:  0.259 
# F-statistic: 136.6 on 2 and 774 DF,  p-value: < 2.2e-16

# 3-3. mean centered and standardized multiple linear regression model:

lm(scale(Grad.Rate) ~ scale(Top10perc.ctrd) + scale(S.F.Ratio.ctrd))

# Coefficients:
# (Intercept)  scale(Top10perc.ctrd)  scale(S.F.Ratio.ctrd)  
# 1.978e-16              4.425e-01             -1.364e-01 

# Conclusion:
## Conclusion 1 : 
# The predictors (Top10perc and S.F.Ratio) explain 26.1% variability in the graduation rate of a college.
# [R-squared = 0.261, R-squared adj = 0.259, F(2, 774) = 136.6. P <0.05].

## Conclusion 2 : 
# The S.F.Ratio (while controlling for the Top10perc) is a sigt negative predictor of the Grad.Rate.
# Controlling for Top10perc, the larger S.F.Ratio is, the less Grad.Rate will be. 
# Specifically, for two colleges with equal Top10perc, for the college with one point higher in S.F.Ratio, 
# that college will have 0.592 points lower in the graduation rate.
# [B = -0.592, ??? ?? = -0.136, t(774) = -4.074, p < .05]

## Conclusion 3 : 
# The Top10perc (while controlling for the S.F.Ratio) is a sig positive predictor of the Grad.Rate.
# Controlling for S.F.Ratio, the larger Top10perc is, the larger Grad.Rate will be. 
# Specifically, for two colleges with equal S.F.Ratio, for the college with one point higher in Top10perc, 
# that college will have 0.431 points lower in the graduation rate.
# [B = 0.431, ?? = -0.443, t(774) = 13.216, p < .05]

## Conclusion 4 : 
# The Grad.Rate with the mean Top10perc and mean S.F.Ratio is significantly greater than zero.
# The intercept is interpreted as the predicted Grad.Rate (estimated to be 61.929) 
# for some college at the mean on Top10perc and mean on the S.F.Ratio scale. 
# [B = 61.929, t(45) = 23.933, p < .05]


###                 Moderation                   ###
###        FPhD -> S.F.Ratio ~ GradRate          ###
####################################################
###         Research Question 5:                 ###
####################################################

# The research question was whether the relationship between the student and faculty ratio (S.F.Ratio) 
# and the graduation rate of a college (Grad.Rate) was moderated by Percent of faculty with Ph.D.'s (FPhD).

#1. Critical value: ?? = 0.05, two-tailed, df =777-3-1= 773
qt(0.975, (773))
# t critical = 1.963

#2. mean-center predictor and moderator

FPhD.ctrd <-  scale(FPhD, center = TRUE,scale = FALSE)
S.F.Ratio.ctrd <-  scale(S.F.Ratio, center = TRUE, scale = FALSE)

#3. Build the model, test the Hypothesis and obtain the unstandardized slope.
summary(lm(Grad.Rate ~ S.F.Ratio.ctrd+FPhD.ctrd + S.F.Ratio.ctrd :FPhD.ctrd))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              65.263487   0.564145 115.686  < 2e-16 ***
#  S.F.Ratio.ctrd           -1.201634   0.143110  -8.397  < 2e-16 ***
#  FPhD.ctrd                 0.260893   0.035402   7.369 4.41e-13 ***
#  S.F.Ratio.ctrd:FPhD.ctrd -0.023717   0.007592  -3.124  0.00185 ** 

# Residual standard error: 15.62 on 773 degrees of freedom
# Multiple R-squared:  0.1759,	Adjusted R-squared:  0.1727 
# F-statistic:    55 on 3 and 773 DF,  p-value: < 2.2e-16

# 4. Obtain the standardized slope.
lm(scale(Grad.Rate) ~ scale(S.F.Ratio.ctrd) + scale(FPhD.ctrd) + scale(S.F.Ratio.ctrd * FPhD.ctrd),data = College)

# Coefficients:
# (Intercept)   scale(S.F.Ratio.ctrd)    scale(FPhD.ctrd)  scale(S.F.Ratio.ctrd * FPhD.ctrd)  
# 2.202e-16        -2.769e-01               2.480e-01               -1.043e-01 






# 5. Visualize the moderation

library(interactions)
install.packages("interactions")
Grad.Rate.out <- lm(Grad.Rate ~ S.F.Ratio.ctrd+FPhD.ctrd + S.F.Ratio.ctrd * FPhD.ctrd,data = College)
interact_plot(Grad.Rate.out, pred = S.F.Ratio.ctrd, modx = FPhD.ctrd, colors = c("#42c5f4","#54f284","#f45dcc"), main.title = "Interaction Plot")


## Using ggplot2 to create interaction effec plot 

dat$sf<-dat$stud.fac.ratio
dat$fphd <- dat$X..fac.with.PHD
dat$y<-dat$graduation.rate
head(dat)

FPhD.SD <- round(c(mean(FPhD)-sd(FPhD),
                   mean(FPhD),
                   mean(FPhD)+sd(FPhD)),2)
# 56.33 72.66 88.99

Grad.Rate.out <- lm(Grad.Rate ~ S.F.Ratio.ctrd+FPhD.ctrd + S.F.Ratio.ctrd * FPhD.ctrd,data = College)

summary(Grad.Rate.out)

library(effects)
# run interaction effect
interX.SD <-effect(c('S.F.Ratio.ctrd * FPhD.ctrd'), Grad.Rate.out, 
                   xlevel=list(FPhD.ctrd=c(56.33, 72.66, 88.99)))
interX.SD<-data.frame(interX.SD)

## rename the three categories
interX.SD$FPhD.ctrd <-factor(interX.SD$FPhD.ctrd,
                        levels = c(56.33, 72.66, 88.99),
                        labels = c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

library(ggplot2) 
Plot.SD<-ggplot(data=interX.SD, aes(x=S.F.Ratio.ctrd, y=fit, group=FPhD.ctrd))+
  geom_line(size=1, aes(color=FPhD.ctrd))+
  ylab("Graduation.Rate")+
  xlab("FPhD.ctrd")+
  scale_color_manual(values=c("#42c5f4","#54f284","#f45dcc"))+ #custom color coding 
  theme_bw()+ #deleting the gray background 
  theme(text = element_text(size=14, color="black"))+ #changing font!
  ggtitle("Interaction Plot - ggplot") #adding a title! 
Plot.SD

# The graph depicts predicted Graduation rate scores given student and faculty ratio for those with high, 
# medium and low ratio of faculty with a PhD degree (where high, medium and low represented those 
# with scores on the ratio of faculty with a PhD degree that were one standard deviation above the mean, 
# at the mean and one standard deviation below the mean, respectively).

# The pattern of the results indicates that the prediction of graduation rate by student and faculty ratio 
# depends on the ration of faculty with a PhD degree. More specifically, the negative relationship between 
# graduation rate and sf becomes less strong the fewer faculty with a PhD degree in a college. 
# When the ratio of the faculty with a PhD degree is getting higher, the negative relationship 
# between the graduation rate and student-faculty ratio will get stronger.

###                 Mediation                    ###
###      AccRate -> Top 10% -> GradRate          ###
###           --------------------^              ###
####################################################
###         Research Question 6:                 ###
####################################################

# Whether The ratio of the applicants accepted and received by a college (AccRate) 
# affected the graduation rate of a college (Grad.Rate) through the indirect effect 
# on percentage of new students from top 10% of high school class (Top10perc). 

# 1. a coefficient 

summary(lm(Top10perc ~ AccRate))
# a = -57.402   Se-a = 3.782   

# 2. b coefficient

summary(lm(Grad.Rate ~ Top10perc + AccRate))

#  b = 0.45175  Se-b = 0.03456

-57.402*0.45175

#  indirect effect estimate: ab = -25.93135

install.packages("RMediation")

library(RMediation)

#95% confidence interval with Meeker & Escobar's distribution of product
medci(mu.x = -57.402, mu.y = 0.45175, se.x = 3.782, se.y =  0.03456, type = "dop")
# [1] -31.08719 -20.97980
# Conclusion: 
# The result led me to reject H0 and infer that the ratio of the applicants accepted 
# and received by a college affects a college's graduation rate through the indirect effect 
# on the ratio of top 10 students enrolled in this college.
# [ab =-25.931, distribution of products 95% CI = (-31.087, -20.980), p <0.05]

