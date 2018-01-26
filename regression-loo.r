# Calculate regression statistics and the reliability through glm (aic, r2), and cross validation through leave one out (loo).
# To this end, also calculate combined adjusted R-squared, confidence intervals and aic.



# packages:

library('MASS')
library('boot')

# get and define data

df <- read.table("~/Documents/PDnijm/wind/data/regression_data.csv",header = TRUE,sep = ",")

logE  = df$logE
logD  = df$logD
logh  = df$logh
dfT   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1)
DHcaduff = 2*(df$logD)+(3/7)*(df$logh)

# define the different models and run glm
# 1.: log E = I + c1 logD +c2 logh +c3T
# 2.: log E = I + c1 logD +c2T
# 3.: log E = I + c1 logh +c3T
# 4.: log E = I + c1 DHcaduff +c3T

# intercept, T
glm1a <- glm(df$logE ~ df$logD + df$logh +df$T, data = df)
glm2a <- glm(df$logE ~ df$logD + df$T, family = gaussian, data = df)
glm3a <- glm(df$logE ~ df$logh + df$T, family = gaussian, data = df)
glm4a <- glm(df$logE ~ DHcaduff + df$T, family = gaussian, data = df)

# intercept, no T
glm1b <- glm(df$logE ~ df$logD + df$logh, family = gaussian, data = df)
glm2b <- glm(df$logE ~ df$logD, family = gaussian, data = df)
glm3b <- glm(df$logE ~ df$logh, family = gaussian, data = df)
glm4b <- glm(df$logE ~ DHcaduff, family = gaussian, data = df)

# no intercept, T
glm1c <- glm(df$logE ~ 0 + df$logD + df$logh +df$T, family = gaussian, data = df)
glm2c <- glm(df$logE ~ 0 + df$logD + df$T, family = gaussian, data = df)
glm3c <- glm(df$logE ~ 0 + df$logh + df$T, family = gaussian, data = df)
glm4c <- glm(df$logE ~ 0 + DHcaduff + df$T, family = gaussian, data = df)

# no intercept, no T
glm1d <- glm(df$logE ~ 0 + df$logD + df$logh, family = gaussian, data = df)
glm2d <- glm(df$logE ~ 0 + df$logD, family = gaussian, data = df)
glm3d <- glm(df$logE ~ 0 + df$logh, family = gaussian, data = df)
glm4d <- glm(df$logE ~ 0 + DHcaduff, family = gaussian, data = df)


# calculate the relevant statistics

#matrix rijen zijn de versch modellen (1 t/m 4), cols de versch types (a t/m d)
aic = matrix(c(glm1a$aic,glm2a$aic,glm3a$aic,glm4a$aic, # col1
               glm1b$aic,glm2b$aic,glm3b$aic,glm4b$aic) # col2
             , nrow = 8,ncol = 1)
print(aic)

ci1a = confint(glm1a)
ci2a = confint(glm2a)
ci3a = confint(glm3a)
ci4a = confint(glm4a)

ci1b = confint(glm1b)
ci2b = confint(glm2b)
ci3b = confint(glm3b)
ci4b = confint(glm4b)

ci1c = confint(glm1c)
ci2c = confint(glm2c)
ci3c = confint(glm3c)
ci4c = confint(glm4c)

ci1d = confint(glm1d)
ci2d = confint(glm2d)
ci3d = confint(glm3d)
ci4d = confint(glm4d)

coefciglm1a = matrix(c(glm1a$coef,ci1a),nrow = 4, ncol = 3)
rownames(coefciglm1a)<-rownames(ci1a)

coefcimat = matrix(c(glm1a$coef[1],glm2a$coef[1],glm3a$coef[1],glm4a$coef[1], #intercept
                     glm1b$coef[1],glm2b$coef[1],glm3b$coef[1],glm4b$coef[1],
                     ci1a[1,1],ci2a[1,1],ci3a[1,1],ci4a[1,1],
                     ci1b[1,1],ci2b[1,1],ci3b[1,1],ci4b[1,1],
                     ci1a[1,2],ci2a[1,2],ci3a[1,2],ci4a[1,2],
                     ci1b[1,2],ci2b[1,2],ci3b[1,2],ci4b[1,2],
                     glm1a$coef[2],glm2a$coef[2],NA,NA, #log D
                     glm1b$coef[2],glm2b$coef[2],NA,NA,
                     ci1a[2,1],ci2a[2,1],NA,NA,
                     ci1b[2,1],ci2b[2,1],NA,NA,
                     ci1a[2,2],ci2a[2,2],NA,NA,
                     ci1b[2,2],ci2b[2,2],NA,NA,
                     glm1a$coef[3],NA,glm3a$coef[2],NA, #logh
                     glm1b$coef[3],NA,glm3b$coef[2],NA,
                     ci1a[3,1],NA,ci3a[2,1],NA,
                     ci1b[3,1],NA,ci3b[2,1],NA,
                     ci1a[3,2],NA,ci3a[2,2],NA,
                     ci1b[3,2],NA,ci3b[2,2],NA,
                     NA,NA,NA,glm4a$coef[2], #caduff
                     NA,NA,NA,glm4b$coef[2],
                     NA,NA,NA,ci4a[2,1],
                     NA,NA,NA,ci4b[2,1],
                     NA,NA,NA,ci4a[2,2],
                     NA,NA,NA,ci4b[2,2],
                     glm1a$coef[4],glm2a$coef[3],glm3a$coef[3],glm4a$coef[3], #T
                     NA,NA,NA,NA,
                     ci1a[4,1],ci2a[3,1],ci3a[3,1],ci4a[3,1],
                     NA,NA,NA,NA,
                     ci1a[4,2],ci2a[3,2],ci3a[3,2],ci4a[3,2],
                     NA,NA,NA,NA
                     ),
                   nrow = 8, ncol = 15)

rownames(coefcimat) <- c("I, logD, logh, T","I, logD, T","I, logh, T","I, caduff, T",
                         "I, logD, logh","I, logD","I, logh","I, caduff"
                         )
colnames(coefcimat) <- c("I","2.5%","97.5%","logD","2.5%","97.5%","logh","2.5%","97.5%","caduff","2.5%","97.5%","T","2.5%","97.5%")

# x1a = glm1a$coefficients[1] + glm1a$coef[2]*logD + glm1a$coef[3]*logh + glm1a$coef[4]*T
# x2a = glm2a$coefficients[1] + glm2a$coef[2]*logD + glm2a$coef[3]*T
# x3a = glm3a$coefficients[1] + glm3a$coef[2]*logh + glm3a$coef[3]*T
# x4a = glm4a$coefficients[1] + glm4a$coef[2]*DHcaduff + glm4a$coef[3]*T
#cor(x1a,logE)

r1a = summary(lm(df$logE ~ df$logD + df$logh +df$T))$adj.r.squared
r2a = summary(lm(df$logE ~ df$logD + df$T))$adj.r.squared
r3a = summary(lm(df$logE ~ df$logh + df$T))$adj.r.squared
r4a = summary(lm(df$logE ~ DHcaduff + df$T))$adj.r.squared

r1b = summary(lm(df$logE ~ df$logD + df$logh))$adj.r.squared
r2b = summary(lm(df$logE ~ df$logD))$adj.r.squared
r3b = summary(lm(df$logE ~ df$logh))$adj.r.squared
r4b = summary(lm(df$logE ~ DHcaduff))$adj.r.squared

# r1c = summary(lm(df$logE ~ 0 + df$logD + df$logh +df$T))$adj.r.squared
# r2c = summary(lm(df$logE ~ 0 + df$logD + df$T))$adj.r.squared
# r3c = summary(lm(df$logE ~ 0 + df$logh + df$T))$adj.r.squared
# r4c = summary(lm(df$logE ~ 0 + DHcaduff + df$T))$adj.r.squared
#
# r1d = summary(lm(df$logE ~ 0 + df$logD + df$logh))$adj.r.squared
# r2d = summary(lm(df$logE ~ 0 + df$logD))$adj.r.squared
# r3d = summary(lm(df$logE ~ 0 + df$logh))$adj.r.squared
# r4d = summary(lm(df$logE ~ 0 + DHcaduff))$adj.r.squared

rmat= matrix(c(r1a,r2a,r3a,r4a, # col1
               r1b,r2b,r3b,r4b#, # col2
               #r1c,r2c,r3c,r4c, # col3
               #r1d,r2d,r3d,r4d # col4!
              )
             , nrow = 8,ncol = 1)


# # calculate loo statistics
# # vector: v<- rep(NA,28)
# # array <- array(NA,dim=c(x,y,z))
# loo_r1a <- rep(NA,28)
# loo_r1b <- rep(NA,28)
# loo_r1c <- rep(NA,28)
# loo_r1d <- rep(NA,28)
#
# loo_r2a <- rep(NA,28)
# loo_r2b <- rep(NA,28)
# loo_r2c <- rep(NA,28)
# loo_r2d <- rep(NA,28)
#


# for(i in 1:28) {
#
#   print(i)
#   dlogE = logE
#   dlogD = logD
#   dlogh = logh
#   dT    = dfT
#   dcad  = DHcaduff
#
#   dlogE[i]  <- NA
#   dlogD[i]  <- NA
#   dlogh[i]  <- NA
#   dT[i]     <- NA
#   dcad[i]   <- NA
#
#   # print(dlogE[1:5])
#
#   loo_r1a[i] <- summary(lm(dlogE ~ dlogD + dlogh +dT))$adj.r.squared
#   loo_r2a[i] <- summary(lm(dlogE ~ dlogD + dT))$adj.r.squared
#   loo_r3a[i] <- summary(lm(dlogE ~ dlogh + dT))$adj.r.squared
#   loo_r4a[i] <- summary(lm(dlogE ~ dcad + dT))$adj.r.squared
#
#   loo_r1b[i] <- summary(lm(dlogE ~ dlogD + dlogh))$adj.r.squared
#   loo_r2b[i] <- summary(lm(dlogE ~ dlogD))$adj.r.squared
#   loo_r3b[i] <- summary(lm(dlogE ~ dlogh))$adj.r.squared
#   loo_r4b[i] <- summary(lm(dlogE ~ dcad))$adj.r.squared

#
# }

glm_cf1a <- rep(NA,4)
glm_cf2a <- rep(NA,3)
glm_cf3a <- rep(NA,3)
glm_cf4a <- rep(NA,3)

glm_cf1b <- rep(NA,3)
glm_cf2b <- rep(NA,2)
glm_cf3b <- rep(NA,2)
glm_cf4b <- rep(NA,2)

looerr1a <- rep(NA,28)
looerr2a <- rep(NA,28)
looerr3a <- rep(NA,28)
looerr4a <- rep(NA,28)

looerr1b <- rep(NA,28)
looerr2b <- rep(NA,28)
looerr3b <- rep(NA,28)
looerr4b <- rep(NA,28)

tss = rep(NA,28)

for(i in 1:28) {

  print(i)
  dlogE = logE
  dlogD = logD
  dlogh = logh
  dT    = dfT
  dcad  = DHcaduff

  dlogE[i]  <- NA
  dlogD[i]  <- NA
  dlogh[i]  <- NA
  dT[i]     <- NA
  dcad[i]   <- NA

  # print(dlogE[1:5])

  # prediction errors
  glm_cf1a <- summary(glm(dlogE ~ dlogD + dlogh +dT))$coefficients[,1]
  glm_cf2a <- summary(glm(dlogE ~ dlogD + dT))$coefficients[,1]
  glm_cf3a <- summary(glm(dlogE ~ dlogh + dT))$coefficients[,1]
  glm_cf4a <- summary(glm(dlogE ~ dcad + dT))$coefficients[,1]

  glm_cf1b <- summary(glm(dlogE ~ dlogD + dlogh))$coefficients[,1]
  glm_cf2b <- summary(glm(dlogE ~ dlogD))$coefficients[,1]
  glm_cf3b <- summary(glm(dlogE ~ dlogh))$coefficients[,1]
  glm_cf4b <- summary(glm(dlogE ~ dcad))$coefficients[,1]

  # prediction errors
  looerr1a[i] <- (logE[i] - (glm_cf1a[1] + glm_cf1a[2]*logD[i] + glm_cf1a[3]*logh[i] + glm_cf1a[4]*dfT[i]))**2
  looerr2a[i] <- (logE[i] - (glm_cf2a[1] + glm_cf2a[2]*logD[i] + glm_cf2a[3]*dfT[i]))**2
  looerr3a[i] <- (logE[i] - (glm_cf3a[1] + glm_cf3a[2]*logh[i] + glm_cf3a[3]*dfT[i]))**2
  looerr4a[i] <- (logE[i] - (glm_cf4a[1] + glm_cf4a[2]*DHcaduff[i] + glm_cf4a[3]*dfT[i]))**2
  #
  looerr1b[i] <- (logE[i] - (glm_cf1b[1] + glm_cf1b[2]*logD[i] + glm_cf1b[3]*logh[i] ))**2
  looerr2b[i] <- (logE[i] - (glm_cf2b[1] + glm_cf2b[2]*logD[i] ))**2
  looerr3b[i] <- (logE[i] - (glm_cf3b[1] + glm_cf3b[2]*logh[i]))**2
  looerr4b[i] <- (logE[i] - (glm_cf4b[1] + glm_cf4b[2]*DHcaduff[i]))**2
  #
  # # total sum of squares
  tss[i] <- (logE[i] - mean(logE))**2
}

rloo1a = 1 - sum(looerr1a)/sum(tss)
rloo2a = 1 - sum(looerr2a)/sum(tss)
rloo3a = 1 - sum(looerr3a)/sum(tss)
rloo4a = 1 - sum(looerr4a)/sum(tss)

rloo1b = 1 - sum(looerr1b)/sum(tss)
rloo2b = 1 - sum(looerr2b)/sum(tss)
rloo3b = 1 - sum(looerr3b)/sum(tss)
rloo4b = 1 - sum(looerr4b)/sum(tss)

#adjusted r2
n = 28
rloo1a = 1-(1-rloo1a**2)*(n-1)/(n-3-1)
rloo2a = 1-(1-rloo2a**2)*(n-1)/(n-2-1)
rloo3a = 1-(1-rloo3a**2)*(n-1)/(n-2-1)
rloo4a = 1-(1-rloo4a**2)*(n-1)/(n-2-1)

rloo1b = 1-(1-rloo1b**2)*(n-1)/(n-2-1)
rloo2b = 1-(1-rloo2b**2)*(n-1)/(n-1-1)
rloo3b = 1-(1-rloo3b**2)*(n-1)/(n-1-1)
rloo4b = 1-(1-rloo4b**2)*(n-1)/(n-1-1)


cat(r1a," ",rloo1a)
cat(r2a," ",rloo2a)
cat(r3a," ",rloo3a)
cat(r4a," ",rloo4a)

cat(r1b," ",rloo1b)
cat(r2b," ",rloo2b)
cat(r3b," ",rloo3b)
cat(r4b," ",rloo4b)

loo_rmat= matrix(c(rloo1a,rloo2a,rloo3a,rloo4a, # col1
                   rloo1b,rloo2b,rloo3b,rloo4b #, # col2
                   # mean(loo_r1c),mean(loo_r2c),mean(loo_r3c),mean(loo_r4c), # col3
                   # mean(loo_r1d),mean(loo_r2d),mean(loo_r3d),mean(loo_r4d)
                   ) # col4!
             , nrow = 8,ncol = 1)


# store data

datamat = matrix(c(coefcimat,rmat,aic,loo_rmat), nrow = 8 , ncol = 18)
rownames(datamat) <- c("I, logD, logh, T","I, logD, T","I, logh, T","I, caduff, T",
                         "I, logD, logh","I, logD","I, logh","I, caduff")

colnames(datamat) <- c("I","2.5%","97.5%","logD","2.5%","97.5%","logh",
                         "2.5%","97.5%","caduff","2.5%","97.5%","T","2.5%","97.5%",
                         "adj R^2","AIC","LOO adj R^2")

write.csv(datamat,file = "~/Documents/PDnijm/wind/data/regression-statistics.csv")
write.csv(aic,file = "~/Documents/PDnijm/wind/data/aic.csv")
write.csv(rmat,file = "~/Documents/PDnijm/wind/data/r.csv")
write.csv(coefciglm1a,file = "~/Documents/PDnijm/wind/data/coeffs.csv")
write.csv(loo_rmat,file = "~/Documents/PDnijm/wind/data/loo_rmat.csv")

