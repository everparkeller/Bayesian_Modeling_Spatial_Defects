######################
### Load Libraries ###
######################

library(coda)
library(mcmcplots)
library(nimble)
library(pacman)
library(reshape2)
library(smfsb)



#####################
### Load Datasets ###
#####################

source("Source - Clean Data.R")
total.data.eh <- total.data.eh[,-which(colnames(total.data.eh) %in% c("hm", "hc"))]
total.data.op <- total.data.op[, which(colnames(total.data.op) %in% c("subjectnumber", "oi"))]
total.data.peb <- total.data.peb[, which(colnames(total.data.peb) %in% c("subjectnumber", "pi"))]
total.data.dc <- total.data.dc[, which(colnames(total.data.dc) %in% c("subjectnumber", "di"))]

total.data.incisal <- merge(total.data.eh, total.data.op, by = "subjectnumber")
total.data.incisal <- merge(total.data.incisal, total.data.peb, by = "subjectnumber")
total.data.incisal <- merge(total.data.incisal, total.data.dc, by = "subjectnumber")


##########################
### Adjust Sample Size ###
##########################

keep.rows <- which(complete.cases(total.data.incisal$hi, total.data.incisal$oi, total.data.incisal$pi, total.data.incisal$di)==TRUE|total.data.incisal$hi==1|total.data.incisal$oi==1|total.data.incisal$pi==1|total.data.incisal$di==1)
total.data.incisal <- total.data.incisal[keep.rows,]
rm(list=setdiff(ls(), "total.data.incisal"))


###########################################
### Check Missingness within Full Model ###
###########################################

## Create Missingness Vectors for Inits in Imputation ##
# Note: Predictors are the same for each outcome thus only using EH to gather missingness vectors
miss.OH2D <- ifelse(is.na(total.data.incisal$OH2D), mean(total.data.incisal$OH2D, na.rm=TRUE), NA)
miss.Ca <- ifelse(is.na(total.data.incisal$Ca), mean(total.data.incisal$Ca, na.rm=TRUE), NA)
miss.P <- ifelse(is.na(total.data.incisal$P), mean(total.data.incisal$P, na.rm=TRUE), NA)
miss.Formula <- ifelse(is.na(total.data.incisal$formula), 0, NA)
miss.MomBMI <- ifelse(is.na(total.data.incisal$mombmi), mean(total.data.incisal$mombmi, na.rm=TRUE), NA)
miss.MomCa_28 <- ifelse(is.na(total.data.incisal$momCa_28), mean(total.data.incisal$momCa_28, na.rm=TRUE), NA)
miss.MomCa_36 <- ifelse(is.na(total.data.incisal$momCa_36), mean(total.data.incisal$momCa_36, na.rm=TRUE), NA)
miss.MomP_12 <- ifelse(is.na(total.data.incisal$momP_12), mean(total.data.incisal$momP_12, na.rm=TRUE), NA)
miss.MomP_28 <- ifelse(is.na(total.data.incisal$momP_28), mean(total.data.incisal$momP_28, na.rm=TRUE), NA)
miss.MomP_36 <- ifelse(is.na(total.data.incisal$momP_36), mean(total.data.incisal$momP_36, na.rm=TRUE), NA)
miss.VitD <- ifelse(is.na(total.data.incisal$vitD), 1, NA)
miss.VitD_Cat <- ifelse(is.na(total.data.incisal$vitD_Cat), 1, NA)
miss.OHD <- ifelse(is.na(total.data.incisal$OHD), mean(total.data.incisal$OHD, na.rm=TRUE), NA)
miss.PTH <- ifelse(is.na(total.data.incisal$PTH), mean(total.data.incisal$PTH, na.rm=TRUE), NA)
miss.MomOHD_28 <- ifelse(is.na(total.data.incisal$momOHD_28), mean(total.data.incisal$momOHD_28, na.rm=TRUE), NA)
miss.MomOHD_36 <- ifelse(is.na(total.data.incisal$momOHD_36), mean(total.data.incisal$momOHD_36, na.rm=TRUE), NA)
miss.MomPTH_12 <- ifelse(is.na(total.data.incisal$momPTH_12), mean(total.data.incisal$momPTH_12, na.rm=TRUE), NA)
miss.MomPTH_28 <- ifelse(is.na(total.data.incisal$momPTH_28), mean(total.data.incisal$momPTH_28, na.rm=TRUE), NA)
miss.MomPTH_36 <- ifelse(is.na(total.data.incisal$momPTH_36), mean(total.data.incisal$momPTH_36, na.rm=TRUE), NA)
miss.Baby_DDS <- ifelse(is.na(total.data.incisal$b_dds), 0, NA)
miss.Baby_FLTX <- ifelse(is.na(total.data.incisal$b_fltx), 0, NA)
miss.Baby_Sm_Ct <- ifelse(is.na(total.data.incisal$b_sm_ct), 0, NA)



########################
########################
### Outcome: Incisal ###
########################
########################

#################################################################
### 1st Model in Nimble: Examine Individual Region Inclusions ###
#################################################################

## Count for Initialized For Loops ##

# Number of Subjects
N.sub <- nrow(total.data.incisal)

# Number of Defects
N.defects <- 4

# Number of Races
N.race <- length(unique(total.data.incisal$race))

# Number of Parameters (Include Intercept)
N.param <- 26



## Set Full Data ##

# Outcome Data: Incisal Region
Incisal.Y.Data <- matrix(cbind(total.data.incisal$hi, total.data.incisal$oi, total.data.incisal$pi, total.data.incisal$di), nrow=N.sub, ncol=N.defects, byrow=FALSE)

# Input Data
X.Data <- data.frame(intercept = rep(1, N.sub), 
                     OH2D = total.data.incisal$OH2D,
                     Ca = total.data.incisal$Ca,
                     P = total.data.incisal$P,
                     gestage = total.data.incisal$gestage,
                     formula = total.data.incisal$formula,
                     momage = total.data.incisal$momage,
                     mombmi = total.data.incisal$mombmi,
                     antacid = total.data.incisal$antacid,
                     momCa_12 = total.data.incisal$momCa_12,
                     momCa_28 = total.data.incisal$momCa_28,
                     momCa_36 = total.data.incisal$momCa_36,
                     momP_12 = total.data.incisal$momP_12,
                     momP_28 = total.data.incisal$momP_28,
                     momP_36 = total.data.incisal$momP_36,
                     OHD = total.data.incisal$OHD,
                     PTH = total.data.incisal$PTH,
                     momOHD_12 = total.data.incisal$momOHD_12,
                     momPTH_12 = total.data.incisal$momPTH_12,
                     momOHD_28 = total.data.incisal$momOHD_28,
                     momPTH_28 = total.data.incisal$momPTH_28,
                     momOHD_36 = total.data.incisal$momOHD_36,
                     momPTH_36 = total.data.incisal$momPTH_36,
                     VitD = as.numeric(total.data.incisal$vitD_Cat),
                     baby_age = total.data.incisal$b_age,
                     baby_dds = total.data.incisal$b_dds,
                     baby_fltx = total.data.incisal$b_fltx,
                     baby_sex = total.data.incisal$b_sex,
                     baby_sm_ct = total.data.incisal$b_sm_ct)

# Full Data: Enamel Hypoplasia
Incisal.Data <- list(Y = Incisal.Y.Data, X.input = X.Data)



## Set Constants and Priors ##

# Identify Race Vector for Imputation Method
Race <- total.data.incisal$race

# Constants
Constants <- list(N.sub = N.sub,
                  N.defects = N.defects,
                  N.param = N.param,
                  N.race = N.race,
                  Race = Race)

# Random Walk Priors
R <- diag(1, N.defects)
sds <- rep(1, N.defects)

# Initialize Missing Values for Imputation
X.data.miss <- data.frame(intercept = rep(NA, N.sub), 
                          OH2D = miss.OH2D,
                          Ca = miss.Ca,
                          P = miss.P,
                          gestage = rep(NA, N.sub),
                          formula = miss.Formula,
                          momage = rep(NA, N.sub),
                          mombmi = miss.MomBMI,
                          antacid = rep(NA, N.sub),
                          momCa_12 = rep(NA, N.sub),
                          momCa_28 = miss.MomCa_28,
                          momCa_36 = miss.MomCa_36,
                          momP_12 = miss.MomP_12,
                          momP_28 = miss.MomP_28,
                          momP_36 = miss.MomP_36,
                          OHD = miss.OHD,
                          PTH = miss.PTH,
                          momOHD_12 = rep(NA, N.sub),
                          momPTH_12 = miss.MomPTH_12,
                          momOHD_28 = miss.MomOHD_28,
                          momPTH_28 = miss.MomPTH_28,
                          momOHD_36 = miss.MomOHD_36,
                          momPTH_36 = miss.MomPTH_36,
                          VitD = miss.VitD_Cat,
                          baby_age = rep(NA, N.sub),
                          baby_dds = miss.Baby_DDS,
                          baby_fltx = miss.Baby_FLTX,
                          baby_sex = rep(NA, N.sub),
                          baby_sm_ct = miss.Baby_Sm_Ct)

# Inits (Note: Doesn't Change Based on Outcome)
Inits <- list(Beta = matrix(0, nrow=N.param, ncol=N.defects), sigma.Beta = matrix(1, nrow=N.param, ncol=N.defects),
              B = rep(0, N.sub), sigma.B = 1, 
              sds=sds, Sigma.Star = R, 
              X.input = X.data.miss, 
              sigma.X.Miss = rep(1, 16),
              prob.X.Binary = rep(NA, 3),
              lambda.X.Count = NA, Pois.Alpha = 1/mean(total.data.incisal$b_sm_ct, na.rm=TRUE),
              I = matrix(0, nrow=N.param, ncol=N.defects), pi = matrix(0.5, nrow=N.param, ncol=N.defects),
              mean.OH2D = mean(total.data.incisal$OH2D, na.rm=TRUE),
              mean.Ca = mean(total.data.incisal$Ca, na.rm=TRUE),
              mean.P = mean(total.data.incisal$P, na.rm=TRUE),
              mean.gestage = mean(total.data.incisal$gestage, na.rm=TRUE),
              mean.momage = mean(total.data.incisal$momage, na.rm=TRUE),
              mean.mombmi = mean(total.data.incisal$mombmi, na.rm=TRUE),
              mean.Ca_12 = mean(total.data.incisal$momCa_12, na.rm=TRUE),
              mean.Ca_28 = mean(total.data.incisal$momCa_28, na.rm=TRUE),
              mean.Ca_36 = mean(total.data.incisal$momCa_36, na.rm=TRUE),
              mean.P_12 = mean(total.data.incisal$momP_12, na.rm=TRUE),
              mean.P_28 = mean(total.data.incisal$momP_28, na.rm=TRUE),
              mean.P_36 = mean(total.data.incisal$momP_36, na.rm=TRUE),
              mean.OHD = mean(total.data.incisal$OHD, na.rm=TRUE),
              mean.momOHD_28 = mean(total.data.incisal$momOHD_28, na.rm=TRUE),
              mean.momOHD_36 = mean(total.data.incisal$momOHD_36, na.rm=TRUE),
              alpha.PTH = rep(5, 4), beta.PTH = rep(5, 4),
              beta0.VitD = 0.5, beta.Race.VitD = rep(0, N.race),
              mean.Baby_Age = mean(total.data.incisal$b_age, na.rm=TRUE))

# Set Dimensions
Dims <- list(Mu = c(N.sub, N.defects))



#####################################################################
### Model in Nimble: Create Upper Triangle Function for LKJ Prior ###
#####################################################################

# Create Upper Triangle Function
uppertri_mult_diag <- nimbleFunction(run = function(mat = double(2), vec = double(1)) {
  returnType(double(2))
  p <- length(vec)
  out <- matrix(nrow = p, ncol = p, init = FALSE)
  for(i in 1:p)
    out[ , i] <- mat[ , i] * vec[i]
  return(out)
})



################################
### Model in Nimble: Incisal ###
################################

## Nimble Model ##
Full.Code <- nimbleCode({
  
  # Center Design Matrix
  for(i in 1:N.sub){
    X[i,1] <- X.input[i,1]                                     # Intercept
    X[i,2] <- X.input[i,2] - mean.OH2D                         # OH2D
    X[i,3] <- X.input[i,3] - mean.Ca                           # Ca
    X[i,4] <- X.input[i,4] - mean.P                            # P
    X[i,5] <- X.input[i,5] - mean.gestage                      # GestAge
    X[i,6] <- X.input[i,6]                                     # Formula
    X[i,7] <- X.input[i,7] - mean.momage                       # MomAge
    X[i,8] <- X.input[i,8] - mean.mombmi                       # MomBMI
    X[i,9] <- X.input[i,9]                                     # Antacid
    X[i,10] <- X.input[i,10] - mean.Ca_12                      # MomCa_12
    X[i,11] <- X.input[i,11] - mean.Ca_28                      # MomCa_28
    X[i,12] <- X.input[i,12] - mean.Ca_36                      # MomCa_36
    X[i,13] <- X.input[i,13] - mean.P_12                       # MomP_12
    X[i,14] <- X.input[i,14] - mean.P_28                       # MomP_28
    X[i,15] <- X.input[i,15] - mean.P_36                       # MomP_36
    X[i,16] <- Child_FVDD[i] * Child_FVDD_Binary[i]            # Child_FVDD
    X[i,17] <- Mom_FVDD_12[i] * Mom_FVDD_12_Binary[i]          # Mom_FVDD_12
    X[i,18] <- Mom_FVDD_28[i] * Mom_FVDD_28_Binary[i]          # Mom_FVDD_28
    X[i,19] <- Mom_FVDD_36[i] * Mom_FVDD_36_Binary[i]          # Mom_FVDD_36
    X[i,20] <- X.input[i,24]*equals(X.input[i,24],1)           # VitD (Cat - 1)
    X[i,21] <- X.input[i,24]*equals(X.input[i,24],2)           # VitD (Cat - 2)
    X[i,22] <- X.input[i,25] - mean.Baby_Age                   # B_Age
    X[i,23] <- X.input[i,26]                                   # B_DDS
    X[i,24] <- X.input[i,27]                                   # B_Fltx
    X[i,25] <- X.input[i,28]                                   # B_Sex
    X[i,26] <- X.input[i,29]                                   # B_Sm_Ct
  }
  
  # Impute Missing Values for Design Matrix
  for(i in 1:N.sub){
    X.input[i,2] ~ dnorm(mean.OH2D, sigma.X.Miss[1])               # OH2D
    X.input[i,3] ~ dnorm(mean.Ca, sigma.X.Miss[2])                 # Ca
    X.input[i,4] ~ dnorm(mean.P, sigma.X.Miss[3])                  # P
    X.input[i,6] ~ dbern(prob.X.Binary[1])                         # Formula
    X.input[i,8] ~ dnorm(mean.mombmi, sigma.X.Miss[4])             # MomBMI
    X.input[i,11] ~ dnorm(mean.Ca_28, sigma.X.Miss[5])             # MomCa_28
    X.input[i,12] ~ dnorm(mean.Ca_36, sigma.X.Miss[6])             # MomCa_36
    X.input[i,13] ~ dnorm(mean.P_12, sigma.X.Miss[7])              # MomP_12
    X.input[i,14] ~ dnorm(mean.P_28, sigma.X.Miss[8])              # MomP_28
    X.input[i,15] ~ dnorm(mean.P_36, sigma.X.Miss[9])              # MomP_36
    X.input[i,16] ~ dnorm(mean.OHD, sigma.X.Miss[10])              # OHD
    X.input[i,17] ~ dgamma(alpha.PTH[1], beta.PTH[1])              # PTH
    X.input[i,19] ~ dgamma(alpha.PTH[2], beta.PTH[2])              # MomPTH_12
    X.input[i,20] ~ dnorm(mean.momOHD_28, sigma.X.Miss[11])        # MomOHD_28
    X.input[i,21] ~ dgamma(alpha.PTH[3], beta.PTH[3])              # MomPTH_28
    X.input[i,22] ~ dnorm(mean.momOHD_36, sigma.X.Miss[12])        # MomOHD_36
    X.input[i,23] ~ dgamma(alpha.PTH[4], beta.PTH[4])              # MomPTH_36
    X.input[i,26] ~ dbern(prob.X.Binary[2])                        # Baby_DDS
    X.input[i,27] ~ dbern(prob.X.Binary[3])                        # Baby_Fltx
    X.input[i,29] ~ dpois(lambda.X.Count)                          # Baby_Sm_Ct
    
    # VitD
    X.input[i,24] ~ dcat(prob.X.VitD[i,1:3])
    prob.X.VitD[i,1:3] <- indiv.X.VitD[i, 1:3] / sum(indiv.X.VitD[i, 1:3])
    indiv.X.VitD[i,1:3] <- beta0.VitD + beta.Race.VitD[Race[i]]
    
    # FVDD Ratios
    Child_FVDD[i] <- X.input[i,16] / X.input[i,17]
    Mom_FVDD_12[i] <- X.input[i,18] / X.input[i,19]
    Mom_FVDD_28[i] <- X.input[i,20] / X.input[i,21]
    Mom_FVDD_36[i] <- X.input[i,22] / X.input[i,23]
    
    # FVDD Binary Step
    Child_FVDD_Binary[i] <- step(Child_FVDD[i] - 0.308)
    Mom_FVDD_12_Binary[i] <- step(Mom_FVDD_12[i] - 0.308)
    Mom_FVDD_28_Binary[i] <- step(Mom_FVDD_28[i] - 0.308)
    Mom_FVDD_36_Binary[i] <- step(Mom_FVDD_36[i] - 0.308)
  }
  
  ## Imputation Hyperpriors ##
  
  # Continuous covariates (16 Total)
  for(p in 1:16){ 
    sigma.X.Miss[p] ~ dgamma(2, 0.5)
  }
  
  # Binary Covariates (3 total)
  for(b in 1:3){
    prob.X.Binary[b] ~ dunif(0, 1)
  }
  
  # Alpha / Beta for Gamma Hyperpriors (4 total)
  for(g in 1:4){
    alpha.PTH[g] ~ dunif(0.1, 10)
    beta.PTH[g] ~ dunif(0.1, 10)
  }
  
  # Count Covariate (1 total)
  lambda.X.Count ~ dexp(Pois.Alpha)                      
  
  # VitD --> Beta.0 & Beta.1
  beta0.VitD ~ dnorm(0, sigma.X.Miss[13])              
  beta.Race.VitD[1] ~ dnorm(0, sigma.X.Miss[14])
  beta.Race.VitD[2] ~ dnorm(0, sigma.X.Miss[15])
  beta.Race.VitD[3] ~ dnorm(0, sigma.X.Miss[16])
  
  # Logistic Model 
  for(i in 1:N.sub){
    for(j in 1:N.defects){
      Y[i,j] ~ dbinom(p[i,j],1)
    }
    logit(p[i,1:4]) ~ dmnorm(Mu[i,1:4], cholesky=Sigma[1:4, 1:4], prec_param=0)
    
    # Random Effects
    B[i] ~ dnorm(0, sigma.B)
    
    # Mean Vector
    Mu[i,1:4] <- X[i,1:N.param] %*% Beta.VS[1:N.param,1:4] + B[i]
  }
  
  # Random Effects - Covariance
  sigma.B ~ dgamma(2, 0.5)
  
  # Fixed Effects
  for(k in 1:N.param){
    for(j in 1:N.defects){
      
      # Indicator Variables
      I[k,j] ~ dbern(pi[k,j])
      pi[k,j] ~ dbeta(0.5, 0.5)
      
      # Parameter Estimates
      Beta[k,j] ~ dnorm(0, sigma.Beta[k,j])
      sigma.Beta[k,j] ~ dgamma(2, 0.5)
      Beta.VS[k,j] <- Beta[k,j]*I[k,j]
    }
  }
  
  # Multivariate Covariance
  Sigma[1:N.defects, 1:N.defects] <- uppertri_mult_diag(Sigma.Star[1:N.defects, 1:N.defects], sds[1:N.defects])
  Sigma.Star[1:N.defects, 1:N.defects] ~ dlkj_corr_cholesky(1, N.defects)
})

## Nimble Model: Check Whether Code is Specified Correctly ##
Incisal.Model <- nimbleModel(code = Full.Code, constants = Constants, data = Incisal.Data, inits = Inits, dimensions = Dims)

## Compile Model: Part I ##
Incisal.Comp.Model <- compileNimble(Incisal.Model)

## Configure MCMC ##
Permute.Spec.Incisal.1 <- configureMCMC(Incisal.Model, print=TRUE, monitors=Incisal.Comp.Model$getNodeNames(stochOnly=FALSE, includeData=FALSE), enableWAIC=TRUE)

## Build MCMC ##
Incisal.MCMC.R1 <- buildMCMC(Permute.Spec.Incisal.1)

## Compile Model: Part II ##
Incisal.MCMC.C1 <- compileNimble(Incisal.MCMC.R1, project=Incisal.Model)

## Set Run Dimensions ##
niter <- 1000000
nburnin <- 800000
thin <- 100

## Run MCMC (Raise Computer for Cooling) ##
Incisal.1.Samples <- runMCMC(Incisal.MCMC.C1, niter = niter, nburnin = nburnin, nchains = 2, thin = thin, summary = TRUE, WAIC = TRUE)

