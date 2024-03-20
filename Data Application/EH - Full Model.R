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
remove(total.data.dc); remove(total.data.op); remove(total.data.peb);



##########################
### Adjust Sample Size ###
##########################

keep.rows <- which(complete.cases(total.data.eh$hc, total.data.eh$hm, total.data.eh$hi)==TRUE|total.data.eh$hc==1|total.data.eh$hm==1|total.data.eh$hi==1)
total.data.eh <- total.data.eh[keep.rows,]



###########################################
### Check Missingness within Full Model ###
###########################################

## Create Missingness Vectors for Inits in Imputation ##
# Note: Predictors are the same for each outcome thus only using EH to gather missingness vectors
miss.OH2D <- ifelse(is.na(total.data.eh$OH2D), mean(total.data.eh$OH2D, na.rm=TRUE), NA)
miss.Ca <- ifelse(is.na(total.data.eh$Ca), mean(total.data.eh$Ca, na.rm=TRUE), NA)
miss.P <- ifelse(is.na(total.data.eh$P), mean(total.data.eh$P, na.rm=TRUE), NA)
miss.Formula <- ifelse(is.na(total.data.eh$formula), 0, NA)
miss.MomBMI <- ifelse(is.na(total.data.eh$mombmi), mean(total.data.eh$mombmi, na.rm=TRUE), NA)
miss.MomCa_28 <- ifelse(is.na(total.data.eh$momCa_28), mean(total.data.eh$momCa_28, na.rm=TRUE), NA)
miss.MomCa_36 <- ifelse(is.na(total.data.eh$momCa_36), mean(total.data.eh$momCa_36, na.rm=TRUE), NA)
miss.MomP_12 <- ifelse(is.na(total.data.eh$momP_12), mean(total.data.eh$momP_12, na.rm=TRUE), NA)
miss.MomP_28 <- ifelse(is.na(total.data.eh$momP_28), mean(total.data.eh$momP_28, na.rm=TRUE), NA)
miss.MomP_36 <- ifelse(is.na(total.data.eh$momP_36), mean(total.data.eh$momP_36, na.rm=TRUE), NA)
miss.VitD <- ifelse(is.na(total.data.eh$vitD), 1, NA)
miss.VitD_Cat <- ifelse(is.na(total.data.eh$vitD_Cat), 1, NA)
miss.OHD <- ifelse(is.na(total.data.eh$OHD), mean(total.data.eh$OHD, na.rm=TRUE), NA)
miss.PTH <- ifelse(is.na(total.data.eh$PTH), mean(total.data.eh$PTH, na.rm=TRUE), NA)
miss.MomOHD_28 <- ifelse(is.na(total.data.eh$momOHD_28), mean(total.data.eh$momOHD_28, na.rm=TRUE), NA)
miss.MomOHD_36 <- ifelse(is.na(total.data.eh$momOHD_36), mean(total.data.eh$momOHD_36, na.rm=TRUE), NA)
miss.MomPTH_12 <- ifelse(is.na(total.data.eh$momPTH_12), mean(total.data.eh$momPTH_12, na.rm=TRUE), NA)
miss.MomPTH_28 <- ifelse(is.na(total.data.eh$momPTH_28), mean(total.data.eh$momPTH_28, na.rm=TRUE), NA)
miss.MomPTH_36 <- ifelse(is.na(total.data.eh$momPTH_36), mean(total.data.eh$momPTH_36, na.rm=TRUE), NA)



##################################
##################################
### Outcome: Enamel Hypoplasia ###
##################################
##################################

#################################################################
### 3rd Model in Nimble: Examine Individual Region Inclusions ###
#################################################################

## Count for Initialized For Loops ##

# Number of Subjects
N.sub <- nrow(total.data.eh)

# Number of Regions
N.region <- 3

# Number of Races
N.race <- length(unique(total.data.eh$race))

# Number of Parameters (Include Intercept)
N.param <- 23



## Set Full Data ##

# Outcome Data: Enamel Hypoplasia
EH.Y.Data <- matrix(cbind(total.data.eh$hc, total.data.eh$hm, total.data.eh$hi), nrow=N.sub, ncol=N.region, byrow=FALSE)

# Input Data
X.Data <- data.frame(intercept = rep(1, N.sub), 
                     OH2D = total.data.eh$OH2D,
                     Ca = total.data.eh$Ca,
                     P = total.data.eh$P,
                     gestage = total.data.eh$gestage,
                     formula = total.data.eh$formula,
                     momage = total.data.eh$momage,
                     mombmi = total.data.eh$mombmi,
                     antacid = total.data.eh$antacid,
                     momCa_12 = total.data.eh$momCa_12,
                     momCa_28 = total.data.eh$momCa_28,
                     momCa_36 = total.data.eh$momCa_36,
                     momP_12 = total.data.eh$momP_12,
                     momP_28 = total.data.eh$momP_28,
                     momP_36 = total.data.eh$momP_36,
                     OHD = total.data.eh$OHD,
                     PTH = total.data.eh$PTH,
                     momOHD_12 = total.data.eh$momOHD_12,
                     momPTH_12 = total.data.eh$momPTH_12,
                     momOHD_28 = total.data.eh$momOHD_28,
                     momPTH_28 = total.data.eh$momPTH_28,
                     momOHD_36 = total.data.eh$momOHD_36,
                     momPTH_36 = total.data.eh$momPTH_36,
                     VitD = as.numeric(total.data.eh$vitD_Cat),
                     baby_age = total.data.eh$b_age,
                     baby_sex = total.data.eh$b_sex)

# Full Data: Enamel Hypoplasia
EH.Data <- list(Y = EH.Y.Data, X.input = X.Data)



## Set Constants and Priors ##

# Identify Race Vector for Imputation Method
Race <- total.data.eh$race

# Constants
Constants <- list(N.sub = N.sub,
                  N.region = N.region,
                  N.param = N.param,
                  N.race = N.race,
                  Race = Race)

# Random Walk Priors
R <- diag(1, N.region)
sds <- rep(1, N.region)

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
                          baby_sex = rep(NA, N.sub))

# Inits (Note: Doesn't Change Based on Outcome)
Inits <- list(Beta = matrix(0, nrow=N.param, ncol=N.region), sigma.Beta = matrix(1, nrow=N.param, ncol=N.region),
              B = rep(0, N.sub), sigma.B = 1, 
              sds=sds, Sigma.Star = R, 
              X.input = X.data.miss, 
              sigma.X.Miss = rep(1, 16),
              prob.X.Binary = NA,
              I = matrix(0, nrow=N.param, ncol=N.region), pi = matrix(0.5, nrow=N.param, ncol=N.region),
              mean.OH2D = mean(total.data.eh$OH2D, na.rm=TRUE),
              mean.Ca = mean(total.data.eh$Ca, na.rm=TRUE),
              mean.P = mean(total.data.eh$P, na.rm=TRUE),
              mean.gestage = mean(total.data.eh$gestage, na.rm=TRUE),
              mean.momage = mean(total.data.eh$momage, na.rm=TRUE),
              mean.mombmi = mean(total.data.eh$mombmi, na.rm=TRUE),
              mean.Ca_12 = mean(total.data.eh$momCa_12, na.rm=TRUE),
              mean.Ca_28 = mean(total.data.eh$momCa_28, na.rm=TRUE),
              mean.Ca_36 = mean(total.data.eh$momCa_36, na.rm=TRUE),
              mean.P_12 = mean(total.data.eh$momP_12, na.rm=TRUE),
              mean.P_28 = mean(total.data.eh$momP_28, na.rm=TRUE),
              mean.P_36 = mean(total.data.eh$momP_36, na.rm=TRUE),
              mean.OHD = mean(total.data.eh$OHD, na.rm=TRUE),
              mean.momOHD_28 = mean(total.data.eh$momOHD_28, na.rm=TRUE),
              mean.momOHD_36 = mean(total.data.eh$momOHD_36, na.rm=TRUE),
              alpha.PTH = rep(5, 4), beta.PTH = rep(5, 4),
              beta0.VitD = 0.5, beta.Race.VitD = rep(0, N.race),
              mean.Baby_Age = mean(total.data.eh$b_age, na.rm=TRUE))

# Set Dimensions
Dims <- list(Mu = c(N.sub, N.region))



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



##########################################
### Model in Nimble: Enamel Hypoplasia ###
##########################################

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
    X[i,23] <- X.input[i,26]                                   # B_Sex
  }
  
  # Impute Missing Values for Design Matrix
  for(i in 1:N.sub){
    X.input[i,2] ~ dnorm(mean.OH2D, sigma.X.Miss[1])               # OH2D
    X.input[i,3] ~ dnorm(mean.Ca, sigma.X.Miss[2])                 # Ca
    X.input[i,4] ~ dnorm(mean.P, sigma.X.Miss[3])                  # P
    X.input[i,6] ~ dbern(prob.X.Binary)                            # Formula
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
  
  # Binary Covariates (1 total)
  prob.X.Binary ~ dunif(0, 1)
  
  # Alpha / Beta for Gamma Hyperpriors (4 total)
  for(g in 1:4){
    alpha.PTH[g] ~ dunif(0.1, 10)
    beta.PTH[g] ~ dunif(0.1, 10)
  }
  
  # VitD --> Beta.0 & Beta.1
  beta0.VitD ~ dnorm(0, sigma.X.Miss[13])              
  beta.Race.VitD[1] ~ dnorm(0, sigma.X.Miss[14])
  beta.Race.VitD[2] ~ dnorm(0, sigma.X.Miss[15])
  beta.Race.VitD[3] ~ dnorm(0, sigma.X.Miss[16])
  
  # Logistic Model 
  for(i in 1:N.sub){
    for(j in 1:N.region){
      Y[i,j] ~ dbinom(p[i,j],1)
    }
    logit(p[i,1:3]) ~ dmnorm(Mu[i,1:3], cholesky=Sigma[1:3, 1:3], prec_param=0)
    
    # Random Effects
    B[i] ~ dnorm(0, sigma.B)
    
    # Mean Vector
    Mu[i,1:3] <- X[i,1:N.param] %*% Beta.VS[1:N.param,1:3] + B[i]
  }
  
  # Random Effects - Covariance
  sigma.B ~ dgamma(2, 0.5)
  
  # Fixed Effects
  for(k in 1:N.param){
    for(j in 1:N.region){
      
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
  Sigma[1:N.region, 1:N.region] <- uppertri_mult_diag(Sigma.Star[1:N.region, 1:N.region], sds[1:N.region])
  Sigma.Star[1:N.region, 1:N.region] ~ dlkj_corr_cholesky(1, N.region)
})

## Nimble Model: Check Whether Code is Specified Correctly ##
EH.Model <- nimbleModel(code = Full.Code, constants = Constants, data = EH.Data, inits = Inits, dimensions = Dims)

## Compile Model: Part I ##
EH.Comp.Model <- compileNimble(EH.Model)

## Configure MCMC ##
Permute.Spec.EH.1 <- configureMCMC(EH.Model, print=TRUE, monitors=EH.Comp.Model$getNodeNames(stochOnly=FALSE, includeData=FALSE), enableWAIC=TRUE)

## Build MCMC ##
EH.MCMC.R1 <- buildMCMC(Permute.Spec.EH.1)

## Compile Model: Part II ##
EH.MCMC.C1 <- compileNimble(EH.MCMC.R1, project=EH.Model)

## Set Run Dimensions ##
niter <- 1000000
nburnin <- 800000
thin <- 100

## Run MCMC (Raise Computer for Cooling) ##
EH.1.Samples <- runMCMC(EH.MCMC.C1, niter = niter, nburnin = nburnin, nchains = 2, thin = thin, summary = TRUE, WAIC = TRUE)
