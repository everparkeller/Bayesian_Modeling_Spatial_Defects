######################
### Load Libraries ###
######################

library(coda)
library(ggplot2)
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
miss.MomP_36 <- ifelse(is.na(total.data.eh$momP_36), mean(total.data.eh$momP_36, na.rm=TRUE), NA)
miss.MomOHD_28 <- ifelse(is.na(total.data.eh$momOHD_28), mean(total.data.eh$momOHD_28, na.rm=TRUE), NA)
miss.MomPTH_12 <- ifelse(is.na(total.data.eh$momPTH_12), mean(total.data.eh$momPTH_12, na.rm=TRUE), NA)
miss.MomPTH_28 <- ifelse(is.na(total.data.eh$momPTH_28), mean(total.data.eh$momPTH_28, na.rm=TRUE), NA)



##################################
##################################
### Outcome: Enamel Hypoplasia ###
##################################
##################################

################################################
### 1st Model in Nimble: Set Data Structures ###
################################################

## Count for Initialized For Loops ##

# Number of Subjects
N.sub <- nrow(total.data.eh)

# Number of Regions
N.region <- 3

# Number of Races
N.race <- length(unique(total.data.eh$race))

# Number of Parameters (Include Intercept)
N.param <- 6



## Set Full Data ##

# Outcome Data: Enamel Hypoplasia
EH.Y.Data <- matrix(cbind(total.data.eh$hc, total.data.eh$hm, total.data.eh$hi), nrow=N.sub, ncol=N.region, byrow=FALSE)

# Input Data
X.Data <- data.frame(intercept = rep(1, N.sub), 
                     gestage = total.data.eh$gestage,
                     momP_36 = total.data.eh$momP_36,
                     momOHD_12 = total.data.eh$momOHD_12,
                     momPTH_12 = total.data.eh$momPTH_12,
                     momOHD_28 = total.data.eh$momOHD_28,
                     momPTH_28 = total.data.eh$momPTH_28,
                     baby_age = total.data.eh$b_age)

# Full Data: Enamel Hypoplasia
EH.Data <- list(Y = EH.Y.Data, X.input = X.Data)



#############################################################
### 1st Model in Nimble: Set Constants and Initial Values ###
#############################################################

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
                          gestage = rep(NA, N.sub),
                          momP_36 = miss.MomP_36,
                          momOHD_12 = rep(NA, N.sub),
                          momPTH_12 = miss.MomPTH_12,
                          momOHD_28 = miss.MomOHD_28,
                          momPTH_28 = miss.MomPTH_28,
                          baby_age = rep(NA, N.sub))

# Inits (Note: Doesn't Change Based on Outcome)
Inits <- list(Beta = matrix(0, nrow=N.param, ncol=N.region), sigma.Beta = matrix(1, nrow=N.param, ncol=N.region),
              B = rep(0, N.sub), sigma.B = 1, 
              sds=sds, Sigma.Star = R, 
              X.input = X.data.miss, 
              sigma.X.Miss = rep(1, 2),
              mean.gestage = mean(total.data.eh$gestage, na.rm=TRUE),
              mean.P_36 = mean(total.data.eh$momP_36, na.rm=TRUE),
              mean.momOHD_28 = mean(total.data.eh$momOHD_28, na.rm=TRUE),
              alpha.PTH = rep(5, 2), beta.PTH = rep(5, 2),
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
    X[i,2] <- X.input[i,2] - mean.gestage                      # GestAge
    X[i,3] <- X.input[i,3] - mean.P_36                         # MomP_36
    X[i,4] <- Mom_FVDD_12[i] * Mom_FVDD_12_Binary[i]           # Mom_FVDD_12
    X[i,5] <- Mom_FVDD_28[i] * Mom_FVDD_28_Binary[i]           # Mom_FVDD_28
    X[i,6] <- X.input[i,8] - mean.Baby_Age                     # B_Age
  }
  
  # Impute Missing Values for Design Matrix
  for(i in 1:N.sub){
    X.input[i,3] ~ dnorm(mean.P_36, sigma.X.Miss[1])               # MomP_36
    X.input[i,5] ~ dgamma(alpha.PTH[1], beta.PTH[1])               # MomPTH_12
    X.input[i,6] ~ dnorm(mean.momOHD_28, sigma.X.Miss[2])          # MomOHD_28
    X.input[i,7] ~ dgamma(alpha.PTH[2], beta.PTH[2])               # MomPTH_28
    
    # FVDD Ratios
    Mom_FVDD_12[i] <- X.input[i,4] / X.input[i,5]
    Mom_FVDD_28[i] <- X.input[i,6] / X.input[i,7]
    
    # FVDD Binary Step
    Mom_FVDD_12_Binary[i] <- step(Mom_FVDD_12[i] - 0.308)
    Mom_FVDD_28_Binary[i] <- step(Mom_FVDD_28[i] - 0.308)
  }
  
  ## Imputation Hyperpriors ##
  
  # Continuous covariates (2 Total)
  for(p in 1:2){ 
    sigma.X.Miss[p] ~ dgamma(2, 0.5)
  }
  
  # Alpha / Beta for Gamma Hyperpriors (2 total)
  for(g in 1:2){
    alpha.PTH[g] ~ dunif(0.1, 10)
    beta.PTH[g] ~ dunif(0.1, 10)
  }
  
  # Logistic Model 
  for(i in 1:N.sub){
    for(j in 1:N.region){
      Y[i,j] ~ dbinom(p[i,j],1)
    }
    logit(p[i,1:3]) ~ dmnorm(Mu[i,1:3], cholesky=Sigma[1:3, 1:3], prec_param=0)
    
    # Random Effects
    B[i] ~ dnorm(0, sigma.B)
    
    # Mean Vector
    Mu[i,1:3] <- X[i,1:N.param] %*% Beta[1:N.param,1:3] + B[i]
  }
  
  # Random Effects - Covariance
  sigma.B ~ dgamma(2, 0.5)
  
  # Fixed Effects
  for(k in 1:N.param){
    for(j in 1:N.region){
      Beta[k,j] ~ dnorm(0, sigma.Beta[k,j])
      sigma.Beta[k,j] ~ dgamma(2, 0.5)
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
niter <- 400000
nburnin <- 300000
thin <- 50

## Run MCMC (Raise Computer for Cooling) ##
EH.1.Samples <- runMCMC(EH.MCMC.C1, niter = niter, nburnin = nburnin, nchains = 2, thin = thin, summary = TRUE, WAIC = TRUE)



##########################
## Create Profile Plots ##
##########################

### Across Regions ###

# Establish Betas
Betas <- c("Beta[1, 1]", "Beta[2, 1]", "Beta[3, 1]", "Beta[4, 1]", "Beta[5, 1]", "Beta[6, 1]",
           "Beta[1, 2]", "Beta[2, 2]", "Beta[3, 2]", "Beta[4, 2]", "Beta[5, 2]", "Beta[6, 2]",
           "Beta[1, 3]", "Beta[2, 3]", "Beta[3, 3]", "Beta[4, 3]", "Beta[5, 3]", "Beta[6, 3]")

# Create Data Frame of Samples on Odds Scale
EH.1.Samples.Chain.1 <- EH.1.Samples$samples$chain1[,which(colnames(EH.1.Samples$samples$chain1) %in% Betas)]
EH.1.Samples.Chain.2 <- EH.1.Samples$samples$chain2[,which(colnames(EH.1.Samples$samples$chain2) %in% Betas)]
EH.1.Samples.Agg.Chains <- rbind(EH.1.Samples.Chain.1, EH.1.Samples.Chain.2)
EH.1.Samples.Agg.Chains.Odds.Scales <- exp(EH.1.Samples.Agg.Chains)

# Compile Plot Data Frame
Params <- rep(c("Intercept", "GestAge", "MomP 36", "Mom FVDD 12", "Mom FVDD 28", "Child Age"), 3)
Regions <- rep(c("Cervical", "Middle", "Incisal"), each=6)
Means <- colMeans(EH.1.Samples.Agg.Chains.Odds.Scales)
Lower.CI <- apply(EH.1.Samples.Agg.Chains.Odds.Scales, 2, quantile, probs = c(0.025))
Upper.CI <- apply(EH.1.Samples.Agg.Chains.Odds.Scales, 2, quantile, probs = c(0.975))
EH.Plot.Data <- data.frame(Params = Params,
                           Regions = Regions,
                           Means = Means,
                           Lower.CI = Lower.CI,
                           Upper.CI = Upper.CI)

# Fix Factor Levels for Facet_Wrap
EH.Plot.Data$Params <- factor(EH.Plot.Data$Params, levels=c("Intercept", "GestAge", "MomP 36", "Mom FVDD 12", "Mom FVDD 28", "Child Age"), labels=c("Intercept", "Gestational Age", "Mother's P at 36 Wks", "Mother's FVDD at 12 Wks", "Mother's FVDD at 28 Wks", "Child's Age at Visit"))
EH.Plot.Data$Regions <- factor(EH.Plot.Data$Regions, levels=c("Cervical", "Middle", "Incisal"), labels=c("Cervical", "Middle", "Incisal"))

# Create Profile Plot
ggplot(EH.Plot.Data, aes(x = Regions, y = Means)) + 
  geom_errorbar(aes(ymin = Lower.CI, ymax = Upper.CI), width = 0.3) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", col = "red") + 
  geom_point(size = 2.5) + facet_wrap(Params ~ ., scales = "fixed", ncol = 7) + 
  theme_bw() + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title=element_text(size=rel(1.2))) + theme(strip.text.x=element_text()) + 
  labs(x = "Region", y = "Odds of Enamel Hypoplasia (95% CI)")
