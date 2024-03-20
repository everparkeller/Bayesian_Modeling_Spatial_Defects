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
N.param <- 9



## Set Full Data ##

# Outcome Data: Incisal Region
Incisal.Y.Data <- matrix(cbind(total.data.incisal$hi, total.data.incisal$oi, total.data.incisal$pi, total.data.incisal$di), nrow=N.sub, ncol=N.defects, byrow=FALSE)

# Input Data
X.Data <- data.frame(intercept = rep(1, N.sub), 
                     gestage = total.data.incisal$gestage,
                     formula = total.data.incisal$formula,
                     mombmi = total.data.incisal$mombmi,
                     momCa_28 = total.data.incisal$momCa_28,
                     momP_28 = total.data.incisal$momP_28,
                     momOHD_36 = total.data.incisal$momOHD_36,
                     momPTH_36 = total.data.incisal$momPTH_36,
                     baby_age = total.data.incisal$b_age,
                     baby_sex = total.data.incisal$b_sex)

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
                          gestage = rep(NA, N.sub),
                          formula = miss.Formula,
                          mombmi = miss.MomBMI,
                          momCa_28 = miss.MomCa_28,
                          momP_28 = miss.MomP_28,
                          momOHD_36 = miss.MomOHD_36,
                          momPTH_36 = miss.MomPTH_36,
                          baby_age = rep(NA, N.sub),
                          baby_sex = rep(NA, N.sub))

# Inits (Note: Doesn't Change Based on Outcome)
Inits <- list(Beta = matrix(0, nrow=N.param, ncol=N.defects), sigma.Beta = matrix(1, nrow=N.param, ncol=N.defects),
              B = rep(0, N.sub), sigma.B = 1, 
              sds=sds, Sigma.Star = R, 
              X.input = X.data.miss, 
              sigma.X.Miss = rep(1, 3),
              prob.X.Binary = NA,
              mean.gestage = mean(total.data.incisal$gestage, na.rm=TRUE),
              mean.mombmi = mean(total.data.incisal$mombmi, na.rm=TRUE),
              mean.Ca_28 = mean(total.data.incisal$momCa_28, na.rm=TRUE),
              mean.P_28 = mean(total.data.incisal$momP_28, na.rm=TRUE),
              mean.momOHD_36 = mean(total.data.incisal$momOHD_36, na.rm=TRUE),
              alpha.PTH = 5, beta.PTH = 5,
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
    X[i,2] <- X.input[i,2] - mean.gestage                      # GestAge
    X[i,3] <- X.input[i,3]                                     # Formula
    X[i,4] <- X.input[i,4] - mean.mombmi                       # MomBMI
    X[i,5] <- X.input[i,5] - mean.Ca_28                        # MomCa_28
    X[i,6] <- X.input[i,6] - mean.P_28                         # MomP_28
    X[i,7] <- Mom_FVDD_36[i] * Mom_FVDD_36_Binary[i]           # Mom_FVDD_36
    X[i,8] <- X.input[i,9] - mean.Baby_Age                     # B_Age
    X[i,9] <- X.input[i,10]                                    # B_Sex
  }
  
  # Impute Missing Values for Design Matrix
  for(i in 1:N.sub){
    X.input[i,3] ~ dbern(prob.X.Binary)                           # Formula
    X.input[i,5] ~ dnorm(mean.Ca_28, sigma.X.Miss[1])             # MomCa_28
    X.input[i,6] ~ dnorm(mean.P_28, sigma.X.Miss[2])              # MomP_28
    X.input[i,7] ~ dnorm(mean.momOHD_36, sigma.X.Miss[3])         # MomOHD_36
    X.input[i,8] ~ dgamma(alpha.PTH, beta.PTH)                    # MomPTH_36
    
    # FVDD Ratios
    Mom_FVDD_36[i] <- X.input[i,7] / X.input[i,8]
    
    # FVDD Binary Step
    Mom_FVDD_36_Binary[i] <- step(Mom_FVDD_36[i] - 0.308)
  }
  
  ## Imputation Hyperpriors ##
  
  # Continuous covariates (3 Total)
  for(p in 1:3){ 
    sigma.X.Miss[p] ~ dgamma(2, 0.5)
  }
  
  # Binary Covariates (1 total)
  prob.X.Binary ~ dunif(0, 1)
  
  # Alpha / Beta for Gamma Hyperpriors (1 total)
  alpha.PTH ~ dunif(0.1, 10)
  beta.PTH ~ dunif(0.1, 10)
  
  # Logistic Model 
  for(i in 1:N.sub){
    for(j in 1:N.defects){
      Y[i,j] ~ dbinom(p[i,j],1)
    }
    logit(p[i,1:4]) ~ dmnorm(Mu[i,1:4], cholesky=Sigma[1:4, 1:4], prec_param=0)
    
    # Random Effects
    B[i] ~ dnorm(0, sigma.B)
    
    # Mean Vector
    Mu[i,1:4] <- X[i,1:N.param] %*% Beta[1:N.param,1:4] + B[i]
  }
  
  # Random Effects - Covariance
  sigma.B ~ dgamma(2, 0.5)
  
  # Fixed Effects
  for(k in 1:N.param){
    for(j in 1:N.defects){
      
      # Parameter Estimates
      Beta[k,j] ~ dnorm(0, sigma.Beta[k,j])
      sigma.Beta[k,j] ~ dgamma(2, 0.5)
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
niter <- 400000
nburnin <- 300000
thin <- 50

## Run MCMC (Raise Computer for Cooling) ##
Incisal.1.Samples <- runMCMC(Incisal.MCMC.C1, niter = niter, nburnin = nburnin, nchains = 2, thin = thin, summary = TRUE, WAIC = TRUE)





##################################
## Create Incisal Region Graphs ##
##################################

### Across Defects ###

# Establish Betas
Betas <- c("Beta[1, 1]", "Beta[2, 1]", "Beta[3, 1]", "Beta[4, 1]", "Beta[5, 1]", "Beta[6, 1]", "Beta[7, 1]", "Beta[8, 1]", "Beta[9, 1]",
           "Beta[1, 2]", "Beta[2, 2]", "Beta[3, 2]", "Beta[4, 2]", "Beta[5, 2]", "Beta[6, 2]", "Beta[7, 2]", "Beta[8, 2]", "Beta[9, 2]",
           "Beta[1, 3]", "Beta[2, 3]", "Beta[3, 3]", "Beta[4, 3]", "Beta[5, 3]", "Beta[6, 3]", "Beta[7, 3]", "Beta[8, 3]", "Beta[9, 3]",
           "Beta[1, 4]", "Beta[2, 4]", "Beta[3, 4]", "Beta[4, 4]", "Beta[5, 4]", "Beta[6, 4]", "Beta[7, 4]", "Beta[8, 4]", "Beta[9, 4]")

# Create Data Frame of Samples on Odds Scale
Incisal.1.Samples.Chain.1 <- Incisal.1.Samples$samples$chain1[,which(colnames(Incisal.1.Samples$samples$chain1) %in% Betas)]
Incisal.1.Samples.Chain.2 <- Incisal.1.Samples$samples$chain2[,which(colnames(Incisal.1.Samples$samples$chain2) %in% Betas)]
Incisal.1.Samples.Agg.Chains <- rbind(Incisal.1.Samples.Chain.1, Incisal.1.Samples.Chain.2)
Incisal.1.Samples.Agg.Chains.Odds.Scales <- exp(Incisal.1.Samples.Agg.Chains)

# Compile Plot Data Frame
Params <- rep(c("Intercept", "GestAge", "Formula", "MomBMI", "MomCa 28", "MomP 28", "Mom FVDD 36", "Child Age", "Child Sex"), 4)
Defects <- rep(c("Enamel Hypoplasia", "Opacity", "Post-Eruptive Breakdown", "Dental Caries"), each=9)
Means <- colMeans(Incisal.1.Samples.Agg.Chains.Odds.Scales)
Lower.CI <- apply(Incisal.1.Samples.Agg.Chains.Odds.Scales, 2, quantile, probs = c(0.025))
Upper.CI <- apply(Incisal.1.Samples.Agg.Chains.Odds.Scales, 2, quantile, probs = c(0.975))
Incisal.Plot.Data <- data.frame(Params = Params,
                                Defects = Defects,
                                Means = Means,
                                Lower.CI = Lower.CI,
                                Upper.CI = Upper.CI)

# Fix Factor Levels for Facet_Wrap
Incisal.Plot.Data$Params <- factor(Incisal.Plot.Data$Params, levels=c("Intercept", "GestAge", "Formula", "MomBMI", "MomCa 28", "MomP 28", "Mom FVDD 36", "Child Age", "Child Sex"), labels=c("Intercept", "Gestational Age", "Formula", "Mother's BMI", "Mother's Ca at 28 Wks", "Mother's P at 28 Wks", "Mother's FVDD at 36 Wks", "Child's Age", "Child's Sex"))
Incisal.Plot.Data$Defects <- factor(Incisal.Plot.Data$Defects, levels=c("Enamel Hypoplasia", "Opacity", "Post-Eruptive Breakdown", "Dental Caries"), labels=c("EH", "OP", "PEB", "DC"))

# Create Profile Plot
ggplot(Incisal.Plot.Data, aes(x = Defects, y = Means)) + 
  geom_errorbar(aes(ymin = Lower.CI, ymax = Upper.CI), width = 0.3) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed", col = "red") + 
  geom_point(size = 2.5) + facet_wrap(Params ~ ., scales = "fixed", ncol = 5) + 
  theme_bw() + theme(panel.spacing = unit(0.8, "lines")) +
  theme(plot.title=element_text(size=rel(1.2))) + theme(strip.text.x=element_text()) + 
  labs(x = "Type of Defect", y = "Odds of Defect in Incisal Region (95% CI)")