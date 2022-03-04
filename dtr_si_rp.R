dat <- read.csv("/Users/francois/Desktop/github repos/MIMIC-DTR/mimic_si.csv")



boot_dtr <- function(d, i=1:nrow(d)) {
        dat<-d[i,]
        dat <- as.data.frame(lapply(dat, defactorize))
        
################################################
#### Backward induction procedure stage k=3 #### 
################################################

# exclude the patients who initiated RRT before stage k=3 
# ( under regular policies, there is no treatment decision
# to make for these patients at stage k=3 )

excluded_k3 <- dat$a2 == 1
included_k3 <- !excluded_k3 # for clarity later
dat_k3 <- dat[included_k3,]

# Fit propensity score for stage k=3

ps_mod_k3 <- glm(a3 ~ bun_k3 + ph_k3 + pot_k3, family = "binomial", data= dat_k3)

library(gam)
par(mfrow=c(2,2))
plot(gam(a3 ~ s(bun_k3) + s(ph_k3) + s(pot_k3), family = "binomial", data= dat_k3), se=T)

library(rms)
summary(glm(a3 ~ bun_k3 + ph_k3 + pol(pot_k3,2), family = "binomial", data= dat_k3))

library(mfp)
mfp(a3 ~ bun_k3 + ph_k3 + fp(pot_k3, df=4, select = 0.05), family = "binomial", data= dat_k3)
summary(glm(a3 ~ bun_k3 + ph_k3 +I((pot_k3/10)^-2)+I((pot_k3/10)^-2*log((pot_k3/10))), family = "binomial", data= dat_k3))

ps_mod_k3 <- glm(a3 ~ bun_k3 + ph_k3 +I((pot_k3/10)^-2)+I((pot_k3/10)^-2*log((pot_k3/10))), family = "binomial", data= dat_k3)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

dat_k3$lpred <- predict(ps_mod_k3)

plot(calibrate(lrm(a3 ~ lpred, data=dat_k3, x=T, y=T)))

w_k3 <- with(dat_k3, abs(a3 - predict(ps_mod_k3, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=3 and the tailoring variables 

#pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k1 + bun_k3 +
#                       a3*uo_k3,
#                        weights = w_k3, data = dat_k3)

pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k3 +
                        a3 * uo_k3 + a3 * I(bun_k3/bun_k1),
                weights = w_k3, data = dat_k3)## keep this model

# Model fit and diagnostics are accessible via
# summary(pr_mod_k3) and plot(pr_mod_k3) respectively

# Store all coefficients for bootstrapping
all_coef_k3 <- coef(pr_mod_k3)
names(all_coef_k3) <- paste0("mod_k3_", names(all_coef_k3))

# Store the effect of tailoring variables 
psi_k3 <- coef(pr_mod_k3)[grepl("a3", names(coef(pr_mod_k3)) )]

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=3
contrast_a1_k3 <- model.matrix(~1 + uo_k3 + I(bun_k3/bun_k1), data=dat_k3) %*% psi_k3

# Contrast for a=0 (reference) at stage k=3
# by definition contrast for reference = 0 at all stages
contrast_a0_k3 <- 0 

# Optimal contrast at stage k=3
optimal_contrast_k3 <- pmax(contrast_a1_k3, contrast_a0_k3)

# Actual contrast at stage k=3
actual_contrast_k3 <- model.matrix(~-1 + a3 + a3:uo_k3 + a3:I(bun_k3/bun_k1), data=dat_k3) %*% psi_k3

# Regret at stage k=3
# By definition, regret = optimal contrast - actual contrast
regret_k3 <- optimal_contrast_k3 - actual_contrast_k3

# Create a copy of the observed outcome
dat$hfd_tilde_2 <- dat$hfd

# For the patients who would have benefited from another treatment at stage k=3
# add this missed benefit (aka regret) to create PSEUDOS-outcomes under optimal treatment at k=3
dat$hfd_tilde_2[included_k3] <-  dat$hfd_tilde_2[included_k3] + regret_k3

################################################
#### Backward induction procedure stage k=2 #### 
################################################

# exclude the patients who initiated RRT before stage k=2 
# ( under regular policies, there is no treatment decision
# to make for these patients at stage k=2 )

excluded_k2 <- dat$a1 == 1
included_k2 <- !excluded_k2 # for clarity later
dat_k2 <- dat[included_k2,]

# Fit propensity score for stage k=2

ps_mod_k2 <- glm(a2 ~ bun_k2 + ph_k2 + pot_k2, family = "binomial", data = dat_k2)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k2 <- with(dat_k2, abs(a2 - predict(ps_mod_k2, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=2 and the tailoring variables 

pr_mod_k2 <- lm(hfd_tilde_2 ~ admission_age + SOFA_24hours + weight + gender +
                        #bun_k1 +
                        #ph_k +
                        #uo_k2 + 
                         #pot_k2 + 
                         a2*bun_k2 + a2:I((ph_k2 - ph_k1)/ph_k2) + a2*I(uo_k2+uo_k1),
                weights = w_k2, data = dat_k2) ### A revoir +++

# Model fit and diagnostics are accessible via
# summary(pr_mod_k2) and plot(pr_mod_k2) respectively

# Store all coefficients for bootstrapping
all_coef_k2 <- coef(pr_mod_k2)
names(all_coef_k2) <- paste0("mod_k2_", names(all_coef_k2))

# Store the effect of tailoring variables 
psi_k2 <- coef(pr_mod_k2)[grepl("a2", names(coef(pr_mod_k2)) )]

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=2
contrast_a1_k2 <- model.matrix(~1 + bun_k2 + I(ph_k2 - ph_k1) + I(uo_k2+uo_k1), data=dat_k2) %*% psi_k2

# Contrast for a=0 (reference) at stage k=2
# by definition contrast for reference = 0 at all stages
contrast_a0_k2 <- 0 

# Optimal contrast at stage k=2
optimal_contrast_k2 <- pmax(contrast_a1_k2, contrast_a0_k2)

# Actual contrast at stage k=2
actual_contrast_k2 <- model.matrix(~-1 + a2 + a2:bun_k2 + a2:I(ph_k2 - ph_k1) + a2:I(uo_k2+uo_k1), data=dat_k2) %*% psi_k2

# Regret at stage k=2
# By definition, regret = optimal contrast - actual contrast
regret_k2 <- optimal_contrast_k2 - actual_contrast_k2

# Create a copy of the PSEUDOS-outcomes under optimal treatment at k=3
dat$hfd_tilde_1 <- dat$hfd
dat$hfd_tilde_1[included_k2] <- dat_k2$hfd_tilde_2

# For the patients who would have benefited from another treatment at stage k=2
# add this missed benefit (aka regret)
dat$hfd_tilde_1[included_k2] <-  dat$hfd_tilde_1[included_k2] + regret_k2

################################################
#### Backward induction procedure stage k=1 #### 
################################################

# no patients are exluded as non patient initiated RRT before stage k=1 
# ( all patients have a decision to make at stage k=1 )

dat_k1 <- dat

# Fit propensity score for stage k=2

ps_mod_k1 <- glm(a1 ~ bun_k1 + ph_k1 + pot_k1, family = "binomial", data = dat_k1)

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k1 <- with(dat_k1, abs(a1 - predict(ps_mod_k1, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=2 and the tailoring variables 

pr_mod_k1 <- lm(hfd_tilde_1 ~ admission_age + SOFA_24hours + weight + #gender +
#                        bun_k1 +
#                       ph_k1 +
                        uo_k1 + 
#                       pot_k1 + 
                        a1*admission_age + a1*creat_k1 + a1*pot_k1,
                weights = w_k1, data = dat_k1) ### A revoir +++

######
# Model fit and diagnostics are accessible via
# summary(pr_mod_k1) and plot(pr_mod_k1) respectively

# Store all coefficients for bootstrapping
all_coef_k1 <- coef(pr_mod_k1)
names(all_coef_k1) <- paste0("mod_k1_", names(all_coef_k1))

# Store the effect of tailoring variables 
psi_k1 <- coef(pr_mod_k1)[grepl("a1", names(coef(pr_mod_k1)) )]


##### FINAL RETURN FROM THE FUNCTION #####
##### returns concatenated coefficients from stage 1,2,3 models #####

return(c(all_coef_k1, all_coef_k2, all_coef_k3))
}


###
n_boot <- 1
res <- list()
all_coefs <- list()
all_coefs_vars <- list()

for(i in 1:exp_dat$m)
{        
imp_dat <- complete(exp_dat, i)
boot_res <- boot(imp_dat, boot_dtr, R=n_boot)
res[[i]] <- boot_res
all_coefs[[i]] <- res[[i]]$t0
all_coefs_vars[[i]] <- apply(res[[i]]$t, 2, var)
print(paste0(i, "/", exp_dat$m, " datasets completed"))
}

rubin_est <- c() ; rubin_ests <- c()
rubin_var <- c() ; rubin_vars <- c()

for(i in 1:length(all_coefs[[1]]))
{
rubin_est <- mean(sapply(all_coefs, function(x) x[i]) )
rubin_ests <- c(rubin_ests, rubin_est)

rubin_var <- rubinr(sapply(all_coefs, function(x) x[i]), sapply(all_coefs_vars, function(x) x[i]))
rubin_vars <- c(rubin_vars, rubin_var)
}

names(rubin_ests) <- names(res[[i]]$t0)
names(rubin_vars) <- names(res[[i]]$t0)

rubin_ests[grepl("a1", names(rubin_ests))]
rubin_ests[grepl("a2", names(rubin_ests))]
rubin_ests[grepl("a3", names(rubin_ests))]

