dat <- read.csv("/Users/francois/Desktop/github repos/MIMIC-DTR/mimic_si.csv")
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

# Create weights as described by Wallace and Moodie, Biometrics 2015
# to achieve double robustness

w_k3 <- with(dat_k3, abs(a3 - predict(ps_mod_k3, type = "response")) )

# Weighted Least Square regression step
# Specificity model with main prognosis variables and interactions between treatment
# at stage k=3 and the tailoring variables 

#pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k1 + bun_k3 +
#                       a3*uo_k3,
#                        weights = w_k3, data = dat_k3)

pr_mod_k3 <- lm(hfd ~ admission_age + weight + gender + SOFA_24hours + bun_k3 +
                        a3 * uo_k3 + a3 * I(bun_k3/bun_k1),
                weights = w_k3, data = dat_k3) ## keep this model

# Model fit and diagnostics are accessible via
# summary(pr_mod_k3) and plot(pr_mod_k3) respectively

# Store all coefficients for bootstrapping
all_coef_k3 <- coef(pr_mod_k3)

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
                        #ph_k2 +
                        #uo_k2 + 
                         #pot_k2 + 
                         a2*bun_k2 + a2:I((ph_k2-ph_k1)/ph_k1) + a2*I(uo_k2*uo_k1),
                weights = w_k2, data = dat_k2) ### A revoir +++

# Model fit and diagnostics are accessible via
# summary(pr_mod_k2) and plot(pr_mod_k2) respectively

# Store all coefficients for bootstrapping
all_coef_k2 <- coef(pr_mod_k2)

# Store the effect of tailoring variables 
psi_k2 <- coef(pr_mod_k2)[grepl("a2", names(coef(pr_mod_k2)) )]

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=2
contrast_a1_k2 <- model.matrix(~1 + bun_k2 + I((ph_k2 - ph_k1)/ph_k1) + I(uo_k2*uo_k1), data=dat_k2) %*% psi_k2

# Contrast for a=0 (reference) at stage k=2
# by definition contrast for reference = 0 at all stages
contrast_a0_k2 <- 0 

# Optimal contrast at stage k=2
optimal_contrast_k2 <- pmax(contrast_a1_k2, contrast_a0_k2)

# Actual contrast at stage k=2
actual_contrast_k2 <- model.matrix(~-1 + a2 + a2:bun_k2 + a2:I((ph_k2 - ph_k1)/ph_k1) + a2:I(uo_k2*uo_k1), data=dat_k2) %*% psi_k2

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

# Store the effect of tailoring variables 
psi_k1 <- coef(pr_mod_k1)[grepl("a1", names(coef(pr_mod_k1)) )]

