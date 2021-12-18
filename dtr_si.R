dat <- read.csv("/Users/francois/Desktop/github repos/MIMIC-DTR/mimic_si_preds.csv")

################################################
#### Backward induction procedure stage k=3 #### 
################################################

# exclude patient who initiated RRT before k=3 
#(there is no treatment decision to make for these patients)

excluded_k3 <- dat$a2 == 1
dat_k3 <- dat[!excluded_k3,]

ps_mod_k3 <- glm(a3 ~ bun_k3 + ph_k3 + pot_k3, family = "binomial", data= dat_k3)

# create weights as described by Wallace and Moodie, Biometrics 2015

w_k3 <- with(dat_k3, abs(a3 - predict(ps_mod_k3, type = "response")) )

# weighted regression step

pr_mod_k3 <- lm(hfd ~ admission_age + bun_k1 + SOFA_24hours +
                       a3*uo_k3,
                        weights = w_k3, data = dat_k3)

### Create pseudo outcomes ###

# Contrast for a=1 at stage k=3
contrast_a1_k3 <- model.matrix(~1 + uo_k3, data=dat_k3) %*% coef(pr_mod_k3)[grepl("a3", names(coef(pr_mod_k3)) )]

# Contrast for a=0 (reference) at stage k=3
# by definition contrast for reference = 0 at all stages
contrast_a0_k3 <- 0 

# Optimal contrast at stage k=3
optimal_contrast_k3 <- pmax(contrast_a1_k3, contrast_a0_k3)

# Actual contrast at stage k=3
actual_contrast_k3 <- model.matrix(~-1 + a3 + a3:uo_k3, data=dat_k3) %*% coef(pr_mod_k3)[grepl("a3", names(coef(pr_mod_k3)) )]

# Regret at stage k=3
# By definition, regret = optimal contrast - actual contrast
regret_k3 <- optimal_contrast_k3 - actual_contrast_k3

# Create a copy of the observed outcome
dat$hfd_tilde_3 <- dat$hfd

# For the patients who would have benefited from another treatment at stage k=3
# add this missed benefit (aka regret)
dat$hfd_tilde_3[!excluded_k3] <-  dat$hfd_tilde_3[!excluded_k3] + regret_k3
