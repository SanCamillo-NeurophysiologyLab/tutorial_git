rm(list=ls())

curr_import_folder = 'intermediate_data/'
curr_export_folder = 'results/'

library(ez)
library(writexl)
source("R_functions/tpairs2.R")
source("R_functions/ezANOVA.pes.0.2.R")

ANOVA_els = c("FC2", "FC4", "FC6", "FC1", "FC3", "FC5")

load(paste(curr_import_folder, "dat_aim2b_theta_old_new.RData", sep=""))

dat_new = NULL

for (iEl in 1:length(ANOVA_els)){
  curr_el = ANOVA_els[iEl]
  
  curr_dat = dat[dat$Electrode==curr_el, ]
  curr_dat = droplevels(curr_dat)
  
  curr_dat$loc = as.factor("right")
  
  if (curr_el %in% c("FC1", "FC3", "FC5")){
    curr_dat$loc = as.factor("left")
  }
  
  dat_new = rbind(dat_new, curr_dat)
  
}

dat_loc = aggregate(Pow ~ loc + ID + cond, data = dat_new, FUN=mean)

res = ezANOVA.pes(data=dat_loc, dv=.(Pow), wid=.(ID), within=.(cond, loc))

write_xlsx(res, path=paste(curr_export_folder, "anova_AIM2b_theta.xlsx", sep=""))
