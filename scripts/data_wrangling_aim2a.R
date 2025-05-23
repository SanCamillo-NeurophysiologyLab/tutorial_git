rm(list=ls())

setwd('')

curr_import_folder = 'original_data/Aim2a/'
curr_export_folder = 'intermediate_data/'
dir.create(curr_export_folder)

all_subj = dir(curr_import_folder)

start_time = -3000
end_time = 3000

library(erpR)
library(stringr)

timewin = c(300, 500)

all_el = read.delim(paste(curr_export_folder, 'electrodes.txt'))
all_el = names(all_el)
exclude_el = "X"
all_el = setdiff(all_el, exclude_el)

dat = NULL

for (iSubj in 1:length(all_subj)){
	curr_subj = all_subj[iSubj]

	print(curr_subj)

	subj_files = dir(paste(curr_import_folder, curr_subj, sep =""))

	subj_dat = NULL

		for (iFile in 1:length(subj_files)){
		curr_file = subj_files[iFile]	

		cond_dat = NULL

		FileName = subj_files[iFile]

		print(FileName)

		curr_file = paste(curr_import_folder, curr_subj, "/", FileName, sep="")
		curr_dat = read.table(curr_file, header=T, sep="\t", skip=1)
				
		# replace missing electrode with NA (if present)
		curr_dat[, setdiff(all_el, names(curr_dat))]=NA

		#FileInfo = readLines(curr_file, 1)
		
		# define condition
		cond_subj = str_locate(FileName, "__")[1] # locates the position of patterns within a string. the string __ is always present in each file, and is a good anchor to retrieve condition name
		cond = substr(FileName, 1, cond_subj-1)
			
		# define timevector
		timevec = seq(start_time, end_time, length.out = dim(curr_dat)[1])

		## add all fields
		curr_dat$Time = timevec
		curr_dat$cond = as.factor(cond)
		curr_dat$ID = as.factor(curr_subj)
		
		## Adjust some variables
		curr_dat$X=NULL
		#cn(curr_dat)
		curr_dat_long = rearrange(curr_dat, deps = c(1:70), oth=c(73:75), name.dep="Ampl", name.newvar="Electrode")
		curr_dat_long$Ampl = curr_dat_long$Ampl*1000000

		curr_dat_avg = aggregate(Ampl ~ Electrode + ID + cond, data = curr_dat_long[curr_dat_long$Time>timewin[1] & curr_dat_long$Time<timewin[2],], FUN=mean)
					
		subj_dat = rbind(curr_dat_avg, subj_dat)

		} # close file (cond)

print("cond done")
dat = rbind(subj_dat, dat)

} # close subj

save(dat, file = paste(curr_export_folder, "dat_aim2a_old_new.RData", sep=""))

