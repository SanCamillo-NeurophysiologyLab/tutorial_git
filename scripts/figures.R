rm(list=ls())

curr_import_folder = "intermediate_data/"
curr_export_folder = "Figures/"
dir.create(curr_export_folder)

library(ggplot2)

load(paste(curr_import_folder, "avg_all_cond.RData", sep=""))

# fig aim 1: man vs nat on N1. No effect of image type or electrode position (left vs right)

dat_aim1 = dat[dat$cond %in% c("man", "nat") & dat$Electrode %in% c("PO7", "O1", "P7", "P5", "PO8", "O2", "P8", "P6"), ]
dat_aim1 = droplevels(dat_aim1)
dat_aim1 = aggregate(Ampl ~ cond + Time, data = dat_aim1, FUN = mean)

png("Figures/aim_1.png", res=300*2, height = 2000*2, width = 2000*2)

ggplot(dat_aim1, aes(Time, Ampl)) + 
geom_rect(data=NULL, aes(xmin = 130, xmax=215, ymin=-2, ymax=12), fill="gray96", alpha=1)+
#geom_rect(data = NULL, aes(xmin = 500, xmax = 700, ymin=-7, ymax=7), fill="gray93", alpha=0.8) +
geom_line(aes(colour=cond, group=cond),size=1.05) + # colour, group both depend on cond2
scale_colour_manual(values = c("gray28","red"))+
scale_x_continuous(breaks = seq(0, 700, 200))+
scale_y_continuous(breaks=seq(-2,10,2), limits = c(-2,12))+ # il terzo elemento sono gli step!!!
theme(
  panel.grid.major = element_blank()
  , panel.grid.minor = element_blank()
  , legend.title = element_text(colour="black",size=11)
  , plot.title = element_text(colour="black", size=14)
  , panel.background = element_blank()
  , axis.line.y = element_line(colour = "black")
  , axis.text = element_text(size = 12)
  , axis.title = element_text(size = 12)
  , text=element_text(family="Arial")
  , legend.position = "bottom"
) +
#legend.title = element_text(colour="black", size=12, face="bold") +
labs(x="Time (ms)", y="Amplitude (µV)") +
geom_hline(yintercept=0) +
theme() +
ggtitle("Effects of image type on N1 amplitude") +
theme(plot.title = element_text(hjust = 0.5), legend.direction = "horizontal")

dev.off()


# fig aim 2a: effect of image novelty on voltage ampl in N400 timewin. Main effect of condition

dat_aim2a = dat[dat$cond %in% c("old", "new") & dat$Electrode %in% c("FC2", "FC4", "FC6", "FC1", "FC3", "FC5"), ]
dat_aim2a = droplevels(dat_aim2a)
dat_aim2a = aggregate(Ampl ~ cond + Time, data = dat_aim2a, FUN = mean)

png("Figures/aim_2a.png", res=300*2, height = 2000*2, width = 2000*2)

ggplot(dat_aim2a, aes(Time, Ampl)) + 
  geom_rect(data=NULL, aes(xmin = 300, xmax=500, ymin=-6, ymax=2), fill="gray96", alpha=1)+
  #geom_rect(data = NULL, aes(xmin = 500, xmax = 700, ymin=-7, ymax=7), fill="gray93", alpha=0.8) +
  geom_line(aes(colour=cond, group=cond),size=1.05) + # colour, group both depend on cond2
  scale_colour_manual(values = c("gray28","red"))+
  scale_x_continuous(breaks = seq(0, 700, 200))+
  scale_y_continuous(breaks=seq(-6,2,2), limits = c(-6,2))+ # il terzo elemento sono gli step!!!
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.title = element_text(colour="black",size=11)
    , plot.title = element_text(colour="black", size=14)
    , panel.background = element_blank()
    , axis.line.y = element_line(colour = "black")
    , axis.text = element_text(size = 12)
    , axis.title = element_text(size = 12)
    , text=element_text(family="Arial")
    , legend.position = "bottom"
  ) +
  #legend.title = element_text(colour="black", size=12, face="bold") +
  labs(x="Time (ms)", y="Amplitude (µV)") +
  geom_hline(yintercept=0) +
  theme() +
  ggtitle("Effects of image novelty on EEG amplitude") +
  theme(plot.title = element_text(hjust = 0.5), legend.direction = "horizontal")

dev.off()

# fig aim 2b: effect of image novelty on theta power in N400 timewin. NO effects whatsoever

rm(dat)
load(paste(curr_import_folder, "avg_aim2_TF.RData", sep=""))

dat_aim2b = dat[dat$cond %in% c("old", "new") & dat$Freq=="theta" & dat$Electrode %in% c("PO7", "O1", "P7", "P5", "PO8", "O2", "P8", "P6"), ]
dat_aim2b = droplevels(dat_aim2b)
dat_aim2b = aggregate(Ampl ~ cond + Time, data = dat_aim2b, FUN = mean)

png("Figures/aim_2b.png", res=300*2, height = 2000*2, width = 2000*2)

ggplot(dat_aim2b, aes(Time, Ampl)) + 
  geom_rect(data=NULL, aes(xmin = 300, xmax=500, ymin=-2, ymax=25), fill="gray96", alpha=1)+
  #geom_rect(data = NULL, aes(xmin = 500, xmax = 700, ymin=-7, ymax=7), fill="gray93", alpha=0.8) +
  geom_line(aes(colour=cond, group=cond),size=1.05) + # colour, group both depend on cond2
  scale_colour_manual(values = c("gray28","red"))+
  scale_x_continuous(breaks = seq(0, 700, 200))+
  scale_y_continuous(breaks=seq(-2,25,2), limits = c(-2,25))+ # il terzo elemento sono gli step!!!
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.title = element_text(colour="black",size=11)
    , plot.title = element_text(colour="black", size=14)
    , panel.background = element_blank()
    , axis.line.y = element_line(colour = "black")
    , axis.text = element_text(size = 12)
    , axis.title = element_text(size = 12)
    , text=element_text(family="Arial")
    , legend.position = "bottom"
  ) +
  #legend.title = element_text(colour="black", size=12, face="bold") +
  labs(x="Time (ms)", y="Power") +
  geom_hline(yintercept=0) +
  theme() +
  ggtitle("Effects of image novelty on theta power") +
  theme(plot.title = element_text(hjust = 0.5), legend.direction = "horizontal")

dev.off()

# fig aim 2c: effect of image novelty on alpha power in N400 timewin. NO effects whatsoever

dat_aim2c = dat[dat$cond %in% c("old", "new") & dat$Freq=="alpha" & dat$Electrode %in% c("PO7", "O1", "P7", "P5", "PO8", "O2", "P8", "P6"), ]
dat_aim2c = droplevels(dat_aim2c)
dat_aim2c = aggregate(Ampl ~ cond + Time, data = dat_aim2c, FUN = mean)

png("Figures/aim_2c.png", res=300*2, height = 2000*2, width = 2000*2)

ggplot(dat_aim2c, aes(Time, Ampl)) + 
  geom_rect(data=NULL, aes(xmin = 300, xmax=500, ymin=-40, ymax=4), fill="gray96", alpha=1)+
  #geom_rect(data = NULL, aes(xmin = 500, xmax = 700, ymin=-7, ymax=7), fill="gray93", alpha=0.8) +
  geom_line(aes(colour=cond, group=cond),size=1.05) + # colour, group both depend on cond2
  scale_colour_manual(values = c("gray28","red"))+
  scale_x_continuous(breaks = seq(0, 700, 200))+
  scale_y_continuous(breaks=seq(-40,4,2), limits = c(-40,4))+ # il terzo elemento sono gli step!!!
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.title = element_text(colour="black",size=11)
    , plot.title = element_text(colour="black", size=14)
    , panel.background = element_blank()
    , axis.line.y = element_line(colour = "black")
    , axis.text = element_text(size = 12)
    , axis.title = element_text(size = 12)
    , text=element_text(family="Arial")
    , legend.position = "bottom"
  ) +
  #legend.title = element_text(colour="black", size=12, face="bold") +
  labs(x="Time (ms)", y="Power") +
  geom_hline(yintercept=0) +
  theme() +
  ggtitle("Effects of image novelty on alpha power") +
  theme(plot.title = element_text(hjust = 0.5), legend.direction = "horizontal")

dev.off()
