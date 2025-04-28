# Load libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(rstatix)
library(ggpubr)
library(readxl)
library(table1)
library(irr)

#####
# Load data and clean it
#####
# Importing specific sheets into R using the read_excel() 
dTable1<-read_excel("EPaudioData_complete.xlsx", 
                       sheet = "EPaudioData") 
dTable2<-read_excel("EPaudioData_complete.xlsx", 
                       sheet = "MultiEPreformat") 
Demographics<-read_excel("EPaudioData_complete.xlsx",
                         sheet = "Demographics")
 

# For viewing the details of sheet 1 
head(dTable1) 
# For viewing the details of sheet 2 
head(dTable2) 

dTable1$Disease<-factor(dTable1$Disease)
str(dTable1)

dTable2$Disease<-factor(dTable2$Disease)
dTable2$Patient<-factor(dTable2$Patient)
dTable2$Measure<-factor(dTable2$Measure)
str(dTable2)

Demographics$Disease<-factor(Demographics$Disease)
Demographics$Race<-factor(Demographics$Race)
Demographics$Ethnicity<-factor(Demographics$Ethnicity)
Demographics$Gender<-factor(Demographics$Gender)
str(Demographics)

#Demographics Table
head(Demographics)
table1(~Gender + Age.years + Race + Ethnicity | Disease, data=Demographics)


# colorblind friendly plots

highcontrast <- c("#004488","#DDAA33","#BB5566")


#####Figure 3B Stats and BoxPlots
####
##summary stats
dTable1 %>%
  group_by(Disease) %>%
  get_summary_stats(LSCC_EP, type = "mean_sd")

dTable1 %>%
  group_by(Disease) %>%
  get_summary_stats(PSCC_EP, type = "mean_sd")

# simple box plots
ggplot(data=dTable1) +
  geom_boxplot(aes(x=Disease, y=LSCC_EP))

ggplot(data=dTable1) +
  geom_boxplot(aes(x=Disease, y=PSCC_EP))

#check for outliers
LSCC_outliers <- dTable1 %>% 
  group_by(Disease) %>%
  identify_outliers(LSCC_EP)

PSCC_outliers <- dTable1 %>% 
  group_by(Disease) %>%
  identify_outliers(PSCC_EP)

#check normality
# Build the linear model
model_LSCC  <- lm(LSCC_EP ~ Disease, data = dTable1)
# Create a QQ plot of residuals
ggqqplot(residuals(model_LSCC))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model_LSCC))

# Build the linear model
model_PSCC  <- lm(PSCC_EP ~ Disease, data = dTable1)
# Create a QQ plot of residuals
ggqqplot(residuals(model_PSCC))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model_PSCC))

#check normality assumption by groups
dTable1 %>%
  group_by(Disease) %>%
  shapiro_test(LSCC_EP)

dTable1 %>%
  group_by(Disease) %>%
  shapiro_test(PSCC_EP)

ggqqplot(dTable1, "LSCC_EP", facet.by = "Disease")

ggqqplot(dTable1, "PSCC_EP", facet.by = "Disease")

#homogeneity of variance assumptions
plot(model_LSCC, 1)

plot(model_PSCC, 1)

dTable1 %>% levene_test(LSCC_EP ~ Disease)

dTable1 %>% levene_test(PSCC_EP ~ Disease)

#ANOVA
res.aov <- dTable1 %>% anova_test(LSCC_EP ~ Disease)
res.aov

res.aov <- dTable1 %>% anova_test(PSCC_EP ~ Disease)
res.aov

#post-hoc tests
# Pairwise comparisons with Tukey test
pwc_LSCC <- dTable1 %>% tukey_hsd(LSCC_EP ~ Disease)
pwc_LSCC

pwc_PSCC <- dTable1 %>% tukey_hsd(PSCC_EP ~ Disease)
pwc_PSCC

#add stats to boxplot for Figure 3B
LSCC_boxplot <- 
  ggplot(data=dTable1) +
  (aes(x=Disease, y=LSCC_EP, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("LSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.1)

pwc_LSCC <- pwc_LSCC %>% add_xy_position(x = "Disease")
LSCC_boxplot + stat_pvalue_manual(pwc_LSCC, label = "p.adj.signif", tip.length = 0.01)

PSCC_boxplot <-
  ggplot(data=dTable1) +
  (aes(x=Disease, y=PSCC_EP, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("PSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.1)

pwc_PSCC <- pwc_PSCC %>% add_xy_position(x = "Disease")
PSCC_boxplot + stat_pvalue_manual(pwc_PSCC, label = "p.adj.signif", tip.length = 0.01)


#####Figure4A
ggplot(data=dTable2) +
  (aes(x=Patient, y=LSCC, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("LSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.085) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data=dTable2) +
  (aes(x=Patient, y=PSCC, color=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_boxplot (outliers = FALSE) +
  geom_jitter(aes(shape=Disease), size=3, alpha=0.9) +
  ylab("PSCC E/P Ratio") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  ylim(0.02, 0.085) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#####Figure 4B
ggplot(data=dTable1) +
  geom_point(size=6,aes(x=LSCC_EP, y=PSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_smooth(aes(x=LSCC_EP, y=PSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("PSCC E/P Ratio") +
  xlab("LSCC E/P Ratio")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0.02, 0.08) +
  ylim(0.02, 0.08) 

#PSCC EP vs LSCC EP correlation
fit<-lm(LSCC_EP ~ PSCC_EP, data=dTable1)
summary(fit)


#####Figure 5
#Figure5A
ggplot(data=dTable1) +
  geom_point(size=6,aes(x=PTA.BC, y=LSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) +
  geom_smooth(aes(x=PTA.BC, y=LSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("LSCC E/P Ratio") +
  xlab("Pure Tone Average (dB HL)")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0, 80) +
  ylim(0.02, 0.08)

#PTA vs. LSCC EP correlation
fit<-lm(LSCC_EP ~ PTA.BC, data=dTable1)
summary(fit)

#Figure5B
ggplot(data=dTable1) +
  geom_point(size=6,aes(x=PTA.BC, y=PSCC_EP, color=Disease, shape=Disease)) +
  scale_colour_manual(values=highcontrast) + 
  geom_smooth(aes(x=PTA.BC, y=PSCC_EP), color="black", method=lm, se=FALSE) +
  ylab("PSCC E/P Ratio") +
  xlab("Pure Tone Average (dB HL)")+
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(size = 0.7, color = "black")) +
  xlim(0, 80) +
  ylim(0.02, 0.08) 

#PTA vs. PSCC EP correlation
fit<-lm(PSCC_EP ~ PTA.BC, data=dTable1)
summary(fit)



#####Intraclass correlation coefficient
# Importing specific sheets into R using the read_excel() 
ICC_LSCC<-read_excel("EPaudioData_complete.xlsx", 
                    sheet = "MultiEPreformat_LSCCnoNA") 

icc(ICC_LSCC, model = "twoway", type = "consistency", unit = "average")

ICC_PSCC<-read_excel("EPaudioData_complete.xlsx", 
                     sheet = "MultiEPreformat_PSCCnoNA") 

icc(ICC_PSCC, model = "twoway", type = "consistency", unit = "average")

