# Predicting psychometric digit-RAN and dice-RAN with gaze duration and 
# eye-voice span (EVS) from computerized assessment of digit-RAN and dice-RAN 
# in Chinese control and dyslexic children. These results are reported in:

# Pan et al. (2013). Eye–voice span during rapid automatized naming of digits  
# and ￼dice in Chinese normal and dyslexic children. Developmental Science. 

# June 2013, Reinhold Kliegl & Jinger Pan
# 7 & 14 December 2016, Reinhold Kliegl

library(ggplot2)
library(grid)
library(reshape2)
library(plyr)
library(MASS)
library(lme4)
library("LMERConvenienceFunctions" ) 
library(gtools)
library(latticeExtra)

library(RePsychLing)

source("remef.v0.6.9.R")

RKstats <- function(x) c(N=length(x), M=mean(x), SD=sd(x), SE=sd(x)/sqrt(length(x)) )

vplayout <- function(x, y) {
viewport(layout.pos.row = x, layout.pos.col = y) 
}

theme_set(theme_bw())

# OVERVIEW 
# Part 1: Setup
# Part 2: Preliminary analyses
# Part 3: Main LMM analyses: Using gaze and EVS as covariates
# Part 4: Figures of paper -- including some new figures (3-group  visualisation)

# PART 1: SETUP

load("pylsk13.rda")

data <- dat
data$Subj <- factor(data$subj)
data$Group <- factor(data$group, labels=c("control", "dyslexic"))
data$Condition <- factor(data$condition, labels=c("digit", "dice"))


# Check distributions of continuous variables
boxcox(ran ~ subj*condition, data=data)
boxcox(gaze ~ subj*condition, data=data)
boxcox(evs ~ subj*condition, data=data)

# Justifies log-transform of ran, not of gaze and evs
data$lran <- log(data$ran)

# Center covariates
data$gaze.c <- scale(data$gaze,scale=FALSE,center=TRUE)
data$evs.c <- scale(data$evs,scale=FALSE,center=TRUE)

# PART 2: PRELIMINARY ANALYSES

# Overall and within-group correlations
data_w <- cbind(data[data$Condition=="digit", ], 
                data[data$Condition=="dice", ])[ , c(9:10, 4, 19, 6, 21, 5, 20)]
names(data_w) <- c("Subj", "Group", "g_ran", "c_ran", "g_evs", "c_evs", "g_gaze", "c_gaze")
options(digits=2)

#  Psychometric RAN (pmRAN)
M_RAN <- ddply(data, c("Group", "Condition"), with, each(RKstats) (lran) )
# Note: SEs only valid for between-subject comparisons (Group)

ggplot(data=M_RAN, aes(x=Group, y=M, group=Condition, shape=Condition)) +
       geom_line() + geom_point() +
	     scale_y_continuous("log(RAN)") + 
       geom_errorbar(aes(ymax=M+2*SE, ymin=M-2*SE, width=0.03)) +
       coord_cartesian(ylim=c(2,4))

r1  <- lmer(lran ~ group*condition + (1 | subj), REML=FALSE, data=data)
print(summary(r1), cor=FALSE)

r1b  <- lmer(lran ~ group*condition + (1 + condition || subj), REML=FALSE, data=data)
print(summary(r1b), cor=FALSE)
summary(rePCA(r1b))

anova(r1, r1b) # not significant

## max model not possible
#r1c  <- lmer(lran ~ group*condition + (1 + condition | subj), REML=FALSE, data=data,
#             control=lmerControl(check.nobs.vs.nRE = "ignore"))
#print(summary(r1c), cor=FALSE)


# Gaze -- replicates pattern for pmRAN
M_gaze <- ddply(data, c("Group", "Condition"),  with, each(RKstats) (gaze) )
# Note: SEs only valid for between-subject comparisons (Group)

qplot(data=M_gaze, x=Group, y=M, group=Condition, 
      shape=Condition, geom=c("line", "point"), ylab="Gaze duration (ms)") + 
      geom_errorbar(aes(max=M+2*SE, min=M-2*SE, width=0.03)) 

g1 <- lmer(gaze ~ group*condition + (1 | subj), REML=FALSE, data=data)
print(summary(g1), cor=FALSE)

g1b <- lmer(gaze ~ group*condition + (1 + condition || subj), REML=FALSE, data=data)
print(summary(g1b), cor=FALSE)
summary(rePCA(g1b))

anova(g1, g1b)

# So if we include gaze as covariate for pmRAN
r1 <- lmer(lran ~ group*condition + (1 | subj), data=data, REML=FALSE)
r2 <- lmer(lran ~ group*condition+gaze + (1 | subj), data=data, REML=FALSE)
print(summary(r2), cor=FALSE)

r2a <- lmer(lran ~ (group+condition+gaze.c)^2 + (1 | subj), data=data, REML=FALSE)
print(summary(r2a), cor=FALSE)

r2b <- lmer(lran ~ (group+condition+gaze.c)^3 + (1 | subj), data=data, REML=FALSE)
print(summary(r2b), cor=FALSE)

anova(r1, r2, r2a, r2b) 

# Add variance components for within-subject effects
r3a <- lmer(lran ~ group*condition+gaze.c + (1 + condition || subj), data=data, REML=FALSE)
print(summary(r3a), cor=FALSE)
summary(rePCA(r3a))
anova(r2, r3a)

r3b <- lmer(lran ~ group*condition+gaze.c + (1 + gaze.c || subj), data=data, REML=FALSE)
print(summary(r3b), cor=FALSE)
summary(rePCA(r3b))
anova(r2, r3b)

# r2 looks like best model

# Three main effects (group, condition, gaze.c), plus significant group * condition interaction
r2 <- lmer(lran ~ group*condition+gaze.c + (1 | subj), REML=FALSE, data=data)
print(summary(r2))

# EVS -- replicates pattern for pmRAN
M_EVS <- ddply(data, c("Group", "Condition"), with, each(RKstats) (evs))
# Note: SEs only valid for between-subject comparisons (Group)

qplot(data=M_EVS, x=Group, y=M, group=Condition, shape=Condition, geom=c("line", "point"), ylab="Eye-voice span (char)") + 
      geom_errorbar(aes(max=M+1*SE, min=M-1*SE, width=0.03)) 

e1  <- lmer(evs ~ group*condition + (1 | subj), REML=FALSE, data=data)
print(summary(e1), cor=FALSE)

# So if we include evs as covariate for pmRAN,
data$evs.c <- scale(data$evs,scale=FALSE,center=TRUE)

r3 <- lmer(lran ~ group*condition+evs.c + (1 | subj), data=data, REML=FALSE)
r3a <- lmer(lran ~ (group+condition+evs.c)^2 + (1 | subj), data=data, REML=FALSE)
r3b <- lmer(lran ~ (group+condition+evs.c)^3 + (1 | subj), data=data, REML=FALSE)

anova(r1, r3, r3a, r3b)  # evs.c is significant covariate
# Three main effects (group, condition, evs.c), plus significant condition*group interaction

# PART 3: Main LMM analyses: Using gaze and EVS as covariates

m00 <- lmer(lran ~ condition+group+evs.c+gaze.c + (1  | subj), data=data, REML=FALSE)
print(summary(m00), cor=FALSE)

# Minimum model given previous results
m05 <- lmer(lran ~ condition*group+evs.c+gaze.c + (1  | subj), data=data, REML=FALSE)
print(summary(m05), cor=FALSE)

# Full factorial 
m10 <- lmer(lran ~ condition*group*evs.c*gaze.c + (1  | subj), data=data, REML=FALSE)
print(summary(m10), cor=FALSE)

# Remove 3 non-significant higher-order interactions involving evs.c:gaze.c
m09 <- lmer(lran ~ condition*group*evs.c*gaze.c - condition:group:evs.c:gaze.c 
	                     - group:evs.c:gaze.c - condition:gaze.c:evs.c
	                     + (1  | subj), data=data, REML=FALSE)
print(summary(m09), corr=FALSE)
anova(m00, m05, m09, m10)
# Best model; reported in Table 3

# Check effect of log-transformation
m09b <- lmer(ran ~ condition*group*evs.c*gaze.c - condition:group:evs.c:gaze.c 
            - group:evs.c:gaze.c - condition:gaze.c:evs.c
            + (1  | subj), data=data, REML=FALSE)
print(summary(m09b), corr=FALSE)

# Check residuals
qqmath(resid(m09))

qplot(x=fitted(m09), y=resid(m09), geom="point", 	
	xlab="Fitted values", ylab="Standardized residuals") + 
	geom_hline(yintercept=0) + 
	geom_density2d(size=1) 

# ... for log-transformed values
qqmath(resid(m09b))      
qplot(x=fitted(m09b), y=resid(m09), geom="point", 	
      xlab="Fitted values", ylab="Standardized residuals") + 
  geom_hline(yintercept=0) + 
  geom_density2d(size=1) 

# PART 4: Figures -- unadjusted and partial plots (model m09) of interactions


# Figure 1: Condition x  Group x EVS (t=-2.5)

# Unadjusted observed scores
p1 <- qplot(data=data, y=lran, x=evs,  group=Condition:Group, color=Condition:Group, 
            geom=c("point", "smooth"), method=lm,
            xlab = "Eye-voice span", ylab="log(RAN)", ylim=c(2, 4)) + 
  theme(legend.position = "none", panel.background=element_rect(fill = "white"))
p1

# -- evs for dice better predictor for dyslexic, 
# -- evs for digits better predictor for control

# Partial effects
data$CndGrpEVS.m09 <- remef(m09, keep=TRUE, grouping=TRUE, fix = c(1, "condition:group:evs.c"), 
                            ran = NULL, plot=FALSE) 

p2 <- 
  ggplot(data=data, aes(y=CndGrpEVS.m09, x=evs, 
                        group=Condition:Group, color=Condition:Group)) +
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_continuous("Eye-voice span") +
  scale_y_continuous("Adjusted log(RAN)") + 
  coord_cartesian(ylim=c(2,4)) +
  theme(legend.position = c(.30, .20), 
        legend.text = element_text(size=8), 
        panel.background=element_rect(fill = "white")) 
p2

#  -- evs only predictor for control in digit-RAN!

grid.newpage() # Figure 1
pushViewport(viewport(layout = grid.layout(1,2))) 
print(p1, vp=vplayout(1,1))
print(p2, vp=vplayout(1,2))


# Figure 2: EVSgroup x GAZE 

# Partial effect for Condition x Group x GD
data$CndGrpGAZE.m09 <- 
  remef(m09, keep=TRUE, grouping=TRUE, fix = c(1, "condition:group:gaze.c"), ran = NULL) 

p3 <- 
  ggplot(data=data, aes(y=CndGrpGAZE.m09, x=gaze, 
                        group=Condition:Group, color=Condition:Group)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_continuous("Gaze duration") +
  scale_y_continuous("Adjusted log(RAN)") + 
  coord_cartesian(ylim=c(2,4)) +
  theme(legend.position = c(.75, .25), panel.background=element_rect(fill = "white")) 
p3

# -- dice control slope looks different, but this difference is not strong enough?

# Partial effect for EVS x GD, visualized for small and large EVS groups

# ... form two EVS groups
idEVS <- ddply(data, "Subj", with, each(M=mean)(evs))
idEVS$EVSgroup.2 <- quantcut(idEVS$M, seq(0, 1, by=1/2), label=c("small", "large"))
data1 <- merge(data, idEVS, by="Subj")

data1$EVS_GAZE.m09  <- remef(m09, keep=TRUE, grouping=TRUE, fix = c(1, "evs.c:gaze.c"), ran = NULL, plot=FALSE)

p4 <- qplot(data=data1, y=EVS_GAZE.m09, x=gaze, group=EVSgroup.2, color=EVSgroup.2, geom=c("point", "smooth"), method=lm, 
		xlab = "Gaze duration", ylab="Adjusted log(RAN)", ylim=c(2, 4)) + scale_colour_hue("EVS group") +
	theme(legend.position = c(.75, .25),panel.background=element_rect(fill = "white")) 

p4b <- qplot(data=data1, y=lran, x=gaze, group=EVSgroup.2, color=EVSgroup.2, geom=c("point", "smooth"), method=lm, 
            xlab = "Gaze duration", ylab="log(RAN)", ylim=c(2, 4)) + scale_colour_hue("EVS group") +
  theme(legend.position = c(.75, .25),panel.background=element_rect(fill = "white")) 

grid.newpage()  #  Figure 2
pushViewport(viewport(layout = grid.layout(1,2))) 
print(p4b, vp=vplayout(1,1))
print(p4, vp=vplayout(1,2))

# Partial effect for EVS x GD, visualized for small, medium, and large EVS groups

# ... form three EVS groups
idEVS2 <- ddply(data, "Subj", with, each(M=mean)(evs))
idEVS2$EVSgroup.3 <- quantcut(idEVS2$M, seq(0, 1, by=1/3), label=c("small", "medium", "large"))
data2 <- merge(data1, idEVS2, by="Subj")

# ... ... partial efffects
p5 <- qplot(data=data2, y=EVS_GAZE.m09, x=gaze, group=EVSgroup.3, color=EVSgroup.3, geom=c("point", "smooth"), method=lm, 
            xlab = "Gaze duration", ylab="Adjusted log(RAN)", ylim=c(2, 4)) + scale_colour_hue("EVS group") +
  theme(legend.position = c(.75, .25),panel.background=element_rect(fill = "white")) 

# ... ... zero-order relations
p5b <- qplot(data=data2, y=lran, x=gaze, group=EVSgroup.3, color=EVSgroup.3, geom=c("point", "smooth"), method=lm, 
             xlab = "Gaze duration", ylab="log(RAN)", ylim=c(2, 4)) + scale_colour_hue("EVS group") +
  theme(legend.position = c(.75, .25),panel.background=element_rect(fill = "white")) 

grid.newpage()  # New Figure (Dec 2016)
pushViewport(viewport(layout = grid.layout(1,2))) 
print(p5b, vp=vplayout(1,1))
print(p5, vp=vplayout(1,2))
