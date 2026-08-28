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
data$gaze_c <- scale(data$gaze,scale=FALSE,center=TRUE)
data$evs_c <- scale(data$evs,scale=FALSE,center=TRUE)

# PART 2: Preliminary analysis: <Skipped see other script>

# PART 3: Main LMM analyses: Using gaze and EVS as covariates

m00 <- lmer(lran ~ condition+group+evs_c+gaze_c + (1  | subj), data=data, REML=FALSE)
print(summary(m00), cor=FALSE)

# Minimum model given previous results
m05 <- lmer(lran ~ condition*group+evs_c+gaze_c + (1  | subj), data=data, REML=FALSE)
print(summary(m05), cor=FALSE)

# Full factorial 
m10 <- lmer(lran ~ condition*group*evs_c*gaze_c + (1  | subj), data=data, REML=FALSE)
print(summary(m10), cor=FALSE)

# Remove 3 non-significant higher-order interactions involving evs_c:gaze_c
m09 <- lmer(lran ~ condition*group*evs_c*gaze_c - condition:group:evs_c:gaze_c 
	                     - group:evs_c:gaze_c - condition:gaze_c:evs_c
	                     + (1  | subj), data=data, REML=FALSE)
print(summary(m09), corr=FALSE)
anova(m00, m05, m09, m10)
# Best model; reported in Table 3

# Check effect of log-transformation
m09b <- lmer(ran ~ condition*group*evs_c*gaze_c - condition:group:evs_c:gaze_c 
            - group:evs_c:gaze_c - condition:gaze_c:evs_c
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
  geom_density2d(linewidth=1) 

# PART 4: Figures -- unadjusted and partial plots (model m09) of interactions


# Figure 1: Condition x  Group x EVS (t=-2.5)

## Unadjusted observed scores
p1 <- 
  ggplot(data=data, aes(y=lran, x=evs,  group=Condition:Group, color=Condition:Group)) +
         geom_point() + geom_smooth(method="lm") +
         scale_x_continuous("Eye-voice span") +
         scale_y_continuous("log(RAN)") + 
  coord_cartesian(ylim=c(2,4)) +
  theme(legend.position = "none", 
        panel.background=element_rect(fill = "white"))
p1

# -- evs for dice better predictor for dyslexic, 
# -- evs for digits better predictor for control

## Partial effects
data$CndGrpEVS.m09 <- 
  remef(m09, keep=TRUE, grouping=TRUE, fix = c(1, "condition:group:evs_c"), 
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

## Combine in two-panel figure

grid.newpage() # Figure 1
pushViewport(viewport(layout = grid.layout(1,2))) 
print(p1, vp=vplayout(1,1))
print(p2, vp=vplayout(1,2))

#-- evs only predictor for control in digit-RAN!
  
# Figure 2: EVSgroup x GAZE 

## Unadjusted observed scores
p3 <- 
  ggplot(data=data, aes(y=lran, x=gaze, 
                        group=Condition:Group, color=Condition:Group)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_continuous("Gaze duration") +
  scale_y_continuous("log(RAN)") + 
  coord_cartesian(ylim=c(2,4)) +
  theme(legend.position = "none") 
p3

## Partial effect for Condition x Group x GD
data$CndGrpGAZE.m09 <- 
  remef(m09, keep=TRUE, grouping=TRUE, fix = c(1, "condition:group:gaze_c"), ran = NULL) 

p4 <- 
  ggplot(data=data, aes(y=CndGrpGAZE.m09, x=gaze, 
                        group=Condition:Group, color=Condition:Group)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  scale_x_continuous("Gaze duration") +
  scale_y_continuous("Adjusted log(RAN)") + 
  coord_cartesian(ylim=c(2,4)) +
	theme(legend.position = c(.75, .25), 
	      panel.background=element_rect(fill = "white")) 
p4

## Combine in two-panel figure

grid.newpage() # Figure 2
pushViewport(viewport(layout = grid.layout(1,2))) 
print(p3, vp=vplayout(1,1))
print(p4, vp=vplayout(1,2))

