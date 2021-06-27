#########################################################################################
### The effect of risk framing on support for restrictive government policy
### regarding the COVID-19 outbreak

### STUDY 2

### Kirill Chmel, Aigul Klimova, Nikita Savin
### June 2021

rm(list = ls(all.names = TRUE))

library(psych)
library(Rmisc)
library(dplyr)
library(effsize)
library(ggpubr)
library(ggplot2)
library(stargazer)

#########################################################################################
#########################################################################################
### Main Analysis

### Load data (see KC's GitHub directory)
load('ChKS_2021_Data_Study2.Rdata')

### Create the dependent variable - support for restrictive measures
dt$r.measures <- rowSums(dplyr::select(dt, D7a_r1, D7a_r2, D7a_r3, D7a_r4, D7a_r5, 
                                    D7a_r8, D7b_r1, D7b_r3, D7b_r4, D7b_r5))


##############################
### Table 6
summary.stats <- dplyr::select(dt, D6, r.measures, D8, 
                               ValuesBENEV, ValuesUNIV,
                               AGE, GENDER, EDUC, A2, A3, A6, F7, B2, F11_r1)
stargazer(summary.stats, type = 'text')



##############################
### Table 7

describeBy(dplyr::select(dt, D6, r.measures, D8), group = dt$Treatment)

summary(aov(D6 ~ Treatment, data = dt))
summary(aov(r.measures ~ Treatment, data = dt))
summary(aov(D8 ~ Treatment, data = dt))



##############################
### Main Factor: Risk Severity

describeBy(dplyr::select(dt, D6, r.measures, D8), group = dt$RiskDegree)

t.test(D6 ~ RiskDegree, data = dt, var.equal = TRUE)
t.test(r.measures ~ RiskDegree, data = dt, var.equal = TRUE)
t.test(D8 ~ RiskDegree, data = dt, var.equal = TRUE)

cohen.d(D6 ~ RiskDegree, data = dt)



##############################
### Main Factor: Risk Target

describeBy(dplyr::select(dt, D6, r.measures, D8), group = dt$RiskTarget)

t.test(D6 ~ RiskTarget, data = dt, var.equal = TRUE)
t.test(r.measures ~ RiskTarget, data = dt, var.equal = TRUE)
t.test(D8 ~ RiskTarget, data = dt, var.equal = TRUE)



##############################
### Table 8

m8.1 <- lm(D6 ~ RiskDegree + RiskTarget, data = dt)
m8.2 <- lm(D6 ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)
m8.3 <- lm(r.measures ~ RiskDegree + RiskTarget, data = dt)
m8.4 <- lm(r.measures ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)
m8.5 <- lm(D8 ~ RiskDegree + RiskTarget, data = dt)
m8.6 <- lm(D8 ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)

stargazer(m8.1, m8.2, m8.3, m8.4, m8.5, m8.6,
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          no.space = T, type = 'text')



##############################
### Table 9

m9.1 <- lm(D6 ~ RiskDegree + RiskTarget + RiskTarget*ValuesBENEV, data = dt)
m9.2 <- lm(D6 ~ RiskDegree + RiskTarget + RiskTarget*ValuesUNIV, data = dt)
m9.3 <- lm(r.measures ~ RiskDegree + RiskTarget + RiskTarget*ValuesBENEV, data = dt)
m9.4 <- lm(r.measures ~ RiskDegree + RiskTarget + RiskTarget*ValuesUNIV, data = dt)
m9.5 <- lm(D8 ~ RiskDegree + RiskTarget + RiskTarget*ValuesBENEV, data = dt)
m9.6 <- lm(D8 ~ RiskDegree + RiskTarget + RiskTarget*ValuesUNIV, data = dt)

stargazer(m9.1, m9.2, m9.3, m9.4, m9.5, m9.6,
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          no.space = T, type = 'text')



##############################
### Table 10
describeBy(dplyr::select(dt, D5), group = dt$RiskDegree)
t.test(D5 ~ RiskDegree, data = dt, var.equal = TRUE)

m10.1 <- lm(D6 ~ RiskDegree*D5, data = dt) 
m10.2 <- lm(r.measures ~ RiskDegree*D5, data = dt) 
m10.3 <- lm(D8 ~ RiskDegree*D5, data = dt) 

stargazer(m10.1, m10.2, m10.3,
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          no.space = T, type = 'text')


##############################
### Figure 2
pd <- position_dodge(0.1)

D6.se <- summarySE(dt, measurevar="D6", groupvars=c("Treatment"))
D6.se <- cbind(D6.se, data.frame(severity = c("Low-risk", 'High-risk',
                                              "High-risk", "Low-risk"),
                               object = c("Individual losses", "Individual losses", 
                                          "Losses to others", "Losses to others")))

r.measures.se <- summarySE(dt, measurevar="r.measures", groupvars=c("Treatment"))
r.measures.se <- cbind(r.measures.se, data.frame(severity = c("Low-risk", 'High-risk', 
                                                              "High-risk", "Low-risk"),
                             object = c("Individual losses", "Individual losses", 
                                        "Losses to others", "Losses to others")))

D8.se <- summarySE(dt, measurevar="D8", groupvars=c("Treatment"))
D8.se <- cbind(D8.se, data.frame(severity = c("Low-risk", 'High-risk', "High-risk", "Low-risk"),
                               object = c("Individual losses", "Individual losses", "Losses to others", "Losses to others")))


F2a <- ggplot(D6.se, aes(x=object, y=D6, color=severity)) + 
  geom_errorbar(aes(ymin=D6-se, ymax=D6+se), width=.1, position=pd) +
  geom_line(aes(x=as.numeric(object), y=D6), position=pd) + 
  geom_point(position=pd) + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=0.1, color="gray"), 
        legend.position=c(0.88, 0.9),
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour ="black"),
        text = element_text(size=8)) +
  guides(colour=guide_legend(title="Risk Severity")) +
  xlab("Object at Risk") +
  ylab("Willingness to sacrifice rights") +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac"))


F2b <- ggplot(r.measures.se, aes(x=object, y=r.measures, color=severity)) + 
  geom_errorbar(aes(ymin=r.measures-se, ymax=r.measures+se), width=.1, position=pd) +
  geom_line(aes(x=as.numeric(object), y=r.measures), position=pd) + 
  geom_point(position=pd) + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=0.1, color="gray"), 
        legend.position=c(0.88, 0.9),
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour ="black"),
        text = element_text(size=8)) +
  guides(colour=guide_legend(title="Risk Severity")) +
  xlab("Object at Risk") +
  ylab("Support for restrictive government policy") +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac"))


F2c <- ggplot(D8.se, aes(x=object, y=D8, color=severity)) + 
  geom_errorbar(aes(ymin=D8-se, ymax=D8+se), width=.1, position=pd) +
  geom_line(aes(x=as.numeric(object), y=D8), position=pd) + 
  geom_point(position=pd) + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=0.1, color="gray"), 
        legend.position=c(0.88, 0.9),
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour ="black"),
        text = element_text(size=8)) +
  guides(colour=guide_legend(title="Risk Severity")) +
  xlab("Object at Risk") +
  ylab("Support for criminal liability for the quarantine violation") +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac"))


F2 <- ggarrange(F2a, F2b, F2c, 
                labels = c("2a", "2b", "2c"),
                ncol = 3, nrow = 1, common.legend = TRUE, legend="top")


##############################
### Table G2

m1.1 <- lm(D6 ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, data = dt)
m1.3 <- lm(r.measures ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, data = dt)
m1.5 <- lm(D8 ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, data = dt)

m1.2 <- lm(D6 ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, 
           data = dt[hatvalues(m1.1) < 0.015,])
m1.4 <- lm(r.measures ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, 
           data = dt[hatvalues(m1.3) < 0.015,])
m1.6 <- lm(D8 ~ RiskDegree + RiskTarget + AGE + GENDER + EDUC + A2 + A3 + A6 + F7 + B2 + F11_r1, 
           data = dt[hatvalues(m1.5) < 0.015,])


se.1.1 <- coeftest(m1.1, vcov = vcovHC(m1.1, type="HC1"))[,2]
se.1.2 <- coeftest(m1.2, vcov = vcovHC(m1.2, type="HC1"))[,2]
se.1.3 <- coeftest(m1.3, vcov = vcovHC(m1.3, type="HC1"))[,2]
se.1.4 <- coeftest(m1.4, vcov = vcovHC(m1.4, type="HC1"))[,2]
se.1.5 <- coeftest(m1.5, vcov = vcovHC(m1.5, type="HC1"))[,2]
se.1.6 <- coeftest(m1.6, vcov = vcovHC(m1.6, type="HC1"))[,2]


stargazer(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6,
          se = list(se.1.1, se.1.2, se.1.3, se.1.4, se.1.5, se.1.6),
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          no.space = T, type = 'text')
