#########################################################################################
### The effect of risk framing on support for restrictive government policy
### regarding the COVID-19 outbreak

### STUDY 1

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
load('ChKS_2021_Data_Study1.Rdata')

### Create the dependent variable - support for restrictive measures
r.measures <- cbind(select(dt, starts_with("Q40")), 
                    select(dt, starts_with("Q41")))
dt$r.measures <- rowSums(r.measures)



##############################
### Table 2
summary.stats <- dplyr::select(dt, Q42, r.measures, Q43, Q62, Q59, Q3, Q12, 
                       Q58, Q47, Q56_r1)
stargazer(summary.stats, type = 'text')



##############################
### Table 3

describeBy(dplyr::select(dt, Q42, r.measures, Q43), group = dt$Treatment)

summary(aov(Q42 ~ Treatment, data = dt))
summary(aov(r.measures ~ Treatment, data = dt))
summary(aov(Q43 ~ Treatment, data = dt))



##############################
### Main Factor: Risk Severity

describeBy(dplyr::select(dt, Q42, r.measures, Q43), group = dt$RiskDegree)

t.test(Q42 ~ RiskDegree, data = dt, var.equal = TRUE)
t.test(r.measures ~ RiskDegree, data = dt, var.equal = TRUE)
t.test(Q43 ~ RiskDegree, data = dt, var.equal = TRUE)

cohen.d(Q42 ~ RiskDegree, data = dt)
cohen.d(r.measures ~ RiskDegree, data = dt)
cohen.d(Q43 ~ RiskDegree, data = dt)



##############################
### Main Factor: Risk Target

describeBy(dplyr::select(dt, Q42, r.measures, Q43), group = dt$RiskTarget)

t.test(Q42 ~ RiskTarget, data = dt, var.equal = TRUE)
t.test(r.measures ~ RiskTarget, data = dt, var.equal = TRUE)
t.test(Q43 ~ RiskTarget, data = dt, var.equal = TRUE)



##############################
### Table 4

m4.1 <- lm(Q42 ~ RiskDegree + RiskTarget, data = dt)
m4.2 <- lm(Q42 ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)
m4.3 <- lm(r.measures ~ RiskDegree + RiskTarget, data = dt)
m4.4 <- lm(r.measures ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)
m4.5 <- lm(Q43 ~ RiskDegree + RiskTarget, data = dt)
m4.6 <- lm(Q43 ~ RiskDegree + RiskTarget + RiskDegree*RiskTarget, data = dt)

stargazer(m4.1, m4.2, m4.3, m4.4, m4.5, m4.6,
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          no.space = T, type = 'text')



##############################
### CATE: Credibility
describeBy(dplyr::select(dt, Q38), group = dt$RiskDegree)
t.test(Q38 ~ RiskDegree, data=dt, var.equal = T)



##############################
### Figure 1
pd <- position_dodge(0.1)

Q42.se <- summarySE(dt, measurevar="Q42", groupvars=c("Treatment"))
Q42.se <- cbind(Q42.se, data.frame(severity = c("Low-risk", 'High-risk',
                                                "High-risk", "Low-risk"),
                                     object = c("Individual losses", "Individual losses", 
                                                "Losses to others", "Losses to others")))

r.measures.se <- summarySE(dt, measurevar="r.measures", groupvars=c("Treatment"))
r.measures.se <- cbind(r.measures.se, data.frame(severity = c("Low-risk", 'High-risk', 
                                                              "High-risk", "Low-risk"),
                                                 object = c("Individual losses", "Individual losses", 
                                                            "Losses to others", "Losses to others")))

Q43.se <- summarySE(dt, measurevar="Q43", groupvars=c("Treatment"))
Q43.se <- cbind(Q43.se, data.frame(severity = c("Low-risk", 'High-risk', 
                                                "High-risk", "Low-risk"),
                                   object = c("Individual losses", "Individual losses", 
                                              "Losses to others", "Losses to others")))


F1a <- ggplot(Q42.se, aes(x=object, y=Q42, color=severity)) + 
  geom_errorbar(aes(ymin=Q42-se, ymax=Q42+se), width=.1, position=pd) +
  geom_line(aes(x=as.numeric(object), y=Q42), position=pd) + 
  geom_point(position=pd) + theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.1, color="gray"), 
        legend.position=c(0.88, 0.9),
        legend.background = element_rect(size=0.2, linetype="solid", 
                                         colour ="black"),
        text = element_text(size=8)) +
  guides(colour=guide_legend(title="Risk Severity")) +
  xlab("Object at Risk") +
  ylab("Willingness to sacrifice rights") +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac"))


F1b <- ggplot(r.measures.se, aes(x=object, y=r.measures, color=severity)) + 
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


F1c <- ggplot(Q43.se, aes(x=object, y=Q43, color=severity)) + 
  geom_errorbar(aes(ymin=Q43-se, ymax=Q43+se), width=.1, position=pd) +
  geom_line(aes(x=as.numeric(object), y=Q43), position=pd) + 
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


F1 <- ggarrange(F1a, F1b, F1c, 
                labels = c("1a", "1b", "1c"),
                ncol = 3, nrow = 1, common.legend = TRUE, legend="top")


##############################
### Table G1

m1.1 <- lm(Q42 ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, data = dt)
m1.3 <- lm(r.measures ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, data = dt)
m1.5 <- lm(Q43 ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, data = dt)

m1.2 <- lm(Q42 ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, 
           data = dt[hatvalues(m1.1) < 0.028,])
m1.4 <- lm(r.measures ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, 
           data = dt[hatvalues(m1.3) < 0.028,])
m1.6 <- lm(Q43 ~ RiskDegree + RiskTarget + Q62 + Q59 + Q3 + Q12 + Q58 + Q47 + Q56_r1, 
           data = dt[hatvalues(m1.5) < 0.028,])


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
