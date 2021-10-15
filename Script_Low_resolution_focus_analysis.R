################################################################################
#         Effect of COVID lockdowns on herp behaviour - ZDZ130
################################################################################

#__Workspace prep__
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lattice)



###___ DATA IMPORT, FORMATTING & EXPLORATION ___________________________________

#__Data import__
df <- read.csv("Low_resolution_focus.csv", sep=";")
lapply(df, class)

#__Variables formatting__
df <- df %>% mutate(Species = factor(Species, ordered = FALSE),
                    Enclosure_status = factor(Enclosure_status, ordered = FALSE),
                    Date = as.Date(Date, format = "%d/%m/%Y"),
                    Month = as.integer(as.character(Date, "%m")))
lapply(df, class)

#__Data check__
summary(df)
Obs_count <- df %>% group_by(Species, Total_ind, Enclosure_status) %>% #pb: Total_ind Gidgees
  tally() %>%
  ungroup()
write.csv2(Obs_count, file = "Summary table - Observation count per species and enclosure status.csv", row.names = FALSE)

#__Prep df for plots__
df_Prop <- df %>% group_by(Species, Enclosure_status, Month) %>%
  mutate(Active=sum(Nb_active),
         Inactive=sum(Nb_inactive),
         OOS=sum(Nb_OOS)) %>%
  select(Species, Enclosure_status, Active, Inactive, OOS, Month) %>%
  distinct() %>%
  ungroup()
df_plots <- gather(df_Prop, Behaviour, Count, Active:OOS, factor_key = TRUE)

#__Plot per species__
df_plots %>% mutate(Enclosure_status = factor(Enclosure_status, ordered = TRUE, levels = c("Open", "Closed"))) %>%
  ggplot(aes(x=Enclosure_status, y=Count, fill=Behaviour)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("grey35", "grey58", "black")) + 
  facet_wrap( ~ Species) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey")) +
  labs(y = "Overall proportions", x = "Enclosure status")



###___ FULL MODELS BUILDING ____________________________________________________

#__Build response variables for proportion models__
df$a <- cbind(df$Nb_active, df$Total_ind-df$Nb_active)
df$o <- cbind(df$Nb_OOS, df$Total_ind-df$Nb_OOS)

#__Build full %Activity and %OOS models__
mA_full <- glmer(a ~ Enclosure_status + Month + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mO_full <- glmer(o ~ Enclosure_status + Month + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)

#__Check overdispersion__
summary(mA_full)
summary(mO_full)



###___ MODEL SELECTION _________________________________________________________

#__Log-likelihood Ratio Tests (REML estimation)__
mA_red <- glm(a ~ Enclosure_status + Month,
              family = binomial(link = "logit"), na.action = "na.fail", data=df)
mO_red <- glm(o ~ Enclosure_status + Month,
              family = binomial(link = "logit"), na.action = "na.fail", data=df)
anova(mA_full, mA_red)
anova(mO_full, mO_red)

#__Information-theoretic approach (using the AIC)__
mA_red1 <- glmer(a ~ Enclosure_status + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mA_red2 <- glmer(a ~ Month + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mA_red3 <- glmer(a ~ (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mO_red1 <- glmer(o ~ Enclosure_status + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mO_red2 <- glmer(o ~ Month + (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
mO_red3 <- glmer(o ~ (1 + Enclosure_status|Species),
                 family = binomial(link = "logit"), na.action = "na.fail", data=df)
AIC(mA_full, mA_red1, mA_red2, mA_red3)
AIC(mO_full, mO_red1, mO_red2, mO_red3)

#__Final models chosen: correlated random intercept and slope__
mA <- mA_red1
mO <- mO_red1



###___ FINAL MODELS ASSUMPTIONS, FIT & ADEQUACY CHECKS _________________________

#__Check overdispersion__
summary(mA)
summary(mO)

#__Check adequacy to models' assumptions__
ggplot(data.frame(x1=df$Enclosure_status,pearson=residuals(mA,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw() +
  labs(title = "Pearson residuals vs. enclosure status for %Active", y = "Pearson's residuals", x = "Enclosure status")
ggplot(data.frame(x1=df$Enclosure_status,pearson=residuals(mO,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw() +
  labs(title = "Pearson residuals vs. enclosure status for %OOS", y = "Pearson's residuals", x = "Enclosure status")

#__Check fit: R² sensus Nakagawa & Schielzeth (2013)__
r.squaredGLMM(mA)
r.squaredGLMM(mO)



###___ MODELS INTERPRETATION ___________________________________________________

summary(mA)
print(mA)
exp(confint(mA))
coef(mA)
ranef(mA)
dotplot(ranef(mA, condVar = TRUE))

summary(mO)
print(mO)
exp(confint(mO))
coef(mO)
ranef(mO)
dotplot(ranef(mO, condVar = TRUE), theme_classic())
