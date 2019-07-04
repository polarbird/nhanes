WORK_DIR = "~/nhanes"
setwd(WORK_DIR)

source <- read.csv("thedata.csv")

df<-source[source$pregnancy!=1, ]

df$egfr_index[df$egfr < 60 ] <- 1
df$egfr_index[df$egfr >= 60 & df$egfr <90 ] <- 2
df$egfr_index[df$egfr >= 90 ] <- 3

df$egfr_index <- factor(df$egfr_index, levels=c(1,2,3),
                        labels=c("Moderately", "Mildly", "Normal"))
table(df$egfr_index)

df$educational_attainment[df$education <= 2 ] <- 1
df$educational_attainment[df$education == 3 ] <- 2
df$educational_attainment[df$education >= 4 ] <- 3

df$educational_attainment <- factor(df$educational_attainment, levels=c(1,2,3),
                        labels=c("Less than high school", "High school", "Some college or above"))
table(df$educational_attainment)

df$poverty_income_ratio <- factor(df$income, levels=c(1,2,3),
                                    labels=c("Low", "Middle", "High"))
table(df$poverty_income_ratio)

df$cigarette_smoking_status[df$smq020 == 2] <- 1
df$cigarette_smoking_status[df$smq020 == 1 & df$smq040 == 3] <- 2
df$cigarette_smoking_status[df$smq020 == 1 & (df$smq040 == 1 | df$smq040 == 2)] <- 3
df$cigarette_smoking_status <- factor(df$cigarette_smoking_status, levels=c(1,2,3),
                                  labels=c("Never smoker", "Former smoker", "Current smoker"))

table(df$cigarette_smoking_status)

df$alcohol_consumption_status[df$alq110 == 2] <- 1
df$alcohol_consumption_status[df$alq110 == 1 & df$alq120q == 0] <- 2
df$alcohol_consumption_status[df$alq110 == 1 & df$alq120q > 0] <- 3
df$alcohol_consumption_status <- factor(df$alcohol_consumption_status, levels=c(1,2,3),
                                      labels=c("Never drinker", "Former drinker", "Current drinker"))

table(df$alcohol_consumption_status)

df$dietary_energy_intake <- (df$energy1 + df$energy2) * 0.5

df$weight_category[df$bmi < 18.5] <- 1
df$weight_category[df$bmi >= 18.5 & df$bmi < 25] <- 2
df$weight_category[df$bmi >= 25 & df$bmi < 30] <- 3
df$weight_category[df$bmi >= 30] <- 4
df$weight_category <- factor(df$weight_category, levels=c(1,2,3,4),
                                        labels=c("Underweight", "Healthy weight", "Overweight", "Obese"))

table(df$weight_category)

df$bpsy <- (df$bpsy1 + df$bpsy2 + df$bpsy3) / 3
df$bpdi <- (df$bpdi1 + df$bpdi2 + df$bpdi3) / 3

table(df$bpsy)
table(df$bpdi)


df$blood_pressure[df$bpsy <= 120 & df$bpdi <= 80] <- 1
df$blood_pressure[(df$bpsy > 120 & df$bpsy < 140) & (df$bpdi > 80 & df$bpdi < 90)] <- 2
df$blood_pressure[df$bpsy >= 140 | df$bpdi >= 90 ] <- 3
df$blood_pressure <- factor(df$blood_pressure, levels=c(1,2,3),
                             labels=c("Normotension", "Prehypertension", "Hypertension"))

table(df$blood_pressure)

df$diebetes[df$hba1c >= 6.5 | df$insulin == 1 | df$hypoglu == 1] <- 1
df$diebetes[df$hba1c < 6.5 & df$insulin != 1 & df$hypoglu != 1] <- 2
df$diebetes <- factor(df$diebetes, levels=c(1,2),
                             labels=c("Yes", "No"))

table(df$diebetes)

library(MASS)

logit0 = polr(egfr_index ~ 1, data=df, method="logistic",Hess=T)

summary(logit0)

formula <- (egfr_index ~ pb + cd + hg + se + mn
            + poverty_income_ratio + diebetes + dietary_energy_intake
            + gender + age + race + educational_attainment + blood_pressure 
            + bmi + alb + alt + ast + bun + scr + ua + chl + tri 
            + alcohol_consumption_status + acr + cot)

formula1 <- (egfr_index ~ pb + cd + hg + se + mn)

logit1 = polr(formula, data=df, method="logistic",Hess=T)

summary(logit1)

ctable <- coef(summary(logit1))
p<- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

summary(ctable)


anova(logit0,logit1)


fit.logit <- glm(formula1, data=df, family=quasibinomial())
summary(fit.logit)

library(VGAM)

formula1 <- (egfr_index ~ pb + cd + hg + se + mn
            + poverty_income_ratio + diebetes + dietary_energy_intake
            + gender + age + race + educational_attainment + blood_pressure 
            + bmi + alb + alt + ast + bun + scr + ua + chl + tri 
            + alcohol_consumption_status + acr + cot)

logit3 <-vglm(formula1, family = propodds, data = df)

sumvglm <-summary(logit3)
coefvglm <-coef(sumvglm)

zCrit <- qnorm(c(0.05/2,  1-0.05/2))
CI <- t(apply(sumvglm, 1, function(x) x["Estimate"] - zCrit * x["Std. Error"]))
names(CI) <- c("Lower", "Upper")
p <- 2*(1 - pnorm(abs(coefvglm[, "z value"])))

(sumvglm <- cbind(sumvglm, CI, "p value" = p))


