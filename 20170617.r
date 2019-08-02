WORK_DIR = "D:/work/nhanes"
setwd(WORK_DIR)

df <- read.csv("thedata.csv")

df$egfr_index[df$egfr < 60 ] <- 1
df$egfr_index[df$egfr >= 60 & df$egfr <90 ] <- 2
df$egfr_index[df$egfr >= 90 ] <- 3

df$egfr_index <- factor(df$egfr_index, levels=c(1,2,3), labels=c("Moderately", "Mildly", "Normal"))

table(df$egfr_index)

formula <- (egfr_index~smq020 + smq040 + pb + cd + hg + se + mn 
            + income + hba1c + energy2 + energy1 + insulin + hypoglu 
            + gender + age + race + education + pregnancy + weightqu 
            + weightex + sdmvpsu + sdmvstra + bpsy1 + bpdi1 + bpsy2 
            + bpdi2 + bpsy3 + bpdi3 + bpq050a + bmi + alb + alt 
            + ast + bun + scr + ua + chl + tri + alq101 + alq110 
            + alq120q + acr + cot + pregnant)


fit.logit <- glm(formula, data=df, family=quasibinomial())

summary(fit.logit)


se_offset <- quantile(df$se, prob=2/3) 
summary(se_offset)

df.se_low <-  subset(df, se <= se_offset)
df.se_high <- subset(df, se > se_offset)

fit.logit2 <- glm(formula, data=df.se_low, family=quasibinomial())
summary(fit.logit2)


fit.logit3 <- glm(formula, data=df.se_high, family=quasibinomial())
summary(fit.logit3)
