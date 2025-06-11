# Load the libraries
library(ISLR2)
library(survival)
library(rpart)
library(rpart.plot)
library(writexl)
library(openxlsx)
write.xlsx(BrainCancer, file = "BrainCancer.xlsx")
cat("File BrainCancer.xlsx telah disimpan di:", getwd(), "\n")

write_xlsx(BrainCancer, "BrainCancer.xlsx")

data("BrainCancer")
attach(BrainCancer)

fit.surv <- survfit(Surv(time, status) ~ 1)
plot(fit.surv, xlab = 'Bulan',
     ylab = 'Estimasi Peluang Survival')

fit.sex <- survfit(Surv(time, status) ~ sex)
plot(fit.sex, xlab = 'Bulan',
     ylab = 'Estimasi Peluang Survival Kovariat Gender', col = c(2,4))
legend("bottom", legend = c("Wanita", "Laki-laki"),
       col = c(2, 4), lty = 1, bty = "n")

logrank.test <- survdiff(Surv(time, status) ~ sex)
logrank.test

#Mau cek antara metode terapi ada perbedaan survival apa engga
fit.sex1 <- survfit(Surv(time, status) ~ stereo)
plot(fit.sex1, xlab = 'Bulan',
     ylab = 'Estimasi Peluang Survival Kovariat Metode Terapi', col = c(2,4))
legend("bottom", legend = c("SRS", "SRT"),
       col = c(2, 4), lty = 1, bty = "n")
table(stereo)

#Cek signifikansinya yang metde  terapi
logrank.test1 <- survdiff(Surv(time, status) ~ stereo)
logrank.test1

#Mau cek kurva survival antara letak tumor
fit.sex2 <- survfit(Surv(time, status) ~ loc)
plot(fit.sex2, xlab = 'Bulan',
     ylab = 'Estimasi Peluang Survival Kovariat Lokasi Tumor', col = c(2,4))
fit.sex2
table(loc)
legend("bottom", legend = c("Infratentorial", "Supratentorial"),
       col = c(2, 4), lty = 1, bty = "n")

#Cek signifikansi antar letak tumor
logrank.test2 <- survdiff(Surv(time, status) ~ ki)
logrank.test2

logrank.test3 <- survdiff(Surv(time, status) ~ gtv)
logrank.test3

table(diagnosis)


#CPH

fit.cox <- coxph(Surv(time, status) ~ sex)
fit.cox
summary(fit.cox)
summary(fit.cox)$logtest
summary(fit.cox)$waldtest
summary(fit.cox)$sctest

fit.all <- coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo)
fit.all
cox.zph(fit.all)
summary(fit.all)
summary(fit.all)$coefficients
AIC(fit.all)

fit.all2 <- coxph(Surv(time, status) ~ sex * diagnosis + loc + ki + gtv + stereo)
fit.all2
cox.zph(fit.all2)
summary(fit.all2)
summary(fit.all2)$coefficients
AIC(fit.all2)

fit.allbest <- coxph(Surv(time, status) ~ sex * diagnosis + ki + gtv)
fit.allbest
cox.zph(fit.allbest)
summary(fit.allbest)
summary(fit.allbest)$coefficients
AIC(fit.allbest)

fit.all3 <- coxph(Surv(time, status) ~ sex + diagnosis * loc + ki + gtv + stereo)
fit.all3

fit.all4 <- coxph(Surv(time, status) ~ sex + diagnosis * stereo + loc + ki + gtv)
fit.all4

fit.all5 <- coxph(Surv(time, status) ~ sex * gtv + diagnosis + loc + ki + stereo)
fit.all5

fit.all3 <- coxph(Surv(time, status) ~ sex * loc + diagnosis + ki + gtv + stereo)
fit.all3

ph_test <- cox.zph(fit.all)
ph_test
plot(ph_test)


  modaldata <- data.frame(
  diagnosis = levels(diagnosis),
  sex = rep("Female", 4),
  loc = rep("Supratentorial", 4),
  ki = rep(mean(ki), 4),
  gtv = rep(mean(gtv), 4),
  stereo = rep("SRT", 4))
survplots <- survfit(fit.all, newdata = modaldata)
plot(survplots , xlab = "Bulan",
     ylab = "Estimasi Peluang Survival Kovariat Diagnosis", col = 2:5)
legend("bottomleft", levels(diagnosis), col = 2:5, lty = 1)
