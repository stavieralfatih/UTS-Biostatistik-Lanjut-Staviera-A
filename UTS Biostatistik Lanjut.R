#Soal no. 3 : Analisis univariat utk pef & age,height,sex,asthma
cor.test(pef_w5rev_c_lj$age,pef_w5rev_c_lj$pef)
cor.test(pef_w5rev_c_lj$height,pef_w5rev_c_lj$pef)
t.test(pef_w5rev_c_lj$pef~pef_w5rev_c_lj$sex)
t.test(pef_w5rev_c_lj$pef~pef_w5rev_c_lj$Asthma)

#Soal no 5,6 : buat model regresi linear
mmod <- lm(pef~age+sex+height, data=pef_w5rev_c_lj)
mmod1 <- lm(pef~sex+height+Asthma, data=pef_w5rev_c_lj)
mmod2 <- lm(pef~age+sex+Asthma, data=pef_w5rev_c_lj)
mmod3 <- lm(pef~age+sex+height+Asthma, data=pef_w5rev_c_lj)
summary(mmod)
summary(mmod1)
summary(mmod2)
summary(mmod3)
AIC(mmod, mmod1, mmod2,mmod3)
