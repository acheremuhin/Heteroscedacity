library(tidyverse)
library(ggplot2)
library(skedastic)
library(AER)
library(gvlma)
library(UsingR)
#### Простой расчет ####
data(diamond)
ggplot(data = diamond, aes(x=carat, y=price)) + geom_point()
model_1 <- lm(price~carat, data=diamond)
summary(model_1)
gvlma(model_1)
ggplot(data = diamond, aes(x=carat, y=model_1$residuals)) + geom_point() + ylab("Error of model")
# Тесты на гетероскедастичность
anscombe(model_1, studentise = TRUE, statonly = FALSE)
bamset(model_1, deflator = NA, k = 3)
bickel(model_1, fitmethod = "lm",a = "identity", b = "hubersq",
  scale_invariant = TRUE, k = 1.345, statonly = FALSE)
breusch_pagan(model_1, auxdesign = NA, koenker = TRUE, statonly = FALSE)
carapeto_holt(model_1,  deflator = NA,  prop_central = 1/3,  group1prop = 1/2,
  qfmethod = "imhof",  alternative ="two.sided", twosidedmethod = "kulinskaya",
  statonly = FALSE)
cook_weisberg(model_1, auxdesign = NA, hetfun = "mult",statonly = FALSE)
diblasi_bowman(model_1,  distmethod = "bootstrap", H = 0.08,  ignorecov = TRUE,
  B = 500L, seed = 1234,  statonly = FALSE)
dufour_etal(model_1, hettest = "breusch_pagan")
evans_king(model_1,  method = "GLS", deflator = NA, lambda_star = 5,
  qfmethod = "imhof", statonly = FALSE)
glejser(model_1, auxdesign = NA, sigmaest = "auxiliary", statonly = FALSE)
godfrey_orme(model_1, hettest = "breusch_pagan")
goldfeld_quandt(model_1, method = "parametric", deflator = NA,
  prop_central = 1/3, group1prop = 1/2, alternative = "two.sided",
  prob = NA,   twosidedmethod = "doubled", restype = "ols",
  statonly = FALSE)
harrison_mccabe(model_1, deflator = NA, m = 0.5, alternative = "two.sided",
  twosidedmethod = "kulinskaya",   qfmethod = "imhof",  statonly = FALSE)
harvey(model_1, auxdesign = NA, statonly = FALSE)
honda(model_1,  deflator = NA, alternative = "two.sided",
  twosidedmethod = "doubled", qfmethod = "imhof", statonly = FALSE)
horn(model_1, deflator = NA,  restype = "ols", alternative = "two.sided",
  statonly = FALSE)
li_yao(model_1, method = "alrt", baipanyin = TRUE, statonly = FALSE)
rackauskas_zuokas(model_1, alpha = 0, pvalmethod = "data",  R = 2^14,
  m = 2^17, sqZ = FALSE, seed = 1234, statonly = FALSE)
simonoff_tsai(model_1, auxdesign = NA, method = "score",
  hetfun = "add", basetest = "cook_weisberg",
  bartlett = TRUE,   optmethod = "Nelder-Mead",   statonly = FALSE)
szroeter(model_1, deflator = NA, qfmethod = "imhof", statonly = FALSE)
verbyla(model_1, auxdesign = NA, statonly = FALSE)
white_lm(model_1, interactions = FALSE, statonly = FALSE)
wilcox_keselman(model_1, gammapar = 0.2, B = 500L,p.adjust.method = "none",
  seed = NA, rqwarn = FALSE, matchWRS = FALSE, statonly = FALSE)
yuce(model_1, method = "A", statonly = FALSE)
zhou_etal(model_1, auxdesign = NA, method = "pooled", Bperturbed = 500L,
  seed = 1234, statonly = FALSE)

#### Интересности ####
# Форма модели
x <- runif(1000, min=-10, max=10)
s <- 10^seq(-2,0,length=10)
Res_mod <- matrix(nrow=100, ncol=11)
for(i in 1:100) {
print(i)
a<-runif(1, min=10, max=30)/10
b<-runif(1, min=2, max=10)
y1y <- a*x+b+rnorm(1000,mean=0, sd=5)
y1n1 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[1]*abs(x))
y1n2 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[2]*abs(x))
y1n3 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[3]*abs(x))
y1n4 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[4]*abs(x))
y1n5 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[5]*abs(x))
y1n6 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[6]*abs(x))
y1n7 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[7]*abs(x))
y1n8 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[8]*abs(x))
y1n9 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[9]*abs(x))
y1n10 <- a*x+b+rnorm(1000,mean=0, sd=5)*(1+s[10]*abs(x))
Base <- as.data.frame(cbind(x,y1y,y1n1,y1n2,y1n3,y1n4,y1n5,
                                  y1n6,y1n7,y1n8,y1n9,y1n10))
lm0 <- lm(y1y~x,data = Base)
lm1 <- lm(y1n1~x,data = Base)
lm2 <- lm(y1n2~x,data = Base)
lm3 <- lm(y1n3~x,data = Base)
lm4 <- lm(y1n4~x,data = Base)
lm5 <- lm(y1n5~x,data = Base)
lm6 <- lm(y1n6~x,data = Base)
lm7 <- lm(y1n7~x,data = Base)
lm8 <- lm(y1n8~x,data = Base)
lm9 <- lm(y1n9~x,data = Base)
lm10 <- lm(y1n10~x,data = Base)
r1 <- zhou_etal(lm0, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r2 <- zhou_etal(lm1, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r3 <- zhou_etal(lm2, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r4 <- zhou_etal(lm3, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r5 <- zhou_etal(lm4, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r6 <- zhou_etal(lm5, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r7 <- zhou_etal(lm6, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r8 <- zhou_etal(lm7, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r9 <- zhou_etal(lm8, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                seed = 1234, statonly = FALSE)
r10 <- zhou_etal(lm9, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                 seed = 1234, statonly = FALSE)
r11 <- zhou_etal(lm10, auxdesign = NA, method = "pooled", Bperturbed = 5L,
                 seed = 1234, statonly = FALSE)
res <- unlist(unname(c(r1[2],r2[2],r3[2],r4[2],r5[2],
         r6[2],r7[2],r8[2],r9[2],r10[2],r11[2])))
Res_mod[i,] <- res
}
res_i <- c(mean(Res_mod[,1]),mean(Res_mod[,2]),mean(Res_mod[,3]),mean(Res_mod[,4]),
           mean(Res_mod[,5]),mean(Res_mod[,6]),mean(Res_mod[,7]),mean(Res_mod[,8]),
           mean(Res_mod[,9]),mean(Res_mod[,10]),mean(Res_mod[,11]))
s1 <- unlist(c(0,s))
Res <- as.data.frame(cbind(res_i,s1))
ggplot(data = Res, aes(x=s1,y=res_i))+geom_point()+geom_line()+labs(x="Выраженность гетероскедастичности", y = "р-значение теста",
                                                                    title = "Эффективность теста Чжоу", subtitle = "Нулевая гипотеза: Гетероскедастичности нет")
