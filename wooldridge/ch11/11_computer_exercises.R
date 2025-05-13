
library(dplyr)

data(hseinv,earns,nyse,phillips,inven,
     consump,traffic2,package = "wooldridge")

# C11.1

# Use hseinv

with(hseinv[-1,],cor(linvpc,linvpc_1)) #(i) (a) FO autocorr in log(invpc)

# linearly detrend linvpc. These residuals do NOT have a time trend
res1 <- as.numeric((lm(linvpc ~ t, data = hseinv))$residuals)  

# Now find the autocorrelation
cor(res1[-1],hseinv$linvpc_1[-1])

# for log(price)

with(hseinv[-1,],cor(lprice,lprice_1)) 
res2 <- lm(lprice ~ t, data = hseinv)$residuals

lm(res2 ~ hseinv$lprice_1)

cor(res2[-1], hseinv$lprice_1[-1])

# (ii) estimate linvpc ~ lprice + t
# create first a mutated version of hseinv 

hseinv_mut <- mutate(hseinv, 
                     diff_lprice = c(NA, diff(lprice)),
                     diff_linvpc = c(NA, diff(linvpc)))

summary(lm(linvpc ~ diff_lprice + t, data = hseinv_mut))

# (iii)  Linearly detrend linvpc and use detrended version as dependent variable
## in the regression on (ii)

hseinv_mut <- mutate(hseinv_mut, 
                     res_linvpc = residuals(lm(linvpc ~ t, data = hseinv_mut)))

summary(lm(res_linvpc ~ diff_lprice + t, data = hseinv_mut))

# (iv)
broom::tidy(lm(diff_linvpc ~ diff_lprice + t, data = hseinv_mut))


# C11.3

nyse_mod.ur <- lm(return ~ return_1 + I((return_1)^2), data = nyse)

summary(nyse_mod.ur)

# test NH that E(return|return_1) does not depend on return_1
# i.e, NH is the EMH, AH is AR(1)
# H0: B1=0 & B2=0; H1 = B1=0 AND B2=0
nyse_mod.r <- lm(return ~ 1, data = nyse)

q <- 2; n <- nrow(nyse); k <- ncol(nyse) + 1
rss_r <- sum((residuals(nyse_mod.r))^2)
rss_ur <- sum((residuals(nyse_mod.ur))^2)
num <- (rss_r - rss_ur)/q
denom <- (rss_ur)/(n-k-1)

(F_val <- num/denom)

# Test the EMH

# AH: AR(1): y = B0 + B1.y(t-1) + u(t)
# NH: B1 = 0

summary(lm(return ~ return_1 + return_1:lag(return_1), data = nyse))


# C11.5

fertil3_mut <- fertil3 %>% mutate(
  d_gfr = c(NA,diff(gfr)),
  d_pe = c(NA,diff(pe)), 
  d_pe1 = c(NA,diff(pe_1)), 
  d_pe2 = c(NA,diff(pe_1))
)

#(i)

summary(lm(diff(gfr) ~ diff(pe) + diff(pe_1) + diff(pe_2) + t[-1], data = fertil3))

summary(lm(diff(gfr) ~ diff(pe) + diff(pe_1) + diff(pe_2) +
             ww2[-1] + pill[-1], data = fertil3)) 



# LRP

fertil3_mut <- mutate(fertil3_mut, D_pe1 = d_pe1 - d_pe, D_pe2 = d_pe2 - d_pe)

summary(lm(gfr ~ pe + D_pe1 + D_pe2 + ww2 + pill, data = fertil3_mut))

summary(lm(diff(gfr) ~ diff(pe) + diff(pe_1 - pe) + diff(pe_2 - pe) + 
           ww2[-1] + pill[-1], data = fertil_mut))


data(fertil3, package = "wooldridge")

# mutate the dataset 
fertil3.mut <- mutate(fertil3, 
                      d_pe1 = pe_1 - pe,
                      d_pe2 = pe_2 - pe,
                      
                      D_gfr = c(NA, diff(gfr)),
                      D_pe = c(NA, diff(pe)), 
                      D_pe1 = c(NA, (diff(pe_1) - diff(pe))),
                      D_pe2 = c(NA, (diff(pe_2) - diff(pe)))) 

summary(lm(gfr ~ pe + I(pe_1 - pe) + I(pe_2 - pe) + ww2 + pill, 
           data = fertil3.mut))

summary(lm(D_gfr ~ D_pe + D_pe1 + D_pe2 + ww2 + pill, 
           data = fertil3.mut))

# C 11.7

mod1 <- lm(gc ~ gc_1, data = consump)

# add i3_1
consump_mut <- mutate(consump, i3_1 = c(NA, diff(i3)))

rmod <- lm(gc ~ gc_1, data = consump_mut)
umod <- lm(gc ~ gc_1 + gy_1 + i3_1, data = consump_mut)

car::linearHypothesis(umod, c("gy_1=0","i3_1=0"))

Fvals <- function(Q=2,dat=consump_mut,rm,um){
  
  n <- nrow(dat);k <- ncol(dat)
  rss_r= sum((residuals(rm))^2)
  rss_ur= sum((residuals(um))^2)
  num <- (rss_r - rss_ur)/q
  denom <- (rss_ur)/(n-k)
  num/denom
  
}; Fvals(rm=rmod, um=umod)


anova_umod <- anova(umod); anova_rmod <- anova(rmod)

rss.r <- anova_rmod$`Sum Sq`[nrow(anova_rmod)]
rss.ur <- anova_umod$`Sum Sq`[nrow(anova_umod)]

num <- (rss.r - rss.ur)/q
denom <- (rss.ur)/(n-k-1)

f.val <- num/denom


# C11.9

with(traffic2[-1,], cor(prcfat, prcfat_1))

traffic2_mut <- mutate(traffic2, 
                       unem_1 = lag(unem), 
                       d_prcfat = c(NA,diff(prcfat)),
                       d_unem = c(NA, diff(unem)))
with(traffic2_mut[-1,], cor(unem, unem_1))


mnths <- names(select(traffic2, feb:dec))
indep_vars <- c("t", mnths, "wkends", "beltlaw","spdlaw", "d_unem")
pasted_indepvars <- paste(indep_vars, collapse = "+")

.formula <- as.formula(paste("d_prcfat ~", pasted_indepvars))

summary(lm(formula = .formula, data = traffic2_mut))


