
library(dplyr)
library(forecast)

data(phillips, intdef, barium, fertil3, prminwge, fair, hseinv,
     intdef, ezanders, volat, traffic2, 
     package = "wooldridge")

# C10.1



intdef <- intdef %>% mutate(
  year = as.Date(paste0(year, "-01-01")),
  post79 = if_else(year < "1980-01-01",0,1)
)

summary(tslm(i3 ~ inf + def + post79, data = as.ts(intdef)))

# C 10.3
summary(tslm(lprepop ~ lmincov + lusgnp + lprgnp + trend, 
             data = as.ts(prminwge)))

# Alternative way by creating trend
prminwge$.t <- 1:nrow(prminwge)

summary(lm(lprepop ~ lmincov + lusgnp + lprgnp + .t, data = prminwge))


# C 10.5

# (i)
ezanders2 <- ezanders %>% 
  select(uclms,jan:dec) %>%
  mutate(luclms = log(uclms)) %>% 
  select(-uclms) %>%
  mutate(across(!luclms, as.factor))


summary(lm(luclms ~., data = ezanders2))

ezanders2.ts <- as.ts(ezanders2)

summary(tslm(luclms ~ trend, data = as.ts(ezanders2)))


# E 10.7
data(consump, package = "wooldridge")

# (i)
summary(lm(gc ~ gy, data = consump))
# (ii)
summary(lm(gc ~ gy + gy_1, data = consump))
# (iii)
summary(lm(gc ~ gy + r3, data = consump))

# E 10.9


summary(lm(rsp500 ~ pcip + i3, data = volat))


# E 10.11

traffic3 <- select(traffic2, ltotacc,t,feb:dec)

# (ii)

summary(lm(ltotacc ~., data = traffic3))




