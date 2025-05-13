data(phillips,intdef,barium,fertil3,prminwge,fair,hseinv,
     package = "wooldridge")


# E 10.1
summary(lm(inf ~ unem, data = phillips))

# E 10.2
summary(lm(i3 ~ inf + def, data = intdef))

# E 10.3
summary(lm(lprepop ~ lmincov + lusgnp, data = prminwge))

# E 10.4
summary(lm(gfr ~ pe + ww2 + pill, data = fertil3))

# E 10.5
summary(lm(gfr ~ pe + lag(pe, 1) + lag(pe, 2) + ww2 + pill, data = fertil3))

# E 10.5
summary(lm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + 
             befile6 + affile6 + afdec6, data = barium))

# E 10.6
summary(lm(demvote ~ partyWH + incum + partyWH:gnews + partyWH:inf,
           data = fair))

# E 10.7
summary(lm(log(invpc) ~ log(price) + t, data = hseinv))

library(forecast)
# convert hseinv into a time series data frame

hseinv.ts <- as.ts(hseinv)

summary(tslm(linvpc ~ lprice + t, data = hseinv.ts))

# convert hseinv to an xts data frame (better)
hseinv.xts <- as.xts(hseinv,
                     order.by = as.Date(paste0(hseinv$year,"-01-01"))) %>%
  subset(select = -year)

summary(lm(linvpc ~ lprice + t, data = hseinv.xts))

# tslm from forecast has its own 'trend' function. Unfortunately, doesn't seem to 
# work well with xts. Works fine with ts
summary(tslm(linvpc ~ lprice + trend, data = hseinv.ts))

# E 10.8
# tslm with trend works here:
summary(tslm(gfr ~ pe + ww2 + pill + trend, data = as.ts(fertil3)))

# add trend squared. Works.
summary(tslm(gfr ~ pe + ww2 + pill + trend + I(trend^2), data = as.ts(fertil3)))

# E 10.9

summary(tslm(lprepop ~ lmincov + lusgnp + trend, data = as.ts(prminwge)))



