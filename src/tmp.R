# Code for exploring the data.

library(dplyr)
library(ggplot2)
fhs.long <- read.csv("data/frmgham2.csv")

# Creating a data set for scatterplots.
fhs.long.grps <- group_by(fhs.long, RANDID)
fhs.tmp <- mutate(fhs.long.grps, 
  sex=SEX,
  max.totchol=max(TOTCHOL),
  mean.totchol=mean(TOTCHOL),
  entry.age=min(AGE),
  max.sysbp=max(SYSBP),
  mean.sysbp=mean(SYSBP),
  max.diabp=max(DIABP),
  mean.diabp=mean(DIABP),
  max.cigpday=max(CIGPDAY),
  max.bmi=max(BMI),
  mean.bmi=mean(BMI),
  max.hr=max(HEARTRTE),
  mean.hr=mean(HEARTRTE),
  stroke=max(STROKE),
  chd=max(ANYCHD),
  cvd=max(CVD))
fhs.tmp <- select(fhs.tmp, sex:cvd)

# Same dataset, but using the daisy chain approach:
fhs.tmp <- group_by(fhs.long, RANDID) %>%  
mutate(sex=SEX,
  max.totchol=max(TOTCHOL),
  mean.totchol=mean(TOTCHOL),
  max.hdlc=max(HDLC, na.rm=TRUE),
  mean.hdlc=mean(HDLC, na.rm=TRUE),
  max.ldlc=max(LDLC, na.rm=TRUE),
  mean.ldlc=mean(LDLC, na.rm=TRUE),
  entry.age=min(AGE),
  max.sysbp=max(SYSBP),
  mean.sysbp=mean(SYSBP),
  max.diabp=max(DIABP),
  mean.diabp=mean(DIABP),
  max.cigpday=max(CIGPDAY),
  max.bmi=max(BMI),
  mean.bmi=mean(BMI),
  max.hr=max(HEARTRTE),
  mean.hr=mean(HEARTRTE),
  death=max(DEATH),
  angina=max(ANGINA),
  hosp.mi=max(HOSPMI),
  mi.fchd=max(MI_FCHD),
  hyperten=max(HYPERTEN),
  stroke=max(STROKE),
  chd=max(ANYCHD),
  cvd=max(CVD)) %>% select(sex:cvd)

out.death <- group_by(fhs.tmp, death) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.angina <- group_by(fhs.tmp, angina) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.hosp.mi <- group_by(fhs.tmp, hosp.mi) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.mi.fchd <- group_by(fhs.tmp, mi.fchd) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))
  
out.hyperten <- group_by(fhs.tmp, hyperten) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.stroke <- group_by(fhs.tmp, stroke) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.chd <- group_by(fhs.tmp, chd) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

out.cvd <- group_by(fhs.tmp, cvd) %>% 
  select(max.totchol:mean.hr) %>%
  summarise(
  mean.max.totchol=mean(max.totchol, na.rm=TRUE),
  mean.mean.totchol=mean(mean.totchol, na.rm=TRUE),
  mean.max.hdlc=mean(max.hdlc, na.rm=TRUE),
  mean.mean.hdlc=mean(mean.hdlc, na.rm=TRUE),
  mean.max.ldlc=mean(max.ldlc, na.rm=TRUE),
  mean.mean.ldlc=mean(mean.ldlc, na.rm=TRUE),
  mean.max.sysbp=mean(max.sysbp, na.rm=TRUE),
  mean.mean.sysbp=mean(mean.sysbp, na.rm=TRUE),
  mean.max.diabp=mean(max.diabp, na.rm=TRUE),
  mean.mean.diabp=mean(mean.diabp, na.rm=TRUE),
  mean.max.cigpday=mean(max.cigpday, na.rm=TRUE),
  mean.max.bmi=mean(max.bmi, na.rm=TRUE),
  mean.mean.bmi=mean(mean.bmi, na.rm=TRUE),
  mean.max.hr=mean(max.hr, na.rm=TRUE),
  mean.mean.hr=mean(mean.hr, na.rm=TRUE))

glimpse(out.death)
glimpse(out.angina)
glimpse(out.hosp.mi)
glimpse(out.mi.fchd)
glimpse(out.hyperten)
glimpse(out.stroke)
glimpse(out.chd)
glimpse(out.cvd)


p1 <- ggplot(fhs.tmp, aes(x=max.sysbp, y=max.bmi, col=as.factor(stroke))) + 
  geom_point(alpha=0.4, cex=3) # not good

p1
            
# Consider only the information from the first check-up. Remember that check-ups
# are 6 years apart, and there are typically 3. Hence the typical subject ages
# 12 years in this data set.
# LDLC is the bad cholesterol, but it is only available for period 3.

fhs.only1 <- filter(fhs.long, PERIOD==1)
fhs.only1 <- transform(fhs.only1, age.cat=cut(AGE, breaks=c(30,44,54,64,74,84)))

# Looks like if you are female in (55,64] and have high SYSBP, you are more
# likely to have a stroke.
p1 <- ggplot(fhs.only1, aes(x=factor(SEX), fill=factor(STROKE)))
p1 + geom_boxplot(aes(y=SYSBP)) + facet_wrap( ~ age.cat)

## KEEP THIS:
## Hypertension has high association with stroke
## Age group 55-64 is most at risk
p2 <- ggplot(fhs.only1, aes(x=factor(HYPERTEN), fill=factor(STROKE)))
## p2 + geom_bar(position="dodge") + facet_wrap( ~ age.cat)
p2 + geom_bar(position="fill") + facet_grid(SEX ~ age.cat)
p2 + geom_bar(position="dodge") + facet_grid(SEX ~ age.cat)
###

p2 <- ggplot(fhs.only1, aes(x=factor(HYPERTEN), fill=factor(CVD)))
p2 + geom_bar(position="fill") + facet_grid(SEX ~ age.cat)

p3 <- ggplot(fhs.only1, aes(x=TOTCHOL, y=BMI, col=factor(HOSPMI)))
p3 + geom_point() + facet_grid(SEX ~ age.cat)

fhs.only3 <- filter(fhs.long, PERIOD==3)
fhs.only3 <- transform(fhs.only3, age.cat=cut(AGE, breaks=c(30,44,54,64,74,84)))

p4 <- ggplot(fhs.only3, aes(x=LDLC, y=BMI, col=factor(CVD)))
p4 + geom_point() + facet_grid(SEX ~ age.cat)

