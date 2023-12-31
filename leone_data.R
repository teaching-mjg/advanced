library(tidyverse)
library(readxl)

dat1 = read_excel("1. Glucose plasma raw data and treatment group, lp.xlsx", sheet = "Raw")
dat2 = read_excel("Insulin and glucagon raw data and treatment group.xlsx", sheet="Raw")

names(dat1)[1]="participant"
names(dat2)[1]="participant"

names(dat1)[3]="time"
names(dat2)[4]="time"

names(dat1)[4]="glucose"
names(dat2)[5]="insulin"
names(dat2)[6]="glucagon"

dat1 = dat1 %>% select(-VTC)
dat2 = dat2 %>% select(-c(VTC, Week))

dat1 = dat1 %>%  mutate(glucose = str_remove(glucose, "\\*") %>% as.numeric() )

# MERGE 

both = full_join(dat1, dat2)

both = both %>% 
  mutate(Treatment=
           factor(Treatment, 
                  levels=c(573, 214), 
                  labels=c("sucrose", "sucrose + arabinose")))

both = both %>% select(participant, Treatment, time,
                       glucose, insulin, glucagon) 

dat = both %>% 
  pivot_longer(cols=c(glucose, insulin, glucagon),
               names_to="measure") 

dat = dat %>% 
  mutate(measure=factor(measure, 
                        levels=c("glucose", "insulin", "glucagon"),
                        labels=c("glucose", "insulin", "glucagon")
  )
  )



ggplot(dat, aes(y=value, x=time, color=Treatment, fill=Treatment, lty=Treatment)) + 
  facet_wrap(~measure, scales="free_y",nrow=3) +
  #geom_smooth() +
  stat_summary(position=position_dodge(width=5), geom='point', fun=mean, pch=21)+
  stat_summary(position=position_dodge(width=5), geom='errorbar', width=7, fun.data=mean_se, lty=1)+
  stat_summary(position=position_dodge(width=5), geom='line', fun=mean)+
  theme_bw() + 
  theme(#panel.grid = element_blank(), 
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0))+
  scale_x_continuous(breaks=unique(dat$time))+
  scale_color_manual(values=c("grey50", "lightgreen"))+
  scale_fill_manual(values=c("grey50", "lightgreen"))+
  labs(
    title="my experimentr",
    subtitle="this is the subtitlr",
    caption="This i the capotiojn", 
  )


ggsave("leone_plot.png", width=4, height=5)


