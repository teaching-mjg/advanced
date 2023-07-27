

ggplot(dat, aes(y=value, x=time, color=Treatment, fill=Treatment, lty=Treatment)) + 
  facet_wrap(~measure, scales="free_y",nrow=3) +
  #geom_smooth() +
  stat_summary(position=position_dodge(width=5), geom='point', fun=mean, pch=21, size=3)+
  stat_summary(position=position_dodge(width=5), geom='errorbar', width=7, fun.data=mean_se)+
  stat_summary(position=position_dodge(width=5), geom='line', fun=mean, lwd=1)+
  theme_bw() + theme(panel.grid = element_blank())+
  scale_x_continuous(breaks=unique(dat$time))+
  scale_color_manual(values=c("grey50", "lightgreen"))+
  scale_fill_manual(values=c("grey50", "lightgreen"))


ggplot(dat, aes(y=value, x=time, color=Treatment))+
  facet_wrap(~measure, nrow=3, scales="free_y") +
  stat_summary(fun.data=mean_se) +
  stat_summary(geom='line')