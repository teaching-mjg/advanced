library("tidyverse")

dat = read_csv("Acute postprandial responses graph data, lp.csv")


dat = dat %>%  filter(!is.na(Time))

dat = dat %>% mutate(`Avg. glucose - Suc`= as.numeric(`Avg. glucose - Suc`))

dat = dat %>% mutate(pid = seq_along(dat$Time)) %>% relocate(pid)

names(dat)

dat_longer = dat %>% pivot_longer(
  cols=3:14,
  names_to=c("which_stat", "which_sugar", "dash", "which", "add", "more"),
  names_sep=" ",
  values_to="blood"
)

dat_longer <- dat_longer %>% 
  mutate(which_stat = str_remove(which_stat, "\\."))

dat_longer <- dat_longer %>% 
  mutate(which_sugar = str_to_sentence(which_sugar))
  
dat_longer = dat_longer %>% 
  mutate(treatment = ifelse(
    is.na(add), which, paste(which, add, more, sep="_"))
  ) %>% 
  mutate(treatment = as_factor(treatment))

dat_longer = dat_longer %>% 
  select(-c(which, dash, add, more))

dat_longer = dat_longer %>% 
  mutate(which_stat=as_factor(which_stat),
         which_sugar=as_factor(which_sugar))
  
dat_longer = dat_longer %>% 
  pivot_wider(names_from=which_stat, values_from = blood)

dat_longer <- dat_longer %>% mutate(upper = Avg+(SD*0.1), lower=Avg-(SD*.1))
