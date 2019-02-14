# Initialize --------------------------------------------------------------
library(ggplot2)
library(data.table)

getRuns <- function(Ups, resolution, iterations){
  lower_curve <- upper_curve <- c()
  for (i in 1:iterations){
    print(i)
    lower_curve <- rbind(lower_curve, fread(paste('curveData_',Ups,'_0.95_subsampleT_',resolution,'_run_',i,'.csv',sep=''), he=T))
    upper_curve <- rbind(upper_curve, fread(paste('curveData_',Ups,'_0.95_subsampleF_',resolution,'_run_',i,'.csv',sep=''), he=T))
  }
  lower_curve$bound <- 'L'
  upper_curve$bound <- 'U'
  curveData <- rbind(lower_curve,upper_curve)
  return(curveData)
}


# Get data ----------------------------------------------------------------
setwd('/home/shai/Documents/Shazia/Results')
d <- NULL
for (r in c(70,80,90,96)){
  for (u in (c('A','BC','ABC'))){
    x <- getRuns(u,r,25)
    d <- rbind(d, x)
  }
}


# Plot --------------------------------------------------------------------
d$Ups <- factor(d$Ups, levels = c('A','BC','ABC'))
EIR = 25

d %>% filter(resolution == 96) %>%
  ggplot(aes(bites/EIR, propVars, color = subsample)) +
  geom_smooth(se = F, method = "gam", formula = y ~ s(log(x + 1))) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c('#b2182b','#2166ac'), name = "Co-transmission", breaks = c("FALSE", "TRUE"), labels = c("Yes", "No")) +
  labs(x = 'Age (years)', y = '% of immunity to DBLα types') +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 25)) +
  scale_y_continuous(limits = c(0,0.95), breaks = scales::pretty_breaks(n = 10), labels = scales::percent) +
  facet_grid(~Ups, scales = 'free_x') +
  background_grid(major = "xy", minor = "none")

