library(tidyverse)

# themes


theme_ng1 <- theme(aspect.ratio=1.0,panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border=element_blank(),
                   axis.line = element_line(size=1),
                   axis.line.x = element_line(color="black", size = 1),
                   axis.line.y = element_line(color="black", size = 1),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black"),
                   axis.title=element_text(color="black"),
                   axis.title.y=element_text(size = 10, family = "Helvetica"),
                   axis.title.x=element_text(size = 10, family = "Helvetica"),
                   axis.text.x=element_text(size = 8, family = "Helvetica"),
                   axis.text.y=element_text(size = 8, family = "Helvetica"),
                   legend.position="none",
                   legend.title=element_blank(),
                   plot.title = element_text(hjust=0))


# load data etc.

hist.data <- read.csv(file = 'coffee_database/consumption.csv')

hist(hist.data.subset$Coffee)

ggplot(data=hist.data.subset, aes(hist.data.subset$Coffee)) + 
  geom_histogram() + 
  theme_bw()

# convert 'Date' to date
hist.data$Date <- as.Date(hist.data$Date)

# coffee consumed per day over time
consumption.by.day <- hist.data %>% 
  group_by(Date) %>% 
  summarise(mean.coffee = mean(Coffee)) %>% 
  mutate(interceding.days = Date - lag(Date))


consumption.by.day$ndays <- sub(" .*", "", consumption.by.day$interceding.days)
consumption.by.day$coffees.per.day <- consumption.by.day$mean.coffee / as.numeric(consumption.by.day$ndays)

# plot it

Consumption.over.time <- ggplot(consumption.by.day, aes(x = Date, y = coffees.per.day)) +
  geom_point(alpha = 0.5, size = 1) + 
  labs(x = "date",
       y = "coffees per day") +
  geom_smooth(method = "loess", level=0.95, size = 1.5, se = F, colour = "darkgreen") +
  # ylim(0, 0.0925) +
  # scale_y_continuous(breaks=seq(0,0.08,0.02)) + 
  # scale_x_continuous(breaks=seq(0,180,45)) +
  # use 'theme_ng1_leg' to plot legend
  theme_ng1
Consumption.over.time

# coffee consumed per day over time
unique.members <- hist.data %>% 
  group_by(Date) %>% 
  summarise(n.member = n_distinct(ID)) %>% 
  mutate(interceding.days = Date - lag(Date))

# how many coffee drinkers
Drinkers.over.time <- ggplot(unique.members, aes(x = Date, y = n.member)) +
  geom_point(alpha = 0.5, size = 1) + 
  labs(x = "date",
       y = "# coffee drinkers") +
  geom_smooth(method = "loess", level=0.95, size = 1.5, se = F, colour = "darkgreen") +
  # ylim(0, 0.0925) +
  # scale_y_continuous(breaks=seq(0,0.08,0.02)) + 
  # scale_x_continuous(breaks=seq(0,180,45)) +
  # use 'theme_ng1_leg' to plot legend
  theme_ng1
Drinkers.over.time

# drinks per person
Current.Period <- hist.data %>% 
  filter(Date == "2018-11-11")

drinks.per.person <- ggplot(Current.Period, aes(x = Coffee)) +
  geom_histogram(alpha=0.4, lwd=0.8, adjust=5) +
  # geom_histogram(aes(y=..density..), alpha=0.6,
  # position="identity", lwd=0.2, binwidth = 0.04) +
  # geom_vline(xintercept = median(FigS9.PlotData.Raw$x), col='green1', lwd=1, linetype = "dashed") +
  # scale_color_manual(values=c("lightseagreen", "orangered2")) +
  # scale_fill_manual(values=c("darkgreen", "green1"),
                    # labels=c("after burn-in", "new mutation")) +
  labs(x = "number of coffees",
       y = "count") +
  # ggtitle('C') +
  # theme(legend.position = c(0.5, 0.9),
        # legend.background = element_rect(colour = "black", linetype = "solid", size = 0.5)) + 
  theme_ng1
drinks.per.person



Figs <- plot_grid(Consumption.over.time, Drinkers.over.time, drinks.per.person, ncol = 3, height = 5, width = 10)


