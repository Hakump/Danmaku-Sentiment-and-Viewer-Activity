library(psych)
library(tidyverse)
library(GGally)
library(effectsize)
libaray(car)
source("610_functions.R")

d = read.csv("danmaku_netpos_count.csv") %>% 
  janitor::clean_names() %>% drop_na()
dim(d)

d = d[d$netpos_prop <= 1,]
d = d[d$netpos_prop >= -1,]


varPlot(d$netpos_prop)

pairs_name = c("netpos_prop","view","danmaku","reply","favorite","coin","share","like")
GGally::ggpairs(d[,pairs_name])

d$coin_ratio = d$coin / d$view
mod_1 = lm(coin_ratio ~ netpos_prop, d)
summary(mod_1)
eta_squared(Anova(mod_1, type=3))

plot(d$netpos_prop, d$coin_ratio)

d$reply_prop = d$reply / d$view
plot(d$netpos_prop, d$reply_prop)


m_graph <- lm(coin_ratio ~ netpos_prop, d)

d_graph <- expand.grid(
  coin_ratio = seq(min(d$coin_ratio, na.rm = T), 
                               max(d$coin_ratio, na.rm = T)),
  netpos_prop = seq(-1,1,length=nrows(d$netpos_prop)))
d_graph <- ggplotPredict(m_graph, d_graph)


plot_a <- ggplot(d, aes(x = netpos_prop, y = coin_ratio))+
  geom_point() +
  geom_smooth(data = d_graph, 
              aes(ymin = CILo, ymax = CIHi, 
                  x = netpos_prop,
                  y = Predicted), #
              stat = "identity",
              color="blue") +
  theme_bw(base_size = 14) +
 labs(x='Proportion of Net Positive Sentiment', y = 'Coin Ratio')

plot_a


# d$coin_prop = d$coin / d$view

