library(psych)
library(tidyverse)
library(GGally)
source("610_functions.R")

d = read.csv("danmaku_netpos_count.csv") %>% 
  janitor::clean_names() %>% drop_na()
dim(d)

d = d[d$netpos_prop <= 1,]
d = d[d$netpos_prop >= -1,]


varPlot(d$netpos_prop)

pairs_name = c("netpos_prop","view","danmaku","reply","favorite","coin","share","like")
GGally::ggpairs(d[,pairs_name])
