library(tidyverse)
library(gridExtra)

# load data
load('rda/papers.rda')
load('rda/citations.rda')
load('rda/total.rda')

## univariate analysis
# summary statistics
summary(total$incitation)
summary(total$outcitation)
# very little info

# plot
p1 <- total %>% 
  ggplot(aes(x = 0, y = incitation)) +
  geom_boxplot() +
  ggtitle('Boxplot of in-citation numbers') +
  theme(plot.title = element_text(size = 10)) +
  xlab('') +
  ylab('')
p1

p2 <- total %>% 
  ggplot(aes(x = 0, y = outcitation)) +
  geom_boxplot() +
  ggtitle('Boxplot of out-citation numbers') +
  theme(plot.title = element_text(size = 10)) +
  xlab('') +
  ylab('')
p2

p <- grid.arrange(p1, p2, nrow = 1)
ggsave('figs/citation-boxplot.png', plot = p)


## multivariate analysis
# in vs. out
total %>% 
  ggplot(aes(x = incitation, y = outcitation)) +
  geom_point() +
  ggtitle('Scatterplot of in vs. out') +
  xlab('in-citation number') +
  ylab('out-citation number')

