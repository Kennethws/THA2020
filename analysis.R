library(tidyverse)
library(gridExtra)
library(ggrepel)
library(stargazer)

# load data
load('rda/papers.rda')
load('rda/citations.rda')
load('rda/total.rda')

### Q1
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
ggsave('figs/in vs out.png')

# in vs. out against year
total %>% 
  ggplot(aes(x = incitation, y = outcitation, color = year)) +
  geom_point() +
  ggtitle('Scatterplot of in vs. out') +
  xlab('in-citation number') +
  ylab('out-citation number')
ggsave('figs/in vs out against years.png')
# generally, old papers cite less and are cited more whereas
# new papers cite more and are cited less


### Q2
## a
avg.out <- total %>% 
  group_by(year) %>% 
  summarise(avg.out = mean(outcitation))

stargazer(avg.out, summary = FALSE, type = 'text', align = TRUE)

## b
avg.out$year <- as.numeric(avg.out$year)

# pretest whether hay linear relationship

# correlation
cor(avg.out$year, avg.out$avg.out, method = 'pearson')
cor(avg.out$year, avg.out$avg.out, method = 'spearman')
# appear to be very strong positive correlation

# so fit linear model to get parameters of interest
lin.md <- lm(avg.out ~ year, data = avg.out)
summary(lin.md)

# plot
para <- lin.md$coefficients
avg.out %>% 
  ggplot(aes(x = year, y = avg.out)) +
  geom_line() +
  geom_abline(slope = para[2], intercept = para[1], col = 'red') +
  ggtitle('linear model of avg.out ~ year')
ggsave('figs/linear model of avg.out ~ year.png')

stargazer(lin.md, type = 'text', title = 'linear model of avg.out ~ year',
          align = TRUE)
# therefore, hay strong evidence that the physicist's claim is true

## c
# for every year that has passed, the estimated average out-citation number 
# increases by 1.596 unit
# given that the linear assumption still holds for years before 1993, 
# the estimated average out-citation will be -3179, which is against
# commonse sense and not likely to be true


### Q3
aut.win <- total %>% 
  group_by(month) %>% 
  summarise(num.incitation = sum(incitation), n = n()) %>% 
  slice(1:3, 10:12)

spr.sum <- total %>% 
  group_by(month) %>% 
  summarise(num.incitation = sum(incitation), m = n()) %>% 
  slice(4:9)

comparison <- cbind(aut.win = aut.win$num.incitation,
                    n = aut.win[,3],
                    spr.sum = spr.sum$num.incitation,
                    m = spr.sum[,3])

# use hypothesis testing - t-test
# by CLT, sum of iid random samples has distribution that's 
# approximately normal
n <- nrow(comparison)
m <- nrow(comparison)
# df = n+m-2
X <- comparison$aut.win
Y <- comparison$spr.sum
X.bar <- mean(X)
Y.bar <- mean(Y)
s.X <- sd(X)
s.Y <- sd(Y)
sp <- sqrt(((n-1) * s.X^2 + (m-1) * s.Y^2) / (n+m-2))

t <- (X.bar - Y.bar) / sp / sqrt(1/n + 1/m)

p <- pt(t, df = n+m-2, lower.tail = 0)
# p > 0.6, no evidence to reject H0

cat('After conducting t-test, p-value is', p, '\nTherefore, cannot reject H0' )
