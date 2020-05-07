library(tidyverse)

#--------------------------------------------------------------------
# read data
papers <- read.csv('data/papers.csv', header = 1)
citations <- read.csv('data/citations.csv', header = 1)

#--------------------------------------------------------------------
# wrangle data
incitation <- citations %>% 
  group_by(ToID) %>% 
  summarise(incitation = n())

outcitation <- citations %>% 
  group_by(FromID) %>% 
  summarise(outcitation = n())

names(incitation)[1] <- 'PaperID'
names(outcitation)[1] <- 'PaperID'

# join the table
total <- papers %>% 
  left_join(incitation) %>% 
  left_join(outcitation)

total$incitation[is.na(total$incitation)] <- 0
total$outcitation[is.na(total$outcitation)] <- 0

#--------------------------------------------------------------------
# save as rda
save(papers, file = 'rda/papers.rda')
save(citations, file = 'rda/citations.rda')
save(total, file = 'rda/total.rda')
