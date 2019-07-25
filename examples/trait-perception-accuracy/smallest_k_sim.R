library(tidyverse)
library(afex)
rnormpre <- function (x, rho=0, ymean=0, ysd=1) {
  n <- length(x)
  y <- stats::rnorm(n)
  z <- rho * scale(x)[,1] + sqrt(1 - rho^2) * 
    scale(stats::resid(stats::lm(y ~ x)))[,1]
  yresult <- ymean + ysd * z
  yresult
}



simstuff <- function(sub_n = 100, # subjects total
                     item_n = c(10, 20, 30, 40, 50), # items 
                     item_dist = c(.1,.2,.4,.2,.1), # distribution of item ratings
                     cor_mean = .2, # mean correlation between ratings and traits
                     cor_sd = .1 # sd of subjects' correaltions between ratings and traits
                     ) {
  
  grand_int <- length(item_dist) / 2 # overall mean (intercept)
  grand_sd <- grand_int/3 # overall SD
  sub_sd <- 0.1 # SD of subject intercepts
  
  max_items <- 1e4
  item <- tibble(
    item_id = paste0("I",1:max_items),
    item_i = sample(1:length(item_dist), max_items, replace = T, prob = item_dist)
  )
  
  sub <- tibble(
    sub_id = paste0("S", 1:sub_n),
    sub_i = rnorm(sub_n, 0, sub_sd),
    sub_r = rnorm(sub_n, cor_mean, cor_sd)
  )
  
  for (i in 1:sub_n) {
    r <- sub[[i,3]]
    sub_i <- sub[[i,2]]
    id <- sub[[i,1]]
    ratings <- rnormpre(item$item_i, r, grand_int + sub_i, grand_sd) %>% 
      round() %>%
      pmax(1, .) %>% # set the minimum value to 1
      pmin(5, .) # set the maximum value to 5
    
    item[id] = ratings
  }
  
  dat <- item %>%
    gather(sub_id, rating, 3:ncol(.)) %>%
    rename(trait = item_i)
  
  for (subsample_n in item_n) {
    samp <- sample(item$item_id, subsample_n)
    
    m <- dat %>%
      filter(item_id %in% samp) %>%
      lmer(rating ~ trait + 
              (1 | item_id) + 
              (1 + trait | sub_id), 
            data = .)
    
    vname = paste0("N", subsample_n)
    sub <- ranef(m)$sub_id %>%
      as.tibble(rownames = "sub_id") %>%
      select(-`(Intercept)`) %>%
      rename(!!vname := trait) %>%
      left_join(sub, by = "sub_id")
  }
  
  sub
}

# replicate a lot
reps_n <- 10
sub_n <- 50

cors <- map_df(1:reps_n, function(i) {

  s <- simstuff(sub_n = sub_n, item_n = seq(55, 75, by = 5))
  
  cormat <- s %>%
    select(-sub_id) %>%
    cor()
  
  cormat["sub_r", ] %>%
    t() %>%
    as.tibble()
})

longcors <- cors %>%
  select(-sub_r) %>%
  mutate(rep = row_number(), 
         dt = lubridate::now()) %>%
  gather(n, cor, 1:(ncol(.)-2)) %>%
  mutate(n = as.integer(str_replace(n, "N", "")),
         sub_n = sub_n)

write_csv(longcors, "data/sims.csv", append = TRUE)

longcors <- read_csv("data/sims.csv")

# graph these correlations
longcors %>%
  ggplot(aes(n, cor)) +
  geom_smooth()
