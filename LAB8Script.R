library(tidyverse)
library(nleqslv)
library(patchwork)
death.rate.table <- read_csv("WorldBankData/DeathRatesTableTest.csv")
set.seed(7272)

death.rate.table.2022 <- death.rate.table %>%
  mutate(rate = `2022`/1000) %>%
  select(rate)

MOM.estimator <- function(data,
                          parameter) {
  alpha = parameter[1]
  beta = parameter[2]
  
  e.x = alpha/(alpha+beta)
  e.x.2 = ((alpha+1)*alpha)/((alpha+beta+1)*(alpha+beta))
  bar.mean = mean(data, na.rm = TRUE)
  bar.mean.2 = mean(data**2, na.rm=TRUE)
  
  return(c(e.x-bar.mean, e.x.2-bar.mean.2))
}

loglik.estimator <- function(data,
                             parameter,
                             neg=FALSE) {
    alpha = parameter[1]
    beta = parameter[2]
    
    loglik = sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)), na.rm=TRUE)
    
    return(ifelse(neg, -loglik, loglik))
}

sample.data <- function(alpha,
                        beta,
                        n = 1000) {
  samples <- rbeta(n = n,
                   shape1 = alpha,
                   shape2 = beta)
}

nleqslv(x = c(1,1),
        fn = MOM.estimator,
        data = death.rate.table.2022$rate)

optim(par = c(1,1),
      fn = loglik.estimator,
      data = death.rate.table.2022$rate,
      neg = T)

samples <- tibble(
  x = sample.data(8.42, 1000)
)

MOM.data <- tibble(x = seq(0,0.02, length.out = 1000)) %>%
  mutate(pdf = dbeta(x=x, shape1 = 8.42, shape2 = 1003.46))

MLE.data <- tibble(x = seq(0,0.02, length.out = 1000)) %>%
  mutate(pdf = dbeta(x=x, shape1 = 8.27, shape2 = 985.04))

sample.histo <- ggplot() +
  geom_histogram(data = samples, aes(x=x, y = after_stat(density)), color = "grey") +
  geom_density(data = samples, aes(x=x, color = "Histogram Density"), size = 1, key_glyph = "path") +
  geom_line(data = MOM.data, aes(x=x, y = pdf, color = "Method of Moments"), size = 1) +
  geom_line(data = MLE.data, aes(x=x, y = pdf, color = "Maximum Log Estimator"), size = 1) +
  labs(color = "Estimator Type") 


sample.histo

alpha.beta.summary <- tibble()

for(i in 1:1000){
  set.seed(7272+i)
  
  samples <- sample.data(8,
                         960,
                         266)
  
  alpha.beta.values <- tibble(
    alpha.MOM = nleqslv(x = c(1,1),
                        fn = MOM.estimator,
                        data = samples)$x[1],
    beta.MOM = nleqslv(x = c(1,1),
                       fn = MOM.estimator,
                       data = samples)$x[2],
    alpha.log = optim(par = c(1,1),
                      fn = loglik.estimator,
                      data = samples,
                      neg = T)$par[1],
    beta.log = optim(par = c(1,1),
                     fn = loglik.estimator,
                     data = samples,
                     neg = T)$par[2]
    
    
  )
  
  alpha.beta.summary <- rbind(alpha.beta.summary,
                              alpha.beta.values)
}

alpha.MOM.plot <- ggplot(data = alpha.beta.summary) +
  geom_density(aes(x=alpha.MOM)) +
  geom_hline(yintercept = 0) +
  labs(x = "x",
       title = "MOM of the Alpha Parameter")
beta.MOM.plot <- ggplot(data = alpha.beta.summary) +
  geom_density(aes(x=beta.MOM)) +
  geom_hline(yintercept = 0) +
  labs(x = "x",
       title = "MOM of the Beta Parameter")
alpha.log.plot <- ggplot(data = alpha.beta.summary) +
  geom_density(aes(x=alpha.log)) +
  geom_hline(yintercept = 0) +
  labs(x = "x",
       title = "Log likelihood of the Alpha Parameter")
beta.log.plot <- ggplot(data = alpha.beta.summary) +
  geom_density(aes(x=beta.log)) +
  geom_hline(yintercept = 0) +
  labs(x = "x",
       title = "Log likelihood of the Beta Parameter")

alpha.MOM.plot +
  beta.MOM.plot +
  alpha.log.plot +
  beta.log.plot

alpha.MOM.summary <- tibble(
  type = "Alpha, MOM",
  bias = mean(alpha.beta.summary$alpha.MOM) - 8 ,
  percision = 1/var(alpha.beta.summary$alpha.MOM),
  MSE = var(alpha.beta.summary$alpha.MOM) + bias**2
)

beta.MOM.summary <- tibble(
  type ="Beta, MOM",
  bias = mean(alpha.beta.summary$beta.MOM) - 950 ,
  percision = 1/var(alpha.beta.summary$beta.MOM),
  MSE = var(alpha.beta.summary$beta.MOM) + bias**2
)

alpha.log.summary <- tibble(
  type = "Alpha, Log",
  bias = mean(alpha.beta.summary$alpha.log) - 8 ,
  percision = 1/var(alpha.beta.summary$alpha.log),
  MSE = var(alpha.beta.summary$alpha.log) + bias**2
)

beta.log.summary <- tibble(
  type = "Beta, Log",
  bias = mean(alpha.beta.summary$beta.log) - 950 ,
  percision = 1/var(alpha.beta.summary$beta.log),
  MSE = var(alpha.beta.summary$beta.log) + bias**2
)

table.summary <- bind_rows(
  alpha.MOM.summary,
  beta.MOM.summary,
  alpha.log.summary,
  beta.log.summary,
)

