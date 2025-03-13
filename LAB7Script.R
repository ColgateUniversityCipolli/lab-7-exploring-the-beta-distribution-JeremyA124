library(tidyverse) #LIBRARY FOR PLOTTING AND DATA MANIPULATION
library(patchwork) #DATA FOR ORGANZIING GRAPHS
library(e1071)     #LIBRARY FOR EADDIOTNLA STATISTICS FUNCTIONS(SKEWNESS AND KURTOSIS)
set.seed(7272)    #MAKE SURE WE ALL GET SAME RESULT

beta.data <- function(alpha,  #THE ALPHA PARAMETER
                      beta) { #THE BETA PARMETER
  
  #FUNCTION PURPOSE:
  #THIS FUCNTION CUALCUATES THE POPULATION LEVEL BETA DISTRUBTUION VALUES
  mean = (alpha)/(alpha+beta) #CALUCLATE MEAN
  variance = (alpha*beta)/(((alpha+beta)**2)*(alpha+beta+1)) #CALCULATE VARIANCE
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta)) #CALCULATE SKEWNESS
  #CCLULATE KURTOSIS
  kurtosis = (6*(((alpha-beta)**2)*(alpha+beta+1)-(alpha*beta)*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  
  distribution.data <- tibble( #PLACES ALL DATA INTO A TIBBLE
    type = "Population level",
    distribution = paste("(", alpha, ",", beta, ")"),
    mean = mean,
    variance = variance,
    skewness = skewness,
    kurtosis = kurtosis
  )
  
  return(distribution.data) #RETURNS DATA
}

beta.plot <- function(alpha, #THE ALPHA PARAMETER
                      beta) {#THE BETA PARAMETER
  
  #FUNCTION PURPOSE:
  #THISN FUCNTION GRPAHS BETA DISTRBUTIONS BASED ON ALPHA ND BETA PARAMETERS
  beta.figure <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%  #generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta))                        #compute the beta PDF
  plotty <- ggplot(data = beta.figure) + #USING THE beta.figure data
    geom_line(aes(x = x, y = beta.pdf)) + #PLOT A LINE REPRESNTING THE BETA DISTRIBUTION
    labs(title = paste("(", alpha, ",", beta, ")")) #ADDS THE TITLE
  
  return(plotty) #RETURNS THE PLOT
}

distribution.data <- rbind(beta.data(2,5), #HERE WE OBATAINED THE DATA FROM MULTIPLE BETA
                           beta.data(5,5), #DISTRIBUTIONS AND MERGED THEM INTO A TIBBLE
                           beta.data(5,2),
                           beta.data(0.50,0.50))

beta.plot(2,5) +     #NEXT WE PLOTTED ALL MUTLIPLE BETA DISTRIBUTION AND MEGRED THEM
beta.plot(5,5) + 
beta.plot(5,2) + 
beta.plot(0.50,0.50)

beta.moment <- function(alpha,      #THE ALPHA PARAMETER
                        beta,       #THE BETA PARAMETER
                        k,          #THE K MOMENTS VLAUE
                        centered) { #BOOL TO DETERMINE WHICH MOM TO US
 
  if(centered == FALSE) { #IF FLASE, USE THE UNCENTERED MOMENT
    integrand = function(x) {(x**k)*dbeta(x, alpha, beta)} #GRAB THE FUNCTION
    result = integrate(integrand, lower = 0, upper = 1)$value #AND CALCULATE THE VALUE
  } else if(centered == TRUE) { #IF TRUE, USE THE CENTREED MOMENT
    integrand = function(x) {(x**k)*dbeta(x, alpha, beta)}
    mu.x = integrate(integrand, lower = 0, upper = 1)$value #FIND THE MEAN
    integrand = function(x) {((x-mu.x)**k)*dbeta(x, alpha, beta)} # GRAB THE FUCNTION
    result = integrate(integrand, lower = 0, upper = 1)$value #FIND THE VALUE
  } else { # IF NEITHERM=, STOP FUCNTION
    warning("Enter a boolean for centered!!") 
  }
  
  return(result) #RETURN THE VALUES
}

beta.data.moments <- function(alpha,  #ALPHA PARAMATER
                              beta) { # BETA PARAMETER
  #FUNCTION PURPOSE:
  #PERFROMS STATIICAL COMPUATIONS USING THE BETA MOMENTS
  mean = beta.moment(alpha, beta, 1, FALSE)   #CALUCLATES MEAN
  variance = beta.moment(alpha, beta, 2, TRUE)#CAULCATES VARIANCE
  #CALCULATES SKEWNESS
  skewness = beta.moment(alpha, beta, 3, TRUE)/(beta.moment(alpha, beta, 2, TRUE)**(3/2))
  #CALCULATES KURTOSIS
  kurtosis = (beta.moment(alpha, beta, 4, TRUE)/(beta.moment(alpha, beta, 2, TRUE)**2))-3
  
  distribution.data <- tibble( #PLACE ALL THE CALCULATED DATA INTO A TIBBLE
    type = "Integrated",
    distribution = paste("(", alpha, ",", beta, ")"),
    mean = mean,
    variance = variance,
    skewness = skewness,
    kurtosis = kurtosis
  )
  
  return(distribution.data) #RETURN THE TIBBLE
}

distribution.data.moments <- rbind(distribution.data,     #HERE I PLACED ALL THE DATA FOR THE MOMENTS
                                   beta.data.moments(2,5),#INTO ONE DATAFRAME
                                   beta.data.moments(5,5),
                                   beta.data.moments(5,2),
                                   beta.data.moments(0.5,0.5))


beta.samples <- function(alpha,     #ALPHA PARAMETER
                         beta,      #BETA PARAMETER
                         n = 500) { #SAMPLE SIZE
  
  #FUNCTION PURPOSE:
  #SIMULATES SAMPLE EXPERIMENT WITH N = 500
  samples <- rbeta(n = n, # sample size
                   shape1 = alpha,  # alpha parameter
                   shape2 = beta)   # beta parameter
  
  return(samples) #RETURN SAMPLES
}

beta.summary <- function(dataframe, #THE INPUTTED DATAFRAME
                         title,     #TYPE OF DISTRIBUTOON
                         type){     #THE METHOD WE USED
  
  #FUCNTION PURPOSE:
  #DOES STATISTICAL COMPUATTIONS ON THE SAMPLES USING TIDYVERSE
  summarized <- dataframe %>%
    summarize(mean = mean(dataframe[,1]), #CALCULATES MEAN
              variance = var(dataframe[,1]), #CALULATES VARIANCE
              skewness = skewness(dataframe[,1]), #CALCULATES SKEWNESS
              kurtosis = kurtosis(dataframe[,1])) %>% #CALCULATES KURTOSIS
    mutate(distribution = title, #ADDS THE DISTRIBUTION TYPE AND METHOD
           type = type) %>%
    select(type,        #ORGANIZE MY RESULTS
           distribution,
           mean,
           variance,
           skewness, 
           kurtosis)
  
  return(summarized) #RETURN THE DATAFRAME
}

beta.samples.plot.combined <- function(dataframe, #INPUTTED DATAFRAME
                                       alpha,     #THE ALPHA PARAMETER
                                       beta) {    #THE BETA PARAMETER
  beta.figure <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%  #generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta))                        #compute the beta PDF
  
  plotty <- ggplot() +
    #PLOTS A DESNITY HISTOGRAM OF THE SAMPLES
    geom_histogram(data = dataframe, aes(x = x, y = after_stat(density), color = "Estimated Distribution")) +
    geom_density(data = dataframe, aes(x = x, color = "Estimated Distribution")) +
    #PLOTS THE POPULATE LEVEL DISTIRBTUION
    geom_line(data = beta.figure, aes(x = x, y = beta.pdf, color = "True Distribution")) +
    #ADDS THE TITLE AND FORMATS THE PLOTS
    labs(title = paste("(", alpha, ",", beta, ")")) +
    theme(legend.position = "bottom")
  
  return(plotty) #RETURNS THE PLOT
}

sample1 <- data.frame(x = beta.samples(2,5)) #HERE I GATHERED THE SAMPLES OF THE DIFFERENT
sample2 <- data.frame(x = beta.samples(5,5)) #BETA DISTRIBUTIONS
sample3 <- data.frame(x = beta.samples(5,2))
sample4 <- data.frame(x = beta.samples(0.5,0.5))

sample.summary.combined <- rbind(distribution.data.moments, #HERE I COMBINED THE OLD DATA WITH THE NEW SAMPLE STATISTICS
                                 beta.summary(sample1, "( 2 , 5 )", "Sampled"),
                                 beta.summary(sample2, "( 5 , 5 )", "Sampled"),
                                 beta.summary(sample3, "( 5 , 2 )", "Sampled"),
                                 beta.summary(sample4, "( 0.5 , 0.5 )", "Sampled"))

beta.samples.plot.combined(sample1, 2, 5) +  #HERE I PLOTTED AND COMBINED ALL THE SAMPLES PLOTS
  beta.samples.plot.combined(sample2, 5, 5) +
  beta.samples.plot.combined(sample3, 5, 2) +
  beta.samples.plot.combined(sample4, 0.5, 0.5)

library(cumstats) #LIBRARY FOR CUMULATIVE STATISTIC CALCS
samples.2.5 = beta.samples(2,5) #GATHERED NEW SAMPLES

cumulative.stats <- tibble( #CREATED A TIBBLE WITH THE CULULATIVE STATS FOR 500 ITERATIONS
  cum.step = 1:500,
  mean = cummean(samples.2.5),
  variance = cumvar(samples.2.5),
  skewness = cumskew(samples.2.5),
  kurtosis = cumkurt(samples.2.5)-3
)

cum.mean.plot <- ggplot(data = cumulative.stats) + #HERE I PLOTTED THE CULMULATIVE MEAN
  geom_line(aes(x = cum.step, y = mean)) +
  geom_hline(yintercept = distribution.data$mean[1]) +
  labs(x = "Cumulative Iterations",
       y = "Mean",
       title = "Cumulative Mean")
cum.variance.plot <- ggplot(data = cumulative.stats) +#HERE I PLOTTED THE CULMULATIVE VARIANCE
  geom_line(aes(x = cum.step, y = variance)) +
  geom_hline(yintercept = distribution.data$variance[1]) +
  labs(x = "Cumulative Iterations",
       y = "Variance",
       title = "Cumulative Variance")
cum.skewness.plot <- ggplot(data = cumulative.stats) +#HERE I PLOTTED THE CULMULATIVE SKEWNESS
  geom_line(aes(x = cum.step, y = skewness)) +
  geom_hline(yintercept = distribution.data$skewness[1]) +
  labs(x = "Cumulative Iterations",
       y = "Skewness",
       title = "Cumulative Skewness")
cum.kurtosis.plot <- ggplot(data = cumulative.stats) +#HERE I PLOTTED THE CULMULATIVE KURTOSIS
  geom_line(aes(x = cum.step, y = kurtosis)) +
  geom_hline(yintercept = distribution.data$kurtosis[1]) +
  labs(x = "Cumulative Iterations",
       y = "Kurtosis (excess)",
       title = "Cumulative Kurtosis")

cum.mean.plot +     #HERE I COMBINED ALL MY PLOTS
  cum.variance.plot +
  cum.skewness.plot +
  cum.kurtosis.plot

for(i in 2:50){
  set.seed(7272+i) #FOR A DIFFERENT RANDOMIZATION
  
  samples.2.5 = beta.samples(2,5) #GET A NEW SAMPLE
  
  cumulative.stats <- tibble( #AND PERFORM CULUKATIVE STATS ON EACH SAMPLE
    cum.step = 1:500,
    mean = cummean(samples.2.5),
    variance = cumvar(samples.2.5),
    skewness = cumskew(samples.2.5),
    kurtosis = cumkurt(samples.2.5)-3
  )
  
  cum.mean.plot <- cum.mean.plot + #PLOT THE MEAN LINE ONTOP OF EACHOTHER
    geom_line(data = cumulative.stats, aes(x = cum.step, y = mean), color = i)
  cum.variance.plot <- cum.variance.plot + #PLOT THE VARIANCE LINE ONTOP OF EACHOTHER
    geom_line(data = cumulative.stats, aes(x = cum.step, y = variance), color = i)
  cum.skewness.plot <- cum.skewness.plot + #PLOT THE SKEWNESS LINE ONTOP OF EACHOTHER
    geom_line(data = cumulative.stats, aes(x = cum.step, y = skewness), color = i)
  cum.kurtosis.plot <- cum.kurtosis.plot +#PLOT THE KURTOSIS LINE ONTOP OF EACHOTHER
    geom_line(data = cumulative.stats, aes(x = cum.step, y = kurtosis), color = i)
}

cum.mean.plot +      #HERE I MERGED ALL THE PLOTS INTO A GRID OF PLOTS
  cum.variance.plot +
  cum.skewness.plot +
  cum.kurtosis.plot

sample.summary = data.frame() # HERE I CREATED A EMPTY DATAFRAME

for(i in 1:1000){
  set.seed(7272+i) #FOR EACH RANDOMIZATION
  
  samples.2.5 <-  data.frame(beta.samples(2,5)) #GET A NEW SAMPLE
  
  sample.summary <- rbind(sample.summary, # PERFORM STATISTICAL CALCULATIONS
                          beta.summary(samples.2.5, "( 2 , 5 )", "Sampled"))
}

mean.histogram <- ggplot(data = sample.summary) + #PLOT THE MEAN HISTOGRAM
  geom_histogram(aes(x = mean)) +
  labs(x = "Means",
       y = "Frequency",
       title = "Simulations of 1000 (2,5) Beta Distribution Means")
variance.histogram <- ggplot(data = sample.summary) + #PLOT THE VARIANCE HISTOGRAM
  geom_histogram(aes(x = variance)) +
  labs(x = "Variances",
       y = "Frequency",
       title = "Simulations of 1000 (2,5) Beta Distribution Variances")
skewness.histogram <- ggplot(data = sample.summary) + #PLOT THE MSKEWNESS HISTOGRAM
  geom_histogram(aes(x = skewness)) +
  labs(x = "Skewnesses",
       y = "Frequency",
       title = "Simulations of 1000 (2,5) Beta Distribution Skewnesses")
kurtosis.histogram <- ggplot(data = sample.summary) + #PLOT THE KURTOSIS HISTOGRAM
  geom_histogram(aes(x = kurtosis)) +
  labs(x = "Kurtosis'",
       y = "Frequency",
       title = "Simulations of 1000 (2,5) Beta Distribution Kurtosis'")

mean.histogram +      #MERGE ALL THE PLOTS INTO A GRID PLOT
  variance.histogram +
  skewness.histogram +
  kurtosis.histogram

