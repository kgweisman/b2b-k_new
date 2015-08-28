# write function to test whether the mean response to inanimate trials (overall) 
# is further from the midpoint (0) the mean response to sentient-only trials
# (overall); NOTE: m1 = inanimate, m2 = sentient-only
compAbsMean <- function(studyNum, var.equal = "test") {
  # make summary table for this study
  tab <- d %>%
    filter(phase == "test" & study == studyNum) %>%
    group_by(sentInam) %>%
    summarise(mean = mean(response, na.rm = T),
              absMean = abs(mean),
              sd = sd(response, na.rm = T),
              n = length(response))
  
  # get means, sds, and ns for sentient-only vs. inanimate trials
  m1 <- tab$absMean[1]; m2 <- tab$absMean[2]
  s1 <- tab$sd[1]; s2 <- tab$sd[2]; 
  n1 <- tab$n[1]; n2 <- tab$n[2]
  
  # test for equal variances
  varTest <- with(d %>% filter(phase == "test" & study == studyNum), 
                  var.test(response ~ sentInam))
  if(var.equal == "test") {
    var.equal <- ifelse(varTest$p.value < 0.10, F, T)
  }
  
  # calculate test statistics
  if(var.equal == F) {
    s.pool <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))
    t <- (m1 - m2) / sqrt(s1^2/n1 + s2^2/n2)
    df <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    p <- pt(t, df, lower.tail = ifelse(t < 0, T, F))
    d <- (m1 - m2)/s.pool
    results <- c("Welch's t" = round(t, 2), df = round(df, 2), 
                 p = round(p, 4), "Cohen's d" = round(d, 2))
  } else if (var.equal == T) {
    s.pool <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))
    t <- (m1 - m2) / (s.pool * sqrt((1/n1) + (1/n2)))
    df <- n1 + n2 - 2
    p <- pt(t, df, lower.tail = ifelse(t < 0, T, F))
    d <- (m1 - m2)/s.pool
    results <- c(t = round(t, 2), df = round(df, 2), 
                 p = round(p, 4), "Cohen's d" = round(d, 2))
  }

  return(list("Summary Table" = tab, "Test of Equal Variances" = varTest, 
              "Test of Absolute Values of Means" = results))
}
    
compAbsMean(studyNum = 1, var.equal = F)
compAbsMean(studyNum = 2)
compAbsMean(studyNum = 3)
compAbsMean(studyNum = 4)


compAbsMean_byCountry <- function(studyNum, var.equal = "test") {
  # make summary table for this study
  tab <- d %>%
    filter(phase == "test" & study %in% studyNum) %>%
    group_by(country, sentInam) %>%
    summarise(mean = mean(response, na.rm = T),
              absMean = abs(mean),
              sd = sd(response, na.rm = T),
              n = length(response))
  
  # get means, sds, and ns for sentient-only vs. inanimate trials
  m1.india <- tab$absMean[1]; m2.india <- tab$absMean[2]
  s1.india <- tab$sd[1]; s2.india <- tab$sd[2]; 
  n1.india <- tab$n[1]; n2.india <- tab$n[2]
  m1.us <- tab$absMean[3]; m2.us <- tab$absMean[4]
  s1.us <- tab$sd[3]; s2.us <- tab$sd[4]; 
  n1.us <- tab$n[3]; n2.us <- tab$n[4]
  
  # test for equal variances
  varTest <- with(d %>% filter(phase == "test" & study == studyNum), 
                  bartlett.test(response ~ interaction(country, sentInam)))
  if(var.equal == "test") {
    var.equal <- ifelse(varTest$p.value < 0.10, F, T)
  }
  
  # calculate test statistics
  if(var.equal == F) {
   
    # india
    s.pool.india <- sqrt(((n1.india - 1)*s1.india^2 + (n2.india - 1)*s2.india^2)/(n1.india + n2.india - 2))
    t.india <- (m1.india - m2.india) / sqrt(s1.india^2/n1.india + s2.india^2/n2.india)
    df.india <- (s1.india^2/n1.india + s2.india^2/n2.india)^2 / ((s1.india^2/n1.india)^2/(n1.india-1) + (s2.india^2/n2.india)^2/(n2.india-1))
    p.india <- pt(t.india, df.india, lower.tail = ifelse(t < 0, T, F))
    d.india <- (m1.india - m2.india)/s.pool.india
    results.india <- c("Welch's t" = round(t.india, 2), df = round(df.india, 2), 
                 p = round(p.india, 4), "Cohen's d" = round(d.india, 2))

    # us
    s.pool.us <- sqrt(((n1.us - 1)*s1.us^2 + (n2.us - 1)*s2.us^2)/(n1.us + n2.us - 2))
    t.us <- (m1.us - m2.us) / sqrt(s1.us^2/n1.us + s2.us^2/n2.us)
    df.us <- (s1.us^2/n1.us + s2.us^2/n2.us)^2 / ((s1.us^2/n1.us)^2/(n1.us-1) + (s2.us^2/n2.us)^2/(n2.us-1))
    p.us <- pt(t.us, df.us, lower.tail = ifelse(t < 0, T, F))
    d.us <- (m1.us - m2.us)/s.pool.us
    results.us <- c("Welch's t" = round(t.us, 2), df = round(df.us, 2), 
                       p = round(p.us, 4), "Cohen's d" = round(d.us, 2))
    
  } else if (var.equal == T) {
    
    # india
    s.pool.india <- sqrt(((n1.india - 1)*s1^2 + (n2.india - 1)*s2^2)/(n1.india + n2.india - 2))
    t.india <- (m1.india - m2.india) / (s.pool * sqrt((1/n1.india) + (1/n2.india)))
    df.india <- n1.india + n2.india - 2
    p.india <- pt(t.india, df.india, lower.tail = ifelse(t < 0, t.india, F))
    d.india <- (m1.india - m2.india)/s.pool.india
    results.india <- c(t = round(t.india, 2), df = round(df.india, 2), 
                 p = round(p.india, 4), "Cohen's d" = round(d.india, 2))

    # us
    s.pool.us <- sqrt(((n1.us - 1)*s1^2 + (n2.us - 1)*s2^2)/(n1.us + n2.us - 2))
    t.us <- (m1.us - m2.us) / (s.pool * sqrt((1/n1.us) + (1/n2.us)))
    df.us <- n1.us + n2.us - 2
    p.us <- pt(t.us, df.us, lower.tail = ifelse(t < 0, t.us, F))
    d.us <- (m1.us - m2.us)/s.pool.us
    results.us <- c(t = round(t.us, 2), df = round(df.us, 2), 
                       p = round(p.us, 4), "Cohen's d" = round(d.us, 2))
  }
  
  # compare effect sizes: d1 = us
  v.india <- (1/n1.india) + (1/n2.india) + d.india^2/(2*(n1.india + n2.india))
  v.us <- (1/n1.us) + (1/n2.us) + d.us^2/(2*(n1.us + n2.us))
  z.comp <- (d.us - d.india) / sqrt(v.us + v.india)
  p.comp <- pnorm(z.comp, lower.tail = ifelse(z.comp < 1, T, F))
  results.compare <- c(z = round(z.comp, 2), p = round(p.comp, 4))
  
  return(list("Summary Table" = tab, "Test of Equal Variances" = varTest, 
              "Test of Absolute Values of Means: India" = results.india,
              "Test of Absolute Values of Means: US" = results.us,
              "Comparison of Effect Sizes (US - India)" = results.compare))
}
compAbsMean_byCountry(studyNum = c(1,3))
compAbsMean_byCountry(studyNum = 4)
