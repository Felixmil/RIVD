#' perfRepet
#' 
#' Computes Assay Precision according to CLSI EP05-A3.
#' 
#' references: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2556577/
#' 
#' @param df: dataframe 
#' @param x: column name of value to evaluate
#' @param day: column name where nuber of days is stored
#'
#' @return dataframe containing main statistics on variance
#' @export 
#'
#' @examples
#' data(Glucose,package="VCA")
#' perfRepet(Glucose, 'result','run', 'day')
#' 
perfRepet <- function(df, x, run, day) {
  require(tidyverse)
  require(VCA)
  
  res <- 
    anovaVCA(reformulate(paste(day,'/',run), response = x), 
             df)
  
    as.data.frame(res[["aov.tab"]]) %>% 
    select(-SS, -MS, -VC)
  
## Tried to reimplement  manually but found the VCA package
# n_day <-  n_distinct(df %>% select(!!day))
# 
# n_rep <- df %>%
#   group_by(!!day) %>%
#   summarise(c=n()) %>%
#   ungroup() %>%
#   pull(mean(c)) %>% unique()
# 
# s_r <- df %>%
#   group_by(!!day) %>%
#   mutate(errorMean = (!!x - mean(!!x))^2) %>%
#   ungroup() %>%
#   mutate(s_r= sqrt(sum(errorMean)/n_day*(n_rep-1))) %>%
#   distinct(s_r) %>%
#   pull()
# 
# s_b <- df %>%
#   group_by(!!day) %>%
#   mutate(mean= mean(!!x)) %>%
#   ungroup() %>%
#   mutate(s_b = sum((mean-mean(!!x))^2)/(n_day-1)) %>%
#   distinct(s_b) %>%
#   pull()
# 
# s_l <- df %>%
#   summarize(sqrt(((n_rep-1/n_rep)*(s_b+s_r)))) %>% pull()
}

#' Plot Precision Chart
#' 
#' A plotting format for plotting assay's precision
#'
#'Middle line is the grand mean, short lines are daily's mean, red points are run means and black point individual observations.
#'
#' @param df a datatable
#' @param x string, column name of the results
#' @param run string, column name of the runs ID
#' @param day string, column name of the Days ID
#'
#' @return ggplot object
#' @export 
#'
#' @examples
#' data(Glucose,package="VCA")
#' plotPrecision(Glucose, 'result','run', 'day')
plotPrecision <- function(df,  x, run, day){
    require(tidyverse)
  
    x <- ensym(x)
    run <- ensym(run)
    day <- ensym(day)
    
    ggplot(df,aes(x=!!run, y=!!x)) +
      geom_point(pch=21) +
      geom_hline(data = df %>% 
                   group_by(!!day) %>% 
                   summarise(mean=mean(!!x)),
                 aes(yintercept = mean),
                 col='firebrick')+
      geom_point(data = df %>% 
                   group_by(!!day, !!run) %>% 
                   summarise(mean=mean(!!x)),
                 aes(y = mean, x = !!run), 
                 col='firebrick')+
      facet_grid(cols = vars(!!day)) + 
      theme_grey()+
      theme(panel.spacing = unit(0.05,"line"),panel.grid.major.x = element_blank()) +
      geom_hline(aes(yintercept = mean(!!x)))
}

perfStab <- function(){
  
}