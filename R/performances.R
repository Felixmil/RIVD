#' perfPrecision
#' 
#' Computes Assay Precision according to CLSI EP05-A3.
#' 
#' Heavily relies on the VCA package for computing the variance component analysis.
#' references: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2556577/
#' 
#' @param df dataframe 
#' @param x column name of the value to evaluate
#' @param day column name where days ids are stored
#' @param run column name where run ids are stored
#'
#' @return dataframe containing main statistics on variance
#' @export 
#' 
#' @importFrom VCA anovaVCA
#' @importFrom stats reformulate
#' @import dplyr
#' 
#' @examples
#' data(Glucose,package="VCA")
#' perfPrecision(Glucose, 'result','run', 'day')
#' 
perfPrecision <- function(df, x, run, day) {
  res <- 
    anovaVCA(reformulate(paste(day,'/',run), response = x), 
             df)
  
    res <- as.data.frame(res[["aov.tab"]])[c(1,5,6,7)]
    
    rownames(res) = c('Withing laboratory', 'within-day', 'within-run', 'error')
    
    res
  
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
