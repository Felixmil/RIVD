#' perfPrecision
#' 
#' Computes Assay Precision according to CLSI EP05-A3.
#' 
#' Heavily relies on the VCA package for computing the variance component analysis.
#' references: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2556577/
#' 
#' @param df dataframe 
#' @param x string. column name of the value to evaluate
#' @param day string. column name where days ids are stored
#' @param run string. column name where run ids are stored
#' @param by string, default to NULL. column name of a slicing variable. Use by if the table contains a variable to repeat the analysis on (for example a QC level or a gold-standard identification variable). If by is used, a list containing one result per slice level is returned.
#'@param site string, column name where sites ids are stored. If site is not null, the outputs gives back reproducibility performances.
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
perfPrecision <- function(df, x, run, day, by = NULL, site = NULL) {
  
  formula <-
    if(is.null(site)){
      paste(day,'/',run)
    } else {
      paste(site,'/',day,'/',run)
    }
  
  res <- 
    anovaVCA(reformulate(formula, response = x), 
             Data = df,
             by = by)
  
  if (length(res) > 1 & names(res[1]) != 'call'){
    precisionResults <- list()
    for (i in seq_along(res)) {
      precisionResults[[names(res[i])]] <- as.data.frame(res[[i]][["aov.tab"]])[,c(1,4,5,6,7)]
      if(!is.null(site)){
        precisionResults[[i]]['repeatability','SD'] = c(sqrt(sum(precisionResults[[i]][c('site:day','site:day:run','error'),"VC"])))
        precisionResults[[i]]['repeatability','CV[%]'] = c(sqrt(sum(precisionResults[[i]][c('site:day', 'site:day:run','error'),"VC"])))/res[[i]]$Mean*100
        rownames(precisionResults[[i]])[grepl('^total$',rownames(precisionResults[[i]]))] = 'reproducibility'
      } else {
        rownames(precisionResults[[i]])[grepl('^total$',rownames(precisionResults[[i]]))] = 'repetability'
      }
      rownames(precisionResults[[i]])[grepl('^site$',rownames(precisionResults[[i]]))] = 'between laboratory'
      rownames(precisionResults[[i]])[grepl('^site:day$',rownames(precisionResults[[i]]))] = 'within day'
      rownames(precisionResults[[i]])[grepl('^day$',rownames(precisionResults[[i]]))] = 'within day'
      rownames(precisionResults[[i]])[grepl('^day:run$',rownames(precisionResults[[i]]))] = 'within run'
      rownames(precisionResults[[i]])[grepl('^site:day:run$',rownames(precisionResults[[i]]))] = 'within run'
    }
  } else {
    precisionResults <- as.data.frame(res[["aov.tab"]])[c(1,4, 5,6,7)]
    if(!is.null(site)){
      precisionResults['repeatability','SD'] = c(sqrt(sum(precisionResults[c('site:day','site:day:run','error'),"VC"])))
      precisionResults['repeatability','CV[%]'] = c(sqrt(sum(precisionResults[c('site:day', 'site:day:run','error'),"VC"])))/res$Mean*100
      rownames(precisionResults)[grepl('^total$',rownames(precisionResults))] = 'reproducibility'
      rownames(precisionResults)[grepl('^site$',rownames(precisionResults))] = 'between laboratory'
      rownames(precisionResults)[grepl('^site\\:day$',rownames(precisionResults))] = 'within day'
      rownames(precisionResults)[grepl('^site\\:day\\:run$',rownames(precisionResults))] = 'within run'
    } else {
      rownames(precisionResults)[grepl('^total$',rownames(precisionResults))] = 'repetability'
    }
    rownames(precisionResults)[grepl('^day$',rownames(precisionResults))] = 'within day'
    rownames(precisionResults)[grepl('^day:run$',rownames(precisionResults))] = 'within run'
  }
  
  return(precisionResults)
  
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
