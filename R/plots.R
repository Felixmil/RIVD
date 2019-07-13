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
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' data(Glucose,package="VCA")
#' plotPrecision(Glucose, 'result','run', 'day')
plotPrecision <- function(df,  x, run, day){
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