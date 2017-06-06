#' get a residual plot
#' @param df dataframe
#' @param .time time variable
#' @param .res residuals to plot
#' @param .ct central tendency
#' @details This function enables you to plot different types of residuals versus a time variable of choice
#' @examples \dontrun{gg_res(data, TAD, CWRES)}
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang !! enexpr eval_tidy quo
#' @export

gg_res <- function(df, .time, .res, .ct=TRUE){
  .time <- rlang::enexpr(.time)
  .res <- rlang::enexpr(.res)
  ggplot_output <- rlang::eval_tidy(rlang::quo(
    df %>% mutate(HIGHRES__=factor(ifelse(abs(!!.res)>2.5,1,0)))%>%
      ggplot(aes(x=!!.time,y=!!.res))+geom_point(aes(color=HIGHRES__))+
      geom_smooth(aes(y=abs(!!.res)),se=F,linetype=2)+
      geom_smooth(aes(y=-abs(!!.res)),se=F,linetype=2)+
      scale_color_manual(values=c("black","red"),name="",labels=c("not outlier","outlier"))+
      theme_bw()
  ))
  if(!.ct){
    return(ggplot_output)
  }
  return(ggplot_output+
           geom_smooth(se=F)
  )
}

