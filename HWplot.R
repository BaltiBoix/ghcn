#HWplot.R

# from: http://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/ with some variations

library(ggplot2)
library(reshape)


HWplot<-function(hw_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
      
      forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
      
      
      for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  
                             dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
      for_values<-mutate(for_values,
                            time=as.Date(paste0(as.integer(time),round((time-as.integer(time))*365+1,0)), "%Y%j"))

            
      fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
      fitted_values<-mutate(fitted_values,
                       time=as.Date(paste0(as.integer(time),round((time-as.integer(time))*365+1,0)), "%Y%j"))

      
      actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
      actual_values<-mutate(actual_values, 
                            time=as.Date(paste0(as.integer(time),round((time-as.integer(time))*365+1,0)), "%Y%j"))

      
      graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
      graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
      graphset[is.na(graphset$dev),  ]$dev<-0
      
      graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  
                         for_values$value_forecast)
      
      
      graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
      
      p<-ggplot(graphset.melt,  aes(x=time,  y=value)) 
      p<-p + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) 
      p<-p + geom_line(aes(colour=variable), size=line.size) 
      p<-p + geom_vline(x=max(as.numeric(actual_values$time)),  lty=2) 
      p<-p + xlab('Time') + ylab('Value')
      p<-p + scale_x_date()
      p<-p + theme(legend.position='bottom', axis.text.x=element_text(angle=45), , hjust = 1) 
      p<-p + scale_colour_hue('')
      return(p)
      
}