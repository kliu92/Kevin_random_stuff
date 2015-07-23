library(ggplot2)
require(gridExtra)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#List of functions

#Date_format = '0000-00-00T00:00:00.000Z', or '0000-00-00 00:00:00.000Z' depending on where you pull the data from

#decile_leads
#competitor_decile_leads
#decile_opps
#competitor_decile_opps
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#quartile_leads
#competitor_quartile_leads
#quartile_opps
#competitor_quartile_opps
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#grade_bucket_leads
#competitor_grade_bucket_leads
#grade_bucket_opps
#competitor_grade_bucket_opps
#grade_bucket_contacts
#grade_bucket_accounts
#competitor_grade_bucket_accounts
#grade_bucket_accounts_new
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#scores_leads
#scores_opps
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#grade_leads
#grade_opps
#grade_contacts
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#marketo_quartile
#marketo_marketo_quartile
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#contacts_processing
#lead_to_opportunity_processing


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE - LEADS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
decile_leads <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/10), ]
  dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
  dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
  dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
  dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
  dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
  dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
  dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
  dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
  dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
  sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
  sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
  sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
  sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
  sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
  decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
  decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
  lift_chart = data.frame(c(0,decile_df$Cum_convert_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_convert_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Leads Lift Chart', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,20,30,40,50,60,70,80,90,100), limits=c(0,0.11))  
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,20,30,40,50,60,70,80,90,100), limits=c(0,1))  
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,20))    
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  #grid.arrange(ftop_conv_graph)
  #grid.arrange(agg_plot)
  #grid.arrange(agg_pie_chart)
  #grid.arrange(agg_plot_lift)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_convert_percent = NULL
  print(decile_df, row.names=FALSE)
  #print(lift_chart, row.names=F)
  
  png(paste(comment(df1),'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_decile_lift.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot_lift)
  dev.off()
  
  png(paste(comment(df1),'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_decile_stats.csv', sep='')
  write.table(decile_df, file_name, row.names=F, sep=',')
}
competitor_decile_leads <- function(df1, competitor, score_name, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/10), ]
  dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
  dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
  dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
  dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
  dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
  dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
  dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
  dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
  dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
  sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
  sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
  sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
  sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
  sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                         
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
  decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
  decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
  lift_chart = data.frame(c(0,decile_df$Cum_convert_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_convert_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Leads Lift Chart', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Decile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,20))
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Infer - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Leads'), x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_convert_percent = NULL
  print(decile_df, row.names=FALSE)
  
  new_name = paste(comment(df1), competitor, sep='_')
  png(paste(new_name,'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(new_name,'_decile_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot_lift)
  dev.off()
  
  png(paste(new_name,'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', new_name, '_decile_stats.csv', sep='')
  write.table(decile_df, file_name, row.names=F, sep=',')
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE - OPPORTUNITIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
decile_opps <- function(df1, isclosed_name, iswon_name, amount_name, closed_date_name, date_constraint){
  df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
  df = subset(df, df[ ,isclosed_name]==TRUE)
  
  dec_1 = df[0:round(nrow(df)/10), ]
  dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
  dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
  dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
  dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
  dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
  dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
  dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
  dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
  dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == TRUE))
  sum_5 = nrow(subset(dec_5, dec_5[ ,iswon_name] == TRUE))
  sum_6 = nrow(subset(dec_6, dec_6[ ,iswon_name] == TRUE))
  sum_7 = nrow(subset(dec_7, dec_7[ ,iswon_name] == TRUE))
  sum_8 = nrow(subset(dec_8, dec_8[ ,iswon_name] == TRUE))
  sum_9 = nrow(subset(dec_9, dec_9[ ,iswon_name] == TRUE))
  sum_10 = nrow(subset(dec_10, dec_10[ ,iswon_name] == TRUE))
  
  revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_5 = sum(subset(dec_5, dec_5[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_6 = sum(subset(dec_6, dec_6[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_7 = sum(subset(dec_7, dec_7[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_8 = sum(subset(dec_8, dec_8[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_9 = sum(subset(dec_9, dec_9[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_10 = sum(subset(dec_10, dec_10[ ,iswon_name] == TRUE)[ ,amount_name])
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  agg_rev2 = revenue_1+revenue_2
  agg_rev3 = agg_rev2+revenue_3
  agg_rev4 = agg_rev3+revenue_4
  agg_rev5 = agg_rev4+revenue_5
  agg_rev6 = agg_rev5+revenue_6
  agg_rev7 = agg_rev6+revenue_7
  agg_rev8 = agg_rev7+revenue_8 
  agg_rev9 = agg_rev8+revenue_9
  agg_rev10 = agg_rev9+revenue_10
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1, agg_sum2, agg_sum3, agg_sum4, agg_sum5, agg_sum6, agg_sum7, agg_sum8, agg_sum9, agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10),
                         c(revenue_1,revenue_2,revenue_3,revenue_4,revenue_5,revenue_6,revenue_7,revenue_8,revenue_9,revenue_10),
                         c(revenue_1/agg_rev10,agg_rev2/agg_rev10,agg_rev3/agg_rev10,agg_rev4/agg_rev10,agg_rev5/agg_rev10
                           ,agg_rev6/agg_rev10,agg_rev7/agg_rev10,agg_rev8/agg_rev10,agg_rev9/agg_rev10,agg_rev10/agg_rev10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Decile_agg_win_percent','Win_amount','Cum_revenue_percent')
  decile_df$Lift = (decile_df$Num_opps_won/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
  decile_df$Win_Percent = (decile_df$Num_opps_won/decile_df$Num_in_decile)*100
  decile_df$Cum_win_percent = decile_df$Cum_win_percent*100
  decile_df$Cum_revenue_percent = decile_df$Cum_revenue_percent*100
  decile_df$Decile_agg_win_percent = decile_df$Decile_agg_win_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  
  lift_chart = data.frame(c(0,decile_df$Cum_win_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_win_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Opportunities Lift Chart', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Win_Percent)
  if (max_height <= .1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  decile_df$positions = cumsum(decile_df$Decile_agg_win_percent) - decile_df$Decile_agg_win_percent/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_win_percent, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_win_percent))
  rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_win_percent = NULL
  
  png(paste(comment(df1),'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_decile_lift.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot_lift)
  dev.off()
  
  png(paste(comment(df1),'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  print(decile_df, row.names=FALSE)
}
competitor_decile_opps <- function(df1, competitor, score_name, iswon_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,'CloseDate'] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/10), ]
  dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
  dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
  dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
  dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
  dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
  dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
  dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
  dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
  dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, IsWon == TRUE))
  sum_2 = nrow(subset(dec_2, IsWon == TRUE))
  sum_3 = nrow(subset(dec_3, IsWon == TRUE))
  sum_4 = nrow(subset(dec_4, IsWon == TRUE))
  sum_5 = nrow(subset(dec_5, IsWon == TRUE))
  sum_6 = nrow(subset(dec_6, IsWon == TRUE))
  sum_7 = nrow(subset(dec_7, IsWon == TRUE))
  sum_8 = nrow(subset(dec_8, IsWon == TRUE))
  sum_9 = nrow(subset(dec_9, IsWon == TRUE))
  sum_10 = nrow(subset(dec_10, IsWon == TRUE))
  
  revenue_1 = sum(subset(dec_1, IsWon == TRUE)$Amount)
  revenue_2 = sum(subset(dec_2, IsWon == TRUE)$Amount)
  revenue_3 = sum(subset(dec_3, IsWon == TRUE)$Amount)
  revenue_4 = sum(subset(dec_4, IsWon == TRUE)$Amount)
  revenue_5 = sum(subset(dec_5, IsWon == TRUE)$Amount)
  revenue_6 = sum(subset(dec_6, IsWon == TRUE)$Amount)
  revenue_7 = sum(subset(dec_7, IsWon == TRUE)$Amount)
  revenue_8 = sum(subset(dec_8, IsWon == TRUE)$Amount)
  revenue_9 = sum(subset(dec_9, IsWon == TRUE)$Amount)
  revenue_10 = sum(subset(dec_10, IsWon == TRUE)$Amount)
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  agg_rev2 = revenue_1+revenue_2
  agg_rev3 = agg_rev2+revenue_3
  agg_rev4 = agg_rev3+revenue_4
  agg_rev5 = agg_rev4+revenue_5
  agg_rev6 = agg_rev5+revenue_6
  agg_rev7 = agg_rev6+revenue_7
  agg_rev8 = agg_rev7+revenue_8 
  agg_rev9 = agg_rev8+revenue_9
  agg_rev10 = agg_rev9+revenue_10
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1, agg_sum2, agg_sum3, agg_sum4, agg_sum5, agg_sum6, agg_sum7, agg_sum8, agg_sum9, agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10),
                         c(revenue_1,revenue_2,revenue_3,revenue_4,revenue_5,revenue_6,revenue_7,revenue_8,revenue_9,revenue_10),
                         c(revenue_1/agg_rev10,agg_rev2/agg_rev10,agg_rev3/agg_rev10,agg_rev4/agg_rev10,agg_rev5/agg_rev10
                           ,agg_rev6/agg_rev10,agg_rev7/agg_rev10,agg_rev8/agg_rev10,agg_rev9/agg_rev10,agg_rev10/agg_rev10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Decile_agg_win_percent','Win_amount','Cum_revenue_percent')
  decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
  decile_df$Win_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_win_percent = decile_df$Cum_win_percent*100
  decile_df$Cum_revenue_percent = decile_df$Cum_revenue_percent*100
  decile_df$Decile_agg_win_percent = decile_df$Decile_agg_win_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  
  lift_chart = data.frame(c(0,decile_df$Cum_win_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_win_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Opportunities Lift Chart', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Win_Percent)
  if (max_height <= .1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  decile_df$positions = cumsum(decile_df$Decile_agg_win_percent) - decile_df$Decile_agg_win_percent/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_win_percent, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_win_percent))
  rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_win_percent = NULL
  
  new_name = paste(comment(df1), competitor, sep='_')
  png(paste(new_name,'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(new_name,'_decile_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot_lift)
  dev.off()
  
  png(paste(new_name,'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  print(decile_df, row.names=FALSE)
}

decile_accts <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/10), ]
  dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
  dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
  dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
  dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
  dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
  dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
  dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
  dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
  dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
  sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
  sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
  sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
  sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
  sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_accts_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
  decile_df$Lift = (decile_df$Num_accts_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
  decile_df$Conversion_Percent = (decile_df$Num_accts_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
  lift_chart = data.frame(c(0,decile_df$Cum_convert_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_convert_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Accounts Lift Chart', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,20,30,40,50,60,70,80,90,100), limits=c(0,0.11))  
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,20,30,40,50,60,70,80,90,100), limits=c(0,1))  
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,20))    
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  #grid.arrange(ftop_conv_graph)
  #grid.arrange(agg_plot)
  #grid.arrange(agg_pie_chart)
  #grid.arrange(agg_plot_lift)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_convert_percent = NULL
  print(decile_df, row.names=FALSE)
  #print(lift_chart, row.names=F)
  
  png(paste(comment(df1),'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_decile_lift.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot_lift)
  dev.off()
  
  png(paste(comment(df1),'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_decile_stats.csv', sep='')
  write.table(decile_df, file_name, row.names=F, sep=',')
}
quartile_accts <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  total_conv = sum_1+sum_2+sum_3+sum_4
  
  agg_quart_1 = sum_1/total_conv
  agg_quart_2 = sum_2/total_conv
  agg_quart_3 = sum_3/total_conv
  agg_quart_4 = sum_4/total_conv
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_accts_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_accts_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Quartile - Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Accounts'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
  
  png(paste(comment(df1),'_quartile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_quartile_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_quartile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUARTILE - LEADS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quartile_leads <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]

  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))

  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  total_conv = sum_1+sum_2+sum_3+sum_4
  
  agg_quart_1 = sum_1/total_conv
  agg_quart_2 = sum_2/total_conv
  agg_quart_3 = sum_3/total_conv
  agg_quart_4 = sum_4/total_conv

  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                         c(sum_1,sum_2,sum_3,sum_4),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                         c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                         c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }

  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
  
  png(paste(comment(df1),'_quartile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_quartile_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_quartile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}
competitor_quartile_leads <- function(df1, competitor, score_name, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  total_conv = sum_1+sum_2+sum_3+sum_4
  
  agg_quart_1 = sum_1/total_conv
  agg_quart_2 = sum_2/total_conv
  agg_quart_3 = sum_3/total_conv
  agg_quart_4 = sum_4/total_conv
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)

  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor,' - Percentage of Converted Leads'), x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUARTILE - OPPORTUNITIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quartile_opps <- function(df1, isclosed_name, iswon_name, amount_name, date_constraint){
  df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,'clo'] >= date_constraint)
  df = subset(df, df[ ,isclosed_name]==TRUE)
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == TRUE))
    
  revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == TRUE)[ ,amount_name])
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  total_win = sum_1+sum_2+sum_3+sum_4
  
  agg_quartile_1 = sum_1/total_win
  agg_quartile_2 = sum_2/total_win
  agg_quartile_3 = sum_3/total_win
  agg_quartile_4 = sum_4/total_win
  
  agg_rev2 = revenue_1+revenue_2
  agg_rev3 = agg_rev2+revenue_3
  agg_rev4 = agg_rev3+revenue_4
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                         c(sum_1,sum_2,sum_3,sum_4),
                         c(sum_1, agg_sum2, agg_sum3, agg_sum4),
                         c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                         c(agg_quartile_1,agg_quartile_2,agg_quartile_3,agg_quartile_4),
                         c(revenue_1,revenue_2,revenue_3,revenue_4),
                         c(revenue_1/agg_rev4,agg_rev2/agg_rev4,agg_rev3/agg_rev4,agg_rev4/agg_rev4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Quartile_agg_win','Win_amount','Cum_revenue_percent')
  quartile_df$Win_Percent = (quartile_df$Num_opps_won/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_win_percent = quartile_df$Cum_win_percent*100
  quartile_df$Quartile_agg_win = quartile_df$Quartile_agg_win*100
  quartile_df$Cum_revenue_percent = quartile_df$Cum_revenue_percent*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Win_Percent)
  if (max_height <= .1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_win) - quartile_df$Quartile_agg_win/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_win))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_win)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Aggregate Revenue Percentage by Quartile (since model creation)', x='Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
  
  png(paste(comment(df1),'_quartile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df1),'_quartile_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_quartile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  png(paste(comment(df1),'_quartile_rev.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(rev_plot)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  file_name = paste(comment(df1), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}
competitor_quartile_opps <- function(df1, competitor, score_name, iswon_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,'CloseDate'] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, IsWon == TRUE))
  sum_2 = nrow(subset(dec_2, IsWon == TRUE))
  sum_3 = nrow(subset(dec_3, IsWon == TRUE))
  sum_4 = nrow(subset(dec_4, IsWon == TRUE))
  
  revenue_1 = sum(subset(dec_1, IsWon == TRUE)$Amount)
  revenue_2 = sum(subset(dec_2, IsWon == TRUE)$Amount)
  revenue_3 = sum(subset(dec_3, IsWon == TRUE)$Amount)
  revenue_4 = sum(subset(dec_4, IsWon == TRUE)$Amount)
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  total_win = sum_1+sum_2+sum_3+sum_4
  
  agg_quartile_1 = sum_1/total_win
  agg_quartile_2 = sum_2/total_win
  agg_quartile_3 = sum_3/total_win
  agg_quartile_4 = sum_4/total_win
  
  agg_rev2 = revenue_1+revenue_2
  agg_rev3 = agg_rev2+revenue_3
  agg_rev4 = agg_rev3+revenue_4
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1, agg_sum2, agg_sum3, agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quartile_1,agg_quartile_2,agg_quartile_3,agg_quartile_4),
                           c(revenue_1,revenue_2,revenue_3,revenue_4),
                           c(revenue_1/agg_rev4,agg_rev2/agg_rev4,agg_rev3/agg_rev4,agg_rev4/agg_rev4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Quartile_agg_win','Win_amount','Cum_revenue_percent')
  quartile_df$Win_Percent = (quartile_df$Num_opps_won/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_win_percent = quartile_df$Cum_win_percent*100
  quartile_df$Quartile_agg_win = quartile_df$Quartile_agg_win*100
  quartile_df$Cum_revenue_percent = quartile_df$Cum_revenue_percent*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Win_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.0),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_win) - quartile_df$Quartile_agg_win/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_win)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(decile_df, row.names=FALSE)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BUCKET - LEADS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_bucket_leads <- function(df1, score_name, isconverted_name, created_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.2),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_leads <- function(df1, competitor, score_name, isconverted_name,created_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 0)
  
  #grade_A = subset(df, df$infer2__Infer_Rating__c == 'A')
  #grade_B = subset(df, df$infer2__Infer_Rating__c == 'B')
  #grade_C = subset(df, df$infer2__Infer_Rating__c == 'C')
  #grade_D = subset(df, df$infer2__Infer_Rating__c == 'D')
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, IsConverted == TRUE))
  c_B = nrow(subset(grade_B, IsConverted == TRUE))
  c_C = nrow(subset(grade_C, IsConverted == TRUE))
  c_D = nrow(subset(grade_D, IsConverted == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= 1){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 25){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.1),size=4)+scale_y_continuous(breaks=c(0,.5,1,1.5,2,2.5,5,10,15,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Leads'), x='Stack-ranked Leads by Grades (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  grid.arrange(comp_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BUCKET - CONTACTS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_bucket_contacts <- function(df1, score_name, isconverted_name, created_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.2),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Converted Contacts', x='Percentage of Stack-ranked Contacts (Cumulative)', y='Percentage of Aggregate Contacts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Contacts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BUCKET - OPPORTUNITIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_bucket_opps <- function(df1, score_name, isclosed_name, iswon_name, amount_name, closed_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
  df = subset(df, df[ ,isclosed_name]==TRUE)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D) 
  
  c_A = nrow(subset(grade_A, grade_A[ ,iswon_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,iswon_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,iswon_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,iswon_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  revenue_A = sum(subset(grade_A, grade_A[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_B = sum(subset(grade_B, grade_B[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_C = sum(subset(grade_C, grade_C[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_D = sum(subset(grade_D, grade_D[ ,iswon_name] == TRUE)[ ,amount_name])
  
  agg_revB = revenue_A+revenue_B
  agg_revC = agg_revB+revenue_C
  agg_revD = agg_revC+revenue_D
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A,num_B,num_C,num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D),
                                 c(revenue_A,revenue_B,revenue_C,revenue_D),
                                 c(revenue_A,agg_revB,agg_revC,agg_revD),
                                 c(revenue_A/agg_revD,agg_revB/agg_revD,agg_revC/agg_revD,agg_revD/agg_revD))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_Range', 'Num_closed_in_bucket', 'Won', 'Win_Rate', 'Agg_Win_Percent','Bucket_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_Win_Percent = bucket_conversion$Agg_Win_Percent*100
  bucket_conversion$Bucket_agg_wr = bucket_conversion$Bucket_agg_wr*100
  bucket_conversion$Agg_Revenue_Percent = bucket_conversion$Agg_Revenue_Percent*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] = 0
  
  max_height = max(bucket_conversion$Win_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 24){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 50){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,55))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Win_Percent,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Opportunities Won', x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Win_Percent)), y=Agg_Win_Percent-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_wr) - bucket_conversion$Bucket_agg_wr/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_wr))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_wr)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  rev_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Revenue_Percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Aggregate Revenue Percentage by Grade', x='Ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Revenue_Percent)), y=Agg_Revenue_Percent-3), size=3)    
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_wr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  png(paste(comment(df1),'_rev.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(rev_plot)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_opps <- function(df1, competitor, score_name, iswon_name, closed_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 0)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D) 
  
  c_A = nrow(subset(grade_A, IsWon == TRUE))
  c_B = nrow(subset(grade_B, IsWon == TRUE))
  c_C = nrow(subset(grade_C, IsWon == TRUE))
  c_D = nrow(subset(grade_D, IsWon == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  revenue_A = sum(subset(grade_A, IsWon == TRUE)$Amount)
  revenue_B = sum(subset(grade_B, IsWon == TRUE)$Amount)
  revenue_C = sum(subset(grade_C, IsWon == TRUE)$Amount)
  revenue_D = sum(subset(grade_D, IsWon == TRUE)$Amount)
  
  agg_revB = revenue_A+revenue_B
  agg_revC = agg_revB+revenue_C
  agg_revD = agg_revC+revenue_D
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A,num_B,num_C,num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D),
                                 c(revenue_A,revenue_B,revenue_C,revenue_D),
                                 c(revenue_A,agg_revB,agg_revC,agg_revD),
                                 c(revenue_A/agg_revD,agg_revB/agg_revD,agg_revC/agg_revD,agg_revD/agg_revD))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_Range', 'Num_closed_in_bucket', 'Won', 'Win_Rate', 'Agg_Win_Percent','Bucket_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_Win_Percent = bucket_conversion$Agg_Win_Percent*100
  bucket_conversion$Bucket_agg_wr = bucket_conversion$Bucket_agg_wr*100
  bucket_conversion$Agg_Revenue_Percent = bucket_conversion$Agg_Revenue_Percent*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] = 0
  
  max_height = max(bucket_conversion$Win_Rate)
  if (max_height <= 0.1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Win_Percent,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title=paste(competitor, ' - Percentage of Opportunities Won'), x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Win_Percent)), y=Agg_Win_Percent-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_wr) - bucket_conversion$Bucket_agg_wr/2
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_wr))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_wr)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  rev_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Revenue_Percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Aggregate Revenue Percentage by Grade', x='Ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Revenue_Percent)), y=Agg_Revenue_Percent-3), size=3)    
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BUCKET - ACCOUNTS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_bucket_accounts <- function(df1, score_name, isconverted_name, date_field, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  #c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  #c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  #c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  #c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  c_A = sum(!is.na(grade_A[ ,isconverted_name]))
  c_B = sum(!is.na(grade_B[ ,isconverted_name]))
  c_C = sum(!is.na(grade_C[ ,isconverted_name]))
  c_D = sum(!is.na(grade_D[ ,isconverted_name]))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_accounts <- function(df1, competitor, score_name, isconverted_name, date_field, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 0)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  #c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  #c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  #c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  #c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  c_A = sum(!is.na(grade_A[ ,isconverted_name]))
  c_B = sum(!is.na(grade_B[ ,isconverted_name]))
  c_C = sum(!is.na(grade_C[ ,isconverted_name]))
  c_D = sum(!is.na(grade_D[ ,isconverted_name]))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Accounts'), x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Accounts'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
}

grade_bucket_accounts_new <- function(df1, score_name, isconverted_name, date_field, date_constraint, indicator_field, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint | is.na(df1[ ,indicator_field]))
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', max(df[ ,score_name]))
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  #c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  #c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  #c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  #c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  c_A = sum(!is.na(grade_A[ ,isconverted_name]))
  c_B = sum(!is.na(grade_B[ ,isconverted_name]))
  c_C = sum(!is.na(grade_C[ ,isconverted_name]))
  c_D = sum(!is.na(grade_D[ ,isconverted_name]))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BY SCORE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
scores_leads <- function(df1, score_name, isconverted_name, date_field, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  grade_10 = subset(df, df[ ,score_name] == 10)
  grade_9 = subset(df, df[ ,score_name] == 9)
  grade_8 = subset(df, df[ ,score_name] == 8)
  grade_7 = subset(df, df[ ,score_name] == 7)
  grade_6 = subset(df, df[ ,score_name] == 6)
  grade_5 = subset(df, df[ ,score_name] == 5)
  grade_4 = subset(df, df[ ,score_name] == 4)
  grade_3 = subset(df, df[ ,score_name] == 3)
  grade_2 = subset(df, df[ ,score_name] == 2)
  grade_1 = subset(df, df[ ,score_name] == 1)
  
  num_10 = nrow(grade_10)
  num_9 = nrow(grade_9)
  num_8 = nrow(grade_8)
  num_7 = nrow(grade_7)
  num_6 = nrow(grade_6)
  num_5 = nrow(grade_5)
  num_4 = nrow(grade_4)
  num_3 = nrow(grade_3)  
  num_2 = nrow(grade_2)
  num_1 = nrow(grade_1)
  
  c_10 = nrow(subset(grade_10, grade_10[ ,isconverted_name] == TRUE))
  c_9 = nrow(subset(grade_9, grade_9[ ,isconverted_name] == TRUE))
  c_8 = nrow(subset(grade_8, grade_8[ ,isconverted_name] == TRUE))
  c_7 = nrow(subset(grade_7, grade_7[ ,isconverted_name] == TRUE))
  c_6 = nrow(subset(grade_6, grade_6[ ,isconverted_name] == TRUE))
  c_5 = nrow(subset(grade_5, grade_5[ ,isconverted_name] == TRUE))
  c_4 = nrow(subset(grade_4, grade_4[ ,isconverted_name] == TRUE))
  c_3 = nrow(subset(grade_3, grade_3[ ,isconverted_name] == TRUE))
  c_2 = nrow(subset(grade_2, grade_2[ ,isconverted_name] == TRUE))
  c_1 = nrow(subset(grade_1, grade_1[ ,isconverted_name] == TRUE))  
  
  if (num_10 != 0){cr_10 = c_10/num_10}
  else {cr_10 = 0}
  if (num_9 != 0){cr_9 = c_9/num_9}
  else {cr_9 = 0}
  if (num_8 != 0){cr_8 = c_8/num_8}
  else {cr_8 = 0}
  if (num_7 != 0){cr_7 = c_7/num_7}
  else {cr_7 = 0}
  if (num_6 != 0){cr_6 = c_6/num_6}
  else {cr_6 = 0}
  if (num_5 != 0){cr_5 = c_5/num_5}
  else {cr_5 = 0}
  if (num_4 != 0){cr_4 = c_4/num_4}
  else {cr_4 = 0}
  if (num_3 != 0){cr_3 = c_3/num_3}
  else {cr_3 = 0}
  if (num_2 != 0){cr_2 = c_2/num_2}
  else {cr_2 = 0}
  if (num_1 != 0){cr_1 = c_1/num_1}
  else {cr_1 = 0}
  
  total_converted = c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1
  
  agg_10 = c_10/total_converted
  agg_9 = (c_10+c_9)/total_converted
  agg_8 = (c_10+c_9+c_8)/total_converted
  agg_7 = (c_10+c_9+c_8+c_7)/total_converted
  agg_6 = (c_10+c_9+c_8+c_7+c_6)/total_converted
  agg_5 = (c_10+c_9+c_8+c_7+c_6+c_5)/total_converted
  agg_4 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4)/total_converted
  agg_3 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3)/total_converted
  agg_2 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2)/total_converted
  agg_1 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1)/total_converted
  
  agg_bucket_10 = c_10/total_converted
  agg_bucket_9 = c_9/total_converted
  agg_bucket_8 = c_8/total_converted
  agg_bucket_7 = c_7/total_converted
  agg_bucket_6 = c_6/total_converted
  agg_bucket_5 = c_5/total_converted
  agg_bucket_4 = c_4/total_converted
  agg_bucket_3 = c_3/total_converted
  agg_bucket_2 = c_2/total_converted
  agg_bucket_1 = c_1/total_converted
  
  score_conv_df = data.frame(c('10','9','8','7','6','5','4','3','2','1'),
                            c(num_10, num_9, num_8, num_7, num_6, num_5, num_4, num_3, num_2, num_1),
                            c(c_10,c_9,c_8,c_7,c_6,c_5,c_4,c_3,c_2,c_1),
                            c(cr_10,cr_9,cr_8,cr_7,cr_6,cr_5,cr_4,cr_3,cr_2,cr_1),
                            c(agg_10,agg_9,agg_8,agg_7,agg_6,agg_5,agg_4,agg_3,agg_2,agg_1),
                            c(agg_bucket_10,agg_bucket_9,agg_bucket_8,agg_bucket_7,agg_bucket_6,agg_bucket_5,agg_bucket_4,agg_bucket_3,agg_bucket_2,agg_bucket_1))
  colnames(score_conv_df) <- c('Score', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Conversion_Percent', 'Score_agg_cr')
  score_conv_df$Conversion_Rate = score_conv_df$Conversion_Rate*100
  score_conv_df$Agg_Conversion_Percent = score_conv_df$Agg_Conversion_Percent*100
  score_conv_df$Score_agg_cr = score_conv_df$Score_agg_cr*100
  score_conv_df$Score_levels <- factor(score_conv_df$Score, c('10','9','8','7','6','5','4','3','2','1'))
  score_conv_df[is.na(score_conv_df)] = 0
  
  #score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Leads Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,.5,10,20,30,40,50,60,70,80,90,100), limits=c(0,1))
  max_height = max(score_conv_df$Conversion_Rate)
  if (max_height <= .1){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,.11))
  } else if (max_height <= 1){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 24){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 50){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,55))
  } else if (max_height <= 70){
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    score_graph = ggplot(score_conv_df, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  score_conv_df$positions = cumsum(score_conv_df$Score_agg_cr) - score_conv_df$Score_agg_cr/2
  agg_pie_chart = ggplot(score_conv_df, aes(x=factor(1), y=Score_agg_cr, fill=Score_levels))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Leads Converted')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=score_conv_df$positions, labels=sprintf('%.2f%%', score_conv_df$Score_agg_cr))
  agg_plot = ggplot(score_conv_df, aes(x=Score_levels, y=Agg_Conversion_Percent,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Leads Converted', x='Scores of Leads (Cumulative)', y='Percentage of Aggregate Leads Converted')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Conversion_Percent)), y=Agg_Conversion_Percent-3), size=3)
  
  score_conv_df$positions <- NULL
  score_conv_df$Score_levels <- NULL
  print(score_conv_df)
  grid.arrange(score_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  
  png(paste(comment(df1),'_score_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(score_graph)
  plot(score_graph)
  dev.off()
  
  png(paste(comment(df1),'_score_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_score_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  
  file_name = paste(comment(df1), '_score_stats.csv', sep='')
  write.table(score_conv_df, file_name, row.names=F, sep=',')
}
scores_opps <- function(df1, score_name, isclosed_name, iswon_name, amount_name, date_field, date_constraint){
  df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  df = subset(df, df[ ,isclosed_name]==TRUE)
  grade_10 = subset(df, df[ ,score_name] == 10)
  grade_9 = subset(df, df[ ,score_name] == 9)
  grade_8 = subset(df, df[ ,score_name] == 8)
  grade_7 = subset(df, df[ ,score_name] == 7)
  grade_6 = subset(df, df[ ,score_name] == 6)
  grade_5 = subset(df, df[ ,score_name] == 5)
  grade_4 = subset(df, df[ ,score_name] == 4)
  grade_3 = subset(df, df[ ,score_name] == 3)
  grade_2 = subset(df, df[ ,score_name] == 2)
  grade_1 = subset(df, df[ ,score_name] == 1)
  
  num_10 = nrow(grade_10)
  num_9 = nrow(grade_9)
  num_8 = nrow(grade_8)
  num_7 = nrow(grade_7)
  num_6 = nrow(grade_6)
  num_5 = nrow(grade_5)
  num_4 = nrow(grade_4)
  num_3 = nrow(grade_3)  
  num_2 = nrow(grade_2)
  num_1 = nrow(grade_1)
  
  c_10 = nrow(subset(grade_10, grade_10[ ,iswon_name] == TRUE))
  c_9 = nrow(subset(grade_9, grade_9[ ,iswon_name] == TRUE))
  c_8 = nrow(subset(grade_8, grade_8[ ,iswon_name] == TRUE))
  c_7 = nrow(subset(grade_7, grade_7[ ,iswon_name] == TRUE))
  c_6 = nrow(subset(grade_6, grade_6[ ,iswon_name] == TRUE))
  c_5 = nrow(subset(grade_5, grade_5[ ,iswon_name] == TRUE))
  c_4 = nrow(subset(grade_4, grade_4[ ,iswon_name] == TRUE))
  c_3 = nrow(subset(grade_3, grade_3[ ,iswon_name] == TRUE))
  c_2 = nrow(subset(grade_2, grade_2[ ,iswon_name] == TRUE))
  c_1 = nrow(subset(grade_1, grade_1[ ,iswon_name] == TRUE))  
  
  if (num_10 != 0){cr_10 = c_10/num_10}
  else {cr_10 = 0}
  if (num_9 != 0){cr_9 = c_9/num_9}
  else {cr_9 = 0}
  if (num_8 != 0){cr_8 = c_8/num_8}
  else {cr_8 = 0}
  if (num_7 != 0){cr_7 = c_7/num_7}
  else {cr_7 = 0}
  if (num_6 != 0){cr_6 = c_6/num_6}
  else {cr_6 = 0}
  if (num_5 != 0){cr_5 = c_5/num_5}
  else {cr_5 = 0}
  if (num_4 != 0){cr_4 = c_4/num_4}
  else {cr_4 = 0}
  if (num_3 != 0){cr_3 = c_3/num_3}
  else {cr_3 = 0}
  if (num_2 != 0){cr_2 = c_2/num_2}
  else {cr_2 = 0}
  if (num_1 != 0){cr_1 = c_1/num_1}
  else {cr_1 = 0}
  
  total_won = c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1
  
  agg_10 = c_10/total_won
  agg_9 = (c_10+c_9)/total_won
  agg_8 = (c_10+c_9+c_8)/total_won
  agg_7 = (c_10+c_9+c_8+c_7)/total_won
  agg_6 = (c_10+c_9+c_8+c_7+c_6)/total_won
  agg_5 = (c_10+c_9+c_8+c_7+c_6+c_5)/total_won
  agg_4 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4)/total_won
  agg_3 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3)/total_won
  agg_2 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2)/total_won
  agg_1 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1)/total_won
  
  agg_bucket_10 = c_10/total_won
  agg_bucket_9 = c_9/total_won
  agg_bucket_8 = c_8/total_won
  agg_bucket_7 = c_7/total_won
  agg_bucket_6 = c_6/total_won
  agg_bucket_5 = c_5/total_won
  agg_bucket_4 = c_4/total_won
  agg_bucket_3 = c_3/total_won
  agg_bucket_2 = c_2/total_won
  agg_bucket_1 = c_1/total_won
  
  revenue_10 = sum(subset(grade_10, grade_10[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_9 = sum(subset(grade_9, grade_9[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_8 = sum(subset(grade_8, grade_8[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_7 = sum(subset(grade_7, grade_7[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_6 = sum(subset(grade_6, grade_6[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_5 = sum(subset(grade_5, grade_5[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_4 = sum(subset(grade_4, grade_4[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_3 = sum(subset(grade_3, grade_3[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_2 = sum(subset(grade_2, grade_2[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_1 = sum(subset(grade_1, grade_1[ ,iswon_name] == TRUE)[ ,amount_name])
  
  agg_rev9 = revenue_10+revenue_9
  agg_rev8 = agg_rev9+revenue_8
  agg_rev7 = agg_rev8+revenue_7
  agg_rev6 = agg_rev7+revenue_6
  agg_rev5 = agg_rev6+revenue_5
  agg_rev4 = agg_rev5+revenue_4
  agg_rev3 = agg_rev4+revenue_3
  agg_rev2 = agg_rev3+revenue_2
  agg_rev1 = agg_rev2+revenue_1
  
  score_win_df = data.frame(c('10','9','8','7','6','5','4','3','2','1'),
                            c(num_10, num_9, num_8, num_7, num_6, num_5, num_4, num_3, num_2, num_1),
                            c(c_10,c_9,c_8,c_7,c_6,c_5,c_4,c_3,c_2,c_1),
                            c(cr_10,cr_9,cr_8,cr_7,cr_6,cr_5,cr_4,cr_3,cr_2,cr_1),
                            c(agg_10,agg_9,agg_8,agg_7,agg_6,agg_5,agg_4,agg_3,agg_2,agg_1),
                            c(agg_bucket_10,agg_bucket_9,agg_bucket_8,agg_bucket_7,agg_bucket_6,agg_bucket_5,agg_bucket_4,agg_bucket_3,agg_bucket_2,agg_bucket_1),
                            c(revenue_10,revenue_9,revenue_8,revenue_7,revenue_6,revenue_5,revenue_4,revenue_3,revenue_2,revenue_1),
                            c(revenue_10,agg_rev9,agg_rev8,agg_rev7,agg_rev6,agg_rev5,agg_rev4,agg_rev3,agg_rev2,agg_rev1),
                            c(revenue_10/agg_rev1,agg_rev9/agg_rev1,agg_rev8/agg_rev1,agg_rev7/agg_rev1,agg_rev6/agg_rev1,agg_rev5/agg_rev1,agg_rev4/agg_rev1,agg_rev3/agg_rev1,agg_rev2/agg_rev1,agg_rev1/agg_rev1))
  colnames(score_win_df) <- c('Score', 'Num_in_bucket', 'Won', 'Win_Rate', 'Agg_Win_Percent', 'Score_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  score_win_df$Win_Rate = score_win_df$Win_Rate*100
  score_win_df$Agg_Win_Percent = score_win_df$Agg_Win_Percent*100
  score_win_df$Score_agg_wr = score_win_df$Score_agg_wr*100
  score_win_df$Agg_Revenue_Percent = score_win_df$Agg_Revenue_Percent*100
  score_win_df$Score_levels <- factor(score_win_df$Score, c('10','9','8','7','6','5','4','3','2','1'))
  score_win_df[is.na(score_win_df)] = 0
  
  #score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,.5,10,20,30,40,50,60,70,80,90,100), limits=c(0,1))
  max_height = max(score_win_df$Win_Rate)
  if (max_height <= .1){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,.11))
  } else if (max_height <= 1){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 24){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 50){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,55))
  } else if (max_height <= 70){
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    score_graph = ggplot(score_win_df, aes(x=Score_levels, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rates', x='Score', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  score_win_df$positions = cumsum(score_win_df$Score_agg_wr) - score_win_df$Score_agg_wr/2
  agg_pie_chart = ggplot(score_win_df, aes(x=factor(1), y=Score_agg_wr, fill=Score_levels))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=score_win_df$positions, labels=sprintf('%.2f%%', score_win_df$Score_agg_wr))
  agg_plot = ggplot(score_win_df, aes(x=Score_levels, y=Agg_Win_Percent,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Opportunities Won', x='Percentage of Opportunities (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Win_Percent)), y=Agg_Win_Percent-3), size=3)
  rev_plot = ggplot(score_win_df, aes(x=Score_levels, y=Agg_Revenue_Percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Aggregate Revenue Percentage by Score', x='Ranked Opportunities by Score (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Revenue_Percent)), y=Agg_Revenue_Percent-3), size=3)    
  
  score_win_df$positions <- NULL
  score_win_df$Score_levels <- NULL
  print(score_win_df)
  grid.arrange(score_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  
  png(paste(comment(df1),'_score_wr.png', sep=''), width = 500, height = 500)
  #grid.arrange(score_graph)
  plot(score_graph)
  dev.off()
  
  png(paste(comment(df1),'_score_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_score_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  png(paste(comment(df1),'_score_rev.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(rev_plot)
  dev.off()
  
  file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df1), '_score_stats.csv', sep='')
  write.table(score_win_df, file_name, row.names=F, sep=',')
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BY GRADE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_leads <- function(df1, grade_name, isconverted_name, date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,grade_name] == 'A')
  grade_B = subset(df, df[ ,grade_name] == 'B')
  grade_C = subset(df, df[ ,grade_name] == 'C')
  grade_D = subset(df, df[ ,grade_name] == 'D')
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = 'A'
  range_B = 'B'
  range_C = 'C'
  range_D = 'D'
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.2),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
grade_opps <- function(df1, grade_name, isclosed_name, iswon_name, amount_name, date_name, date_constraint){
  df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_name] >= date_constraint)
  df = subset(df, df[ ,isclosed_name]==TRUE)
  
  grade_A = subset(df, df[ ,grade_name] == 'A')
  grade_B = subset(df, df[ ,grade_name] == 'B')
  grade_C = subset(df, df[ ,grade_name] == 'C')
  grade_D = subset(df, df[ ,grade_name] == 'D')
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = 'A'
  range_B = 'B'
  range_C = 'C'
  range_D = 'D'
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D) 
  
  c_A = nrow(subset(grade_A, grade_A[ ,iswon_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,iswon_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,iswon_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,iswon_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  revenue_A = sum(subset(grade_A, grade_A[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_B = sum(subset(grade_B, grade_B[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_C = sum(subset(grade_C, grade_C[ ,iswon_name] == TRUE)[ ,amount_name])
  revenue_D = sum(subset(grade_D, grade_D[ ,iswon_name] == TRUE)[ ,amount_name])
  
  agg_revB = revenue_A+revenue_B
  agg_revC = agg_revB+revenue_C
  agg_revD = agg_revC+revenue_D
  
  bucket_conversion = data.frame(c(range_A, range_B, range_C, range_D),
                                 c(num_A,num_B,num_C,num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D),
                                 c(revenue_A,revenue_B,revenue_C,revenue_D),
                                 c(revenue_A,agg_revB,agg_revC,agg_revD),
                                 c(revenue_A/agg_revD,agg_revB/agg_revD,agg_revC/agg_revD,agg_revD/agg_revD))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Num_closed_in_bucket', 'Won', 'Win_Rate', 'Agg_Win_Percent','Bucket_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_Win_Percent = bucket_conversion$Agg_Win_Percent*100
  bucket_conversion$Bucket_agg_wr = bucket_conversion$Bucket_agg_wr*100
  bucket_conversion$Agg_Revenue_Percent = bucket_conversion$Agg_Revenue_Percent*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] = 0
  
  max_height = max(bucket_conversion$Win_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 24){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 50){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,55))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Win_Percent,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Opportunities Won', x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Win_Percent)), y=Agg_Win_Percent-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_wr) - bucket_conversion$Bucket_agg_wr/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_wr))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_wr)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  rev_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Revenue_Percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Aggregate Revenue Percentage by Grade', x='Ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Revenue_Percent)), y=Agg_Revenue_Percent-3), size=3)    
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_wr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  png(paste(comment(df1),'_rev.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(rev_plot)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
grade_contacts <- function(df1, grade_name, isconverted_name, date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,grade_name] == 'A')
  grade_B = subset(df, df[ ,grade_name] == 'B')
  grade_C = subset(df, df[ ,grade_name] == 'C')
  grade_D = subset(df, df[ ,grade_name] == 'D')
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = 'A'
  range_B = 'B'
  range_C = 'C'
  range_D = 'D'
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == TRUE))
  c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == TRUE))
  c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == TRUE))
  c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == TRUE))
  
  cr_A = c_A/num_A
  cr_B = c_B/num_B
  cr_C = c_C/num_C
  cr_D = c_D/num_D
  total_conv = c_A+c_B+c_C+c_D
  
  agg_A = c_A/total_conv
  agg_B = (c_A+c_B)/total_conv
  agg_C = (c_A+c_B+c_C)/total_conv
  agg_D = (c_A+c_B+c_C+c_D)/total_conv
  
  agg_bucket_A = c_A/total_conv
  agg_bucket_B = c_B/total_conv
  agg_bucket_C = c_C/total_conv
  agg_bucket_D = c_D/total_conv
  
  bucket_conversion = data.frame(c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_bucket_A,agg_bucket_B,agg_bucket_C,agg_bucket_D))
  colnames(bucket_conversion) <- c('Grade', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Bucket_agg_converted')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
  bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade)
  bucket_conversion[is.na(bucket_conversion)] <- 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= .1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11), limits=c(0,0.11))
  } else if (max_height <= 1){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), limits=c(0,1))
  } else if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.2),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Contact Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Converted Contacts', x='Percentage of Stack-ranked Contacts (Cumulative)', y='Percentage of Aggregate Contacts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Contacts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  
  png(paste(comment(df1),'_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_bucket_graph)
  plot(ftop_bucket_graph)
  dev.off()
  
  png(paste(comment(df1),'_agg.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df1),'_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_stats.csv', sep='')
  write.table(bucket_conversion, file_name, row.names=F, sep=',')
}


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Marketo Quartile 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
marketo_quartile <- function(df){
  #df = subset(df, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled'))
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  num_q1 = nrow(dec_1)
  num_q2 = nrow(dec_2)
  num_q3 = nrow(dec_3)
  num_q4 = nrow(dec_4)
  
  #num_q1 = nrow(subset(dec_1, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q2 = nrow(subset(dec_2, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q3 = nrow(subset(dec_3, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q4 = nrow(subset(dec_4, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  
  #sum_1 = nrow(subset(dec_1, IsConverted == 'true'))
  #sum_2 = nrow(subset(dec_2, IsConverted == 'true'))
  #sum_3 = nrow(subset(dec_3, IsConverted == 'true'))
  #sum_4 = nrow(subset(dec_4, IsConverted == 'true'))
  
  sum_1 = nrow(subset(dec_1, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_1, Status == 'Sales Lead – Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_2, Status == 'Sales Lead – Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_3, Status == 'Sales Lead – Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_4, Status == 'Sales Lead – Potential Opportunity'))
  total_sum = sum_1+sum_2+sum_3+sum_4
  
  #sum_1 = nrow(subset(dec_1, Status == 'Recycled'))#+nrow(subset(dec_1, Status == 'Sales Lead – Potential Opportunity'))
  #sum_2 = nrow(subset(dec_2, Status == 'Recycled'))#+nrow(subset(dec_2, Status == 'Sales Lead – Potential Opportunity'))
  #sum_3 = nrow(subset(dec_3, Status == 'Recycled'))#+nrow(subset(dec_3, Status == 'Sales Lead – Potential Opportunity'))
  #sum_4 = nrow(subset(dec_4, Status == 'Recycled'))#+nrow(subset(dec_4, Status == 'Sales Lead – Potential Opportunity'))
  #total_sum = sum_1+sum_2+sum_3+sum_4
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  agg_quart1 = sum_1/total_sum
  agg_quart2 = sum_2/total_sum
  agg_quart3 = sum_3/total_sum
  agg_quart4 = sum_4/total_sum
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(num_q1,num_q2,num_q3,num_q4),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quart1, agg_quart2, agg_quart3, agg_quart4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100 
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 2){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.10),size=4)+scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,2))
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,15,20,25,30,35,40,45,50), limits=c(0,10.5))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,15,20,25,30,35,40,45,50), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100)
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25,30,35,40,45,50), limits=c(0,10))
  agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$positions = NULL
  print(quartile_df)
  
  png(paste(comment(df),'_quartile_cr.png', sep=''), width = 500, height = 500)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df),'_quartile_agg.png', sep=''), width = 500, height = 500)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df),'_quartile_pie.png', sep=''), width = 500, height = 500)
  plot(agg_pie_chart)
  dev.off()
  
  file_name = paste(comment(df), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}
marketo_marketo_quartile <- function(df){
  #df = subset(df, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled'))
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  num_q1 = nrow(dec_1)
  num_q2 = nrow(dec_2)
  num_q3 = nrow(dec_3)
  num_q4 = nrow(dec_4)
  
  #num_q1 = nrow(subset(dec_1, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q2 = nrow(subset(dec_2, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q3 = nrow(subset(dec_3, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #num_q4 = nrow(subset(dec_4, Status %in% c('Opportunity','Sales Lead – Potential Opportunity','Disqualified (Mktg)','Disqualified (Sales)','Recycled')))
  #sum_1 = nrow(subset(dec_1, IsConverted == 'true'))
  #sum_2 = nrow(subset(dec_2, IsConverted == 'true'))
  #sum_3 = nrow(subset(dec_3, IsConverted == 'true'))
  #sum_4 = nrow(subset(dec_4, IsConverted == 'true'))
  
  sum_1 = nrow(subset(dec_1, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_1, Status == 'Sales Lead – Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_2, Status == 'Sales Lead – Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_3, Status == 'Sales Lead – Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status %in% c('Opportunity','Sales Lead – Potential Opportunity')))#+nrow(subset(dec_4, Status == 'Sales Lead – Potential Opportunity'))
  total_sum = sum_1+sum_2+sum_3+sum_4
  
  #sum_1 = nrow(subset(dec_1, Status == 'Recycled'))#+nrow(subset(dec_1, Status == 'Sales Lead – Potential Opportunity'))
  #sum_2 = nrow(subset(dec_2, Status == 'Recycled'))#+nrow(subset(dec_2, Status == 'Sales Lead – Potential Opportunity'))
  #sum_3 = nrow(subset(dec_3, Status == 'Recycled'))#+nrow(subset(dec_3, Status == 'Sales Lead – Potential Opportunity'))
  #sum_4 = nrow(subset(dec_4, Status == 'Recycled'))#+nrow(subset(dec_4, Status == 'Sales Lead – Potential Opportunity'))
  #total_sum = sum_1+sum_2+sum_3+sum_4
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  agg_quart1 = sum_1/total_sum
  agg_quart2 = sum_2/total_sum
  agg_quart3 = sum_3/total_sum
  agg_quart4 = sum_4/total_sum
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(num_q1,num_q2,num_q3,num_q4),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quart1, agg_quart2, agg_quart3, agg_quart4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100 
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
  
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 2){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.10),size=4)+scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,2))
  } else if (max_height <= 5){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.10),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))
  } else if (max_height <= 10){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,15,20,25,30,35,40,45,50), limits=c(0,10.5))  
  } else if (max_height <= 25){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.50),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  } else if (max_height <= 100) {
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  }
  
  #marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25,30,35,40,45,50), limits=c(0,10))
  marketo_agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='mediumpurple4')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Marketo - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  marketo_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Marketo - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('mediumpurple4','mediumpurple3','mediumpurple1','lavender'))
  #marketo_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Marketo - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  grid.arrange(marketo_conv_graph)
  grid.arrange(marketo_agg_plot)
  grid.arrange(marketo_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$positions = NULL
  print(quartile_df)
  
  png(paste(comment(df),'_marketo_quartile_cr.png', sep=''), width = 500, height = 500)
  plot(marketo_conv_graph)
  dev.off()
  
  png(paste(comment(df),'_marketo_quartile_agg.png', sep=''), width = 500, height = 500)
  plot(marketo_agg_plot)
  dev.off()
  
  png(paste(comment(df),'_marketo_quartile_pie.png', sep=''), width = 500, height = 500)
  plot(marketo_pie_chart)
  dev.off()
  
  file_name = paste(comment(df), '_marketo_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}


#MODE FUNCTION
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#PROCESS CONTACTS FILES
contacts_processing <- function(df1, df2, df1_id, df2_id, conversion_indicator, df3=NA, df3_id=NA, df2_df3_connection=NA){
  df2$Id = NULL
  df2$id = NULL
  merge_df = merge(df1, df2, by.x=df1_id, by.y=df2_id, all.x=T)
  if (!is.na(df3_id)){
    merge_df = merge(merge_df, df3, by.x=df2_df3_connection, by.y=df3_id, all.x=T)
  }
  merge_df = merge_df[!duplicated(merge_df[,df1_id]),]
  merge_df$converted = !is.na(merge_df[,conversion_indicator])
  merge_df
}

#LEAD TO OPP
lead_to_opportunity_processing <- function(leads_df, leads_id, opps_df, opps_id, sfdc_leads_df, sfdc_leads_id, sfdc_contact_id, contact_role, cr_contact_id, cr_opportunity_id){
  contact_opp = merge(contact_role, opps_df, by.x=cr_contact_id, by.y=opps_id, all.x=T)
  lead_opp = merge(sfdc_leads_df, contact_opp, by.x=sfdc_contact_id, by.y=cr_contact_id, all.x=T)
  lead_opp$opp_conv = (!is.na(lead_opp[,cr_opportunity_id]))
  lead_opp = merge(leads_df, lead_opp, by.x=leads_id, by.y=sfdc_leads_id)
  lead_opp
}






#NEED TO SET CORRECT WORKING DRIVE EACH WEEK TO PREVENT OVERWRITING PREVIOUS GRAPHS
setwd("/Users/fliptop/Desktop/Customer_Stats/Weekly_ROI_stats/07_13_2015/")
#options(digits=8)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blackbaud_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_leads.csv', header=T, stringsAsFactor=F)
blackbaud_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_opportunities.csv', header=T, stringsAsFactor=F)
blackbaud_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_accounts.csv', header=T, stringsAsFactor=F)
blackbaud_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/acct_opp_query.csv', header=T, stringsAsFactor=F)
blackbaud_accts = merge(blackbaud_accts, blackbaud_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
blackbaud_accts = blackbaud_accts[!duplicated(blackbaud_accts$rec), ]

comment(blackbaud_leads) = 'blackbaud_leads'; comment(blackbaud_opps) = 'blackbaud_opps'; comment(blackbaud_accts) = 'blackbaud_accts'

grade_bucket_leads(blackbaud_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-24T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(blackbaud_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-05-27T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(blackbaud_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 6, 3)

comment(blackbaud_accts) = 'blackbaud_accts_new_opps'
grade_bucket_accounts_new(blackbaud_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-05-18 00:00:00', 'closed_date', 9, 6, 3)

blackbaud_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_contacts.csv', header=T, stringsAsFactor=F)
blackbaud_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/contact_role.csv', header=T, stringsAsFactor=F)

blackbaud_contacts_processing = contacts_processing(blackbaud_contact, blackbaud_contact_role, 'rec', 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(blackbaud_contacts_processing) = 'blackbaud_contacts'; grade_bucket_contacts(blackbaud_contacts_processing, 'ssv3.2.sco', 'converted', 'cre', '2015-05-18T00:00:00.000Z', 9,6,3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
on24_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/ON24_leads.csv', header=T, stringsAsFactor=F)
on24_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/ON24_opportunities.csv', header=T, stringsAsFactor=F)
on24_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/ON24_contacts.csv', header=T, stringsAsFactor=F)
on24_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/contact_role.csv', header=T, stringsAsFactor=F)
on24_constrain_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/on24_sfdc_opps.csv', header=T, stringsAsFactor=F)
on24_merge_constrain = merge(on24_opps, on24_constrain_opps, by.x='rec', by.y='Id')
on24_merge_constrain = subset(on24_merge_constrain, on24_merge_constrain$IsClosed=='true')
comment(on24_leads) = 'on24_leads'; comment(on24_opps) = 'on24_opps'
comment(on24_merge_constrain) = 'on24_constrained_opps'

grade_bucket_leads(on24_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-03-14T00:00:00.000Z', 7, 5, 3)
grade_bucket_opps(on24_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-03-14T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(on24_merge_constrain, 'ssv3.100001.sco', 'IsClosed', 'IsWon', 'amo', 'clo', '2015-03-14T00:00:00.000Z', 9, 6, 3)

on24_contacts_processing = contacts_processing(on24_contact, on24_contact_role, 'rec', 'salesforce_contact_id', 'IsClosed', on24_constrain_opps, 'Id', 'salesforce_opportunity_id')
comment(on24_contacts_processing) = 'on24_contacts'
grade_bucket_contacts(on24_contacts_processing, 'ssv3.2.sco', 'converted', 'cre', '2015-03-13T00:00:00.000Z', 9,6,3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
csod_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_leads.csv', header=T, stringsAsFactor=F)  
csod_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_opportunities.csv', header=T, stringsAsFactor=F)  
#csod_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_sfdc_opps.csv', header=T, stringsAsFactor=F)
#csod_merge = merge(csod_opps, csod_sfdc, by.x='rec', by.y='Id')
csod_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_accounts.csv', header=T, stringsAsFactor=F)
csod_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/acct_opp_query.csv', header=T, stringsAsFactor=F)
csod_accts = merge(csod_accts, csod_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
csod_accts = csod_accts[!duplicated(csod_accts$rec), ]
comment(csod_leads) = 'csod_leads'; comment(csod_opps) = 'csod_opps'; comment(csod_accts) = 'csod_accts'

grade_bucket_leads(csod_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-04-07T00:00:00.000Z', 10,7,3)
#csod_opps = subset(csod_opps, csod_opps$clo < '2015-05-19T00:00:00.000Z')
grade_bucket_opps(csod_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-23T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(csod_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 7, 6, 3)
#comment(csod_merge) = ("csod_opps_v3");grade_bucket_opps(csod_merge, 'ssv3.100001.sco', 'IsWon', 'amo', 'clo', '2015-04-07T00:00:00.000Z', 9, 6, 3)

comment(csod_accts) = 'csod_accts_new_opps'
grade_bucket_accounts_new(csod_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-04-21 00:00:00.000Z', 'closed_date', 7, 6, 3)

csod_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_sfdc_leads.csv', header=T, stringsAsFactor=F)
comment(csod_leads) = 'csod_leads_sfdc'; grade_leads(csod_leads, 'Ftop__SpendGrade__c', 'IsConverted', 'CreatedDate', '2015-04-07T00:00:00.000Z')

csod_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_sfdc_contacts.csv', header=T, stringsAsFactor=F)
csod_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/sfdc_contact_role.csv', header=T, stringsAsFactor=F)

csod_contacts_processing = contacts_processing(csod_contact, csod_contact_role, 'Id', 'ContactId', 'OpportunityId')
comment(csod_contacts_processing) = 'csod_contacts'; grade_contacts(csod_contacts_processing, 'Ftop__SpendGrade__c', 'converted', 'CreatedDate', '2015-04-21T00:00:00.000Z')


csod_opps_2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_opportunities_2.csv', header=T, stringsAsFactor=F)
grade_bucket_opps(csod_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-23T00:00:00.000Z', 9, 6, 3)





csod_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/csod_sfdc_opps_1.csv', header=T, stringsAsFactor=F)
#csod_sfdc = subset(csod_sfdc, (csod_sfdc$IsClosed=='true'))
csod_sfdc$created_date_1 <- as.Date(strptime(csod_sfdc$CreatedDate, "%Y-%m-%d"))
csod_sfdc$close_date_1 <- as.Date(strptime(csod_sfdc$CloseDate, "%Y-%m-%d"))
csod_sfdc$cycle_length <- csod_sfdc$close_date_1 - csod_sfdc$created_date_1
csod_sfdc$cycle_length <- as.numeric(csod_sfdc$cycle_length)
#csod_sfdc = subset(csod_sfdc, (csod_sfdc$close_date_1 >= '2015-02-01')&(csod_sfdc$close_date_1 <= '2015-06-1'))

csod_sfdc = subset(csod_sfdc, (csod_sfdc$IsClosed=='true')&!is.na(csod_sfdc$Ftop__SpendScore__c))#&(csod_sfdc$CloseDate>='2015-04-23T00:00:00.000Z')&(csod_sfdc$CloseDate<='2015-05-19T00:00:00.000Z'))
#csod_sfdc = subset(csod_sfdc, csod_sfdc$CreatedDate <= '2015-05-09T00:00:00.000Z')
grade_bucket_opps(csod_sfdc, 'Ftop__SpendScore__c', 'IsWon', 'Amount', 'CloseDate', '2015-04-09T00:00:00.000Z',9,6,3)
grade_opps(csod_sfdc, 'Ftop__SpendGrade__c', 'IsWon', 'Amount', 'CloseDate', '2015-04-09')

csod_sfdc$created_date_1 <- as.Date(strptime(csod_sfdc$CreatedDate, "%Y-%m-%d"))
csod_sfdc$close_date_1 <- as.Date(strptime(csod_sfdc$CloseDate, "%Y-%m-%d"))
csod_sfdc$cycle_length <- csod_sfdc$close_date_1 - csod_sfdc$created_date_1
csod_sfdc$cycle_length <- as.numeric(csod_sfdc$cycle_length)
csod_sfdc = subset(csod_sfdc, (csod_sfdc$close_date_1 >= '2015-02-01')&(csod_sfdc$close_date_1 <= '2015-06-1'))


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mir3_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_leads.csv', header=T, stringsAsFactor=F)  
mir3_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_opportunities.csv', header=T, stringsAsFactor=F)  
mir3_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_accounts.csv', header=T, stringsAsFactor=F)
mir3_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/acct_opp_query.csv', header=T, stringsAsFactor=F)
mir3_accts = merge(mir3_accts, mir3_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
mir3_accts = mir3_accts[!duplicated(mir3_accts$rec), ]
comment(mir3_leads) = 'mir3_leads'; comment(mir3_opps) = 'mir3_opps'; comment(mir3_accts) = 'mir3_accts'

grade_bucket_leads(mir3_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-04-02T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(mir3_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-02T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(mir3_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 6, 3)

comment(mir3_accts) = 'mir3_accts_new_opps'
grade_bucket_accounts_new(mir3_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-02-09 00:00:00.000Z', 'closed_date', 9, 6, 3)

#mir3_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/mir3_sfdc_opps.csv', stringsAsFactor=F, header=T)
#mir3_opps_merged = merge(mir3_opps, mir3_sfdc, by.x='rec', by.y='Id'); 
#mir3_opps_merged = subset(mir3_opps_merged, !is.na(mir3_opps_merged$Ftop__SpendScore__c) & (mir3_opps_merged$IsClosed == 'true'))
#mir3_opps_merged = subset(mir3_opps_merged, mir3_opps_merged$IsClosed == 'true')
#comment(mir3_opps_merged) = 'mir3_constrained'; grade_bucket_opps(mir3_opps_merged, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2015-02-09T00:00:00.000Z', 9,6,3)
#comment(mir3_opps_merged) = 'mir3_constrained'; grade_bucket_opps(mir3_opps_merged, 'ssv3.100001.sco', 'IsWon', 'amo', 'clo', '2015-02-09T00:00:00.000Z', 9,4,2)
#time_subset = subset(mir3_opps_merged, mir3_opps_merged$clo >= '2015-02-09T00:00:00.000Z')
#write.table(time_subset[,c('rec', 'CreatedDate', 'CloseDate', 'Account.Name', 'Account.Website', 'IsClosed', 'IsWon', 'Amount', 'Ftop__SpendScore__c', 'Ftop__SpendGrade__c', 'ssv3.100001.sco')], '/Users/fliptop/Desktop/Customer_Stats/MIR3/before_after/before_after_scores.csv', sep=',', row.names=F)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
guardian_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_leads.csv', header=T, stringsAsFactor=F)  
guardian_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_opportunities_2.csv', header=T, stringsAsFactor=F)  
guardian_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_accounts.csv', header=T, stringsAsFactor=F)
guardian_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/acct_opp_query.csv', header=T, stringsAsFactor=F)
guardian_accts = merge(guardian_accts, guardian_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
guardian_accts = guardian_accts[!duplicated(guardian_accts$rec), ]
comment(guardian_leads) = 'guardian_leads'; comment(guardian_opps) = 'guardian_opps'; comment(guardian_accts) = 'guardian_accts'

grade_bucket_leads(guardian_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-05-08T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(guardian_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-05-08T00:00:00.000Z', 9, 7, 3)
grade_bucket_accounts(guardian_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 7, 4)

comment(guardian_accts) = 'guardian_accts_new_opps'
grade_bucket_accounts_new(guardian_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-05-15 00:00:00.000Z', 'closed_date', 9, 7, 4)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
trackmaven_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Trackmaven/Trackmaven_leads.csv', header=T, stringsAsFactor=F)  
trackmaven_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Trackmaven/trackmaven_sfdc_leads.csv', header=T, stringsAsFactor=F)
trackmaven_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Trackmaven/Trackmaven_opportunities.csv', header=T, stringsAsFactor=F)  
comment(trackmaven_leads) = 'trackmaven_leads'; comment(trackmaven_opps) = 'trackmaven_opps'

grade_bucket_leads(trackmaven_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-11-04T00:00:00.000Z', 10, 4, 2)
grade_bucket_opps(trackmaven_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-03-04T00:00:00.000Z', 8, 3, 2)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jama_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_leads.csv', header=T, stringsAsFactor=F)  
jama_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_opportunities.csv', header=T, stringsAsFactor=F)  
jama_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_accounts.csv', header=T, stringsAsFactor=F)
jama_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/acct_opp_query.csv', header=T, stringsAsFactor=F)
jama_accts = merge(jama_accts, jama_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
jama_accts = jama_accts[!duplicated(jama_accts$rec), ]
comment(jama_leads) = 'jama_leads'; comment(jama_opps) = 'jama_opps'; comment(jama_accts) = 'jama_accts'

grade_bucket_leads(jama_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-11-24T00:00:00.000Z', 6, 5, 4)
grade_bucket_opps(jama_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2014-11-24T00:00:00.000Z', 6, 5, 4)
grade_bucket_accounts(jama_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 6, 5, 4)

comment(jama_accts) = 'jama_accts_new_opps'
grade_bucket_accounts_new(jama_accts, 'ssv3.100001.sco', 'closed', 'created_date', '2014-11-24 00:00:00.000Z', 'closed_date', 6, 5, 4)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
insideview_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_leads.csv', header=T, stringsAsFactor=F)  
insideview_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_opportunities.csv', header=T, stringsAsFactor=F)  
insideview_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_accounts.csv', header=T, stringsAsFactor=F)
insideview_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/acct_opp_query.csv', header=T, stringsAsFactor=F)
insideview_accts = merge(insideview_accts, insideview_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
insideview_accts = insideview_accts[!duplicated(insideview_accts$rec), ]
comment(insideview_leads) = 'insideview_leads'; comment(insideview_opps) = 'insideview_opps'; comment(insideview_accts) = 'insideview_accts'

grade_bucket_leads(insideview_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-08-19T00:00:00.000Z', 8, 6, 5)
grade_bucket_opps(insideview_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2014-08-19T00:00:00.000Z', 8, 6, 5)
grade_bucket_accounts(insideview_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 8, 6, 5)

comment(insideview_accts) = 'insideview_accts_new_opps'
grade_bucket_accounts_new(insideview_accts, 'ssv3.100001.sco', 'closed', 'created_date', '2014-08-19 00:00:00.000Z', 'closed_date', 8, 6, 5)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gainsight_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_leads.csv', header=T, stringsAsFactor=F)  
gainsight_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_opportunities.csv', header=T, stringsAsFactor=F)  
gainsight_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_accounts.csv', header=T, stringsAsFactor=F)
gainsight_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/acct_opp_query.csv', header=T, stringsAsFactor=F)
gainsight_accts = merge(gainsight_accts, gainsight_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
gainsight_accts = gainsight_accts[!duplicated(gainsight_accts$rec), ]
comment(gainsight_leads) = 'gainsight_leads'; comment(gainsight_opps) = 'gainsight_opps'; comment(gainsight_accts) = 'gainsight_accts'

grade_bucket_leads(gainsight_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-01-15T00:00:00.000Z', 10, 5, 3)
grade_bucket_opps(gainsight_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-01-15T00:00:00.000Z', 10, 5, 3)
grade_bucket_accounts(gainsight_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 10, 6, 3)

comment(gainsight_accts) = 'gainsight_accts_new_opps'
grade_bucket_accounts_new(gainsight_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-01-16 00:00:00.000Z', 'closed_date', 10, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
newvoicemedia_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_leads.csv', header=T, stringsAsFactor=F)  
newvoicemedia_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_opportunities.csv', header=T, stringsAsFactor=F)  
newvoicemedia_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/nvm_sfdc_opps.csv', header=T, stringsAsFactor=F)
newvoicemedia_opps.merge = merge(newvoicemedia_opps, newvoicemedia_sfdc, by.x='rec', by.y='Id')
newvoicemedia_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_accounts.csv', header=T, stringsAsFactor=F)
newvoicemedia_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/acct_opp_query.csv', header=T, stringsAsFactor=F)

newvoicemedia_accts = merge(newvoicemedia_accts, newvoicemedia_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
newvoicemedia_accts = newvoicemedia_accts[!duplicated(newvoicemedia_accts$rec), ]
comment(newvoicemedia_leads) = 'newvoicemedia_leads'; comment(newvoicemedia_opps) = 'newvoicemedia_opps'; comment(newvoicemedia_accts) = 'newvoicemedia_accts'

grade_bucket_leads(newvoicemedia_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-27T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(newvoicemedia_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-15T00:00:00.000Z', 8, 3, 2)
grade_bucket_accounts(newvoicemedia_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

comment(newvoicemedia_accts) = 'newvoicemedia_accts_new_opps'
grade_bucket_accounts_new(newvoicemedia_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-02-27 00:00:00.000Z', 'closed_date', 9, 6, 3)

comment(newvoicemedia_opps.merge) = 'nvm_opps_nn_bucket';grade_bucket_opps(newvoicemedia_opps.merge, 'ssv3.100001.sco', 'IsClosed', 'IsWon', 'Total_Order_Intake__c', 'CloseDate', '2015-04-15', 8, 3, 2)
comment(newvoicemedia_opps.merge) = 'nvm_opps_nn_score';scores_opps(newvoicemedia_opps.merge, 'ssv3.100001.sco', 'IsClosed', 'IsWon', 'Total_Order_Intake__c', 'CloseDate', '2015-04-15')
nvm_nn = subset(newvoicemedia_opps.merge, (newvoicemedia_opps.merge$CloseDate >= '2015-04-15')&(newvoicemedia_opps.merge$IsClosed=='true'))
write.table(nvm_nn, '/Users/fliptop/Desktop/Customer_Stats/NVM/for_Brendan/net_new_opportunities.csv', row.names=F, sep=',')

grade_opps(newvoicemedia_opps.merge, 'Ftop__SpendGrade__c', 'IsClosed', 'IsWon', 'Total_Order_Intake__c', 'CloseDate', '2015-04-15')

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jitterbit_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_leads.csv', header=T, stringsAsFactor=F)  
jitterbit_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_opportunities.csv', header=T, stringsAsFactor=F)  
jitterbit_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_sfdc_leads.csv', header=T, stringsAsFactor=F)
jitterbit_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/contact_role.csv', header=T, stringsAsFactor=F)
jitterbit_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_accounts.csv', header=T, stringsAsFactor=F)
jitterbit_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/acct_opp_query.csv', header=T, stringsAsFactor=F)
jitterbit_accts = merge(jitterbit_accts, jitterbit_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
jitterbit_accts = jitterbit_accts[!duplicated(jitterbit_accts$rec), ]

comment(jitterbit_leads) = 'jitterbit_leads'; comment(jitterbit_opps) = 'jitterbit_opps'; comment(jitterbit_accts) = 'jitterbit_accts'

grade_bucket_leads(jitterbit_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-03-20T00:00:00.000Z', 7, 5, 3)
grade_bucket_opps(jitterbit_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-03-25T00:00:00.000Z', 9, 5, 2)
grade_bucket_accounts(jitterbit_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

comment(jitterbit_accts) = 'jitterbit_accts_new_opps'
grade_bucket_accounts_new(jitterbit_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-03-25 00:00:00.000Z', 'closed_date', 9, 6, 3)


jitterbit_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_leads.csv', header=T, stringsAsFactor=F)
jitterbit_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_opportunities.csv', header=T, stringsAsFactor=F)  
jitterbit_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_sfdc_leads.csv', header=T, stringsAsFactor=F)
jitterbit_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/contact_role.csv', header=T, stringsAsFactor=F)

jitterbit_leads_opps2 = lead_to_opportunity_processing(jitterbit_leads, 'rec', jitterbit_opps, 'rec', jitterbit_sfdc, 'Id', 'ConvertedContactId', jitterbit_contact_role, 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(jitterbit_leads_opps2) = 'jitterbit_leads_to_opps_mongodb'
grade_bucket_leads(jitterbit_leads_opps2, 'ssv3.2.sco.x', 'opp_conv', 'cre.x', '2015-03-20T00:00:00.000Z', 7, 5, 3)
comment(jitterbit_leads_opps2) = 'jitterbit_leads_to_opps_sfdc'
grade_leads(jitterbit_leads_opps2, 'Ftop__SpendGrade__c', 'opp_conv', 'CreatedDate', '2015-03-20T00:00:00.000Z')

old_stuff <- function(x){
  jitterbit_sfdc$opp_conv = jitterbit_sfdc$ConvertedOpportunityId!=''
  jitterbit_contact_opp = merge(jitterbit_contact_role, jitterbit_opps, by.x='salesforce_contact_id', by.y='rec', all.x=T)
  jitterbit_leads_opps = merge(jitterbit_sfdc, jitterbit_contact_opp, by.x='ConvertedContactId', by.y='salesforce_contact_id', all.x=T)
  jitterbit_leads_opps$opp_conv = (!is.na(jitterbit_leads_opps$salesforce_opportunity_id))
  jitterbit_leads_opps = merge(jitterbit_leads, jitterbit_leads_opps, by.x='rec', by.y='Id')
  
  comment(jitterbit_leads_opps) = 'jitterbit_leads_to_opps_mongodb'
  grade_bucket_leads(jitterbit_leads_opps, 'ssv3.2.sco.x', 'opp_conv', 'cre.x', '2015-03-20T00:00:00.000Z', 7, 5, 3)
  comment(jitterbit_leads_opps) = 'jitterbit_leads_to_opps_sfdc'
  grade_leads(jitterbit_leads_opps, 'Ftop__SpendGrade__c', 'opp_conv', 'CreatedDate', '2015-03-20T00:00:00.000Z')
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kissmetrics_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_leads.csv', header=T, stringsAsFactor=F)  
kissmetrics_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_opportunities.csv', header=T, stringsAsFactor=F)  
kissmetrics_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_accounts.csv', header=T, stringsAsFactor=F)
kissmetrics_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/acct_opp_query.csv', header=T, stringsAsFactor=F)
kissmetrics_accts = merge(kissmetrics_accts, kissmetrics_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
kissmetrics_accts = kissmetrics_accts[!duplicated(kissmetrics_accts$rec), ]
comment(kissmetrics_leads) = 'kissmetrics_leads'; comment(kissmetrics_opps) = 'kissmetrics_opps'; comment(kissmetrics_accts) = 'kissmetrics_accts'

grade_bucket_leads(kissmetrics_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-12-10T00:00:00.000Z', 10,9,4)
grade_bucket_opps(kissmetrics_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2014-11-26T00:00:00.000Z', 9,6,3)
grade_bucket_accounts(kissmetrics_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

comment(kissmetrics_accts) = 'kissmetrics_accts_new_opps'
grade_bucket_accounts_new(kissmetrics_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2014-11-11 00:00:00.000Z', 'closed_date', 9, 6, 3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reliant_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Reliantfunding/Reliantfunding_leads.csv', header=T, stringsAsFactor=F)
reliant_opps= read.csv('/Users/fliptop/Desktop/Customer_Stats/Reliantfunding/Reliantfunding_opportunities.csv', header=T, stringsAsFactor=F)
comment(reliant_leads) = 'reliant_leads'; comment(reliant_opps) = 'reliant_opps'

grade_bucket_leads(reliant_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-18T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(reliant_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-02-18T00:00:00.000Z', 9, 4, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sumo_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_leads.csv', header=T, stringsAsFactor=F)
sumo_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_opportunities.csv', header=T, stringsAsFactor=F)
sumo_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_accounts.csv', header=T, stringsAsFactor=F)
sumo_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/acct_opp_query.csv', header=T, stringsAsFactor=F)
sumo_accts = merge(sumo_accts, sumo_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
sumo_accts = sumo_accts[!duplicated(sumo_accts$rec), ]
comment(sumo_leads) = 'sumo_leads'; comment(sumo_opps) = 'sumo_opps'; comment(sumo_accts) = 'sumo_accts'

grade_bucket_leads(sumo_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-01-22T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(sumo_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-01-22T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(sumo_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

comment(sumo_accts) = 'sumo_accts_new_opps'
grade_bucket_accounts_new(sumo_accts, 'ssv3.100001.sco', 'closed', 'created_date', '2015-01-22 00:00:00.000Z', 'closed_date', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cetera_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Cetera/Cetera_leads.csv', header=T, stringsAsFactor=F)
cetera_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Cetera/Cetera_contacts.csv', header=T, stringsAsFactor=F)
comment(cetera_leads) = 'cetera_leads'; comment(cetera_contacts) = 'cetera_contacts'

grade_bucket_leads(cetera_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-19T00:00:00.000Z', 9, 6, 3)
#grade_bucket_leads(cetera_contacts, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-24T00:00:00.000Z', 10, 8, 3)
#grade_bucket_leads(cetera_leads, 'ssv3.2.sco', 'isCon', 'cre', '0000-01-20T00:00:00.000Z', 9, 6, 3)

cetera_contacts_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Cetera/cetera_contacts_sfdc.csv', header=T, stringsAsFactor=F)
cetera_contacts_sfdc = cetera_contacts_sfdc[!duplicated(cetera_contacts_sfdc$Contact__c),]
cetera_contacts_merge = merge(cetera_contacts, cetera_contacts_sfdc, by.x='rec', by.y='Contact__r.Id', all.x=T)
comment(cetera_contacts_merge) = 'cetera_contacts_merge'
cetera_contacts_merge$Contact_Converted = !is.na(cetera_contacts_merge$Contact__c)
grade_bucket_leads(cetera_contacts_merge, 'ssv3.2.sco', 'Contact_Converted', 'cre', '2015-02-24T00:00:000Z', 10, 8, 3) 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#fiksu lead to opportunity conversion
fiksu_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/Fiksu_leads.csv', header=T, stringsAsFactor=F)
fiksu_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/Fiksu_opportunities.csv', header=T, stringsAsFactor=F)
fiksu_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/fiksu_sfdc_leads.csv', header=T, stringsAsFactor=F)
fiksu_sfdc.opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/fiksu_sfdc_opps.csv', header=T, stringsAsFactor=F)
#fiksu_opps.1 = fiksu_opps[,c('rec','nam','ssv3.100001.sco','isClo','won','amo','typ')]
#fiksu_opps.1 = subset(fiksu_opps.1, fiksu_opps.1$typ=='New Business (NN)')
comment(fiksu_leads) = 'fiksu_leads'; comment(fiksu_opps) = 'fiksu_opps'

#fiksu_opps_constrained = subset(fiksu_opps, fiksu_opps$clo >= '2015-01-01T00:00:00.000Z' & fiksu_opps$clo <='2015-02-13T00:00:00.000Z' & fiksu_opps$typ == 'New Business (NN)')

grade_bucket_leads(fiksu_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-02-25T00:00:00.000Z', 10, 8, 3)
grade_bucket_opps(fiksu_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-02-18T00:00:00.000Z', 8,3,2)

fiksu_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/Fiksu_leads.csv', header=T, stringsAsFactor=F)
fiksu_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/Fiksu_opportunities.csv', header=T, stringsAsFactor=F)  
fiksu_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/fiksu_sfdc_leads.csv', header=T, stringsAsFactor=F)
fiksu_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fiksu/contact_role.csv', header=T, stringsAsFactor=F)
fiksu_leads_opps = lead_to_opportunity_processing(fiksu_leads, 'rec', fiksu_opps, 'rec', fiksu_sfdc, 'Id', 'ConvertedContactId', fiksu_contact_role, 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(fiksu_leads_opps) = 'fiksu_leads_to_opps_mongodb'
grade_bucket_leads(fiksu_leads_opps, 'ssv3.2.sco.x', 'opp_conv', 'cre.x', '2015-02-25T00:00:00.000Z', 10, 8, 3)
comment(fiksu_leads_opps) = 'fiksu_leads_to_opps_sfdc'
grade_leads(fiksu_leads_opps, 'Ftop__SpendGrade__c', 'opp_conv', 'CreatedDate', '2015-02-25T00:00:00.000Z')


old_stuff <- function(x){
  #fiksu_leads_opps = merge(fiksu_leads, fiksu_sfdc, by.x='rec', by.y='Id', all.x=T)
  #fiksu_leads_opps$ConvertedOpportunityId[fiksu_leads_opps$ConvertedOpportunityId ==''] = NA
  #fiksu_leads_opps = merge(fiksu_leads_opps, fiksu_opps.1, by.x='ConvertedOpportunityId', by.y='rec', all.x=T)
  #fiksu_leads_opps$opp_conv = (!is.na(fiksu_leads_opps$ssv3.100001.sco.y))&(fiksu_leads_opps$typ.y=='New Business (NN)')
  #comment(fiksu_leads_opps) = 'fiksu_leads_to_opportunity_conversion'
  #grade_bucket_leads(fiksu_leads_opps, 'ssv3.2.sco', 'opp_conv', 'cre', '2015-01-01T00:00:00.000Z', 10, 8, 3)
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
revel_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Revel/Revel_leads.csv', header=T, stringsAsFactor=F)
revel_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Revel/Revel_opportunities.csv', header=T, stringsAsFactor=F)
comment(revel_leads) = 'revel_leads'; comment(revel_opps) = 'revel_opps'

grade_bucket_leads(revel_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-03-03T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(revel_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-03-03T00:00:00.000Z', 10,5,3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bitly_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads.csv', header=T, stringsAsFactor=F)
bitly_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities.csv', header=T, stringsAsFactor=F)
#bitly_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities2.csv', header=T, stringsAsFactor=F)


#bitly_sfdc.leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_sfdc_leads.csv', header=T, stringsAsFactor=F)
#bitly_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities_1.csv', header=T, stringsAsFactor=F)
#bitly_sfdc.opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities_sfdc.csv', header=T, stringsAsFactor=F)

#bitly_merge.leads = merge(bitly_leads, bitly_sfdc.leads, by.x='rec', by.y='Id')
#bitly_merge = merge(bitly_opps, bitly_sfdc.opps, by.x='rec', by.y='Id')
#bitly_merge = subset(bitly_merge, bitly_merge$CloseDate >= '2015-04-14')
#bitly_leads = subset(bitly_leads, bitly_leads$cre >= '2015-04-14T00:00:00.000Z')

comment(bitly_leads) = 'bitly_leads';grade_bucket_leads(bitly_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-04-14T00:00:00.000Z', 9, 6, 3)
comment(bitly_opps) = 'bitly_opps';grade_bucket_opps(bitly_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-14T00:00:00.000Z', 9, 6, 3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
brafton_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_leads.csv', header=T, stringsAsFactor=F)
brafton_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_opportunities.csv', header=T, stringsAsFactor=F)
#brafton_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_sfdc_opportunities.csv', header=T, stringsAsFactor=F)
brafton_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_accounts.csv', header=T, stringsAsFactor=F)
brafton_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/acct_opp_query.csv', header=T, stringsAsFactor=F)
brafton_accts = merge(brafton_accts, brafton_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
brafton_accts = brafton_accts[!duplicated(brafton_accts$rec), ]

#brafton_merge = merge(brafton_opps, brafton_sfdc, by.x='rec', by.y='Id', all.y=T)
comment(brafton_leads) = 'brafton_leads';# comment(brafton_merge) = 'brafton_opps'
comment(brafton_opps) = 'brafton_opps';
grade_bucket_leads(brafton_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-04-28T00:00:00.000Z', 9, 6, 3)
#grade_bucket_opps(brafton_merge, 'ssv3.100001.sco', 'IsClosed', 'IsWon', 'amo', 'CloseDate', '2015-04-23T00:00:00.000Z', 9,6,3)
grade_bucket_opps(brafton_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-05-31T00:00:00.000Z', 9,6,3)
comment(brafton_accts) = 'brafton_accts'
grade_bucket_accounts(brafton_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

#brafton_accts_new = subset(brafton_accts, ((brafton_accts$created_date >= '2015-04-24T00:00:00')|(is.na(brafton_accts$closed_date)))&((brafton_accts$cre >= '2014-01-01T00:00:00.000Z')&((brafton_accts$cre < '2014-07-01T00:00:00.000Z'))))
#comment(brafton_accts_new) = 'brafton_accts_new_opps'
#grade_bucket_accounts(brafton_accts_new, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

comment(brafton_accts) = 'brafton_accts_new_opps'
grade_bucket_accounts_new(brafton_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-04-24 00:00:00.000Z', 'closed_date', 9, 7, 3)

#grade_bucket_opps(brafton_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-04-20T00:00:00.000Z', 9,6,3)
brafton_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_contacts.csv', header=T, stringsAsFactor=F)
brafton_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/contact_role.csv', header=T, stringsAsFactor=F)

brafton_contacts_processed = contacts_processing(brafton_contact, brafton_contact_role, 'rec', 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(brafton_contacts_processed) = 'brafton_contacts'; grade_bucket_contacts(brafton_contacts_processed, 'ssv3.2.sco', 'converted', 'cre','2015-04-20T00:00:00.000Z', 9,6,3)


brafton_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_sfdc_opportunities.csv', header=T, stringsAsFactor=F)
brafton_sfdc = subset(brafton_sfdc, brafton_sfdc$IsClosed == 'true')
grade_opps(brafton_sfdc, 'Ftop__SpendGrade__c', 'IsWon', 'Amount', 'CloseDate', '2015-05-31')

x = as.data.frame.matrix(table(brafton_accts$sta, brafton_accts$IsConverted))
x$ratio_conv = x[,'TRUE']/(x[,'TRUE'] + x[,'FALSE'])
x = x[order(-x$ratio_conv),]
#brafton_2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Brafton/Brafton_sfdc_opportunities2.csv', header=T, stringsAsFactor=F)
#b_merge = merge(brafton_opps, brafton_2, by.x='rec', by.y='Id'); b_merge = b_merge[!is.na(b_merge$Ftop__SpendScore__c),]
#table(b_merge$ssv3.100001.sco); table(b_merge$Ftop__SpendScore__c)
#comment(b_merge) = 'new_opps'; grade_bucket_opps(b_merge,'ssv3.100001.sco','IsClosed','IsWon','Amount','CloseDate', '2000-04-23',9,6,3)
#comment(b_merge) = 'old_opps';grade_bucket_opps(b_merge,'ssv3.100001.sco','IsClosed','IsWon','Amount','CloseDate', '2000-04-23',9,6,3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
advantage_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Advantage/Advantage_leads.csv', header=T, stringsAsFactor=F)
advantage_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Advantage/Advantage_opportunities.csv', header=T, stringsAsFactor=F)

comment(advantage_leads) = 'advantage_leads'
grade_bucket_leads(advantage_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-05-20T00:00:00.000Z', 9,6,3)
advantage_leads_june = subset(advantage_leads, advantage_leads$cre >= '2015-05-20T00:00:00.000Z')
#grade_bucket_opps(advantage_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-05-25T00:00:00.000Z', 9,6,3)

advantage_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/Advantage/Advantage_contacts.csv', header=T, stringsAsFactor=F)
advantage_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Advantage/contact_role.csv', header=T, stringsAsFactor=F)

advantage_contacts_processed = contacts_processing(advantage_contact, advantage_contact_role, 'rec', 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(advantage_contacts_processed) = 'advantage_contacts'; grade_bucket_contacts(advantage_contacts_processed, 'ssv3.2.sco', 'converted', 'cre','2015-05-21T00:00:00.000Z', 9,6,3)


advantage_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Advantage/Advantage_sfdc_leads.csv', header=T, stringsAsFactor=F)
advantage_leads.merge = merge(advantage_leads, advantage_sfdc, by.x='rec', by.y='Id', all.y=T)
#advantage_leads.merge$Ftop__SpendScore__c[is.na(advantage_leads.merge$Ftop__SpendScore__c)] = advantage_leads.merge$ssv3.2.sco[is.na(advantage_leads.merge$Ftop__SpendScore__c)]
advantage_leads_april = subset(advantage_leads.merge, advantage_leads.merge$CreatedDate < '2015-05-01T00:00:00.000Z')
comment(advantage_leads_april) = 'advantage_leads_april'; grade_bucket_leads(advantage_leads_april, 'ssv3.2.sco', 'IsConverted', 'CreatedDate', '2015-04-01T00:00:00.000Z', 9,6,3)
advantage_leads_may = subset(advantage_leads.merge, advantage_leads.merge$CreatedDate < '2015-06-01T00:00:00.000Z')
comment(advantage_leads_may) = 'advantage_leads_may'; grade_bucket_leads(advantage_leads_may, 'ssv3.2.sco', 'IsConverted', 'CreatedDate', '2015-05-01T00:00:00.000Z', 9,6,3)
comment(advantage_leads.merge) = 'advantage_leads_june'; grade_bucket_leads(advantage_leads.merge, 'ssv3.2.sco', 'IsConverted', 'CreatedDate', '2015-06-01T00:00:00.000Z', 9,6,3)
advantage_leads.merge_net_new = subset(advantage_leads.merge, advantage_leads.merge$cre >= '2015-05-20T00:00:00.000Z')
comment(advantage_leads.merge_net_new) = 'advantage_leads_net_new';grade_bucket_leads(advantage_leads.merge_net_new, 'ssv3.2.sco', 'IsConverted', 'CreatedDate', '2015-05-20T00:00:00.000Z', 9,6,3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
k2_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/K2_leads.csv', header=T, stringsAsFactor=F)
k2_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/K2_opportunities.csv', header=T, stringsAsFactor=F)
k2_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/K2_contacts.csv', header=T, stringsAsFactor=F)
k2_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/K2_accounts.csv', header=T, stringsAsFactor=F)
k2_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/acct_opp_query.csv', header=T, stringsAsFactor=F)
k2_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/K2/contact_role.csv', header=T, stringsAsFactor=F)
k2_accts = merge(k2_accts, k2_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
k2_accts = k2_accts[!duplicated(k2_accts$rec), ]

comment(k2_leads) = 'k2_leads';comment(k2_opps) = 'k2_opps';comment(k2_accts) = 'k2_accts';
grade_bucket_leads(k2_leads, 'ssv3.2.sco', 'isCon', 'cre', '2000-05-28T00:00:00.000Z', 10, 8, 5)
grade_bucket_opps(k2_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-05-27T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(k2_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 7, 3)

comment(k2_accts) = 'k2_accts_new_opps'
grade_bucket_accounts_new(k2_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-05-27 00:00:00.000Z', 'closed_date', 9, 7, 3)

k2_contacts_processed = contacts_processing(k2_contacts, k2_contact_role, 'rec', 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(k2_contacts_processed) = 'k2_contacts'; grade_bucket_contacts(k2_contacts_processed, 'ssv3.2.sco', 'converted', 'cre','2015-05-27T00:00:00.000Z', 9,6,3)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
financialforce_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/FinancialForce_leads.csv', header=T, stringsAsFactor=F)
financialforce_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/FinancialForce_opportunities.csv', header=T, stringsAsFactor=F)
financialforce_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/FinancialForce_contacts.csv', header=T, stringsAsFactor=F)
financialforce_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/FinancialForce_accounts.csv', header=T, stringsAsFactor=F)
financialforce_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/acct_opp_query.csv', header=T, stringsAsFactor=F)
financialforce_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/contact_role.csv', header=T, stringsAsFactor=F)
financialforce_accts = merge(financialforce_accts, financialforce_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
financialforce_accts = financialforce_accts[!duplicated(k2_accts$rec), ]
comment(financialforce_leads) = 'financialforce_leads'; comment(financialforce_opps) = 'financialforce_opps'; comment(financialforce_accts) = 'financialforce_accts'

grade_bucket_leads(financialforce_leads, 'ssv3.32.sco', 'isCon', 'cre', '2015-06-11T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(financialforce_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-06-10T00:00:00.000Z', 10,5,3)
grade_bucket_accounts(financialforce_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 7, 3)

comment(financialforce_accts) = 'financialforce_accts_new_opps'
grade_bucket_accounts_new(financialforce_accts, 'ssv3.100002.sco', 'closed', 'created_date', '2015-06-10 00:00:00.000Z', 'closed_date', 9, 7, 3)

financialforce_contacts_processed = contacts_processing(financialforce_contacts, financialforce_contact_role, 'rec', 'salesforce_contact_id', 'salesforce_opportunity_id')
comment(financialforce_contacts_processed) = 'financialforce_contacts'; grade_bucket_contacts(financialforce_contacts_processed, 'ssv3.2.sco', 'converted', 'cre','2015-06-16T00:00:00.000Z', 9,6,3)


ff_rf = financialforce_leads[order(-financialforce_leads$ssv3.2.sco, -financialforce_leads$ssv3.2.rscos.2, -financialforce_leads$ssv3.2.rscos.1),]
ff_gb = financialforce_leads[order(-financialforce_leads$ssv3.12.sco, -financialforce_leads$ssv3.12.rscos.2, -financialforce_leads$ssv3.12.rscos.1),]
ff_lr = financialforce_leads[order(-financialforce_leads$ssv3.32.sco, -financialforce_leads$ssv3.32.rscos.2, -financialforce_leads$ssv3.32.rscos.1),]

decile_leads(ff_rf, 'isCon', 'cre', '2015-06-11T00:00:00.000Z')
decile_leads(ff_gb, 'isCon', 'cre', '2015-06-11T00:00:00.000Z')
decile_leads(ff_lr, 'isCon', 'cre', '2015-06-11T00:00:00.000Z')

#ff_nn2 = subset(financialforce_leads, financialforce_leads$cre >= '2015-06-11T00:00:00.000Z')

ff_nn = subset(financialforce_leads, financialforce_leads$cre >= '2015-06-11T00:00:00.000Z')
write.table(ff_nn[,c('rec','isCon')], '/Users/fliptop/Desktop/Customer_Stats/FinancialForce/lead_comparison/financialforce_lead_id.csv', row.names=F, sep=',')
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mkto_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/NEW/Marketo_leads.csv', header=T, stringsAsFactor=F)
mkto_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/NEW/Marketo_accounts.csv', header=T, stringsAsFactor=F)
mkto_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/NEW/sfdc_acct_opp_query.csv', header=T, stringsAsFactor=F)
mkto_accts = merge(mkto_accts, mkto_query, by.x='rec', by.y='Account.Id', all.x=T)
mkto_accts = mkto_accts[!duplicated(mkto_accts$rec), ]

comment(mkto_leads) = 'mkto_leads'; comment(mkto_accts) = 'mkto_accts'

grade_bucket_leads(mkto_leads, 'msv1.700002.sco', 'isCon', 'cre', '2015-06-11T00:00:00.000Z', 82, 66, 50)
grade_bucket_accounts(mkto_accts, 'msv1.700002.sco', 'IsClosed', 'cre', '0000-00-00 00:00:00', 82, 66, 50)

comment(mkto_accts) = 'mkto_accts_new_opps'
grade_bucket_accounts_new(mkto_accts, 'msv1.700002.sco', 'IsClosed', 'CreatedDate', '2015-06-11 00:00:00', 'CreatedDate', 82, 66, 50)

mkto_contact = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/NEW/Marketo_contacts.csv', header=T, stringsAsFactor=F)
mkto_contact_role = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/NEW/sfdc_contact_role.csv', header=T, stringsAsFactor=F)

mkto_contacts_processing = contacts_processing(mkto_contact, mkto_contact_role, 'rec', 'ContactId', 'OpportunityId')
comment(mkto_contacts_processing) = 'mkto_contacts'; grade_bucket_contacts(mkto_contacts_processing, 'msv1.700002.sco', 'converted', 'cre', '2015-06-11T00:00:00.000Z', 82, 66, 50)






mkto_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_accounts_accounts.csv', header=T, stringsAsFactor=F)
mkto_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_accounts_contacts.csv', header=T, stringsAsFactor=F)
mkto_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_accounts_leads.csv', header=T, stringsAsFactor=F)

mkto_accts = mkto_accts[order(-mkto_accts$msv1.700002.sco),]
mkto_contacts = mkto_contacts[order(-mkto_contacts$msv1.700002.sco),]
mkto_leads = mkto_leads[order(-mkto_leads$msv1.700002.sco),]


acct_q1 = mkto_accts[0:round(nrow(mkto_accts)/4), ]
acct_q2 = mkto_accts[(round(nrow(mkto_accts)/4)+1):round(nrow(mkto_accts)*2/4), ]
acct_q3 = mkto_accts[(round(nrow(mkto_accts)/2)+1):round(nrow(mkto_accts)*3/4), ]
acct_q4 = mkto_accts[(round(nrow(mkto_accts)*(3/4))+1):round(nrow(mkto_accts)), ]

range(acct_q1$msv1.700002.sco);range(acct_q2$msv1.700002.sco);range(acct_q3$msv1.700002.sco);range(acct_q4$msv1.700002.sco, na.rm=T);

con_q1 = mkto_contacts[0:round(nrow(mkto_contacts)/4), ]
con_q2 = mkto_contacts[(round(nrow(mkto_contacts)/4)+1):round(nrow(mkto_contacts)*2/4), ]
con_q3 = mkto_contacts[(round(nrow(mkto_contacts)/2)+1):round(nrow(mkto_contacts)*3/4), ]
con_q4 = mkto_contacts[(round(nrow(mkto_contacts)*(3/4))+1):round(nrow(mkto_contacts)), ]

range(con_q1$msv1.700002.sco);range(con_q2$msv1.700002.sco);range(con_q3$msv1.700002.sco);range(con_q4$msv1.700002.sco, na.rm=T);

lead_q1 = mkto_leads[0:round(nrow(mkto_leads)/4), ]
lead_q2 = mkto_leads[(round(nrow(mkto_leads)/4)+1):round(nrow(mkto_leads)*2/4), ]
lead_q3 = mkto_leads[(round(nrow(mkto_leads)/2)+1):round(nrow(mkto_leads)*3/4), ]
lead_q4 = mkto_leads[(round(nrow(mkto_leads)*(3/4))+1):round(nrow(mkto_leads)), ]

range(lead_q1$msv1.700002.sco);range(lead_q2$msv1.700002.sco);range(lead_q3$msv1.700002.sco);range(lead_q4$msv1.700002.sco, na.rm=T);

mkto_contacts = subset(mkto_contacts, !is.na(mkto_contacts$msv1.700002.sco))
mkto_accts = subset(mkto_accts, !is.na(mkto_accts$msv1.700002.sco))
mkto_leads = subset(mkto_leads, !is.na(mkto_leads$msv1.700002.sco))
#mkto_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/acct_opp_query.csv', header=T, stringsAsFactor=F)
mkto_opps.merge = merge(mkto_opps, mkto_query, by.x='Id', by.y='id', all.x=T)
mkto_accts = merge(mkto_accts, mkto_opps.merge, by.x='rec', by.y='salesforce_account_id', all.x=T)
mkto_accts = mkto_accts[!duplicated(mkto_accts$rec), ]

accounts_100 <- function(df1, score_name, isconverted_name, date_field, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  
  dec_1 = subset(df, df[ ,score_name] >= 90)
  dec_2 = subset(df, df[ ,score_name] <= 89 & df[ ,score_name] >= 80)
  dec_3 = subset(df, df[ ,score_name] <= 79 & df[ ,score_name] >= 70)
  dec_4 = subset(df, df[ ,score_name] <= 69 & df[ ,score_name] >= 60)
  dec_5 = subset(df, df[ ,score_name] <= 59 & df[ ,score_name] >= 50)
  dec_6 = subset(df, df[ ,score_name] <= 49 & df[ ,score_name] >= 40)
  dec_7 = subset(df, df[ ,score_name] <= 39 & df[ ,score_name] >= 30)
  dec_8 = subset(df, df[ ,score_name] <= 29 & df[ ,score_name] >= 20)
  dec_9 = subset(df, df[ ,score_name] <= 19 & df[ ,score_name] >= 10)
  dec_10 = subset(df, df[ ,score_name] <= 9 & df[ ,score_name] >= 0)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
  sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
  sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
  sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
  sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
  sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
  sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
  
  agg_decile_1 = sum_1/total_conv
  agg_decile_2 = sum_2/total_conv
  agg_decile_3 = sum_3/total_conv
  agg_decile_4 = sum_4/total_conv
  agg_decile_5 = sum_5/total_conv
  agg_decile_6 = sum_6/total_conv
  agg_decile_7 = sum_7/total_conv
  agg_decile_8 = sum_8/total_conv
  agg_decile_9 = sum_9/total_conv
  agg_decile_10 = sum_10/total_conv
  
  decile_df = data.frame(c('90+','80-89','70-79','60-69','50-59','40-49','30-39','20-29','10-19','<10'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                         c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
  colnames(decile_df) = c('Score','Num_in_group','Num_accts_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
  decile_df$Lift = (decile_df$Num_accts_converted/decile_df$Num_in_group)/(agg_sum10/nrow(df))
  decile_df$Conversion_Percent = (decile_df$Num_accts_converted/decile_df$Num_in_group)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Score, c('90+','80-89','70-79','60-69','50-59','40-49','30-39','20-29','10-19','<10'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
  lift_chart = data.frame(c(0,decile_df$Cum_convert_percent),
                          c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  colnames(lift_chart) = c('Cum_convert_percent','Agg_axis')
  lift_chart$Agg_axis = factor(lift_chart$Agg_axis,c('0%','10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  agg_plot_lift = ggplot(lift_chart, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Leads Lift Chart', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot_lift = agg_plot_lift + geom_abline(intercept=-10, slope=10, col='red', )
  
  max_height = max(decile_df$Conversion_Percent)
  if (max_height <= 0.1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,20,30,40,50,60,70,80,90,100), limits=c(0,0.11))  
  } else if (max_height <= 1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,20,30,40,50,60,70,80,90,100), limits=c(0,1))  
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account Conversion Rates', x='Stack-ranked Accounts', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,50,60,70,80,90,100), limits=c(0,30))  
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  #grid.arrange(ftop_conv_graph)
  #grid.arrange(agg_plot)
  #grid.arrange(agg_pie_chart)
  #grid.arrange(agg_plot_lift)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  decile_df$Cum_convert_percent = NULL
  print(decile_df, row.names=FALSE)
  #print(lift_chart, row.names=F)
  
  png(paste(comment(df1),'_decile_cr.png', sep=''), width = 500, height = 500)
  #grid.arrange(ftop_conv_graph)
  plot(ftop_conv_graph)
  dev.off()
  
  #png(paste(comment(df1),'_decile_lift.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_plot)
  #plot(agg_plot_lift)
  #dev.off()
  
  png(paste(comment(df1),'_decile_pie.png', sep=''), width = 500, height = 500)
  #grid.arrange(agg_pie_chart)
  plot(agg_pie_chart)
  dev.off()
  
  grid.arrange(ftop_conv_graph)
  #grid.arrange(agg_plot_lift)
  grid.arrange(agg_pie_chart)
  file_name = paste(comment(df1), '_decile_stats.csv', sep='')
  write.table(decile_df, file_name, row.names=F, sep=',')
}

mkto_accts$isconverted = !is.na(mkto_accts$closed)
comment(mkto_accts) = 'Marketo_accts'; accounts_100(mkto_accts, 'msv1.700002.sco', 'isconverted', 'cre', '0000-00-00 00:00:00')



ftop_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fliptop/Fliptop_leads.csv', header=T, stringsAsFactor=F)
ftop_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Fliptop/Fliptop_opportunities.csv', header=T, stringsAsFactor=F)
grade_bucket_leads(ftop_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-06-04T00:00:00.000Z', 9,6,3)
grade_bucket_opps(ftop_opps, 'ssv3.100001.sco', 'isClo', 'won', 'amo', 'clo', '2015-07-06T00:00:00.000Z', 7,5,3)



#Demandforce

l = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Lifestyle/Lifestyle_leads.csv', header=T, stringAsFactor=F)


















#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE EVALUATIONS (LEADS)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
insideview_leads = insideview_leads[order(-insideview_leads$ssv3.1.sco),]
decile_leads(insideview_leads, 'isCon', 'cre', '2014-08-19T00:00:00.000Z')
jitterbit_leads = jitterbit_leads[order(-jitterbit_leads$ssv3.2.sco, -jitterbit_leads$ssv3.2.rscos.2, -jitterbit_leads$ssv3.2.rscos.1),]
decile_leads(jitterbit_leads, 'isCon', 'cre', '2015-03-20T00:00:00.000Z')
trackmaven_leads = trackmaven_leads[order(-trackmaven_leads$ssv3.2.sco, -trackmaven_leads$ssv3.2.rscos.2, -trackmaven_leads$ssv3.2.rscos.1),]
decile_leads(trackmaven_leads, 'isCon', 'cre', '2014-11-04T00:00:00.000Z')
jama_leads = jama_leads[order(-jama_leads$ssv3.2.sco, -jama_leads$ssv3.2.rscos.2, -jama_leads$ssv3.2.rscos.1),]
decile_leads(jama_leads, 'isCon', 'cre', '2014-11-24T00:00:00.000Z')
kissmetrics_leads = kissmetrics_leads[order(-kissmetrics_leads$ssv3.2.sco, -kissmetrics_leads$ssv3.2.rscos.2, -kissmetrics_leads$ssv3.2.rscos.1),]
decile_leads(kissmetrics_leads, 'isCon', 'cre', '2014-12-10T00:00:00.000Z')
#newvoicemedia_leads = newvoicemedia_leads[order(-newvoicemedia_leads$ssv3.2.sco, -newvoicemedia_leads$ssv3.2.rscos.2, -newvoicemedia_leads$ssv3.2.rscos.1),]
#decile_leads(newvoicemedia_leads, 'isCon', 'cre', '2015-02-27T00:00:00.000Z')
guardian_leads = guardian_leads[order(-guardian_leads$ssv3.2.sco, -guardian_leads$ssv3.2.rscos.2, -guardian_leads$ssv3.2.rscos.1),]
decile_leads(guardian_leads, 'isCon', 'cre', '2015-05-08T00:00:00.000Z')
blackbaud_leads = blackbaud_leads[order(-blackbaud_leads$ssv3.2.sco, -blackbaud_leads$ssv3.2.rscos.2, -blackbaud_leads$ssv3.2.rscos.1),]
decile_leads(blackbaud_leads, 'isCon', 'cre', '2015-02-20T00:00:00.000Z')
gainsight_leads = gainsight_leads[order(-gainsight_leads$ssv3.2.sco, -gainsight_leads$ssv3.2.rscos.2, -gainsight_leads$ssv3.2.rscos.1),]
decile_leads(gainsight_leads, 'isCon', 'cre', '2015-01-15T00:00:00.000Z')
mir3_leads = mir3_leads[order(-mir3_leads$ssv3.2.sco, -mir3_leads$ssv3.2.rscos.2, -mir3_leads$ssv3.2.rscos.1),]
decile_leads(mir3_leads, 'isCon', 'cre', '2015-02-09T00:00:00.000Z')
reliant_leads = reliant_leads[order(-reliant_leads$ssv3.2.sco, -reliant_leads$ssv3.2.rscos.2, -reliant_leads$ssv3.2.rscos.1),]
decile_leads(reliant_leads, 'isCon', 'cre', '2015-02-18T00:00:00.000Z')
fiksu_leads = fiksu_leads[order(-fiksu_leads$ssv3.2.sco, -fiksu_leads$ssv3.2.rscos.2, -fiksu_leads$ssv3.2.rscos.1),]
decile_leads(fiksu_leads, 'isCon', 'cre', '2015-01-01T00:00:00.000Z')
revel_leads = revel_leads[order(-revel_leads$ssv3.2.sco, -revel_leads$ssv3.2.rscos.2, -revel_leads$ssv3.2.rscos.1),]
decile_leads(revel_leads, 'isCon', 'cre', '2015-03-03T00:00:00.000Z')
on24_leads = on24_leads[order(-on24_leads$ssv3.2.sco, -on24_leads$ssv3.2.rscos.2, -on24_leads$ssv3.2.rscos.1),]
decile_leads(on24_leads, 'isCon', 'cre', '2015-03-14T00:00:00.000Z')
cetera_leads = cetera_leads[order(-cetera_leads$ssv3.2.sco, -cetera_leads$ssv3.2.rscos.2, -cetera_leads$ssv3.2.rscos.1),]
decile_leads(cetera_leads, 'isCon', 'cre', '2015-04-10T00:00:00.000Z')
bitly_leads = bitly_leads[order(-bitly_leads$ssv3.2.sco, -bitly_leads$ssv3.2.rscos.2, -bitly_leads$ssv3.2.rscos.1),]
decile_leads(bitly_leads, 'isCon', 'cre', '2015-04-14T00:00:00.000Z')
#brafton_leads = brafton_leads[order(-brafton_leads$ssv3.2.sco),]
#decile_leads(brafton_leads, 'isCon', 'cre', '')
onstream_leads = onstream_leads[order(-onstream_leads$ssv3.2.sco, -onstream_leads$ssv3.2.rscos.2, -onstream_leads$ssv3.2.rscos.1),]
decile_leads(onstream_leads, 'isCon', 'cre', '2015-04-21T00:00:00.000Z')
#amalto_leads = amalto_leads[order(-amalto_leads$ssv3.2.sco),]
#decile_leads(amalto_leads, 'isCon', 'cre', '')
randstad_leads = randstad_leads[order(-randstad_leads$ssv3.2.sco, -randstad_leads$ssv3.2.rscos.2, -randstad_leads$ssv3.2.rscos.1),]
decile_leads(randstad_leads, 'isCon', 'cre', '2015-04-07T00:00:00.000Z')

decile_leads(sfdc.mongo, 'IsConverted', 'CreatedDate', '2015-01-01T00:00:00.000Z')
decile_leads(sfdc.sort_snapshot, 'IsConverted', 'CreatedDate', '2015-01-01T00:00:00.000Z')
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE EVALUATIONS (OPPS)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
insideview_opps = insideview_opps[order(-insideview_opps$ssv3.100001.sco, -insideview_opps$ssv3.100001.rscos.1),]
decile_opps(insideview_opps, 'isClo', 'won', 'amo', 'clo', '2014-08-19T00:00:00.000Z')
jitterbit_opps = jitterbit_opps[order(-jitterbit_opps$ssv3.100001.sco, -jitterbit_opps$ssv3.100001.rscos.1),]
decile_opps(jitterbit_opps, 'isClo', 'won', 'amo', 'clo','2015-03-20T00:00:00.000Z')
trackmaven_opps = trackmaven_opps[order(-trackmaven_opps$ssv3.100001.sco, -trackmaven_opps$ssv3.100001.rscos.1),]
decile_opps(trackmaven_opps, 'isClo', 'won', 'amo', 'clo','2015-03-04T00:00:00.000Z')
jama_opps = jama_opps[order(-jama_opps$ssv3.100001.sco, -jama_opps$ssv3.100001.rscos.1),]
decile_opps(jama_opps, 'isClo', 'won', 'amo', 'clo','2014-11-24T00:00:00.000Z')
kissmetrics_opps = kissmetrics_opps[order(-kissmetrics_opps$ssv3.100001.sco, -kissmetrics_opps$ssv3.100001.rscos.1),]
decile_opps(kissmetrics_opps, 'isClo', 'won', 'amo', 'clo','2014-11-26T00:00:00.000Z')
newvoicemedia_opps.merge = newvoicemedia_opps.merge[order(-newvoicemedia_opps.merge$ssv3.100001.sco, -newvoicemedia_opps.merge$ssv3.100001.rscos.1),]
decile_opps(newvoicemedia_opps.merge, 'isClo', 'won', 'amo', 'CloseDate','2015-04-15')
guardian_opps = guardian_opps[order(-guardian_opps$ssv3.100001.sco, -guardian_opps$ssv3.100001.rscos.1),]
decile_opps(guardian_opps, 'isClo', 'won', 'amo', 'clo','2015-05-08T00:00:00.000Z')
blackbaud_opps = blackbaud_opps[order(-blackbaud_opps$ssv3.100001.sco, -blackbaud_opps$ssv3.100001.rscos.1),]
decile_opps(blackbaud_opps, 'isClo', 'won', 'amo', 'clo','2015-02-20T00:00:00.000Z')
gainsight_opps = gainsight_opps[order(-gainsight_opps$ssv3.100001.sco, -gainsight_opps$ssv3.100001.rscos.1),]
decile_opps(gainsight_opps, 'isClo', 'won', 'amo', 'clo','2015-01-15T00:00:00.000Z')
mir3_opps = mir3_opps[order(-mir3_opps$ssv3.100001.sco, -mir3_opps$ssv3.100001.rscos.1),]
decile_opps(mir3_opps, 'isClo', 'won', 'amo', 'clo','2015-02-09T00:00:00.000Z')
reliant_opps = reliant_opps[order(-reliant_opps$ssv3.100001.sco, -reliant_opps$ssv3.100001.rscos.1),]
decile_opps(reliant_opps, 'isClo', 'won', 'amo', 'clo','2015-02-18T00:00:00.000Z')
fiksu_opps = fiksu_opps[order(-fiksu_opps$ssv3.100001.sco, -fiksu_opps$ssv3.100001.rscos.1),]
decile_opps(fiksu_opps, 'isClo', 'won', 'amo', 'clo','2015-01-01T00:00:00.000Z')
revel_opps = revel_opps[order(-revel_opps$ssv3.100001.sco, -revel_opps$ssv3.100001.rscos.1),]
decile_opps(revel_opps, 'isClo', 'won', 'amo', 'clo','2015-03-03T00:00:00.000Z')
on24_merge_constrain = on24_merge_constrain[order(-on24_merge_constrain$ssv3.100001.sco, -on24_merge_constrain$ssv3.100001.rscos.1),]
decile_opps(on24_merge_constrain, 'isClo', 'won', 'amo', 'clo','2015-03-14T00:00:00.000Z')
bitly_opps = bitly_opps[order(-bitly_opps$ssv3.100001.sco, -bitly_opps$ssv3.100001.rscos.1),]
decile_opps(bitly_opps, 'isClo', 'won', 'amo', 'clo','2015-04-14T00:00:00.000Z')
brafton_merge = brafton_merge[order(-brafton_merge$ssv3.100001.sco, -brafton_merge$ssv3.100001.rscos.1),]
decile_opps(brafton_merge, 'isClo', 'won', 'amo', 'clo','2015-04-23T00:00:00.000Z')
onstream_opps = subset(onstream_opps, (onstream_opps$isClo=='true')&(onstream_opps$cre!='')); onstream_opps = onstream_opps[order(-onstream_opps$ssv3.100001.sco, -onstream_opps$ssv3.100001.rscos.1),]
decile_opps(onstream_opps, 'isClo', 'won', 'amo', 'clo','2015-04-23T00:00:00.000Z')
#amalto_leads = amalto_leads[order(-amalto_leads$ssv3.2.sco),]
#decile_leads(amalto_leads, 'isCon', 'cre', '')
randstad_opps = randstad_opps[order(-randstad_opps$ssv3.100001.sco, -randstad_opps$ssv3.100001.rscos.1),]
decile_opps(randstad_opps, 'isClo', 'won', 'amo', 'clo','2015-04-08T00:00:00.000Z')






























--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Quartile 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_all = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_all_data.csv', header=T, stringsAsFactor=F)
data_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_4.csv', header=T, stringsAsFactor=F)
data_mongo= read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_leads.csv', header=T, stringsAsFactor=F)
data_mongo_w_activity = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_leads2.csv', header=T, stringsAsFactor=F)
data_mongo_wo_activity = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_leads3.csv', header=T, stringsAsFactor=F)
marketo_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_contacts.csv', header=T, stringsAsFactor=F)
emails = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/emails.csv', header=T, stringsAsFactor=F)
lead_contact = merge(data_sfdc, marketo_contacts, by.x='ConvertedContactId', by.y='Id', all.x=T)
lead_contact$CreatedDate.y[is.na(lead_contact$CreatedDate.y)] <- '2014-10-01T00:00:00.000Z'
#na.fill(lead_contact$CreatedDate.y, '2014-10-01T00:00:00.000Z')
lead_contact = subset(lead_contact, lead_contact$CreatedDate.x >= '2014-10-01T00:00:00.000Z' & lead_contact$CreatedDate.x < '2015-01-01T00:00:00.000Z' & lead_contact$CreatedDate.y >= '2014-10-01T00:00:00.000Z')
lead_contact = lead_contact[complete.cases(lead_contact$Email.x, lead_contact$Ftop__SpendScore__c), ]


#data_all = data_all[!is.na(data_all$ssv3.2.sco), ]
data = subset(data_sfdc, data_sfdc$CreatedDate >= '2014-10-01T00:00:00.000Z' & data_sfdc$CreatedDate < '2015-01-01T00:00:00.000Z')
#data_1 = merge(data_mongo, data, by.x='rec', by.y='Id')
data_w = merge(data_mongo_w_activity, data, by.x='rec', by.y='Id')
data_wo = merge(data_mongo_wo_activity, data, by.x='rec', by.y='Id')
data_repeat = data
#data = data[complete.cases(data$email, data$Ftop__SpendScore__c), ]
#data = data[!is.na(data$ssv3.2.sco), ]
marketo_fliptop_sort = data[order(-data$ssv3.2.sco, -data$ssv3.2.rscos.2, -data$ssv3.2.rscos.1), ]

data_sort_only = data_1[!is.na(data_1$Sort_Score__c),]
data_sort_only$Ftop__SpendScore__c[is.na(data_sort_only$Ftop__SpendScore__c)] <- data_sort_only$ssv3.2.sco[is.na(data_sort_only$Ftop__SpendScore__c)]
data_sort_only = data_sort_only[order(-data_sort_only$ssv3.2.sco, -data_sort_only$ssv3.2.rscos.2, -data_sort_only$ssv3.2.rscos.1),]
#data_sort_only = data_sort_only[order(-data_sort_only$Ftop__SpendScore__c),]
marketo_sort_only = data_sort_only[order(-data_sort_only$Sort_Score__c),]


ftop_sort = data_repeat[order(-data_repeat$Ftop__SpendScore__c), ]
#marketo_fliptop_sort = lead_contact[order(-lead_contact$Ftop__SpendScore__c), ]
#marketo_marketo_sort = lead_contact[order(-lead_contact$Sort_Score__c), ]


#disjoint_touch1 = subset(data, Ftop__SpendScore__c >=8 & Touch_Stage_Sort_Score_Snapshot__c < 100)
#disjoint_touch2 = subset(data_touch, Ftop__SpendScore__c >=9 & Touch_Stage_Sort_Score_Snapshot__c < 100)

#disjoint_ftop1 = subset(data, Ftop__SpendScore__c < 8 & Touch_Stage_Sort_Score_Snapshot__c >= 100)
#disjoint_ftop2 = subset(data, Ftop__SpendScore__c < 9 & Touch_Stage_Sort_Score_Snapshot__c >= 100)

#marketo_marketo_sort = data[order(-data$Sort_Score__c), ]
#marketo_marketo_sort1 = data_spendscore[order(-data_spendscore$Sort_Score__c), ]

#data_all_touch_only = subset(data_all, data_all$CreatedDate >= '2014-10-01T00:00:00.000Z' & data_all$CreatedDate < '2015-01-01T00:00:00.000Z')
#data_all_touch_only = data_all_touch_only[!is.na(data_all_touch_only$Touch_Stage_Sort_Score_Snapshot__c),]
#data_all_touch_only$Ftop__SpendScore__c[is.na(data_all_touch_only$Ftop__SpendScore__c)] <- data_all_touch_only$ssv3.2.sco[is.na(data_all_touch_only$Ftop__SpendScore__c)]
d#ata_all_touch_only = data_all_touch_only[order(-data_all_touch_only$Ftop__SpendScore__c), ]


#data_touch_only = data_1[!is.na(data_1$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only = data_w[!is.na(data_w$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only = data_wo[!is.na(data_wo$Touch_Stage_Sort_Score_Snapshot__c),]

#data_touch_only$Ftop__SpendScore__c[is.na(data_touch_only$Ftop__SpendScore__c)] <- data_touch_only$ssv3.2.sco[is.na(data_touch_only$Ftop__SpendScore__c)]
#data_touch_only = merge(data_touch_only, emails, by.x='rec', by.y='Id', all.x=T)
#data_touch_only = data_touch_only[order(-data_touch_only$Ftop__SpendScore__c),]
data_touch_only_rf3cl = data_touch_only[order(-data_touch_only$ssv3.2.sco, -data_touch_only$ssv3.2.rscos.2, -data_touch_only$ssv3.2.rscos.1),]
data_touch_only_gb3cl = data_touch_only[order(-data_touch_only$ssv3.12.sco, -data_touch_only$ssv3.12.rscos.2, -data_touch_only$ssv3.12.rscos.1),]
marketo_touch_only = data_touch_only[order(-data_touch_only$Touch_Stage_Sort_Score_Snapshot__c), ]
#write.table(data_touch_only, '/Users/fliptop/Desktop/Touch_stage_1.csv', row.names=F, sep=',')

data_touch = data
data_touch$Touch_Stage_Sort_Score_Snapshot__c[is.na(data_touch$Touch_Stage_Sort_Score_Snapshot__c)] = 0
marketo_touch_sort_1 = data[order(-data$Touch_Stage_Sort_Score_Snapshot__c), ]
marketo_touch_sort1 = data_touch[order(-data_touch$Touch_Stage_Sort_Score_Snapshot__c), ]
marketo_touch_sort2 = data_touch_only[order(-data_touch_only$Touch_Stage_Sort_Score_Snapshot__c), ]
ftop_touch_sort2 = data_touch_only[order(-data_touch_only$Ftop__SpendScore__c), ]

data_touch2 = subset(data, Touch_Stage_Sort_Score_Snapshot__c <= 99| is.na(Touch_Stage_Sort_Score_Snapshot__c))
data_touch3 = subset(data, Touch_Stage_Sort_Score_Snapshot__c >= 100)

data_touch_sort = data_touch3[order(-data_touch3$Touch_Stage_Sort_Score_Snapshot__c), ]
  
ftop_touch_sort = data_touch2[order(-data_touch2$ssv3.2.sco, -data_touch2$ssv3.2.rscos.2, data_touch2$ssv3.2.rscos.1), ]


data_spendscore = data[!is.na(data$Ftop__SpendScore__c), ]
data_spendscore1 = data[is.na(data$Ftop__SpendScore__c), ]
data_spendscore_bind = rbind(data_spendscore, data_spendscore1)
marketo_fliptop_sort1 = data_spendscore[order(-data_spendscore$Ftop__SpendScore__c), ]
marketo_fliptop_sort1_1 = data_spendscore_bind[order(-data_spendscore_bind$Ftop__SpendScore__c), ]

data_spendscore_snapshot = data
data_spendscore_snapshot = data[!is.na(data$Fliptop_SpendScore_Snapshot__c), ]
snapshot_spendscore_sort1 = data_spendscore_snapshot[order(-data_spendscore_snapshot$Fliptop_SpendScore_Snapshot__c), ]
snapshot_spendscore_sort2 = data_spendscore_snapshot[order(-data_spendscore_snapshot$Ftop__SpendScore__c), ]




data_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_4.csv', header=T, stringsAsFactor=F)
data_sfdc_test = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/4_01/Marketo_sfdc_04_01_test.csv', header=T, stringsAsFactor=F)
data_mongo = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_leads_04_01.csv', header=T, stringsAsFactor=F)
data_mongo_wo_activity = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/Marketo_leads3.csv', header=T, stringsAsFactor=F)

data = subset(data_sfdc, data_sfdc$CreatedDate >= '2015-01-01T08:00:00.000Z' & data_sfdc$CreatedDate < '2015-03-31T16:00:00.000Z')
data_test = merge(data_mongo, data_sfdc_test, by.x='rec', by.y='Id')
#data_test = subset(data_test, data_test$CreatedDate >= '2015-01-01T00:00:00.000Z')# & data_sfdc$CreatedDate < '2015-01-01T00:00:00.000Z')
data_test = subset(data_test, data_test$IsConverted == 'false' & data_test$IsDeleted == 'false')
data_test = data_test[!is.na(data_test$Touch_Stage_Sort_Score_Snapshot__c),]
data_jan_feb = subset(data_sfdc, data_sfdc$CreatedDate >= '2015-01-01T08:00:00.000Z' & data_sfdc$CreatedDate < '2015-03-01T16:00:00.000Z')
data_jan_feb = subset(data_test, data_test$CreatedDate >= '2015-01-01T08:00:00.000Z' & data_test$CreatedDate < '2015-03-01T16:00:00.000Z')
data_march = subset(data_sfdc, data_sfdc$CreatedDate >= '2015-03-01T00:00:00.000Z' & data_sfdc$CreatedDate < '2015-04-01T00:00:00.000Z')
data_q4 = subset(data_sfdc, data_sfdc$CreatedDate >= '2014-10-01T00:00:00.000Z' & data_sfdc$CreatedDate < '2015-01-01T00:00:00.000Z')
#data_w = merge(data_mongo, data, by.x='rec', by.y='Id')
#data_wo = merge(data_mongo_wo_activity, data, by.x='rec', by.y='Id')

#----------------------------------------------------------------------------
data_100 = merge(data_mongo, data, by.x='rec', by.y='Id')
data_touch_only_100 = data_100[!is.na(data_100$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only_100_scale = data_touch_only_100[order(-data_touch_only_100$msv1.700002.sco),]
comment(data_touch_only_100_scale) = 'Marketo_touch_only_ftop_100_scale'; marketo_quartile(data_touch_only_100_scale)

marketo_touch_only_100 = data_touch_only_100_scale[order(-data_touch_only_100_scale$Touch_Stage_Sort_Score_Snapshot__c), ]
comment(marketo_touch_only_100) = 'Marketo_touch_only_marketo'; marketo_marketo_quartile(marketo_touch_only_100)

#----------------------------------------------------------------------------
data_100_jan_feb = merge(data_mongo, data_jan_feb, by.x='rec', by.y='Id')
data_touch_only_100_jan_feb = data_100_jan_feb[!is.na(data_100_jan_feb$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only_100_scale_jan_feb = data_touch_only_100_jan_feb[order(-data_touch_only_100_jan_feb$msv1.700002.sco),]
comment(data_touch_only_100_scale_jan_feb) = 'Marketo_touch_only_ftop_100_scale_jan_feb'; marketo_quartile(data_touch_only_100_scale_jan_feb)

marketo_touch_only_100_jan_feb = data_touch_only_100_scale_jan_feb[order(-data_touch_only_100_scale_jan_feb$Touch_Stage_Sort_Score_Snapshot__c), ]
comment(marketo_touch_only_100_jan_feb) = 'Marketo_touch_only_marketo_jan_feb'; marketo_marketo_quartile(marketo_touch_only_100_jan_feb)

#----------------------------------------------------------------------------
data_100_march = merge(data_mongo, data_march, by.x='rec', by.y='Id')
data_touch_only_100_march = data_100_march[!is.na(data_100_march$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only_100_scale_march = data_touch_only_100_march[order(-data_touch_only_100_march$msv1.700002.sco),]
comment(data_touch_only_100_scale_march) = 'Marketo_touch_only_ftop_100_scale_march'; marketo_quartile(data_touch_only_100_scale_march)

marketo_touch_only_100_march = data_touch_only_100_scale_march[order(-data_touch_only_100_scale_march$Touch_Stage_Sort_Score_Snapshot__c), ]
comment(marketo_touch_only_100_march) = 'Marketo_touch_only_marketo_march'; marketo_marketo_quartile(marketo_touch_only_100_march)

#----------------------------------------------------------------------------
data_100_q4 = merge(data_mongo, data_q4, by.x='rec', by.y='Id')
data_touch_only_100_q4 = data_100_march[!is.na(data_100_march$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only_100_q4 = data_touch_only_100_q4[order(-data_touch_only_100_q4$ssv3.2.sco, -data_touch_only_100_q4$ssv3.2.rscos.2, -data_touch_only_100_q4$ssv3.2.rscos.1),]
comment(data_touch_only_100_q4) = 'Marketo_touch_only_ftop_100_scale_q4'; marketo_quartile(data_touch_only_100_q4)

marketo_touch_only_100_q4 = data_touch_only_100_q4[order(-data_touch_only_100_q4$Touch_Stage_Sort_Score_Snapshot__c), ]
comment(marketo_touch_only_100_q4) = 'Marketo_touch_only_marketo_q4'; marketo_marketo_quartile(marketo_touch_only_100_q4)

#----------------------------------------------------------------------------
data_touch_only_100_scale$IsConverted_custom = data_touch_only_100_scale$Status == 'Opportunity' | data_touch_only_100_scale$Status == 'Sales Lead - Potential Opportunity'
data_touch_only_100_scale_jan_feb$IsConverted_custom = data_touch_only_100_scale_jan_feb$Status == 'Opportunity' | data_touch_only_100_scale_jan_feb$Status == 'Sales Lead - Potential Opportunity'
data_touch_only_100_scale_march$IsConverted_custom = data_touch_only_100_scale_march$Status == 'Opportunity' | data_touch_only_100_scale_march$Status == 'Sales Lead - Potential Opportunity'
#----------------------------------------------------------------------------



data_touch_only_100_scale$IsConverted_custom = data_touch_only_100_scale$Status == 'Opportunity' | data_touch_only_100_scale$Status == 'Sales Lead - Potential Opportunity'
write.table(data_touch_only_100_scale[,c('rec','CreatedDate','msv1.700002.sco','IsConverted_custom','Touch_Stage_Sort_Score_Snapshot__c')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_31/records_list_100_scale.csv', row.names=F, sep=',')


data_touch_only_w = data_w[!is.na(data_w$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only_wo = data_wo[!is.na(data_wo$Touch_Stage_Sort_Score_Snapshot__c),]

data_touch_only_rf3cl_w = data_touch_only_w[order(-data_touch_only_w$ssv3.2.sco, -data_touch_only_w$ssv3.2.rscos.2, -data_touch_only_w$ssv3.2.rscos.1),]
data_touch_only_gb3cl_w = data_touch_only_w[order(-data_touch_only_w$ssv3.12.sco, -data_touch_only_w$ssv3.12.rscos.2, -data_touch_only_w$ssv3.12.rscos.1),]

data_touch_only_rf3cl_wo = data_touch_only_wo[order(-data_touch_only_wo$ssv3.2.sco, -data_touch_only_wo$ssv3.2.rscos.2, -data_touch_only_wo$ssv3.2.rscos.1),]
data_touch_only_gb3cl_wo = data_touch_only_wo[order(-data_touch_only_wo$ssv3.12.sco, -data_touch_only_wo$ssv3.12.rscos.2, -data_touch_only_wo$ssv3.12.rscos.1),]

comment(data_touch_only_rf3cl_w) = 'Marketo_touch_only_ftop_rf3cl'; marketo_quartile(data_touch_only_rf3cl_w)
comment(data_touch_only_gb3cl_w) = 'Marketo_touch_only_ftop_gb3cl'; marketo_quartile(data_touch_only_gb3cl_w)

comment(data_touch_only_rf3cl_wo) = 'Marketo_touch_only_ftop_rf3cl_wo_activity'; marketo_quartile(data_touch_only_rf3cl_wo)
comment(data_touch_only_gb3cl_wo) = 'Marketo_touch_only_ftop_gb3cl_wo_activity'; marketo_quartile(data_touch_only_gb3cl_wo)

marketo_touch_only_w = data_touch_only_w[order(-data_touch_only_w$Touch_Stage_Sort_Score_Snapshot__c), ]
marketo_touch_only_wo = data_touch_only_wo[order(-data_touch_only_wo$Touch_Stage_Sort_Score_Snapshot__c), ]
comment(marketo_touch_only_w) = 'Marketo_touch_only_marketo'; marketo_marketo_quartile(marketo_touch_only_w)
comment(marketo_touch_only_wo) = 'Marketo_touch_only_marketo_wo_activity'; marketo_marketo_quartile(marketo_touch_only_wo)

data_touch_only_rf3cl_w$IsConverted_custom = data_touch_only_rf3cl_w$Status == 'Opportunity' | data_touch_only_rf3cl_w$Status == 'Sales Lead - Potential Opportunity'

write.table(data_touch_only_rf3cl_w[,c('rec','IsConverted_custom')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/current_records_list.csv', sep=',', row.names=F)
write.table(data_touch_only_rf3cl_w[,c('rec','IsConverted_custom')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/previous_records_list.csv', sep=',', row.names=F)





data_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_4.csv', header=T, stringsAsFactor=F)
data_mongo_jan_feb = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_wo_activities_03_25.csv', header=T, stringsAsFactor=F)
data_date_constrain = subset(data_sfdc, data_sfdc$CreatedDate >= '2015-01-01T00:00:00.000Z' & data_sfdc$CreatedDate < '2015-03-01T00:00:00.000Z')
data_jan_feb = merge(data_mongo_jan_feb, data_date_constrain, by.x='rec', by.y='Id')

data_jan_feb = data_jan_feb[!is.na(data_jan_feb$Touch_Stage_Sort_Score_Snapshot__c),]


data_jan_feb$rf3cl_p12 = data_jan_feb$ssv3.2.rscos.2+data_jan_feb$ssv3.2.rscos.1
data_jan_feb$gb3cl_p12 = data_jan_feb$ssv3.12.rscos.2+data_jan_feb$ssv3.12.rscos.1

data_touch_only_rf3cl_p12 = data_jan_feb[order(-data_jan_feb$rf3cl_p12),]
data_touch_only_gb3cl_p12 = data_jan_feb[order(-data_jan_feb$gb3cl_p12),]

comment(data_touch_only_rf3cl_p12) = 'Marketo_touch_only_ftop_rf3cl_jan_feb_p12'; marketo_quartile(data_touch_only_rf3cl_p12)
comment(data_touch_only_gb3cl_p12) = 'Marketo_touch_only_ftop_gb3cl_jan_feb_p12'; marketo_quartile(data_touch_only_gb3cl_p12)

write.table(data_touch_only_rf3cl_p12, '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_25/records_list_03_25.csv', row.names=F, sep=',')

data_touch_only_rf3cl = data_jan_feb[order(-data_jan_feb$ssv3.2.sco, -data_jan_feb$ssv3.2.rscos.2, -data_jan_feb$ssv3.2.rscos.1),]
data_touch_only_gb3cl = data_jan_feb[order(-data_jan_feb$ssv3.12.sco, -data_jan_feb$ssv3.12.rscos.2, -data_jan_feb$ssv3.12.rscos.1),]
marketo_touch_only = data_jan_feb[order(-data_jan_feb$Touch_Stage_Sort_Score_Snapshot__c), ]

comment(data_touch_only_rf3cl) = 'Marketo_touch_only_ftop_rf3cl_jan_feb'; marketo_quartile(data_touch_only_rf3cl)
comment(data_touch_only_gb3cl) = 'Marketo_touch_only_ftop_gb3cl_jan_feb'; marketo_quartile(data_touch_only_gb3cl)
comment(marketo_touch_only) = 'Marketo_touch_only_marketo_jan_feb'; marketo_marketo_quartile(marketo_touch_only)

data_touch_only_rf3cl$IsConverted_custom = data_touch_only_rf3cl$Status == 'Opportunity' | data_touch_only_rf3cl$Status == 'Sales Lead - Potential Opportunity'

write.table(data_touch_only_rf3cl[,c('rec','IsConverted_custom')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_16_Jan_Feb/records_list_JantoFeb.csv', sep=',', row.names=F)



mani_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/marketo_mani_leads.csv', header=T, stringsAsFactor=F)
sfdc_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_04_07.csv', header=T, stringsAsFactor=F)
sfdc_new$Id_sub = substr(sfdc_new$Id, 0, 15)
mani_merge = merge(mani_leads, sfdc_new, by.x='Lead.ID', by.y='Id_sub', all.x=T)
data_mongo$Id_sub = substr(data_mongo$rec, 0, 15)
mani_merge2 = merge(mani_leads, data_mongo, by.x='Lead.ID', by.y='Id_sub')

mani_mismatch = mani_merge[mani_merge$Touch_Stage_Sort_Score_Snapshot__c.x != mani_merge$Touch_Stage_Sort_Score_Snapshot__c.y,]
mani_mismatch = mani_mismatch[!is.na(mani_mismatch$SpendScore),]
mani_missing = mani_merge[is.na(mani_merge$Id),]

write.table(mani_mismatch[,c('Lead.ID','Lead.Status','SpendScore','Touch_Stage_Sort_Score_Snapshot__c.x','Touch_Stage_Sort_Score_Snapshot__c.y')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/Mani_stuff/mismatch_leads.csv', row.names=F, sep=',')
write.table(mani_missing[,c('Lead.ID','Lead.Status','SpendScore','Touch_Stage_Sort_Score_Snapshot__c.x')], '/Users/fliptop/Desktop/Customer_Stats/Marketo/Mani_stuff/missing_leads.csv', row.names=F, sep=',')



marketo_quartile <- function(df){
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]

  #sum_1 = nrow(subset(dec_1, IsConverted == 'true'))
  #sum_2 = nrow(subset(dec_2, IsConverted == 'true'))
  #sum_3 = nrow(subset(dec_3, IsConverted == 'true'))
  #sum_4 = nrow(subset(dec_4, IsConverted == 'true'))
  
  sum_1 = nrow(subset(dec_1, Status == 'Opportunity'))+nrow(subset(dec_1, Status == 'Sales Lead ? Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status == 'Opportunity'))+nrow(subset(dec_2, Status == 'Sales Lead ? Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status == 'Opportunity'))+nrow(subset(dec_3, Status == 'Sales Lead ? Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status == 'Opportunity'))+nrow(subset(dec_4, Status == 'Sales Lead ? Potential Opportunity'))
  total_sum = sum_1+sum_2+sum_3+sum_4
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4

  agg_quart1 = sum_1/total_sum
  agg_quart2 = sum_2/total_sum
  agg_quart3 = sum_3/total_sum
  agg_quart4 = sum_4/total_sum
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                       c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                       c(sum_1,sum_2,sum_3,sum_4),
                       c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                       c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                       c(agg_quart1, agg_quart2, agg_quart3, agg_quart4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100 
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)

  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,15,20,25,30,35,40,45,50), limits=c(0,10.5))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,15,20,25,30,35,40,45,50), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }

  ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25,30,35,40,45,50), limits=c(0,15))
  agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))

  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$positions = NULL
  print(quartile_df)
  
  png(paste(comment(df),'_quartile_cr.png', sep=''), width = 500, height = 500)
  plot(ftop_conv_graph)
  dev.off()
  
  png(paste(comment(df),'_quartile_agg.png', sep=''), width = 500, height = 500)
  plot(agg_plot)
  dev.off()
  
  png(paste(comment(df),'_quartile_pie.png', sep=''), width = 500, height = 500)
  plot(agg_pie_chart)
  dev.off()
  
  file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}
marketo_marketo_quartile <- function(df){
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  #sum_1 = nrow(subset(dec_1, IsConverted == 'true'))
  #sum_2 = nrow(subset(dec_2, IsConverted == 'true'))
  #sum_3 = nrow(subset(dec_3, IsConverted == 'true'))
  #sum_4 = nrow(subset(dec_4, IsConverted == 'true'))
  
  sum_1 = nrow(subset(dec_1, Status == 'Opportunity'))+nrow(subset(dec_1, Status == 'Sales Lead ? Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status == 'Opportunity'))+nrow(subset(dec_2, Status == 'Sales Lead ? Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status == 'Opportunity'))+nrow(subset(dec_3, Status == 'Sales Lead ? Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status == 'Opportunity'))+nrow(subset(dec_4, Status == 'Sales Lead ? Potential Opportunity'))
  
  total_sum = sum_1+sum_2+sum_3+sum_4
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  agg_quart1 = sum_1/total_sum
  agg_quart2 = sum_2/total_sum
  agg_quart3 = sum_3/total_sum
  agg_quart4 = sum_4/total_sum
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                           c(agg_quart1, agg_quart2, agg_quart3, agg_quart4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
  quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
  quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100 
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_Cum = c(4,3,2,1)
    
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 5){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.10),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))
  } else if (max_height <= 10){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.15),size=4)+scale_y_continuous(breaks=c(0,2.5,5,7.5,10,15,20,25,30,35,40,45,50), limits=c(0,10.5))  
  } else if (max_height <= 25){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.50),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  } else if (max_height <= 100) {
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  }
  
  marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25,30,35,40,45,50), limits=c(0,15))
  marketo_agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='mediumpurple4')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Marketo - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  marketo_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Marketo - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('mediumpurple4','mediumpurple3','mediumpurple1','lavender'))
  #marketo_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Marketo - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  grid.arrange(marketo_conv_graph)
  grid.arrange(marketo_agg_plot)
  grid.arrange(marketo_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$positions = NULL
  print(quartile_df)
  
  png(paste(comment(df),'_quartile_cr.png', sep=''), width = 500, height = 500)
  plot(marketo_conv_graph)
  dev.off()
  
  png(paste(comment(df),'_quartile_agg.png', sep=''), width = 500, height = 500)
  plot(marketo_agg_plot)
  dev.off()
  
  png(paste(comment(df),'_quartile_pie.png', sep=''), width = 500, height = 500)
  plot(marketo_pie_chart)
  dev.off()
  
  file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df), '_quartile_stats.csv', sep='')
  write.table(quartile_df, file_name, row.names=F, sep=',')
}







x = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/current_records_list.csv', header=T, stringsAsFactor=F)
y = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/previous_records_list.csv', header=T, stringsAsFactor=F)
x$current=T
y$previous=T

x_y = merge(x,y, by.x='x', by.y='x', all.x=T)
x_y[is.na(x_y)] = F
write.table(x_y, '/Users/fliptop/Desktop/Customer_Stats/Marketo/3_13/records_list.csv', row.names=F, sep=',')





#marketo_opps <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_opportunities.csv', header=T, stringsAsFactor=F)
marketo_opps <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/opp_query.csv', header=T, stringsAsFactor=F)
marketo_opps <- subset(marketo_opps, marketo_opps$won == 1 & marketo_opps$created_date >= '2014-00-00 00:00:00' & marketo_opps$created_date < '2015-00-00 00:00:00')# & marketo_opps$closed_date < '2015-00-00 00:00:00')
marketo_opps$created_date_1 <- as.Date(strptime(marketo_opps$created_date, "%Y-%m-%d"))
marketo_opps$close_date_1 <- as.Date(strptime(marketo_opps$closed_date, "%Y-%m-%d"))
marketo_opps$cycle_length <- marketo_opps$close_date_1 - marketo_opps$created_date_1
marketo_opps_quarter = subset(marketo_opps, marketo_opps$created_date >= '2014-10-01 00:00:00')
marketo_opps_month = subset(marketo_opps, marketo_opps$created_date >= '2014-12-01 00:00:00')

mean(marketo_opps$cycle_length);mean(marketo_opps_quarter$cycle_length);mean(marketo_opps_month$cycle_length)

marketo_leads_cycle <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_year_14.csv', stringsAsFactor=F, header=T)
marketo_leads_cycle <- subset(marketo_leads_cycle, marketo_leads_cycle$IsConverted == 'true' & marketo_leads_cycle$CreatedDate < '2015-01-01T00:00:00.000Z' & marketo_leads_cycle$ConvertedDate < '2015-01-01T00:00:00.000Z')
marketo_leads_cycle$created_date_1 <- as.Date(strptime(marketo_leads_cycle$CreatedDate, "%Y-%m-%d"))
marketo_leads_cycle$close_date_1 <- as.Date(strptime(marketo_leads_cycle$ConvertedDate, "%Y-%m-%d"))
marketo_leads_cycle$cycle_length <- marketo_leads_cycle$close_date_1 - marketo_leads_cycle$created_date_1
marketo_leads_quarter = subset(marketo_leads_cycle, marketo_leads_cycle$created_date >= '2014-10-01 00:00:00')
marketo_leads_month = subset(marketo_leads_cycle, marketo_leads_cycle$created_date >= '2014-12-01 00:00:00')

mean(marketo_leads_cycle$cycle_length);mean(marketo_leads_quarter$cycle_length);mean(marketo_leads_month$cycle_length)



bitly_opps2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities_repush.csv', header=T, stringsAsFactor=F)
bitly_0 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities_0.csv', header=T, stringsAsFactor=F)
bitly_merge = merge(bitly_0, bitly_opps2, by.x='Id', by.y='id', all.x=T)
write.table(bitly_merge, '/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities_repush2.csv', sep=',', row.names=F)

bitly_0 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads_0.csv', header=T, stringsAsFactor=F)
bitly_leads2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads_repush.csv', header=T, stringsAsFactor=F)
bitly_merge_leads = merge(bitly_0, bitly_leads2, by.x='Id', by.y='id', all.x=T)
write.table(bitly_merge_leads, '/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads_repush2.csv', sep=',', row.names=F)




bitly_0 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads_unscored.csv', header=T, stringsAsFactor=F)
bitly_leads2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads.csv', header=T, stringsAsFactor=F)
bitly_merge_leads = merge(bitly_0, bitly_leads2, by.x='Id', by.y='rec', all.x=T)
write.table(bitly_merge_leads, '/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_leads_unscored_scored.csv', sep=',', row.names=F)

bitly_0 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opps_unscored.csv', header=T, stringsAsFactor=F)
bitly_opps2 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opportunities.csv', header=T, stringsAsFactor=F)
bitly_merge_opps = merge(bitly_0, bitly_opps2, by.x='Id', by.y='rec', all.x=T)
write.table(bitly_merge_opps, '/Users/fliptop/Desktop/Customer_Stats/Bitly/Bitly_opps_unscored_scored.csv', sep=',', row.names=F)





