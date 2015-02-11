library(ggplot2)
require(gridExtra)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#List of functions

#Date_format = '0000-00-00T00:00:00.000Z'

#decile_leads(df1,isconverted_name,date_constraint)
#competitor_decile_leads(df1,competitor_name,date_constraint)

#decile_opps(df1,iswon_name,amount_name,date_constraint)
#competitor_decile_opps(df1,competitor_name,date_constraint)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#quartile_leads(df1,isconverted_name,date_constraint)
#competitor_quartile_leads(df1,competitor_name,date_constraint)

#quartile_opps(df1,iswon_name,amount_name,date_constraint)
#competitor_quartile_opps(df1,competitor_name,date_constraint)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#grade_bucket_leads(df1,score_name,isconverted_name,date_constraint, A_min, B_min, C_min)
#competitor_grade_bucket_leads(df1,competitor_name,score_name,date_constraint)

#grade_bucket_opps(df1,score_name,iswon_name,amount_name,date_constraint, A_min, B_min, C_min)
#competitor_grade_bucket_opps(df1,competitor_name,score_name,date_constraint)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE - LEADS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
decile_leads <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
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
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == 'True'))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == 'True'))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == 'True'))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == 'True'))
  sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == 'True'))
  sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == 'True'))
  sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == 'True'))
  sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == 'True'))
  sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == 'True'))
  sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == 'True'))
    
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
  decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
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
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2,0.225,20,30,40,50,60,70,80,90,100), limits=c(0,0.21))    
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  print(decile_df, row.names=FALSE)
}
competitor_decile_leads <- function(df1, competitor, score_name, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
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
  
  sum_1 = nrow(subset(dec_1, IsConverted == 'True'))
  sum_2 = nrow(subset(dec_2, IsConverted == 'True'))
  sum_3 = nrow(subset(dec_3, IsConverted == 'True'))
  sum_4 = nrow(subset(dec_4, IsConverted == 'True'))
  sum_5 = nrow(subset(dec_5, IsConverted == 'True'))
  sum_6 = nrow(subset(dec_6, IsConverted == 'True'))
  sum_7 = nrow(subset(dec_7, IsConverted == 'True'))
  sum_8 = nrow(subset(dec_8, IsConverted == 'True'))
  sum_9 = nrow(subset(dec_9, IsConverted == 'True'))
  sum_10 = nrow(subset(dec_10, IsConverted == 'True'))
  
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
  decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
  decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
  decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  
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
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Decile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,2.5,5,7.5,10,15,20,30,40,50,60,70,80,90,100), limits=c(0,12))
  #agg_plot = ggplot(decile_df, aes(x=Decile_Cum, y=Cum_convert_percent))+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Infer - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Leads'), x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  print(decile_df, row.names=FALSE)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#DECILE - OPPORTUNITIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
decile_opps <- function(df1, iswon_name, amount_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
  df = subset(df1, df1[ ,'clo'] >= date_constraint)
  
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
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == 'True'))
  sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == 'True'))
  sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == 'True'))
  sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == 'True'))
  sum_5 = nrow(subset(dec_5, dec_5[ ,iswon_name] == 'True'))
  sum_6 = nrow(subset(dec_6, dec_6[ ,iswon_name] == 'True'))
  sum_7 = nrow(subset(dec_7, dec_7[ ,iswon_name] == 'True'))
  sum_8 = nrow(subset(dec_8, dec_8[ ,iswon_name] == 'True'))
  sum_9 = nrow(subset(dec_9, dec_9[ ,iswon_name] == 'True'))
  sum_10 = nrow(subset(dec_10, dec_10[ ,iswon_name] == 'True'))
  
  revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_5 = sum(subset(dec_5, dec_5[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_6 = sum(subset(dec_6, dec_6[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_7 = sum(subset(dec_7, dec_7[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_8 = sum(subset(dec_8, dec_8[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_9 = sum(subset(dec_9, dec_9[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_10 = sum(subset(dec_10, dec_10[ ,iswon_name] == 'True')[ ,amount_name])
  
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
  colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Decile_agg_wr','Win_amount','Cum_revenue_percent')
  decile_df$Win_Percent = (decile_df$Num_opps_won/decile_df$Num_in_decile)*100
  decile_df$Cum_win_percent = decile_df$Cum_win_percent*100
  decile_df$Cum_revenue_percent = decile_df$Cum_revenue_percent*100
  decile_df$Decile_agg_wr = decile_df$Decile_agg_wr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  
  max_height = max(decile_df$Win_Percent)
  if (max_height <= 1){
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
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  decile_df$positions = cumsum(decile_df$Decile_agg_wr) - decile_df$Decile_agg_wr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_wr, fill=Decile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_wr))
  rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  print(decile_df, row.names=FALSE)
}
competitor_decile_opps <- function(df1, competitor, score_name, iswon_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
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
  
  sum_1 = nrow(subset(dec_1, IsWon == 'True'))
  sum_2 = nrow(subset(dec_2, IsWon == 'True'))
  sum_3 = nrow(subset(dec_3, IsWon == 'True'))
  sum_4 = nrow(subset(dec_4, IsWon == 'True'))
  sum_5 = nrow(subset(dec_5, IsWon == 'True'))
  sum_6 = nrow(subset(dec_6, IsWon == 'True'))
  sum_7 = nrow(subset(dec_7, IsWon == 'True'))
  sum_8 = nrow(subset(dec_8, IsWon == 'True'))
  sum_9 = nrow(subset(dec_9, IsWon == 'True'))
  sum_10 = nrow(subset(dec_10, IsWon == 'True'))
  
  revenue_1 = sum(subset(dec_1, IsWon == 'True')$Amount)
  revenue_2 = sum(subset(dec_2, IsWon == 'True')$Amount)
  revenue_3 = sum(subset(dec_3, IsWon == 'True')$Amount)
  revenue_4 = sum(subset(dec_4, IsWon == 'True')$Amount)
  revenue_5 = sum(subset(dec_5, IsWon == 'True')$Amount)
  revenue_6 = sum(subset(dec_6, IsWon == 'True')$Amount)
  revenue_7 = sum(subset(dec_7, IsWon == 'True')$Amount)
  revenue_8 = sum(subset(dec_8, IsWon == 'True')$Amount)
  revenue_9 = sum(subset(dec_9, IsWon == 'True')$Amount)
  revenue_10 = sum(subset(dec_10, IsWon == 'True')$Amount)
  
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
  colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Decile_agg_wr','Win_amount','Cum_revenue_percent')
  decile_df$Win_Percent = (decile_df$Num_opps_won/decile_df$Num_in_decile)*100
  decile_df$Cum_win_percent = decile_df$Cum_win_percent*100
  decile_df$Cum_revenue_percent = decile_df$Cum_revenue_percent*100
  decile_df$Decile_agg_wr = decile_df$Decile_agg_wr*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
  decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
  
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
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  decile_df$positions = cumsum(decile_df$Decile_agg_wr) - decile_df$Decile_agg_wr/2
  agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_wr, fill=Decile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_wr))
  rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  decile_df$Decile_Cum = NULL
  decile_df$Agg_axis = NULL
  decile_df$positions = NULL
  print(decile_df, row.names=FALSE)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#QUARTILE - LEADS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quartile_leads <- function(df1, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]

  sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == 'True'))
  sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == 'True'))
  sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == 'True'))
  sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == 'True'))

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
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }

  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste('Fliptop - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
}
competitor_quartile_leads <- function(df1, competitor, score_name, isconverted_name, created_date_name, date_constraint){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  sum_1 = nrow(subset(dec_1, IsConverted == 'True'))
  sum_2 = nrow(subset(dec_2, IsConverted == 'True'))
  sum_3 = nrow(subset(dec_3, IsConverted == 'True'))
  sum_4 = nrow(subset(dec_4, IsConverted == 'True'))
  
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
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor,' - Percentage of Converted Leads'), x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  
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
quartile_opps <- function(df1, iswon_name, amount_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
  df = subset(df1, df1[ ,'clo'] >= date_constraint)
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == 'True'))
  sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == 'True'))
  sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == 'True'))
  sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == 'True'))
    
  revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == 'True')[ ,amount_name])
  
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
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_win) - quartile_df$Quartile_agg_win/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_win))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_win)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Aggregate Revenue Percentage by Quartile (since model creation)', x='Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
  grid.arrange(rev_plot)
  quartile_df$Quartile_Cum = NULL
  quartile_df$Agg_axis = NULL
  quartile_df$positions = NULL
  print(quartile_df, row.names=FALSE)
}
competitor_quartile_opps <- function(df1, competitor, score_name, iswon_name, date_constraint){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
  df = subset(df1, df1[ ,'CloseDate'] >= date_constraint)
  df = df[order(-df[,score_name]), ]
  
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):nrow(df), ]
  
  sum_1 = nrow(subset(dec_1, IsWon == 'True'))
  sum_2 = nrow(subset(dec_2, IsWon == 'True'))
  sum_3 = nrow(subset(dec_3, IsWon == 'True'))
  sum_4 = nrow(subset(dec_4, IsWon == 'True'))
  
  revenue_1 = sum(subset(dec_1, IsWon == 'True')$Amount)
  revenue_2 = sum(subset(dec_2, IsWon == 'True')$Amount)
  revenue_3 = sum(subset(dec_3, IsWon == 'True')$Amount)
  revenue_4 = sum(subset(dec_4, IsWon == 'True')$Amount)
  
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
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate (since model creation)'), x='Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  #rev_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_revenue_percent)), y=Cum_revenue_percent-3), size=3)  
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cum_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_win_percent)), y=Cum_win_percent-3), size=3)  
  quartile_df$positions = cumsum(quartile_df$Quartile_agg_win) - quartile_df$Quartile_agg_win/2
  #agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_win, fill=Quartile))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Quartile_agg_win)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
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
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
  df = subset(df1, df1[ ,created_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] <= 10 & df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == 'True'))
  c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == 'True'))
  c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == 'True'))
  c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == 'True'))
  
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
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,10))  
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
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
  #file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df1), '_stats.csv', sep='')
  #write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_leads <- function(df1, competitor, score_name, isconverted_name,created_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
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
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  c_A = nrow(subset(grade_A, IsConverted == 'True'))
  c_B = nrow(subset(grade_B, IsConverted == 'True'))
  c_C = nrow(subset(grade_C, IsConverted == 'True'))
  c_D = nrow(subset(grade_D, IsConverted == 'True'))
  
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
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Lead Conversion Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.1),size=4)+scale_y_continuous(breaks=c(0,.5,1,1.5,2,2.5,5,10,15,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Leads'), x='Stack-ranked Leads by Grades (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Leads'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  grid.arrange(comp_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#BUCKET - OPPORTUNITIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
grade_bucket_opps <- function(df1, score_name, iswon_name, amount_name, closed_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
  df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] <= 10 & df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D) 
  
  c_A = nrow(subset(grade_A, grade_A[ ,iswon_name] == 'True'))
  c_B = nrow(subset(grade_B, grade_B[ ,iswon_name] == 'True'))
  c_C = nrow(subset(grade_C, grade_C[ ,iswon_name] == 'True'))
  c_D = nrow(subset(grade_D, grade_D[ ,iswon_name] == 'True'))
  
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
  
  revenue_A = sum(subset(grade_A, grade_A[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_B = sum(subset(grade_B, grade_B[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_C = sum(subset(grade_C, grade_C[ ,iswon_name] == 'True')[ ,amount_name])
  revenue_D = sum(subset(grade_D, grade_D[ ,iswon_name] == 'True')[ ,amount_name])
  
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
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_Range', 'Num_in_bucket', 'Won', 'Win_Rate', 'Agg_WR','Bucket_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_WR = bucket_conversion$Agg_WR*100
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
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.05),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 10){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.15),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100), limits=c(0,10))
  } else if (max_height <= 24){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 50){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_WR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Fliptop - Percentage of Opportunities Won', x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_WR)), y=Agg_WR-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_wr) - bucket_conversion$Bucket_agg_wr/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_wr))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Won Opportunities')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_wr)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
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
  
  #file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df1), '_stats.csv', sep='')
  #write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_opps <- function(df1, competitor, score_name, iswon_name, closed_date_name, date_constraint, A_min, B_min, C_min){
  df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', 'True')
  df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 0)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D) 
  
  c_A = nrow(subset(grade_A, IsWon == 'True'))
  c_B = nrow(subset(grade_B, IsWon == 'True'))
  c_C = nrow(subset(grade_C, IsWon == 'True'))
  c_D = nrow(subset(grade_D, IsWon == 'True'))
  
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
  
  revenue_A = sum(subset(grade_A, IsWon == 'True')$Amount)
  revenue_B = sum(subset(grade_B, IsWon == 'True')$Amount)
  revenue_C = sum(subset(grade_C, IsWon == 'True')$Amount)
  revenue_D = sum(subset(grade_D, IsWon == 'True')$Amount)
  
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
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_Range', 'Num_in_bucket', 'Won', 'Win_Rate', 'Agg_WR','Bucket_agg_wr', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_WR = bucket_conversion$Agg_WR*100
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
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+1.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= paste(competitor, ' - Opportunity Win Rate'), x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_WR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title=paste(competitor, ' - Percentage of Opportunities Won'), x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_WR)), y=Agg_WR-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_wr) - bucket_conversion$Bucket_agg_wr/2
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_wr))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_wr, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Won Opportunities'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_wr)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
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
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] <= 10 & df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 1)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  #c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == 'True'))
  #c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == 'True'))
  #c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == 'True'))
  #c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == 'True'))
  
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
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Account to Opportunity Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Accounts', x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
  
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
  
  #file_name = paste('/Users/fliptop/Desktop/Customer_Stats/JUNK/', comment(df1), '_stats.csv', sep='')
  #write.table(bucket_conversion, file_name, row.names=F, sep=',')
}
competitor_grade_bucket_accounts <- function(df1, competitor, score_name, isconverted_name, date_field, date_constraint, A_min, B_min, C_min){
  df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', 'True')
  df = subset(df1, df1[ ,date_field] >= date_constraint)
  
  grade_A = subset(df, df[ ,score_name] >= A_min)
  grade_B = subset(df, df[ ,score_name] <= A_min-1 & df[ ,score_name] >= B_min)
  grade_C = subset(df, df[ ,score_name] <= B_min-1 & df[ ,score_name] >= C_min)
  grade_D = subset(df, df[ ,score_name] <= C_min-1 & df[ ,score_name] >= 0)
  
  #range_A = paste(min(grade_A[ ,score_name]), ',', max(grade_A[ ,score_name]))
  #range_B = paste(min(grade_B[ ,score_name]), ',', max(grade_B[ ,score_name]))
  #range_C = paste(min(grade_C[ ,score_name]), ',', max(grade_C[ ,score_name]))
  #range_D = paste(min(grade_D[ ,score_name]), ',', max(grade_D[ ,score_name]))
  
  range_A = paste(A_min, ',', 10)
  range_B = paste(B_min, ',', A_min-1)
  range_C = paste(C_min, ',', B_min-1)
  range_D = paste(1, ',', C_min-1)
  
  num_A = nrow(grade_A)
  num_B = nrow(grade_B)
  num_C = nrow(grade_C)
  num_D = nrow(grade_D)  
  
  #c_A = nrow(subset(grade_A, grade_A[ ,isconverted_name] == 'True'))
  #c_B = nrow(subset(grade_B, grade_B[ ,isconverted_name] == 'True'))
  #c_C = nrow(subset(grade_C, grade_C[ ,isconverted_name] == 'True'))
  #c_D = nrow(subset(grade_D, grade_D[ ,isconverted_name] == 'True'))
  
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
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title=paste(competitor, ' - Account to Opportunity Rates'), x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title=paste(competitor, ' - Percentage of Converted Accounts'), x='Percentage of Stack-ranked Accounts (Cumulative)', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
  bucket_conversion$positions = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
  agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle(paste(competitor, ' - Percentage of Total Converted Accounts'))+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=bucket_conversion$positions, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  #agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Accounts')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=paste(sprintf('%.2f%%', Bucket_agg_converted)),y=positions))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('lemonchiffon4','lemonchiffon3','lemonchiffon1','ivory'))
  
  bucket_conversion$Grade_Bucket_levels <- NULL
  bucket_conversion$positions <- NULL
  print(bucket_conversion, row.names=FALSE)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(agg_pie_chart)
}








#NEED TO SET CORRECT WORKING DRIVE EACH WEEK TO PREVENT OVERWRITING PREVIOUS GRAPHS
setwd("/Users/fliptop/Desktop/Customer_Stats/02_16_2015/")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
blackbaud_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_leads.csv', header=T, stringsAsFactor=F)
blackbaud_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Blackbaud/Blackbaud_opportunities.csv', header=T, stringsAsFactor=F)
blackbaud_opps = subset(blackbaud_opps, blackbaud_opps$isClo=='true')
comment(blackbaud_leads) = 'blackbaud_leads'; comment(blackbaud_opps) = 'blackbaud_opps'

grade_bucket_leads(blackbaud_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-01-22T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(blackbaud_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2015-01-22T00:00:00.000Z', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

on24_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/ON24_leads.csv', header=T, stringsAsFactor=F)
on24_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/ON24/ON24_opportunities.csv', header=T, stringsAsFactor=F)
on24_opps = subset(on24_opps, on24_opps$isClo=='true')
comment(on24_leads) = 'on24_leads'; comment(on24_opps) = 'on24_opps'

grade_bucket_leads(on24_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-10-24T00:00:00.000Z', 7, 5, 3)
grade_bucket_opps(on24_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2015-01-22T00:00:00.000Z', 9, 5, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
csod_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_leads.csv', header=T, stringsAsFactor=F)  
csod_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_opportunities.csv', header=T, stringsAsFactor=F)  
csod_opps = subset(csod_opps, csod_opps$isClo=='true')
csod_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_accounts.csv', header=T, stringsAsFactor=F)
csod_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/acct_opp_query.csv', header=T, stringsAsFactor=F)
csod_accts = merge(csod_accts, csod_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
csod_accts = csod_accts[!duplicated(csod_accts$rec), ]
comment(csod_leads) = 'csod_leads'; comment(csod_opps) = 'csod_opps'; comment(csod_accts) = 'csod_accts'

grade_bucket_opps(csod_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-11-16T00:00:00.000Z', 6, 3, 2)
grade_bucket_accounts(csod_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 7, 6, 3)

csod_acct_new = subset(csod_accts, csod_accts$created_date >= '2014-11-16 00:00:00' | is.na(csod_accts$closed_date))
comment(csod_acct_new) = 'csod_acct_new'
grade_bucket_accounts(csod_acct_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 7, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mir3_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_leads.csv', header=T, stringsAsFactor=F)  
mir3_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_opportunities.csv', header=T, stringsAsFactor=F)  
mir3_opps = subset(mir3_opps, mir3_opps$isClo=='true')
mir3_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/MIR3_accounts.csv', header=T, stringsAsFactor=F)
mir3_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/MIR3/acct_opp_query.csv', header=T, stringsAsFactor=F)
mir3_accts = merge(mir3_accts, mir3_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
mir3_accts = mir3_accts[!duplicated(mir3_accts$rec), ]
comment(mir3_leads) = 'mir3_leads'; comment(mir3_opps) = 'mir3_opps'; comment(mir3_accts) = 'mir3_accts'

grade_bucket_leads(mir3_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-12-12T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(mir3_opps, 'ssv3.100011.sco', 'won', 'amo', 'clo', '2014-12-12T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(mir3_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 6, 3)

mir3_accts_new = subset(mir3_accts, mir3_accts$created_date >= '2014-11-16 00:00:00' | is.na(mir3_accts$closed_date))
comment(mir3_accts_new) = 'mir3_accts_new'
grade_bucket_accounts(mir3_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
guardian_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_leads.csv', header=T, stringsAsFactor=F)  
guardian_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_opportunities.csv', header=T, stringsAsFactor=F)  
guardian_opps = subset(guardian_opps, guardian_opps$isClo=='true')
guardian_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/Guardian_accounts.csv', header=T, stringsAsFactor=F)
guardian_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/acct_opp_query.csv', header=T, stringsAsFactor=F)
guardian_accts = merge(guardian_accts, guardian_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
guardian_accts = guardian_accts[!duplicated(guardian_accts$rec), ]
comment(guardian_leads) = 'guardian_leads'; comment(guardian_opps) = 'guardian_opps'; comment(guardian_accts) = 'guardian_accts'

guardian_sfdc_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Guardian/guardian_sfdc.csv', header=T, stringsAsFactor=F)
guardian_sfdc_opps = subset(guardian_sfdc_opps, guardian_sfdc_opps$IsClosed=='true')

grade_bucket_leads(guardian_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-12-21T00:00:00.000Z', 9, 7, 3)
grade_bucket_opps(guardian_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-12-21T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(guardian_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 7, 3)

guardian_accts_new = subset(guardian_accts, guardian_accts$created_date >= '2014-11-16 00:00:00' | is.na(guardian_accts$closed_date))
comment(guardian_accts_new) = 'guardian_accts_new'
grade_bucket_accounts(guardian_accts_new, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00 00:00:00', 9, 7, 3)
#grade_bucket_opps(guardian_sfdc_opps, 'Ftop__SpendScore__c', 'IsWon', 'Total_ARR__c', '2014-12-21', 9,7,3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
trackmaven_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Trackmaven/Trackmaven_leads.csv', header=T, stringsAsFactor=F)  
trackmaven_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Trackmaven/Trackmaven_opportunities.csv', header=T, stringsAsFactor=F)  
trackmaven_opps = subset(trackmaven_opps, trackmaven_opps$isClo=='true')
comment(trackmaven_leads) = 'trackmaven_leads'; comment(trackmaven_opps) = 'trackmaven_opps'

grade_bucket_leads(trackmaven_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-11-03T00:00:00.000Z', 10, 6, 3)
grade_bucket_opps(trackmaven_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-11-03T00:00:00.000Z', 10, 9, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jama_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_leads.csv', header=T, stringsAsFactor=F)  
jama_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_opportunities.csv', header=T, stringsAsFactor=F)  
jama_opps = subset(jama_opps, jama_opps$isClo=='true')
jama_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/Jama_accounts.csv', header=T, stringsAsFactor=F)
jama_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jama/acct_opp_query.csv', header=T, stringsAsFactor=F)
jama_accts = merge(jama_accts, jama_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
jama_accts = jama_accts[!duplicated(jama_accts$rec), ]
comment(jama_leads) = 'jama_leads'; comment(jama_opps) = 'jama_opps'; comment(jama_accts) = 'jama_accts'

grade_bucket_leads(jama_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-11-24T00:00:00.000Z', 6, 5, 4)
grade_bucket_opps(jama_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-11-24T00:00:00.000Z', 6, 5, 4)
grade_bucket_accounts(jama_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 6, 5, 4)

jama_accts_new = subset(jama_accts, jama_accts$created_date >= '2014-11-24 00:00:00' | is.na(jama_accts$closed_date))
comment(jama_accts_new) = 'jama_accts_new'
grade_bucket_accounts(jama_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 6, 5, 4)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
insideview_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_leads.csv', header=T, stringsAsFactor=F)  
insideview_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_opportunities.csv', header=T, stringsAsFactor=F)  
insideview_opps = subset(insideview_opps, insideview_opps$isClo=='true')
insideview_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/Insideview_accounts.csv', header=T, stringsAsFactor=F)
insideview_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Insideview/acct_opp_query.csv', header=T, stringsAsFactor=F)
insideview_accts = merge(insideview_accts, insideview_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
insideview_accts = insideview_accts[!duplicated(insideview_accts$rec), ]
comment(insideview_leads) = 'insideview_leads'; comment(insideview_opps) = 'insideview_opps'; comment(insideview_accts) = 'insideview_accts'

grade_bucket_leads(insideview_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-08-19T00:00:00.000Z', 8, 6, 5)
grade_bucket_opps(insideview_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-08-19T00:00:00.000Z', 8, 6, 5)
grade_bucket_accounts(insideview_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 8, 6, 5)

insideview_accts_new = subset(insideview_accts, insideview_accts$created_date >= '2014-08-19 00:00:00' | is.na(insideview_accts$closed_date))
comment(insideview_accts_new) = 'insideview_accts_new'
grade_bucket_accounts(insideview_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00 00:00:00', 8, 6, 5)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gainsight_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_leads.csv', header=T, stringsAsFactor=F)  
gainsight_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_opportunities.csv', header=T, stringsAsFactor=F)  
gainsight_opps = subset(gainsight_opps, gainsight_opps$isClo=='true')
gainsight_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Gainsight_accounts.csv', header=T, stringsAsFactor=F)
gainsight_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/acct_opp_query.csv', header=T, stringsAsFactor=F)
gainsight_accts = merge(gainsight_accts, gainsight_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
gainsight_accts = gainsight_accts[!duplicated(gainsight_accts$rec), ]
comment(gainsight_leads) = 'gainsight_leads'; comment(gainsight_opps) = 'gainsight_opps'; comment(gainsight_accts) = 'gainsight_accts'

#grade_bucket_leads(gainsight_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-10-28T00:00:00.000Z', 8, 3, 2)
grade_bucket_leads(gainsight_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-10-28T00:00:00.000Z', 10, 5, 3)
grade_bucket_opps(gainsight_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-10-28T00:00:00.000Z', 10, 5, 3)
grade_bucket_accounts(gainsight_accts, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 10, 6, 3)

gainsight_accts_new = subset(gainsight_accts, gainsight_accts$created_date >= '2014-10-28 00:00:00' | is.na(gainsight_accts$closed_date))
comment(gainsight_accts_new) = 'gainsight_accts_new'
grade_bucket_accounts(gainsight_accts_new, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 10, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
newvoicemedia_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_leads.csv', header=T, stringsAsFactor=F)  
newvoicemedia_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_opportunities.csv', header=T, stringsAsFactor=F)  
newvoicemedia_opps = subset(newvoicemedia_opps, newvoicemedia_opps$isClo=='true')
newvoicemedia_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_accounts.csv', header=T, stringsAsFactor=F)
newvoicemedia_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/acct_opp_query.csv', header=T, stringsAsFactor=F)
#newvoicemedia_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_query_nb.csv', header=T, stringsAsFactor=F)
newvoicemedia_accts = merge(newvoicemedia_accts, newvoicemedia_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
newvoicemedia_accts = newvoicemedia_accts[!duplicated(newvoicemedia_accts$rec), ]
comment(newvoicemedia_leads) = 'newvoicemedia_leads'; comment(newvoicemedia_opps) = 'newvoicemedia_opps'; comment(newvoicemedia_accts) = 'newvoicemedia_accts'

#grade_bucket_leads(newvoicemedia_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-12-03T00:00:00.000Z', 8, 3, 2)
grade_bucket_opps(newvoicemedia_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-12-03T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(newvoicemedia_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

newvoicemedia_accts_new = subset(newvoicemedia_accts, newvoicemedia_accts$created_date >= '2014-12-03 00:00:00' | is.na(newvoicemedia_accts$closed_date))
comment(newvoicemedia_accts_new) = 'newvoicemedia_accts_new'
grade_bucket_accounts(newvoicemedia_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jitterbit_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_leads.csv', header=T, stringsAsFactor=F)  
jitterbit_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_opportunities.csv', header=T, stringsAsFactor=F)  
jitterbit_opps = subset(jitterbit_opps, jitterbit_opps$isClo=='true')
jitterbit_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/Jitterbit_accounts.csv', header=T, stringsAsFactor=F)
jitterbit_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Jitterbit/acct_opp_query.csv', header=T, stringsAsFactor=F)
jitterbit_accts = merge(jitterbit_accts, jitterbit_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
jitterbit_accts = jitterbit_accts[!duplicated(jitterbit_accts$rec), ]
comment(jitterbit_leads) = 'jitterbit_leads'; comment(jitterbit_opps) = 'jitterbit_opps'; comment(jitterbit_accts) = 'jitterbit_accts'

grade_bucket_leads(jitterbit_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-01-13T00:00:00.000Z', 8, 6, 4)
grade_bucket_opps(jitterbit_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2014-11-11T00:00:00.000Z', 10, 5, 3)
grade_bucket_accounts(jitterbit_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 10, 5, 3)

jitterbit_accts_new = subset(jitterbit_accts, jitterbit_accts$created_date >= '2014-11-11 00:00:00' | is.na(jitterbit_accts$closed_date))
comment(jitterbit_accts_new) = 'jitterbit_accts_new'
grade_bucket_accounts(jitterbit_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kissmetrics_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_leads.csv', header=T, stringsAsFactor=F)  
kissmetrics_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_opportunities.csv', header=T, stringsAsFactor=F)  
kissmetrics_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/Kissmetrics_accounts.csv', header=T, stringsAsFactor=F)
kissmetrics_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/acct_opp_query.csv', header=T, stringsAsFactor=F)
kissmetrics_accts = merge(kissmetrics_accts, kissmetrics_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
kissmetrics_accts = kissmetrics_accts[!duplicated(kissmetrics_accts$rec), ]
comment(kissmetrics_leads) = 'kissmetrics_leads'; comment(kissmetrics_opps) = 'kissmetrics_opps'; comment(kissmetrics_accts) = 'kissmetrics_accts'

kissmetrics_accts_new = subset(kissmetrics_accts, kissmetrics_accts$created_date >= '2014-11-11 00:00:00' | is.na(kissmetrics_accts$closed_date))
comment(kissmetrics_accts_new) = 'kissmetrics_accts_new'
grade_bucket_accounts(kissmetrics_accts_new, 'ssv3.100002.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)


kissmetrics_sfdc_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/km_infer_leads.csv', header=T, stringsAsFactor=F)
kissmetrics_sfdc_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/km_infer_opps.csv', header=T, stringsAsFactor=F)

kissmetrics_leads = kissmetrics_leads[order(-kissmetrics_leads$ssv3.2.sco, -kissmetrics_leads$ssv3.2.rscos.2, -kissmetrics_leads$ssv3.2.rscos.1), ]
decile_leads(kissmetrics_leads, 'isCon', 'cre', '2014-12-10T00:00:00.000Z')

kissmetrics_merged_leads = merge(kissmetrics_leads, kissmetrics_sfdc_leads, by.x='rec', by.y='Id')
kissmetrics_merged_leads = kissmetrics_merged_leads[complete.cases(kissmetrics_merged_leads$infer2__Infer_Score__c), ]
#kissmetrics_merged_leads = kissmetrics_merged_leads[complete.cases(kissmetrics_merged_leads$infer2__Infer_Score__c, kissmetrics_merged_leads$Ftop__SpendScore__c), ]
kissmetrics_merged_leads$combined = kissmetrics_merged_leads$ssv3.2.rscos.2+kissmetrics_merged_leads$ssv3.2.rscos.1
kissmetrics_merged_leads = kissmetrics_merged_leads[order(-kissmetrics_merged_leads$ssv3.2.sco, -kissmetrics_merged_leads$ssv3.2.rscos.2, -kissmetrics_merged_leads$ssv3.2.rscos.1), ]
kissmetrics_merged_leads = kissmetrics_merged_leads[order(-kissmetrics_merged_leads$combined), ]


decile_leads(kissmetrics_merged_leads, 'IsConverted', 'CreatedDate', '2014-12-10T00:00:00.000Z')
competitor_decile_leads(kissmetrics_merged_leads, 'Infer', 'infer2__Infer_Score__c', 'IsConverted', 'CreatedDate', '2014-12-10T00:00:00.000Z')
grade_bucket_leads(kissmetrics_merged_leads, 'Ftop__SpendScore__c', 'IsConverted', '2014-12-10T00:00:00.00', 8,5,4)
competitor_grade_bucket_leads(kissmetrics_merged_leads, 'Infer', 'infer2__Infer_Score__c', 'IsConverted', '2014-12-10T00:00:00.00', 48,31,28)

kissmetrics_merged_opps = merge(kissmetrics_opps, kissmetrics_sfdc_opps, by.x='rec', by.y='Id')
kissmetrics_merged_opps = kissmetrics_merged_opps[complete.cases(kissmetrics_merged_opps$infer2__Infer_Score__c, kissmetrics_merged_opps$Ftop__SpendScore__c), ]

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reliant_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Reliantfunding/Reliantfunding_leads.csv', header=T, stringsAsFactor=F)
reliant_opps= read.csv('/Users/fliptop/Desktop/Customer_Stats/Reliantfunding/Reliantfunding_opportunities.csv', header=T, stringsAsFactor=F)
reliant_opps = subset(reliant_opps, reliant_opps$isClo=='true')
comment(reliant_leads) = 'reliant_leads'; comment(reliant_opps) = 'reliant_opps'

grade_bucket_leads(reliant_leads, 'ssv3.2.sco', 'isCon', 'cre', '2014-12-16T00:00:00.000Z', 8, 5, 2)
grade_bucket_opps(reliant_opps, 'ssv3.100011.sco', 'won', 'amo', 'clo', '2014-12-16T00:00:00.000Z', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sumo_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_leads.csv', header=T, stringsAsFactor=F)
sumo_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_opportunities.csv', header=T, stringsAsFactor=F)
sumo_opps = subset(sumo_opps, sumo_opps$isClo=='true')
sumo_accts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/Sumologic_accounts.csv', header=T, stringsAsFactor=F)
sumo_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/Sumologic/acct_opp_query.csv', header=T, stringsAsFactor=F)
#newvoicemedia_query = read.csv('/Users/fliptop/Desktop/Customer_Stats/NVM/NVM_query_nb.csv', header=T, stringsAsFactor=F)
sumo_accts = merge(sumo_accts, sumo_query, by.x='rec', by.y='salesforce_account_id', all.x=T)
sumo_accts = sumo_accts[!duplicated(sumo_accts$rec), ]
comment(sumo_leads) = 'sumo_leads'; comment(sumo_opps) = 'sumo_opps'; comment(sumo_accts) = 'sumo_accts'

grade_bucket_leads(sumo_leads, 'ssv3.2.sco', 'isCon', 'cre', '2015-01-22T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(sumo_opps, 'ssv3.100001.sco', 'won', 'amo', 'clo', '2015-01-22T00:00:00.000Z', 9, 6, 3)
grade_bucket_accounts(sumo_accts, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)

sumo_accts_new = subset(sumo_accts, sumo_accts$created_date >= '2015-01-22 00:00:00' | is.na(sumo_accts$closed_date))
comment(sumo_accts_new) = 'sumo_accts_new'
grade_bucket_accounts(sumo_accts_new, 'ssv3.100001.sco', 'closed', 'cre', '0000-00-00T00:00:00.000Z', 9, 6, 3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#DemandForce Verticals

#Lifestyle
lifestyle_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Lifestyle/Lifestyle_leads.csv', header=T, stringsAsFactor=F)
lifestyle_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Lifestyle/Lifestyle_opportunities.csv', header=T, stringsAsFactor=F)  
lifestyle_opps = subset(lifestyle_opps, lifestyle_opps$isClo=='true')

grade_bucket_leads(lifestyle_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-07-02T00:00:00.000Z', 7, 6, 2)
grade_bucket_opps(lifestyle_opps, 'ssv3.1.sco', 'won', 'amo', 'clo', '2014-07-02T00:00:00.000Z', 7, 6, 2)

#Health
health_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Health/Health_leads.csv', header=T, stringsAsFactor=F)
health_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Health/Health_opportunities.csv', header=T, stringsAsFactor=F)
health_opps = subset(health_opps, health_opps$isClo=='true')

grade_bucket_leads(health_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-07-11T00:00:00.000Z', 7, 6, 2)
grade_bucket_opps(health_opps, 'ssv3.1.sco', 'won', 'amo', 'clo', '2014-07-11T00:00:00.000Z', 9, 6, 2)

#Medical
medical_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Medical/Medical_leads.csv', header=T, stringsAsFactor=F)  
medical_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Medical/Medical_opportunities.csv', header=T, stringsAsFactor=F)
medical_opps = subset(medical_opps, medical_opps$isClo=='true')

grade_bucket_leads(medical_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-07-12T00:00:00.000Z', 9, 6, 2)
grade_bucket_opps(medical_opps, 'ssv3.1.sco', 'won', 'amo', 'clo', '2014-07-12T00:00:00.000Z', 9, 6, 2)

#AnimalCare
animal_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Animal/Animal_leads.csv', header=T, stringsAsFactor=F)
animal_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Animal/Animal_opportunities.csv', header=T, stringsAsFactor=F)
animal_opps = subset(animal_opps, animal_opps$isClo=='true')

grade_bucket_leads(animal_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-07-12T00:00:00.000Z', 9, 6, 3)
grade_bucket_opps(animal_opps, 'ssv3.1.sco', 'won', 'amo', 'clo', '2014-07-12T00:00:00.000Z', 9, 6, 3)

#Automotive
auto_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Auto/Auto_leads.csv', header=T, stringsAsFactor=F)  
auto_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Auto/Auto_opportunities.csv', header=T, stringsAsFactor=F)
auto_opps = subset(auto_opps, auto_opps$isClo=='true')

grade_bucket_leads(auto_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-10-17T00:00:00.000Z', 7, 6, 4)
grade_bucket_opps(auto_opps, 'ssv3.100002.sco', 'won', 'amo', 'clo', '2014-10-17T00:00:00.000Z', 6, 4, 3) 

#Quickbooks
quickbooks_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Quickbooks/Quickbooks_leads.csv', header=T, stringsAsFactor=F)
quickbooks_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/DF/Quickbooks/Quickbooks_opportunities.csv', header=T, stringsAsFactor=F)
quickbooks_opps = subset(quickbooks_opps, quickbooks_opps$isClo=='true')

grade_bucket_leads(quickbooks_leads, 'ssv3.1.sco', 'isCon', 'cre', '2014-07-11T00:00:00.000Z', 8, 6, 4)
grade_bucket_opps(quickbooks_opps, 'ssv3.1.sco', 'won', 'amo', 'clo', '2014-07-11T00:00:00.000Z', 8, 6, 2) 





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
gainsight_accounts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Comparative/Gainsight_accounts.csv', header=T, stringsAsFactor=F)  
gainsight_accounts_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Comparative/gainsight_accounts_sfdc.csv', header=T, stringsAsFactor=F)  
gainsight_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Comparative/gainsight_opps.csv', header=T, stringsAsFactor=F)
gainsight_opps_NN_won = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Comparative/gainsight_opps_new_won.csv', header=T, stringsAsFactor=F)
gainsight_leads_1 = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/Comparative/gainsight_leads.csv', header=T, stringsAsFactor=F)
gainsight_leads_1 = gainsight_leads_1[complete.cases(gainsight_leads_1$infer2__Infer_Score__c, gainsight_leads_1$Ftop__SpendScore__c),]
gainsight_leads = gainsight_leads[complete.cases(gainsight_leads$Ftop__SpendScore__c),]
gainsight_leads_merged = merge(gainsight_leads, gainsight_leads_1, by.x='rec', by.y='Id')
gainsight_leads_merged$Ftop__SpendScore__c[is.na(gainsight_leads_merged$Ftop__SpendScore__c)]<- gainsight_leads_merged$ssv3.2.sco[is.na(gainsight_leads_merged$Ftop__SpendScore__c)]
gainsight_leads_merged = gainsight_leads_merged[complete.cases(gainsight_leads_merged$infer2__Infer_Score__c), ]

gainsight_opps$Id <- NULL
gainsight_acct_opp = merge(gainsight_accounts, gainsight_opps, by.x='rec', by.y='AccountId', all.x=T)
gainsight_acct_opp = gainsight_acct_opp[!duplicated(gainsight_acct_opp$rec), ]
gainsight_acct_opp$Amount[is.na(gainsight_acct_opp$Amount)] <- 0

gainsight_acct_opp_sub = subset(gainsight_acct_opp, gainsight_acct_opp$CloseDate >= '2014-10-28T00:00:00.000Z' & gainsight_acct_opp$Type=='New Business' & gainsight_acct_opp$IsClosed=='true')
gainsight_acct_opp_sub_dedup = gainsight_acct_opp_sub[!duplicated(gainsight_acct_opp_sub$rec),]
gainsight_acct_opp_sub_A = subset(gainsight_acct_opp, gainsight_acct_opp$ssv3.100002.sco >= 10  & is.na(gainsight_acct_opp$IsClosed))
gainsight_acct_opp_sub_B = subset(gainsight_acct_opp, gainsight_acct_opp$ssv3.100002.sco >= 5 & gainsight_acct_opp$ssv3.100002.sco < 10  & is.na(gainsight_acct_opp$IsClosed))
gainsight_acct_opp_A = subset(gainsight_acct_opp, gainsight_acct_opp$ssv3.100002.sco >= 6 & gainsight_acct_opp$IsWon=='true')

table(gainsight_acct_opp$ssv3.100002.sco, is.na(gainsight_acct_opp$IsClosed))
table(gainsight_acct_opp$ssv3.100002.sco)
gainsight_acct_opp = gainsight_acct_opp[order(-gainsight_acct_opp$infer2__Infer_Score__c), ]


gainsight_acct_opp_1 = merge(gainsight_accounts_sfdc, gainsight_opps, by.x='Id', by.y='AccountId', all.x=T)
gainsight_acct_opp_1 = gainsight_acct_opp_1[!duplicated(gainsight_acct_opp_1$Id), ]

table(gainsight_acct_opp_1$infer2__Infer_Rating__c.x, is.na(gainsight_acct_opp_1$IsClosed))
gainsight_acct_opp_1 = gainsight_acct_opp_1[order(-gainsight_acct_opp_1$infer2__Infer_Score__c.x), ]








grade_bucket_accounts(gainsight_acct_opp, 'ssv3.100002.sco', 'IsClosed', 'cre', '2014-10-28T00:00:00.000Z', 10, 6, 3)
competitor_grade_bucket_accounts(gainsight_acct_opp_1, 'Infer', 'infer2__Infer_Score__c.x', 'IsClosed', 'CreatedDate.x', '2014-10-28T00:00:00.000Z', 63, 38, 31)

grade_bucket_accounts(gainsight_acct_opp, 'ssv3.100002.sco', 'IsClosed', 'cre', '0000-00-00T00:00:00.000Z', 10, 6, 3)
competitor_grade_bucket_accounts(gainsight_acct_opp_1, 'Infer', 'infer2__Infer_Score__c.x', 'IsClosed', 'CreatedDate.x', '0000-00-00T00:00:00.000Z', 65, 38, 33)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
kiss_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/kissmetrics_leads.csv', stringsAsFactor=F, header=T)
#kiss_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/km_leads.csv', stringsAsFactor=F, header=T)
kiss_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/kissmetrics_leads_new.csv', stringsAsFactor=F, header=T)
kiss_infer = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/km_infer.csv', stringsAsFactor=F, header=T)
kiss_infer_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Kissmetrics/km_infer_new.csv', stringsAsFactor=F, header=T)

kiss_new = merge(kiss_new, kiss_infer_new, by.x='rec', by.y='Id')
kiss_new = kiss_new[complete.cases(kiss_new$Ftop__SpendScore__c), ]

kiss_leads$isCon = replace(kiss_leads$isCon, kiss_leads$isCon == 'true', 'True')
kiss_infer$IsConverted = replace(kiss_infer$IsConverted, kiss_infer$IsConverted == 'true', 'True')
kiss_infer = na.omit(kiss_infer)
kiss_leads = kiss_leads[!is.na(kiss_leads$ssv3.2.sco),]
kiss_leads = merge(kiss_infer, kiss_leads, by.x='Id', by.y='rec')

kiss_leads_2 = kiss_leads[order(-kiss_leads$ssv3.2.sco, -kiss_leads$ssv3.2.rscos.2, -kiss_leads$ssv3.2.rscos.1), ]

kiss_leads_2$combine1_2 = kiss_leads_2$ssv3.2.rscos.2+kiss_leads_2$ssv3.2.rscos.1
kiss_leads_2 = kiss_leads_2[order(-kiss_leads_2$combine1_2), ]

kiss_new_leads_2 = kiss_new[order(-kiss_new$ssv3.2.sco, -kiss_new$ssv3.2.rscos.2, -kiss_new$ssv3.2.rscos.1), ]
kiss_new_infer = kiss_new[order(-kiss_new$infer2__Infer_Score__c), ]
#kiss_infer_sort = kiss_infer[order(-kiss_infer$infer2__Infer_Score__c), ]
kiss_infer_sort = kiss_leads[order(-kiss_leads$infer2__Infer_Score__c), ]
#leads_decile_partition2(kiss_leads_2)
#infer_leads_decile_partition(kiss_infer_sort)
leads_decile_partition2(kiss_new_leads_2)
infer_leads_decile_partition(kiss_new_infer)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#elastic_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/elastic_leads_new.csv', stringsAsFactor=F, header=T)
elastic_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/elastic_leads.csv', stringsAsFactor=F, header=T)
elastic_infer = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/infer_scores.csv', stringsAsFactor=F, header=T)
elastic_infer_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/infer_scores_new.csv', stringsAsFactor=F, header=T)

elastic_merged = merge(elastic_leads, elastic_infer, by.x='rec', by.y='Id')
elastic_merged_new = merge(elastic_leads, elastic_infer_new, by.x='rec', by.y='Id')

elastic_merged$isCon = replace(elastic_merged$isCon, elastic_merged$isCon == 'true', 'True')
elastic_merged$IsConverted = replace(elastic_merged$IsConverted, elastic_merged$IsConverted == 'true', 'True')
#elastic_merged_1 = elastic_merged[order(-elastic_merged$ssv3.1.rscos.1), ]
#elastic_merged_11 = elastic_merged[order(-elastic_merged$ssv3.11.rscos.1), ]
elastic_merged_2 = elastic_merged[order(-elastic_merged$ssv3.2.sco, -elastic_merged$ssv3.2.rscos.1), ]
elastic_merged_infer = elastic_merged[order(-elastic_merged$infer2__Infer_Score__c), ]

#elastic_merged_new_1 = elastic_merged_new[order(-elastic_merged_new$ssv3.1.rscos.1), ]
#elastic_merged_new_11 = elastic_merged_new[order(-elastic_merged_new$ssv3.11.rscos.1), ]
elastic_merged_new_2 = elastic_merged_new[order(-elastic_merged_new$ssv3.2.sco, -elastic_merged_new$ssv3.2.rscos.2, -elastic_merged_new$ssv3.2.rscos.1), ]
elastic_merged_infer_new = elastic_merged_new[order(-elastic_merged_new$infer2__Infer_Score__c), ]

elastic_merged$combine1_2 = elastic_merged$ssv3.2.rscos.2+elastic_merged$ssv3.2.rscos.1
elastic_merged_combined = elastic_merged[order(-elastic_merged$combine1_2), ]

elastic_merged_new$combine1_2 = elastic_merged_new$ssv3.2.rscos.2+elastic_merged_new$ssv3.2.rscos.1
elastic_merged_combined_new = elastic_merged_new[order(-elastic_merged_new$combine1_2), ]

leads_decile_partition(elastic_merged_1)
leads_decile_partition(elastic_merged_11)
leads_decile_partition(elastic_merged_2)

leads_decile_partition(elastic_merged_new_1)
leads_decile_partition(elastic_merged_new_11)
leads_decile_partition(elastic_merged_new_2)


elastic_leads_excluded = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/leads_excluded.csv', header=T, stringsAsFactor=F)
elastic_leads_excluded$isCon = replace(elastic_leads_excluded$isCon, elastic_leads_excluded$isCon == 'true', 'True')



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jama = read.csv('/Users/fliptop/Desktop/Customer_Stats/jama_leads.csv', stringsAsFactor=F, header=T)
jama_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/jama_leads_new.csv', stringsAsFactor=F, header=T)
jama$isCon = replace(jama$isCon, jama$isCon == 'true', 'True')
jama_1 = jama[order(-jama$ssv3.1.rscos.1), ]
jama_11 = jama[order(-jama$ssv3.11.rscos.1), ]
jama_new_1 = jama_new[order(-jama_new$ssv3.1.rscos.1), ]
jama_new_11 = jama_new[order(-jama_new$ssv3.11.rscos.1), ]  

leads_decile_partition(jama_1)
leads_decile_partition(jama_11)
leads_decile_partition(jama_new_1)
leads_decile_partition(jama_new_11)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Test - Decile
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

leads = smarsh_new_2
leads = lattice
leads = leads_11
leads = reliant_new_1

dec_1 = leads[0:round(nrow(leads)/10), ]
dec_2 = leads[(round(nrow(leads)/10)+1):round(nrow(leads)*2/10), ]
dec_3 = leads[(round(nrow(leads)*2/10)+1):round(nrow(leads)*3/10), ]
dec_4 = leads[(round(nrow(leads)*3/10)+1):round(nrow(leads)*4/10), ]
dec_5 = leads[(round(nrow(leads)*4/10)+1):round(nrow(leads)*5/10), ]
dec_6 = leads[(round(nrow(leads)*5/10)+1):round(nrow(leads)*6/10), ]
dec_7 = leads[(round(nrow(leads)*6/10)+1):round(nrow(leads)*7/10), ]
dec_8 = leads[(round(nrow(leads)*7/10)+1):round(nrow(leads)*8/10), ]
dec_9 = leads[(round(nrow(leads)*8/10)+1):round(nrow(leads)*9/10), ]
dec_10 = leads[(round(nrow(leads)*9/10)+1):nrow(leads), ]

sum_1 = nrow(subset(dec_1, IsConverted == 'True'))
sum_2 = nrow(subset(dec_2, IsConverted == 'True'))
sum_3 = nrow(subset(dec_3, IsConverted == 'True'))
sum_4 = nrow(subset(dec_4, IsConverted == 'True'))
sum_5 = nrow(subset(dec_5, IsConverted == 'True'))
sum_6 = nrow(subset(dec_6, IsConverted == 'True'))
sum_7 = nrow(subset(dec_7, IsConverted == 'True'))
sum_8 = nrow(subset(dec_8, IsConverted == 'True'))
sum_9 = nrow(subset(dec_9, IsConverted == 'True'))
sum_10 = nrow(subset(dec_10, IsConverted == 'True'))

agg_sum2 = sum_1+sum_2
agg_sum3 = agg_sum2+sum_3
agg_sum4 = agg_sum3+sum_4
agg_sum5 = agg_sum4+sum_5
agg_sum6 = agg_sum5+sum_6
agg_sum7 = agg_sum6+sum_7
agg_sum8 = agg_sum7+sum_8
agg_sum9 = agg_sum8+sum_9
agg_sum10 = agg_sum9+sum_10

response_1 = nrow(subset(dec_1, Status == 'Activated'))+nrow(subset(dec_1, Status == 'Failed'))
response_2 = nrow(subset(dec_2, Status == 'Activated'))+nrow(subset(dec_2, Status == 'Failed'))
response_3 = nrow(subset(dec_3, Status == 'Activated'))+nrow(subset(dec_3, Status == 'Failed'))
response_4 = nrow(subset(dec_4, Status == 'Activated'))+nrow(subset(dec_4, Status == 'Failed'))
response_5 = nrow(subset(dec_5, Status == 'Activated'))+nrow(subset(dec_5, Status == 'Failed'))
response_6 = nrow(subset(dec_6, Status == 'Activated'))+nrow(subset(dec_6, Status == 'Failed'))
response_7 = nrow(subset(dec_7, Status == 'Activated'))+nrow(subset(dec_7, Status == 'Failed'))
response_8 = nrow(subset(dec_8, Status == 'Activated'))+nrow(subset(dec_8, Status == 'Failed'))
response_9 = nrow(subset(dec_9, Status == 'Activated'))+nrow(subset(dec_9, Status == 'Failed'))
response_10 = nrow(subset(dec_10, Status == 'Activated'))+nrow(subset(dec_10, Status == 'Failed'))

activated_1 = nrow(subset(dec_1, Status == 'Activated'))
activated_2 = nrow(subset(dec_2, Status == 'Activated'))
activated_3 = nrow(subset(dec_3, Status == 'Activated'))
activated_4 = nrow(subset(dec_4, Status == 'Activated'))
activated_5 = nrow(subset(dec_5, Status == 'Activated'))
activated_6 = nrow(subset(dec_6, Status == 'Activated'))
activated_7 = nrow(subset(dec_7, Status == 'Activated'))
activated_8 = nrow(subset(dec_8, Status == 'Activated'))
activated_9 = nrow(subset(dec_9, Status == 'Activated'))
activated_10 = nrow(subset(dec_10, Status == 'Activated'))

decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                       c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                         nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                       c(sum_1,sum_2,sum_3,sum_4,sum_5,
                         sum_6,sum_7,sum_8,sum_9,sum_10),
                       c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),
                       c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                         agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                       c(response_1,response_2,response_3,response_4,response_5,response_6,response_7,response_8,response_9,response_10),
                       c(activated_1,activated_2,activated_3,activated_4,activated_5,activated_6,activated_7,activated_8,activated_9,activated_10))
colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent','Response_num', 'Activated_num')
decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
decile_df$Response_percent = (decile_df$Response_num/decile_df$Num_in_decile)*100
decile_df$Activated_percent = (decile_df$Activated_num/decile_df$Num_in_decile)*100
decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))

max_height = max(decile_df$Conversion_Percent)
num_height = 0.5
if (max_height <= 5){
  num_height = 0.05
} else if (max_height <= 25){
  num_height = 0.15
} else if (max_height <= 40){
  num_height = 0.5
} else if (max_height <= 60){
  num_height = 0.75
}  else if (max_height <= 100) {
  num_height = 1
}

ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cum_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cum_convert_percent)), y=Cum_convert_percent-3), size=3)
reponse
grid.arrange(ftop_conv_graph)
grid.arrange(agg_plot)
decile_df$Decile_Cum = NULL
decile_df$Agg_axis = NULL
print(decile_df)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Score to bucket mapping (Conversion rates per bucket)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

leads = smarsh_leads_2
leads = kiss_new_leads_2
leads2 = kiss_new_infer
elastic_infer = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/SFDC_scores.csv', header=T, stringsAsFactor=F)
elastic_infer = elastic_infer[order(-elastic_infer$infer2__Infer_Score__c), ]
elastic_ftop = elastic_infer[complete.cases(elastic_infer$Ftop__SpendScore__c), ]
elastic_infer$IsConverted = replace(elastic_infer$IsConverted, elastic_infer$IsConverted == 'true', 'True')
elastic_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/elastic_leads.csv', header=T, stringsAsFactor=F)

elastic_merged = merge(elastic_leads, elastic_sfdc, by.x='rec', by.y='Id')
elastic_merged$isCon = replace(elastic_sfdc$isCon, elastic_sfdc$isCon == 'true', 'True')




--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Test - Conversion Bucket
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
leads = smarsh_new_2
  

grade_A = subset(df, ssv3.2.sco <= 10 & ssv3.2.sco >= 10)
grade_B = subset(df, ssv3.2.sco <= 9 & ssv3.2.sco >= 8)
grade_C = subset(df, ssv3.2.sco <= 7 & ssv3.2.sco >= 2)
grade_D = subset(df, ssv3.2.sco <= 1 & ssv3.2.sco >= 1)

#grade_A = df[0:187, ]
#grade_B = df[188:1168, ]
#grade_C = df[1169:2479, ]
#grade_D = df[2480:5837, ]

range_A = paste(min(grade_A$ssv3.2.sco), ',', max(grade_A$ssv3.2.sco))
range_B = paste(min(grade_B$ssv3.2.sco), ',', max(grade_B$ssv3.2.sco))
range_C = paste(min(grade_C$ssv3.2.sco), ',', max(grade_C$ssv3.2.sco))
range_D = paste(min(grade_D$ssv3.2.sco), ',', max(grade_D$ssv3.2.sco))

num_A = nrow(grade_A)
num_B = nrow(grade_B)
num_C = nrow(grade_C)
num_D = nrow(grade_D)  

c_A = nrow(subset(grade_A, IsConverted == 'True'))
c_B = nrow(subset(grade_B, IsConverted == 'True'))
c_C = nrow(subset(grade_C, IsConverted == 'True'))
c_D = nrow(subset(grade_D, IsConverted == 'True'))

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
colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted', 'Bucket_agg_converted')
bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
bucket_conversion$Agg_Converted = bucket_conversion$Agg_Converted*100
bucket_conversion$Bucket_agg_converted = bucket_conversion$Bucket_agg_converted*100
bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)

ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.1),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,10,20,30,40,50,60,70,80,90,100), limits=c(0,2))
agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
y.breaks = cumsum(bucket_conversion$Bucket_agg_converted) - bucket_conversion$Bucket_agg_converted/2
agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=y.breaks, labels=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted))+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))
agg_pie_chart = ggplot(bucket_conversion, aes(x=factor(1), y=Bucket_agg_converted, fill=Grade_Bucket))+geom_bar(width=1, stat='identity',color='black')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+geom_text(aes(label=sprintf('%.2f%%', bucket_conversion$Bucket_agg_converted),y=y.breaks))+scale_y_continuous(breaks=c())+scale_fill_manual(values=c('green4','green3','green1','honeydew1'))

bucket_conversion$Grade_Bucket_levels <- NULL
bucket_conversion$Agg_CR <- NULL
print(bucket_conversion)
grid.arrange(ftop_bucket_graph)
grid.arrange(agg_plot)
grid.arrange(agg_pie_chart)



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Quartile 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_all = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_all_data.csv', header=T, stringsAsFactor=F)
data_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_2.csv', header=T, stringsAsFactor=F)
data_mongo = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_leads.csv', header=T, stringsAsFactor=F)
marketo_contacts = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_sfdc_contacts.csv', header=T, stringsAsFactor=F)
emails = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/emails.csv', header=T, stringsAsFactor=F)
lead_contact = merge(data_sfdc, marketo_contacts, by.x='ConvertedContactId', by.y='Id', all.x=T)
lead_contact$CreatedDate.y[is.na(lead_contact$CreatedDate.y)] <- '2014-10-01T00:00:00.000Z'
#na.fill(lead_contact$CreatedDate.y, '2014-10-01T00:00:00.000Z')
lead_contact = subset(lead_contact, lead_contact$CreatedDate.x >= '2014-10-01T00:00:00.000Z' & lead_contact$CreatedDate.x < '2015-01-01T00:00:00.000Z' & lead_contact$CreatedDate.y >= '2014-10-01T00:00:00.000Z')
lead_contact = lead_contact[complete.cases(lead_contact$Email.x, lead_contact$Ftop__SpendScore__c), ]


#data_all = data_all[!is.na(data_all$ssv3.2.sco), ]
data = subset(data_sfdc, data_sfdc$CreatedDate >= '2014-10-01T00:00:00.000Z' & data_sfdc$CreatedDate < '2015-01-01T00:00:00.000Z')
data_1 = merge(data_mongo, data, by.x='rec', by.y='Id')
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

data_all_touch_only = subset(data_all, data_all$CreatedDate >= '2014-10-01T00:00:00.000Z' & data_all$CreatedDate < '2015-01-01T00:00:00.000Z')
data_all_touch_only = data_all_touch_only[!is.na(data_all_touch_only$Touch_Stage_Sort_Score_Snapshot__c),]
data_all_touch_only$Ftop__SpendScore__c[is.na(data_all_touch_only$Ftop__SpendScore__c)] <- data_all_touch_only$ssv3.2.sco[is.na(data_all_touch_only$Ftop__SpendScore__c)]
data_all_touch_only = data_all_touch_only[order(-data_all_touch_only$Ftop__SpendScore__c), ]


data_touch_only = data_1[!is.na(data_1$Touch_Stage_Sort_Score_Snapshot__c),]
data_touch_only$Ftop__SpendScore__c[is.na(data_touch_only$Ftop__SpendScore__c)] <- data_touch_only$ssv3.2.sco[is.na(data_touch_only$Ftop__SpendScore__c)]
data_touch_only = merge(data_touch_only, emails, by.x='rec', by.y='Id', all.x=T)
#data_touch_only = data_touch_only[order(-data_touch_only$Ftop__SpendScore__c),]
data_touch_only = data_touch_only[order(-data_touch_only$ssv3.2.sco, -data_touch_only$ssv3.2.rscos.2, -data_touch_only$ssv3.2.rscos.1),]
marketo_touch_only = data_touch_only[order(-data_touch_only$Touch_Stage_Sort_Score_Snapshot__c), ]
write.table(data_touch_only, '/Users/fliptop/Desktop/Touch_stage_1.csv', row.names=F, sep=',')

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







marketo_quartile(ftop_sort)
marketo_marketo_quartile(marketo_touch_sort1)

marketo_quartile <- function(df){
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]

  sum_1 = nrow(subset(dec_1, Status == 'Opportunity'))+nrow(subset(dec_1, Status == 'Sales Lead - Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status == 'Opportunity'))+nrow(subset(dec_2, Status == 'Sales Lead - Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status == 'Opportunity'))+nrow(subset(dec_3, Status == 'Sales Lead - Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status == 'Opportunity'))+nrow(subset(dec_4, Status == 'Sales Lead - Potential Opportunity'))
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
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,15,20,25,30,35,40,45,50), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,15,20,25,30,35,40,45,50), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }

  ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,15,20,25,30,35,40,45,50), limits=c(0,10))
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

}

marketo_marketo_quartile <- function(df){
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
  
  sum_1 = nrow(subset(dec_1, Status == 'Opportunity'))+nrow(subset(dec_1, Status == 'Sales Lead - Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status == 'Opportunity'))+nrow(subset(dec_2, Status == 'Sales Lead - Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status == 'Opportunity'))+nrow(subset(dec_3, Status == 'Sales Lead - Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status == 'Opportunity'))+nrow(subset(dec_4, Status == 'Sales Lead - Potential Opportunity'))
  
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
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.15),size=4)+scale_y_continuous(breaks=c(0,2.5,5,7.5,10,15,20,25,30,35,40,45,50), limits=c(0,10))  
  } else if (max_height <= 25){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.50),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  } else if (max_height <= 100) {
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  }
  
  marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50), limits=c(0,10))
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
}

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








smarsh_stuff = read.csv('/Users/fliptop/Desktop/Smarsh_combined.csv', header=T, stringsAsFactor=F)
df = data.frame(c(10,9,8,7,6,5,4,3,2,1))
df$Counts = c(nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==10, ])-1, nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==9, ])-1,
              nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==8, ])-1, nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==7, ])-1,
              nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==6, ])-1, nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==5, ])-1,
              nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==4, ])-1, nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==3, ])-1,
              nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==2, ])-1, nrow(smarsh_stuff[smarsh_stuff$ssv3.100001.sco==1, ])-1)
colnames(df) = c('Fliptop_Scores', 'Counts')
df_fliptop_graph = ggplot(df, aes(x=Fliptop_Scores, y=Counts)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title='Fliptop - Win Count by Score', x='Fliptop Scores', y='Counts')+geom_text(aes(label=Counts, y=Counts+3), size=4)+scale_y_continuous(breaks=c(0,50,100,150,200,250), limits=c(0,225))+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))


df_lattice = data.frame(c(24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0),
              c(nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==24, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==23, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==22, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==21, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==20, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==19, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==18, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==17, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==16, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==15, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==14, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==13, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==12, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==11, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==10, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==9, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==8, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==7, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==6, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==5, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==4, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==3, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==2, ])-3, nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==1, ])-3,
              nrow(smarsh_stuff[smarsh_stuff$Lattice.Score==0, ])-3)
              )
colnames(df_lattice) = c('Lattice_Scores', 'Counts')
df_lattice_graph = ggplot(df_lattice, aes(x=Lattice_Scores, y=Counts)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title='Lattice - Win Count by Score', x='Lattice Scores', y='Counts')+geom_text(aes(label=Counts, y=Counts+3), size=4)+scale_y_continuous(breaks=c(0,50,100,150,200,250), limits=c(0,225))+scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))



df = merged_gainsight[order(-merged_gainsight$ssv3.100002.sco), ]
  
grade_A = subset(df, ssv3.100002.sco <= 10 & ssv3.100002.sco >= 8)
grade_B = subset(df, ssv3.100002.sco <= 7 & ssv3.100002.sco >= 3)
grade_C = subset(df, ssv3.100002.sco <= 2 & ssv3.100002.sco >= 2)
grade_D = subset(df, ssv3.100002.sco <= 1 & ssv3.100002.sco >= 1)

num_A = nrow(grade_A)
num_B = nrow(grade_B)
num_C = nrow(grade_C)
num_D = nrow(grade_D) 

c_A = sum(!is.na(grade_A$CreatedDate))
c_B = sum(!is.na(grade_B$CreatedDate))
c_C = sum(!is.na(grade_C$CreatedDate))
c_D = sum(!is.na(grade_D$CreatedDate))

cr_A = c_A/nrow(grade_A)
cr_B = c_B/nrow(grade_B)
cr_C = c_C/nrow(grade_C)
cr_D = c_D/nrow(grade_D)
total_conv = c_A+c_B+c_C+c_D

agg_A = c_A/total_conv
agg_B = (c_A+c_B)/total_conv
agg_C = (c_A+c_B+c_C)/total_conv
agg_D = (c_A+c_B+c_C+c_D)/total_conv

bucket_conversion = data.frame(c('A','B','C','D'),
                               c(num_A, num_B, num_C, num_D),
                               c(c_A,c_B,c_C,c_D),
                               c(cr_A,cr_B,cr_C,cr_D),
                               c(agg_A,agg_B,agg_C,agg_D))
colnames(bucket_conversion) <- c('Grade_Bucket', 'Num_in_bucket', 'Opps_created', 'Opps_Created_Rate', 'Agg_OR')  
bucket_conversion$Opps_Created_Rate = bucket_conversion$Opps_Created_Rate*100
bucket_conversion$Agg_OR = bucket_conversion$Agg_OR*100
bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)

account_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Opps_Created_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Account to Opportunity Conversions By Grade', x='Grades', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Opps_Created_Rate)), y=Opps_Created_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,0.5,1,1.5,2,2.5,5,10,15,20,30,40,50,60,70,80,90,100), limits=c(0,2.5))
agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_OR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Percentage of Accounts Converted', x='Stack-ranked Leads by Grades', y='Percentage of Aggregate Accounts')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_OR)), y=Agg_OR-3), size=3)
bucket_conversion$Grade_Bucket_levels <- NULL
print(bucket_conversion)
grid.arrange(account_bucket_graph)
grid.arrange(agg_plot)



df = gainsight_opps_new
df_2 = subset(df, IsClosed == 'True')

grade_A = subset(df_2, Ftop__SpendScore__c <= 10 & Ftop__SpendScore__c >= 10)
grade_B = subset(df_2, Ftop__SpendScore__c <= 9 & Ftop__SpendScore__c >= 5)
grade_C = subset(df_2, Ftop__SpendScore__c <= 4 & Ftop__SpendScore__c >= 3)
grade_D = subset(df_2, Ftop__SpendScore__c <= 2 & Ftop__SpendScore__c >= 1)

num_A = nrow(grade_A)
num_B = nrow(grade_B)
num_C = nrow(grade_C)
num_D = nrow(grade_D) 

c_A = nrow(subset(grade_A, IsWon == 'True'))
c_B = nrow(subset(grade_B, IsWon == 'True'))
c_C = nrow(subset(grade_C, IsWon == 'True'))
c_D = nrow(subset(grade_D, IsWon == 'True'))

l_A = nrow(subset(grade_A, IsWon == 'False'))
l_B = nrow(subset(grade_B, IsWon == 'False'))
l_C = nrow(subset(grade_C, IsWon == 'False'))
l_D = nrow(subset(grade_D, IsWon == 'False'))

total_win = c_A+c_B+c_C+c_D
total_lost = l_A+l_B+l_C+l_D
total_amount = nrow(df)

rough_df = data.frame(c('Total','Win','Lost'),
                      c(total_amount, total_win, total_lost))
colnames(rough_df) = c('Category','Count')
rough_df$Category = factor(rough_df$Category, c('Total','Win','Lost'))
rough_plot = ggplot(rough_df, aes(x=Category, y=Count)) + geom_bar(fill='green1', binwidth=1, stat='identity')+ labs(title= 'All Opportunities Created Since Model Created', x='Stage', y='Count') + geom_text(aes(label=Count, y=Count+5),size=4)+scale_y_continuous(breaks=c(0,50,100,150), limits=c(0,150))



cr_A = c_A/num_A
cr_B = c_B/num_B
cr_C = c_C/num_C
cr_D = c_D/num_D
total_conv = c_A+c_B+c_C+c_D

agg_A = c_A/total_conv
agg_B = (c_A+c_B)/total_conv
agg_C = (c_A+c_B+c_C)/total_conv
agg_D = (c_A+c_B+c_C+c_D)/total_conv

revenue_A = sum(subset(grade_A, IsWon == 'True')$Amount)
revenue_B = sum(subset(grade_B, IsWon == 'True')$Amount)
revenue_C = sum(subset(grade_C, IsWon == 'True')$Amount)
revenue_D = sum(subset(grade_D, IsWon == 'True')$Amount)

agg_revB = revenue_A+revenue_B
agg_revC = agg_revB+revenue_C
agg_revD = agg_revC+revenue_D

bucket_conversion = data.frame(c('A','B','C','D'),
                               c(range_A, range_B, range_C, range_D),
                               c(num_A,num_B,num_C,num_D),
                               c(c_A,c_B,c_C,c_D),
                               c(cr_A,cr_B,cr_C,cr_D),
                               c(agg_A,agg_B,agg_C,agg_D),
                               c(revenue_A,revenue_B,revenue_C,revenue_D),
                               c(revenue_A,agg_revB,agg_revC,agg_revD),
                               c(revenue_A/agg_revD,agg_revB/agg_revD,agg_revC/agg_revD,agg_revD/agg_revD))
colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_Range', 'Num_in_bucket', 'Won', 'Win_Rate', 'Agg_WR', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
bucket_conversion$Agg_WR = bucket_conversion$Agg_WR*100
bucket_conversion$Agg_Revenue_Percent = bucket_conversion$Agg_Revenue_Percent*100
bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
bucket_conversion[is.na(bucket_conversion)] = 0


rough_plot2 = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Won)) + geom_bar(fill='green1', binwidth=1, stat='identity')+labs(title= 'Closed Won Since Model Created', x='Grade', y='Count') + geom_text(aes(label=Won, y=Won+.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,50,100,150), limits=c(0,10))





df = merge(sfdc_elastic, elastic_leads, by.x='Id', by.y='rec')
df$isCon = replace(df$isCon, df$isCon == 'true', 'True')


grade_10 = subset(df, Ftop__SpendScore__c == 10)
grade_9 = subset(df, Ftop__SpendScore__c == 9)
grade_8 = subset(df, Ftop__SpendScore__c == 8)
grade_7 = subset(df, Ftop__SpendScore__c == 7)
grade_6 = subset(df, Ftop__SpendScore__c == 6)
grade_5 = subset(df, Ftop__SpendScore__c == 5)
grade_4 = subset(df, Ftop__SpendScore__c == 4)
grade_3 = subset(df, Ftop__SpendScore__c == 3)
grade_2 = subset(df, Ftop__SpendScore__c == 2)
grade_1 = subset(df, Ftop__SpendScore__c == 1)

grade_10 = subset(df, ssv3.2.sco == 10)
grade_9 = subset(df, ssv3.2.sco == 9)
grade_8 = subset(df, ssv3.2.sco == 8)
grade_7 = subset(df, ssv3.2.sco == 7)
grade_6 = subset(df, ssv3.2.sco == 6)
grade_5 = subset(df, ssv3.2.sco == 5)
grade_4 = subset(df, ssv3.2.sco == 4)
grade_3 = subset(df, ssv3.2.sco == 3)
grade_2 = subset(df, ssv3.2.sco == 2)
grade_1 = subset(df, ssv3.2.sco == 1)

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

c_10 = nrow(subset(grade_10, IsConverted == 'True'))
c_9 = nrow(subset(grade_9, IsConverted == 'True'))
c_8 = nrow(subset(grade_8, IsConverted == 'True'))
c_7 = nrow(subset(grade_7, IsConverted == 'True'))
c_6 = nrow(subset(grade_6, IsConverted == 'True'))
c_5 = nrow(subset(grade_5, IsConverted == 'True'))
c_4 = nrow(subset(grade_4, IsConverted == 'True'))
c_3 = nrow(subset(grade_3, IsConverted == 'True'))
c_2 = nrow(subset(grade_2, IsConverted == 'True'))
c_1 = nrow(subset(grade_1, IsConverted == 'True'))

cr_10 = c_10/num_10
cr_9 = c_9/num_9
cr_8 = c_8/num_8
cr_7 = c_7/num_7
cr_6 = c_6/num_6
cr_5 = c_5/num_5
cr_4 = c_4/num_4
cr_3 = c_3/num_3
cr_2 = c_2/num_2
cr_2 = 0
#cr_3 = 0
#cr_4 = 0
cr_1 = c_1/num_1


total_conv = c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1

agg_10 = c_10/total_conv
agg_9 = (c_10+c_9)/total_conv
agg_8 = (c_10+c_9+c_8)/total_conv
agg_7 = (c_10+c_9+c_8+c_7)/total_conv
agg_6 = (c_10+c_9+c_8+c_7+c_6)/total_conv
agg_5 = (c_10+c_9+c_8+c_7+c_6+c_5)/total_conv
agg_4 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4)/total_conv
agg_3 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3)/total_conv
agg_2 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2)/total_conv
agg_1 = (c_10+c_9+c_8+c_7+c_6+c_5+c_4+c_3+c_2+c_1)/total_conv

score_conversion = data.frame(c('10','9','8','7','6','5','4','3','2','1'),
                               c(num_10, num_9, num_8, num_7, num_6, num_5, num_4, num_3, num_2, num_1),
                               c(c_10,c_9,c_8,c_7,c_6,c_5,c_4,c_3,c_2,c_1),
                               c(cr_10,cr_9,cr_8,cr_7,cr_6,cr_5,cr_4,cr_3,cr_2,cr_1),
                               c(agg_10,agg_9,agg_8,agg_7,agg_6,agg_5,agg_4,agg_3,agg_2,agg_1))
colnames(score_conversion) <- c('Score', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted')
score_conversion$Conversion_Rate = score_conversion$Conversion_Rate*100
score_conversion$Agg_Converted = score_conversion$Agg_Converted*100
score_conversion$Score_levels <- factor(score_conversion$Score, c('10','9','8','7','6','5','4','3','2','1'))
score_conversion[is.na(score_conversion)] = 0

score_graph = ggplot(score_conversion, aes(x=Score_levels, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Score', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,.5,10,20,30,40,50,60,70,80,90,100), limits=c(0,1))
agg_plot = ggplot(score_conversion, aes(x=Score_levels, y=Agg_Converted,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Converted)), y=Agg_Converted-3), size=3)
score_conversion$Score_levels <- NULL
print(score_conversion)
grid.arrange(score_graph)
grid.arrange(agg_plot)




