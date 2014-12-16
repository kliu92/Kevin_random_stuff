library(ggplot2)
require(gridExtra)

leads_decile_partition <- function(df){
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
  
  sum_1 = nrow(subset(dec_1, isCon == 'True'))
  sum_2 = nrow(subset(dec_2, isCon == 'True'))
  sum_3 = nrow(subset(dec_3, isCon == 'True'))
  sum_4 = nrow(subset(dec_4, isCon == 'True'))
  sum_5 = nrow(subset(dec_5, isCon == 'True'))
  sum_6 = nrow(subset(dec_6, isCon == 'True'))
  sum_7 = nrow(subset(dec_7, isCon == 'True'))
  sum_8 = nrow(subset(dec_8, isCon == 'True'))
  sum_9 = nrow(subset(dec_9, isCon == 'True'))
  sum_10 = nrow(subset(dec_10, isCon == 'True'))
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  decile_df$Conversion_Percent = (decile_df$Num_leads_created/decile_df$Num_in_decile)*100
  decile_df$Cumulative_convert_percent = decile_df$Cumulative_convert_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  max_height = max(decile_df$Conversion_Percent)
  #num_height = 0.75
  if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
  } else if (max_height <= 7.5){
    #num_height = 0.05
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))  
  } else if (max_height <= 25){
    #num_height = 0.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    #num_height = 0.5
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    #num_height = 0.75
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    #num_height = 1.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  agg_plot = ggplot(decile_df, aes(x=Decile_cumulative, y=Cumulative_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  decile_df$Decile_cumulative = NULL
  decile_df$Agg_axis = NULL
  print(decile_df)
}

leads_decile_partition2 <- function(df){
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
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  decile_df$Conversion_Percent = (decile_df$Num_leads_created/decile_df$Num_in_decile)*100
  decile_df$Cumulative_convert_percent = decile_df$Cumulative_convert_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
  
  max_height = max(decile_df$Conversion_Percent)
  #num_height = 0.75
  if (max_height <=1){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.0025),size=4)+scale_y_continuous(breaks=c(0,0.05,0.1,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,0.1))  
  } else if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
  } else if (max_height <= 7.5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,7.5,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))  
  } else if (max_height <= 25){   
    #num_height = 0.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    #num_height = 0.5
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
  } else if (max_height <= 60){
    #num_height = 0.75
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  } else if (max_height <= 100) {
    #num_height = 1.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(decile_df, aes(x=Decile_cumulative, y=Cumulative_convert_percent))+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  decile_df$Decile_cumulative = NULL
  decile_df$Agg_axis = NULL
  print(decile_df)
}

opps_decile_partition <- function(df){
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
  
  sum_1 = nrow(subset(dec_1, won == 'True'))
  sum_2 = nrow(subset(dec_2, won == 'True'))
  sum_3 = nrow(subset(dec_3, won == 'True'))
  sum_4 = nrow(subset(dec_4, won == 'True'))
  sum_5 = nrow(subset(dec_5, won == 'True'))
  sum_6 = nrow(subset(dec_6, won == 'True'))
  sum_7 = nrow(subset(dec_7, won == 'True'))
  sum_8 = nrow(subset(dec_8, won == 'True'))
  sum_9 = nrow(subset(dec_9, won == 'True'))
  sum_10 = nrow(subset(dec_10, won == 'True'))
  
  revenue_1 = sum(subset(dec_1, won == 'True')$amo)
  revenue_2 = sum(subset(dec_2, won == 'True')$amo)
  revenue_3 = sum(subset(dec_3, won == 'True')$amo)
  revenue_4 = sum(subset(dec_4, won == 'True')$amo)
  revenue_5 = sum(subset(dec_5, won == 'True')$amo)
  revenue_6 = sum(subset(dec_6, won == 'True')$amo)
  revenue_7 = sum(subset(dec_7, won == 'True')$amo)
  revenue_8 = sum(subset(dec_8, won == 'True')$amo)
  revenue_9 = sum(subset(dec_9, won == 'True')$amo)
  revenue_10 = sum(subset(dec_10, won == 'True')$amo)
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  agg_sum5 = agg_sum4+sum_5
  agg_sum6 = agg_sum5+sum_6
  agg_sum7 = agg_sum6+sum_7
  agg_sum8 = agg_sum7+sum_8
  agg_sum9 = agg_sum8+sum_9
  agg_sum10 = agg_sum9+sum_10
  
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
                         c(revenue_1,revenue_2,revenue_3,revenue_4,revenue_5,revenue_6,revenue_7,revenue_8,revenue_9,revenue_10),
                         c(revenue_1/agg_rev10,agg_rev2/agg_rev10,agg_rev3/agg_rev10,agg_rev4/agg_rev10,agg_rev5/agg_rev10
                           ,agg_rev6/agg_rev10,agg_rev7/agg_rev10,agg_rev8/agg_rev10,agg_rev9/agg_rev10,agg_rev10/agg_rev10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cumulative_opps_won','Cumulative_win_percent','Win_amount','Cumulative_revenue_percent')
  decile_df$Win_Percent = (decile_df$Num_opps_won/decile_df$Num_in_decile)*100
  decile_df$Cumulative_win_percent = decile_df$Cumulative_win_percent*100
  decile_df$Cumulative_revenue_percent = decile_df$Cumulative_revenue_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
  decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
  
  max_height = max(decile_df$Win_Percent)
  #num_height = 0.5
  if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    #num_height = 0.05
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 25){
    #num_height = 0.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    #num_height = 0.5
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    #num_height = 0.75
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    #num_height = 1
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Win_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Opportunity Win Rate (since model creation)', x='Decile - Stack-ranked Opportunities', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Percent)), y=Win_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  #agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_win_percent)), y=Cumulative_win_percent-3), size=3)  
  #rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_x_reverse(breaks=1:10)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_revenue_percent)), y=Cumulative_revenue_percent-3), size=3)  
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_win_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Opportunities Won (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_win_percent)), y=Cumulative_win_percent-3), size=3)  
  rev_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_revenue_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Revenue Percentage by Decile (since model creation)', x='Decile - Stack-ranked Opportunities', y='Aggregate Revenue')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_revenue_percent)), y=Cumulative_revenue_percent-3), size=3)    
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  grid.arrange(rev_plot)
  decile_df$Decile_cumulative = NULL
  decile_df$Agg_axis = NULL
  print(decile_df)
}

infer_leads_decile_partition <- function(df){
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
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  decile_df$Conversion_Percent = (decile_df$Num_leads_created/decile_df$Num_in_decile)*100
  decile_df$Cumulative_convert_percent = decile_df$Cumulative_convert_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
  
  max_height = max(decile_df$Conversion_Percent)
  #num_height = 0.5
  if (max_height <= 5){
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,7.5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 7.5){
    #num_height = 0.05
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 25){
    #num_height = 0.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    #num_height = 0.5
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    #num_height = 0.75
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    #num_height = 1
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Infer: Lead Conversion Rates', x='Decile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  #agg_plot = ggplot(decile_df, aes(x=Decile_cumulative, y=Cumulative_convert_percent))+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Infer - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Infer - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  decile_df$Decile_cumulative = NULL
  print(decile_df)
}

lattice_leads_decile_partition <- function(df){
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
  
  decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                           nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                         c(sum_1,sum_2,sum_3,sum_4,sum_5,
                           sum_6,sum_7,sum_8,sum_9,sum_10),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),
                         c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                           agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10))
  colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  decile_df$Conversion_Percent = (decile_df$Num_leads_created/decile_df$Num_in_decile)*100
  decile_df$Cumulative_convert_percent = decile_df$Cumulative_convert_percent*100
  decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
  decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
  decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
  decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
  
  max_height = max(decile_df$Conversion_Percent)
  #num_height = 0.5
  if (max_height <= 7.5){
    #num_height = 0.05
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,7.5))
  } else if (max_height <= 25){
    #num_height = 0.25
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    #num_height = 0.5
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    #num_height = 0.75
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    #num_height = 1
    ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  ftop_conv_graph = ggplot(decile_df, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Lattice - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  #agg_plot = ggplot(decile_df, aes(x=Decile_cumulative, y=Cumulative_convert_percent))+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100))+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Lattice - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Lattice - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  decile_df$Decile_cumulative = NULL
  print(decile_df)
}

quartile_leads_partition <- function(df){
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

  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                         c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                         c(sum_1,sum_2,sum_3,sum_4),
                         c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                         c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_created/quartile_df$Num_in_quartile)*100
  quartile_df$Cumulative_convert_percent = quartile_df$Cumulative_convert_percent*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Agg_axis = c('25%','50%','75%','100%')
  quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
  quartile_df$Quartile_cumulative = c(4,3,2,1)

  max_height = max(quartile_df$Conversion_Percent)
  num_height = 0.5
  if (max_height <= 5){
    num_height = 0.05
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  } else if (max_height <= 25){
    num_height = 0.15
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    num_height = 0.5
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,50))
  } else if (max_height <= 60){
    num_height = 0.75
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    num_height = 1
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }

  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  quartile_df$Quartile_cumulative = NULL
  print(quartile_df)
}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
csod_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_leads_new.csv', stringsAsFactor=F, header=T)
csod_all = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/csod_leads.csv', stringsAsFactor=F, header=T)
#all_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_leads.csv', stringsAsFactor=F, header=T)
opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/CSOD/CSOD_opps_new.csv', stringsAsFactor=F, header=T)
#all_opps = read.csv('/Users/fliptop/Desktop/CSOD/CSOD_opps.csv', stringsAsFactor=F, header=T)

csod_new_1 = csod_new[order(-csod_new$ssv3.1.rscos.1), ]
csod_new_11 = csod_new[order(-csod_new$ssv3.11.rscos.1), ]

csod_all$isCon = replace(csod_all$isCon, csod_all$isCon == 'true', 'True')
csod_all_1 = csod_all[order(-csod_all$ssv3.1.rscos.1), ]
csod_all_11 = csod_all[order(-csod_all$ssv3.11.rscos.1), ]

opps_1 = opps[order(-opps$ssv3.100001.rscos.1), ]
opps_11 = opps[order(-opps$ssv3.100011.rscos.1), ]
opps_11 = opps[order(-opps$ssv3.11.rscos.1), ]

leads_decile_partition(csod_all_11)
leads_decile_partition(csod_new_11)
opps_decile_partition(opps_11)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
smarsh_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Smarsh_leads_new.csv', stringsAsFactor=F, header=T)
smarsh_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Smarsh_leads.csv', stringsAsFactor=F, header=T)
lattice <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Lattice_leads.csv', stringsAsFactor=F)
lattice_new <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Lattice_leads_new.csv', stringsAsFactor=F, header=T)
lattice$IsConverted = replace(lattice$IsConverted, lattice$IsConverted == 'true', 'True')
lattice_new$IsConverted = replace(lattice_new$IsConverted, lattice_new$IsConverted == 'true', 'True')
lattice = lattice[order(-lattice$Lattice_Score__c),]
lattice_new = lattice_new[order(-lattice_new$Lattice_Score__c),]

smarsh_new = merge(smarsh_new, lattice_new, by.x ='rec', by.y='Id')

smarsh_new_1 = smarsh_new[order(-smarsh_new$ssv3.1.rscos.1), ]
smarsh_new_11 = smarsh_new[order(-smarsh_new$ssv3.11.rscos.1), ]
smarsh_new_2 = smarsh_new[order(-smarsh_new$ssv3.2.sco, -smarsh_new$ssv3.2.rscos.2), ]

smarsh_leads = merge(smarsh_leads, lattice, by.x ='rec', by.y='Id')

smarsh_leads$isCon = replace(smarsh_leads$isCon, smarsh_leads$isCon == 'true', 'True')
smarsh_leads_1 = smarsh_leads[order(-smarsh_leads$ssv3.1.rscos.1), ]
smarsh_leads_11 = smarsh_leads[order(-smarsh_leads$ssv3.11.rscos.1), ]
smarsh_leads_2 = smarsh_leads[order(-smarsh_leads$ssv3.2.sco, -smarsh_leads$ssv3.2.rscos.2), ]

lattice_leads = smarsh_leads[order(-smarsh_leads$Lattice_Score__c),]

leads_decile_partition(smarsh_leads_1)
leads_decile_partition(smarsh_leads_2)
leads_decile_partition(smarsh_new_2)

infer_leads_decile_partition(lattice_leads)

x = merge(smarsh_new_2, lattice, by.x ='rec', by.y='Id')
x = x[order(-x$ssv3.2.sco, -x$ssv3.2.rscos.2), ]
leads_decile_partition(x)
write.table(smarsh_leads, '/Users/fliptop/Desktop/Customer_Stats/Smarsh/Smarsh/smarsh_score_comparison_raw.csv', row.names=F, col.names=T, sep=',')
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
elastic_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/elastic_leads_new.csv', stringsAsFactor=F, header=T)
elastic_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/elastic_leads.csv', stringsAsFactor=F, header=T)
elastic_infer = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/infer_scores.csv', stringsAsFactor=F, header=T)
elastic_infer_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ElasticSearch/infer_scores_new.csv', stringsAsFactor=F, header=T)

elastic_merged = merge(elastic_leads, elastic_infer, by.x='rec', by.y='Id')
elastic_merged_new = merge(elastic_new, elastic_infer_new, by.x='rec', by.y='Id')

elastic_merged$isCon = replace(elastic_merged$isCon, elastic_merged$isCon == 'true', 'True')
elastic_merged$IsConverted = replace(elastic_merged$IsConverted, elastic_merged$IsConverted == 'true', 'True')
elastic_merged_1 = elastic_merged[order(-elastic_merged$ssv3.1.rscos.1), ]
elastic_merged_11 = elastic_merged[order(-elastic_merged$ssv3.11.rscos.1), ]
elastic_merged_2 = elastic_merged[order(-elastic_merged$ssv3.2.sco, -elastic_merged$ssv3.2.rscos.1), ]
elastic_merged_infer = elastic_merged[order(-elastic_merged$infer2__Infer_Score__c), ]

elastic_merged_new_1 = elastic_merged_new[order(-elastic_merged_new$ssv3.1.rscos.1), ]
elastic_merged_new_11 = elastic_merged_new[order(-elastic_merged_new$ssv3.11.rscos.1), ]
elastic_merged_new_2 = elastic_merged_new[order(-elastic_merged_new$ssv3.2.sco, -elastic_merged_new$ssv3.2.rscos.2, -elastic_merged_new$ssv3.2.rscos.1), ]
elastic_merged_infer_new = elastic_merged_new[order(-elastic_merged_new$infer2__Infer_Score__c), ]

leads_decile_partition(elastic_merged_1)
leads_decile_partition(elastic_merged_11)
leads_decile_partition(elastic_merged_2)

leads_decile_partition(elastic_merged_new_1)
leads_decile_partition(elastic_merged_new_11)
leads_decile_partition(elastic_merged_new_2)

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
lattice <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Lattice_leads.csv', stringsAsFactor=F)
lattice <- read.csv('/Users/fliptop/Desktop/Customer_Stats/Smarsh/Lattice_leads_new.csv')
lattice$IsConverted = replace(lattice$IsConverted, lattice$IsConverted == 'true', 'True')
lattice = lattice[order(-lattice$Lattice_Score__c),]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
reliant_sfdc = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/RF_leads.csv', stringsAsFactor=F, header=T)
reliant_mongo = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/reliant_leads.csv', stringsAsFactor=F, header=T)

reliant = merge(reliant_mongo, reliant_sfdc, by.x='rec', by.y='Id')

reliant_sfdc_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/RF_leads_new.csv', stringsAsFactor=F, header=T)
reliant_mongo_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/reliant_leads_new.csv', stringsAsFactor=F, header=T)

reliant_sfdc_new_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/RF_opps_new.csv', stringsAsFactor=F, header=T)
reliant_mongo_new_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/ReliantFunding/reliant_opps.csv', stringsAsFactor=F, header=T)

reliant_new = merge(reliant_mongo_new, reliant_sfdc_new, by.x='rec', by.y='Id')
reliant_opps = merge(reliant_mongo_new_opps, reliant_sfdc_new_opps, by.x='rec', by.y='Id')

reliant_new$IsConverted = replace(reliant_new$IsConverted, reliant_new$IsConverted == 'true', 'True')

reliant_new_1 = reliant_new[order(-reliant_new$ssv3.1.sco, -reliant_new$ssv3.1.rscos.1),]
reliant_new_11 = reliant_new[order(-reliant_new$ssv3.11.sco, -reliant_new$ssv3.11.rscos.1),]

reliant_opps_1 = reliant_opps[order(-reliant_opps$ssv3.100001.sco, -reliant_opps$ssv3.100001.rscos.1),]
reliant_opps_11 = reliant_opps[order(-reliant_opps$ssv3.100011.sco, -reliant_opps$ssv3.100011.rscos.1),]
#data = reliant_new_1
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gainsight_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_leads.csv', header=T, stringsAsFactor=F)  
gainsight_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_closed_opps.csv', header=T, stringsAsFactor=F)  
gainsight_leads_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_leads_new.csv', header=T, stringsAsFactor=F)
gainsight_opps_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_closed_opps_new.csv', header=T, stringsAsFactor=F)
gainsight_opps_new = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_opps.csv', header=T, stringsAsFactor=F)

gainsight_account = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_accounts.csv', header=T, stringsAsFactor=F)
gainsight_opps = read.csv('/Users/fliptop/Desktop/Customer_Stats/Gainsight/gainsight_opps.csv', header=T, stringsAsFactor=F)

merged_gainsight = merge(gainsight_account, gainsight_opps, by.x='rec', by.y='AccountId', all.x=TRUE)

gainsight_opps_new = replace(gainsight_opps_new, gainsight_opps_new == 'true', 'True')
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
colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent','Response_num', 'Activated_num')
decile_df$Conversion_Percent = (decile_df$Num_leads_created/decile_df$Num_in_decile)*100
decile_df$Cumulative_convert_percent = decile_df$Cumulative_convert_percent*100
decile_df$Response_percent = (decile_df$Response_num/decile_df$Num_in_decile)*100
decile_df$Activated_percent = (decile_df$Activated_num/decile_df$Num_in_decile)*100
decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
decile_df$Decile_cumulative = c(10,20,30,40,50,60,70,80,90,100)
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
agg_plot = ggplot(decile_df, aes(x=Agg_axis, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
reponse
grid.arrange(ftop_conv_graph)
grid.arrange(agg_plot)
decile_df$Decile_cumulative = NULL
decile_df$Agg_axis = NULL
print(decile_df)


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Score to bucket mapping (Conversion rates per bucket)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

leads = smarsh_leads_2
leads = kiss_new_leads_2
leads2 = kiss_new_infer

grade_bucket_conversions <- function(df){
  #grade_A = subset(df, ssv3.11.sco <= 10 & ssv3.11.sco >= 7)
  #grade_B = subset(df, ssv3.11.sco <= 6 & ssv3.11.sco >= 5)
  #grade_C = subset(df, ssv3.11.sco <= 4 & ssv3.11.sco >= 3)
  #grade_D = subset(df, ssv3.11.sco <= 2 & ssv3.11.sco >= 1)
  
  grade_A = df[0:187, ]
  grade_B = df[188:1168, ]
  grade_C = df[1169:2479, ]
  grade_D = df[2480:5837, ]

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
  
  agg_A_cr = agg_A/total_conv
  agg_B_cr = agg_B/total_conv
  agg_C_cr = agg_C/total_conv
  agg_D_cr = agg_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_A_cr,agg_B_cr,agg_C_cr,agg_D_cr))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Agg_CR')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_CR = bucket_conversion$Agg_CR*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,15))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_CR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_CR)), y=Agg_CR-3), size=3)
  bucket_conversion$Grade_Bucket_levels <- NULL
  print(bucket_conversion)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
}

grade_bucket_conversions2 <- function(df){
  grade_A = subset(df, Ftop__SpendScore__c <= 10 & Ftop__SpendScore__c >= 8)
  grade_B = subset(df, Ftop__SpendScore__c <= 7 & Ftop__SpendScore__c >= 3)
  grade_C = subset(df, Ftop__SpendScore__c <= 2 & Ftop__SpendScore__c >= 2)
  grade_D = subset(df, Ftop__SpendScore__c <= 1 & Ftop__SpendScore__c >= 1)
  
  #grade_A = df[0:183, ]
  #grade_B = df[184:1147, ]
  #grade_C = df[1148:2443, ]
  #grade_D = df[2444:5799, ]
  
  range_A = paste(min(grade_A$Ftop__SpendScore__c), ',', max(grade_A$Ftop__SpendScore__c))
  range_B = paste(min(grade_B$Ftop__SpendScore__c), ',', max(grade_B$Ftop__SpendScore__c))
  range_C = paste(min(grade_C$Ftop__SpendScore__c), ',', max(grade_C$Ftop__SpendScore__c))
  range_D = paste(min(grade_D$Ftop__SpendScore__c), ',', max(grade_D$Ftop__SpendScore__c))
  
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
  
  agg_A_cr = agg_A/total_conv
  agg_B_cr = agg_B/total_conv
  agg_C_cr = agg_C/total_conv
  agg_D_cr = agg_D/total_conv
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(agg_A_cr,agg_B_cr,agg_C_cr,agg_D_cr))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_Converted','Agg_CR')
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_CR = bucket_conversion$Agg_CR*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] = 0
  
  max_height = max(bucket_conversion$Conversion_Rate)
  if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))  
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 60){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+0.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_CR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_CR)), y=Agg_CR-3), size=3)
  bucket_conversion$Grade_Bucket_levels <- NULL
  print(bucket_conversion)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
}


competitor_grade_bucket <- function(df){
  grade_A = subset(df, infer2__Infer_Score__c <= 100 & infer2__Infer_Score__c >= 51)
  grade_B = subset(df, infer2__Infer_Score__c <= 50 & infer2__Infer_Score__c >= 46)
  grade_C = subset(df, infer2__Infer_Score__c <= 45 & infer2__Infer_Score__c >= 39)
  grade_D = subset(df, infer2__Infer_Score__c <= 38 & infer2__Infer_Score__c >= 0)
  
  grade_A = subset(df, df$infer2__Infer_Rating__c == 'A')
  grade_B = subset(df, df$infer2__Infer_Rating__c == 'B')
  grade_C = subset(df, df$infer2__Infer_Rating__c == 'C')
  grade_D = subset(df, df$infer2__Infer_Rating__c == 'D')
  
  range_A = paste(min(grade_A$infer2__Infer_Score__c), ',', max(grade_A$infer2__Infer_Score__c))
  range_B = paste(min(grade_B$infer2__Infer_Score__c), ',', max(grade_B$infer2__Infer_Score__c))
  range_C = paste(min(grade_C$infer2__Infer_Score__c), ',', max(grade_C$infer2__Infer_Score__c))
  range_D = paste(min(grade_D$infer2__Infer_Score__c), ',', max(grade_D$infer2__Infer_Score__c))
  
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
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(range_A, range_B, range_C, range_D),
                                 c(num_A, num_B, num_C, num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Score_range', 'Num_in_bucket', 'Converted', 'Conversion_Rate', 'Agg_CR')  
  bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
  bucket_conversion$Agg_CR = bucket_conversion$Agg_CR*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  
  comp_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='lemonchiffon3', binwidth=1, stat='identity') + labs(title= 'Brand X - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.25),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,30,40,50,60,70,80,90,100), limits=c(0,3.5))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_CR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='lemonchiffon3')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Brand X - Percentage of Converted Leads', x='Stack-ranked Leads by Grades (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_CR)), y=Agg_CR-3), size=3)
  bucket_conversion$Grade_Bucket_levels <- NULL
  print(bucket_conversion)
  grid.arrange(comp_bucket_graph)
  grid.arrange(agg_plot)
}

grade_bucket_opp_won <- function(df){
  #grade_A = subset(df, ssv3.11.sco <= 10 & ssv3.11.sco >= 10)
  #grade_B = subset(df, ssv3.11.sco <= 9 & ssv3.11.sco >= 5)
  #grade_C = subset(df, ssv3.11.sco <= 4 & ssv3.11.sco >= 3)
  #grade_D = subset(df, ssv3.11.sco <= 2 & ssv3.11.sco >= 1)
  
  grade_A = subset(df, Ftop__SpendScore__c <= 10 & Ftop__SpendScore__c >= 10)
  grade_B = subset(df, Ftop__SpendScore__c <= 9 & Ftop__SpendScore__c >= 5)
  grade_C = subset(df, Ftop__SpendScore__c <= 4 & Ftop__SpendScore__c >= 3)
  grade_D = subset(df, Ftop__SpendScore__c <= 2 & Ftop__SpendScore__c >= 1)
  
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
  
  revenue_A = sum(subset(grade_A, IsWon == 'True')$Amount)
  revenue_B = sum(subset(grade_B, IsWon == 'True')$Amount)
  revenue_C = sum(subset(grade_C, IsWon == 'True')$Amount)
  revenue_D = sum(subset(grade_D, IsWon == 'True')$Amount)
  
  agg_revB = revenue_A+revenue_B
  agg_revC = agg_revB+revenue_C
  agg_revD = agg_revC+revenue_D
  
  bucket_conversion = data.frame(c('A','B','C','D'),
                                 c(num_A,num_B,num_C,num_D),
                                 c(c_A,c_B,c_C,c_D),
                                 c(cr_A,cr_B,cr_C,cr_D),
                                 c(agg_A,agg_B,agg_C,agg_D),
                                 c(revenue_A,revenue_B,revenue_C,revenue_D),
                                 c(revenue_A,agg_revB,agg_revC,agg_revD),
                                 c(revenue_A/agg_revD,agg_revB/agg_revD,agg_revC/agg_revD,agg_revD/agg_revD))
  colnames(bucket_conversion) <- c('Grade_Bucket', 'Num_in_bucket', 'Won', 'Win_Rate', 'Agg_WR', 'Revenue', 'Agg_Revenue', 'Agg_Revenue_Percent')
  bucket_conversion$Win_Rate = bucket_conversion$Win_Rate*100
  bucket_conversion$Agg_WR = bucket_conversion$Agg_WR*100
  bucket_conversion$Agg_Revenue_Percent = bucket_conversion$Agg_Revenue_Percent*100
  bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
  bucket_conversion[is.na(bucket_conversion)] = 0
  
  max_height = max(bucket_conversion$Win_Rate)
  if (max_height <= 5){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))
  } else if (max_height <= 25){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))
  } else if (max_height <= 70){
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
  }  else if (max_height <= 100) {
    ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,105))
  }
  
  #ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Win_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Opportunity Win Rate', x='Grades - Adjusted for Comparative Analysis', y='Win Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Win_Rate)), y=Win_Rate+.5),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,30))
  agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_WR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Opportunities Won', x='Stack-ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_WR)), y=Agg_WR-3), size=3)
  rev_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_Revenue_Percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(-5,100))+labs(title='Aggregate Revenue Percentage by Grade', x='Ranked Opportunities by Grade (Cumulative)', y='Percentage of Aggregate Opportunities')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_Revenue_Percent)), y=Agg_Revenue_Percent-3), size=3)    
  bucket_conversion$Grade_Bucket_levels <- NULL
  print(bucket_conversion)
  grid.arrange(ftop_bucket_graph)
  grid.arrange(agg_plot)
  grid.arrange(rev_plot)
}

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Test - Conversion Bucket
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
leads = smarsh_new_2
  
grade_A = subset(leads, ssv3.2.sco <= 10 & ssv3.2.sco >= 10)
grade_B = subset(leads, ssv3.2.sco <= 9 & ssv3.2.sco >= 8)
grade_C = subset(leads, ssv3.2.sco <= 7 & ssv3.2.sco >= 6)
grade_D = subset(leads, ssv3.2.sco <= 5 & ssv3.2.sco >= 1)

cr_A = nrow(subset(grade_A, isCon == 'True'))/nrow(grade_A)
cr_B = nrow(subset(grade_B, isCon == 'True'))/nrow(grade_B)
cr_C = nrow(subset(grade_C, isCon == 'True'))/nrow(grade_C)
cr_D = nrow(subset(grade_D, isCon == 'True'))/nrow(grade_D)
total_conv = nrow(subset(grade_A, isCon == 'True'))+nrow(subset(grade_B, isCon == 'True'))+nrow(subset(grade_C, isCon == 'True'))+nrow(subset(grade_D, isCon == 'True'))

agg_A = nrow(subset(grade_A, isCon == 'True'))/total_conv
agg_B = (nrow(subset(grade_A, isCon == 'True'))+nrow(subset(grade_B, isCon == 'True')))/total_conv
agg_C = (nrow(subset(grade_A, isCon == 'True'))+nrow(subset(grade_B, isCon == 'True'))+nrow(subset(grade_C, isCon == 'True')))/total_conv
agg_D = (nrow(subset(grade_A, isCon == 'True'))+nrow(subset(grade_B, isCon == 'True'))+nrow(subset(grade_C, isCon == 'True'))+nrow(subset(grade_D, isCon == 'True')))/total_conv

bucket_conversion = data.frame(c('A','B','C','D'), c(cr_A,cr_B,cr_C,cr_D), c(agg_A,agg_B,agg_C,agg_D))
colnames(bucket_conversion) <- c('Grade_Bucket', 'Conversion_Rate', 'Agg_CR')
bucket_conversion$Conversion_Rate = bucket_conversion$Conversion_Rate*100
bucket_conversion$Agg_CR = bucket_conversion$Agg_CR*100
bucket_conversion$Grade_Bucket_levels <- levels(bucket_conversion$Grade_Bucket)
ftop_bucket_graph = ggplot(bucket_conversion, aes(x=Grade_Bucket, y=Conversion_Rate)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Grades - Adjusted for Comparative Analysis', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Rate)), y=Conversion_Rate+.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))
agg_plot = ggplot(bucket_conversion, aes(x=Grade_Bucket_levels, y=Agg_CR,group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Aggregate Leads Converted', x='Stack-ranked Leads', y='Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Agg_CR)), y=Agg_CR-3), size=3)
print(bucket_conversion)
grid.arrange(ftop_bucket_graph)
grid.arrange(agg_plot)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Quartile 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data = read.csv('/Users/fliptop/Desktop/Customer_Stats/Marketo/Marketo_stats.csv')
data = data[!is.na(data$ssv3.2.sco),]
marketo_fliptop_sort = data[order(-data$ssv3.2.sco, -data$ssv3.2.rscos.2, -data$ssv3.2.rscos.1), ]
marketo_marketo_sort = data[order(-data$Sort_Score__c), ]

data_sql = data[!is.na(data$SQL_Snapshot_Sort_Score__c),]
marketo_sql_sort = data_sql[order(-data_sql$SQL_Snapshot_Sort_Score__c), ]

data_touch = data[!is.na(data$Touch_Stage_Sort_Score_Snapshot__c),]
marketo_touch_sort = data_touch[order(-data_touch$Touch_Stage_Sort_Score_Snapshot__c), ]

data_snapshot = data[!is.na(data$Fliptop_SpendScore_Snapshot__c),]
marketo_fliptop_snapshot_sort = data_snapshot[order(-data_snapshot$Fliptop_SpendScore_Snapshot__c), ]

marketo_quartile <- function(df){
  dec_1 = df[0:round(nrow(df)/4), ]
  dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
  dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
  dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]

  sum_1 = nrow(subset(dec_1, Status == 'Opportunity'))+nrow(subset(dec_1, Status == 'Sales Lead - Potential Opportunity'))
  sum_2 = nrow(subset(dec_2, Status == 'Opportunity'))+nrow(subset(dec_2, Status == 'Sales Lead - Potential Opportunity'))
  sum_3 = nrow(subset(dec_3, Status == 'Opportunity'))+nrow(subset(dec_3, Status == 'Sales Lead - Potential Opportunity'))
  sum_4 = nrow(subset(dec_4, Status == 'Opportunity'))+nrow(subset(dec_4, Status == 'Sales Lead - Potential Opportunity'))

  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4

  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                       c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                       c(sum_1,sum_2,sum_3,sum_4),
                       c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                       c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_created/quartile_df$Num_in_quartile)*100
  quartile_df$Cumulative_convert_percent = quartile_df$Cumulative_convert_percent*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_cumulative = c(4,3,2,1)

  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 5){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))  
  } else if (max_height <= 10){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,10))
  } else if (max_height <= 25){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50), limits=c(0,25))
  } else if (max_height <= 40){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,10,15,20,25,30,35,40,45,50), limits=c(0,50))  
  } else if (max_height <= 60){
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
  }  else if (max_height <= 100) {
    ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
  }

  #ftop_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='green1')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Fliptop - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(ftop_conv_graph)
  grid.arrange(agg_plot)
  quartile_df$Quartile_cumulative = NULL
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
  
  agg_sum2 = sum_1+sum_2
  agg_sum3 = agg_sum2+sum_3
  agg_sum4 = agg_sum3+sum_4
  
  quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                           c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                           c(sum_1,sum_2,sum_3,sum_4),
                           c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                           c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4))
  colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_created', 'Cumulative_convert', 'Cumulative_convert_percent')
  quartile_df$Conversion_Percent = (quartile_df$Num_leads_created/quartile_df$Num_in_quartile)*100
  quartile_df$Cumulative_convert_percent = quartile_df$Cumulative_convert_percent*100
  quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
  quartile_df$Quartile_cumulative = c(4,3,2,1)
  
  max_height = max(quartile_df$Conversion_Percent)
  if (max_height <= 5){
    marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.05),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5.5))
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
  
  #marketo_conv_graph = ggplot(quartile_df, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='mediumpurple4', binwidth=1, stat='identity') + labs(title= 'Marketo - Lead Conversion Rates', x='Quartile - Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+num_height),size=4)+scale_y_continuous(breaks=c(0,2.5,5,10,15,20,25,30,35,40,45,50), limits=c(0,5))
  marketo_agg_plot = ggplot(quartile_df, aes(x=Quartile, y=Cumulative_convert_percent, group=1))+stat_summary(geom='line')+geom_line(size=1.25, color='mediumpurple4')+geom_point(colour='black', size=3)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))+labs(title='Marketo - Percentage of Converted Leads', x='Percentage of Stack-ranked Leads (Cumulative)', y='Percentage of Aggregate Leads')+geom_text(aes(label=paste(sprintf('%.2f%%',Cumulative_convert_percent)), y=Cumulative_convert_percent-3), size=3)
  grid.arrange(marketo_conv_graph)
  grid.arrange(marketo_agg_plot)
  quartile_df$Quartile_cumulative = NULL
  print(quartile_df)
}



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





