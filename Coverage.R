setwd('/Users/Fliptop/Reports/')


options(digits=2)

#for (i in 1:length(fileList)) assign(fileList[i], read.csv(fileList[i]))

#So far I have configured it to the point where there are 2 manual sections in generating the full data coverage sheet.
#The first is reading the file into the code and adding it to the list of collective data
# 1 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#reliant <- read.csv('/Users/fliptop/Training_files/data_science_import_912251_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#mir3 <- read.csv('/Users/fliptop/Training_files/data_science_import_912154_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#jitterbit <- read.csv('/Users/fliptop/Training_files/data_science_import_911862_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#guardian <- read.csv('/Users/fliptop/Training_files/data_science_import_912064_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#cetera <- read.csv('/Users/fliptop/Training_files/data_science_import_912206_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#csod <- read.csv('/Users/fliptop/Training_files/data_science_import_911943_explicitly_company_training.csv', header=T, stringsAsFactor=F)
#pb <- read.csv('/USers/fliptop/Downloads/data_science_import_912516_explicitly_3cl_training.csv', header=T, stringsAsFactor=F)

#total_name_list <- c('Cetera', 'Reliant', 'MIR3', 'Jitterbit', 'Guardian', 'CSOD', 'PB')
#total_list <- c(cetera, reliant, mir3, jitterbit, guardian, csod, pb)

#total_name_list = c()
#for (i in 1:length(df_list)){
#  total_name_list = c(total_name_list, i)
#}

#total_list = c()
#for (i in 1:length(df_list)){
#  total_list = c(total_list, df_list[i])
#}
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------


trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

dataCoverage <- function(df, dfname){
  #Data coverage - column wise
  #print(as.character(bquote(dfname)))
  
  m<-sapply(df, function(x) sum(trim(x) =='-1' | trim(x) =='' | is.na(x)))
  t<- data.frame(1-(m[order(m)] / nrow(df)))
  t$signalName <- row.names(t)
  t$nrows <- nrow(df)
  t <- data.frame(cbind(t[,2],t[,1],t[,3]), stringsAsFactors=FALSE)
  names(t) <- c('signalName', sprintf('%s_coveragePct', dfname), sprintf('%s_N', dfname))
  t
}
# Second Manual section is to run the dataCoverage function for each file previously read (can probably be automated).
# 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#cetera_dc <- dataCoverage(cetera, "Cetera")
#reliant_dc <- dataCoverage(reliant, "Reliant")
#mir3_dc <- dataCoverage(mir3, "MIR3")
#jitterbit_dc <- dataCoverage(jitterbit, "Jitterbit")
#guardian_dc <- dataCoverage(guardian, "Guardian")
#csod_dc <- dataCoverage(csod, "CSOD")
#pb_dc <- dataCoverage(pb, "PB")

#total_list_dc <- list(cetera_dc, reliant_dc, mir3_dc, jitterbit_dc, guardian_dc, csod_dc, pb_dc)

#total_list_dc = list()
#for (i in 1:length(total_list)){
#  total_list_dc[[i]] = dataCoverage(total_list[[i]],total_name_list[[i]])
#}
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
final_coverage = function(df_all){
  xfinal <- as.data.frame(df_all[[1]]$signalName)
  colnames(xfinal) = 'signalName'
  for (name in total_list_dc){
    if (nrow(xfinal) > nrow(name)){
      xfinal <- merge(xfinal, name, by='signalName', all=T)
    }
    else{
      xfinal <- merge(xfinal, name, by='signalName', all=T)
    }
  }

  rownames(xfinal)<-xfinal$signalName
  xfinal$signalName <-NULL

  xfinal[is.na(xfinal)] <- 0

  xfinal$numSum <- 0
  xfinal$denomSum <- 0
  for (name in total_name_list){
    x_name_count <- sprintf('%s_N', name)
    x_name_pct <- sprintf('%s_coveragePct', name)
    xfinal$numSum <- xfinal$numSum + (as.numeric(xfinal[,x_name_count]) * as.numeric(xfinal[,x_name_pct]))
    xfinal$denomSum <- xfinal$denomSum + as.numeric(xfinal[,x_name_count])
  }
  
  xfinal$denomSum2 = max(xfinal$denomSum)

  xfinal$globalPercentage <- xfinal$numSum/xfinal$denomSum
  xfinal$globalPercentage2 <- xfinal$numSum/xfinal$denomSum2

  x_final_names_old <- colnames(xfinal)
  x_final_names_new <- c(x_final_names_old)

  for (name in total_name_list){
    x_name <- sprintf('%s_coveragePct', name)
    x_name_index <- sprintf('%s_index', name)
    x_final_names_new <- c(x_final_names_new, x_name_index)
    xfinal <- cbind(xfinal, as.numeric(xfinal[ ,x_name])/xfinal$globalPercentage)
    colnames(xfinal) <- x_final_names_new
  }
  xfinal
}




setwd('/Users/fliptop/Downloads/Training_files/')
fileList = list.files('/Users/fliptop/Downloads/Training_files/', pattern=".csv")
df_list = sapply(fileList, read.csv)

total_name_list = c()
for (i in 1:length(df_list)){
  total_name_list = c(total_name_list, i)
}

total_list = c()
for (i in 1:length(df_list)){
  total_list = c(total_list, df_list[i])
}

total_list_dc = list()
for (i in 1:length(total_list)){
  total_list_dc[[i]] = dataCoverage(total_list[[i]],total_name_list[[i]])
}

final_final = final_coverage(total_list_dc)
write.csv(final_coverage(total_list_dc), "/Users/fliptop/Downloads/Coverage_Report2.csv")







mkto_accts = read.csv('/Users/fliptop/Downloads/Training_files/data_science_import_912625_explicitly_3cl_company_training.csv', header=T, stringsAsFactor=F)
df1_coverage = dataCoverage(mkto_accts, "mkto")
write.csv(df1_coverage, '/Users/fliptop/Downloads/smb.csv')


ff_training = read.csv('/Users/fliptop/Downloads/Training_files/Leads/data_science_import_912658_explicitly_3cl_training.csv', header=T, stringsAsFactor=F)
ff_coverage = dataCoverage(ff_training, "ff")


ff = read.csv('/Users/fliptop/Downloads/batch_scoring_912658_9018126_ExplicitlyRF3CL_2015-07-06.03-22-36_src.csv', header=T, stringsAsFactor=F)
ff_leads = read.csv('/Users/fliptop/Desktop/Customer_Stats/FinancialForce/FinancialForce_leads.csv', header=T, stringsAsFactor=F)
ff_nn = subset(ff_leads, ff_leads$cre >= '2015-06-10T00:00:00.000Z')
ff_nn = ff_nn[c('X_id','rec','ssv3.2.sco')]
ff_nn.merge = merge(ff, ff_nn, by.x='uuid', by.y='X_id', all.y=T)
ff_nn.merge$rec = NULL; ff_nn.merge$ssv3.2.sco = NULL;
ff_nn_coverage = dataCoverage(ff_nn.merge, 'ff_nn')

write.table(ff_coverage, '/Users/fliptop/Desktop/Customer_Stats/FinancialForce/lead_comparison/ff_training_coverage.csv', sep=',', row.names=F)
write.table(ff_nn_coverage, '/Users/fliptop/Desktop/Customer_Stats/FinancialForce/lead_comparison/ff_nn_coverage.csv', sep=',', row.names=F)




nvm_proc = read.csv('/Users/fliptop/Downloads/data_science_import_912307_explicitly_company_training.csv', header=T, stringsAsFactor=F)
nvm_raw = read.csv('/Users/fliptop/Downloads/rawFeatures_withConstraintsAndLabels.csv', header=T, stringsAsFactor=F)
nvm_raw_train = subset(nvm_raw, nvm_raw$is_test == 0)

nvm_raw_train_converage = dataCoverage(nvm_raw_train, 'nvm_raw')
nvm_proc_converage = dataCoverage(nvm_proc, 'nvm_proc')

write.table(nvm_raw_train_converage, '/Users/fliptop/Desktop/Customer_Stats/NVM/07_15_tests/nvm_raw_coverage.csv', sep=',', row.names=F)
write.table(nvm_proc_converage, '/Users/fliptop/Desktop/Customer_Stats/NVM/07_15_tests/nvm_processed_converage.csv', sep=',', row.names=F)
