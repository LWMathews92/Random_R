#####---------------------------FUNCTION INTRODUCTION------------------------------------------------------------------------------------------------------------------------------####
#   Function designed to calculate suggested payouts for a recently hired referral based on position worth and employee performance. The mean payout (x) is 
#   calculated by finding the highest value of x which is less than the desired total.spent on the previous year augmented by the percentage variable. Position worth
#   is measured by the size of an interaction term (int.var.c) which is the mean salary of a position by market and age divided by the mean salary of the entire
#   company. Multiplying int.var.c and x creates what this author calls a "pre-payout" which is what the recommended sum of payouts 1 & 2 would be if performance 
#   dynamics were not considered. Payout 1, then, is half of the "pre-payout" and Payout 1 augmented by the performance variable generates Payout 2.
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
## Author - Lee W Mathews, LSU MSA class of 2016                ##
## In collaboration with Christian Halley and Anvesh Yadulla    ##
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

####Load Data Sets####
#---------------------------------------------------#
#Loads the data from your working directory as .csv #
#Actual data sets and path names may change         #
#---------------------------------------------------#
setwd("Your Working Directory")
HR<-read.csv("your_data.csv") # 

####----************************** THE FOLLOWING VARIABLES MUST BE IN THE HR DATASET UNDER THE FOLLOWING NAMES *********************************************************************-------------#
# ID <- an Identification Variable, Job <- Position Identifier or Uniform Description, Market <- Market Identifier, Age , Job.level <- Job level identifier, Annual.Salary <- Annual Employee Salary,
# Hire.Date <- Dat of hire in mm/dd/yyyy format, *Business.Unit <- Section of Business* (can be ignored), Applicant <- binary identifier of referred applicants,
# Year.Performance <- ordinal performance rating the default rating uses a 0-5 scale, but alteration of the "performance" option can help tailor the dynamics of the second payout to any 0-n scale. 
####----****************************************************************************************************************************************************************************-------------#

### Function ###
ERP  <- function(HR, applicant.id,performance.var,total.spent,ERP.BUs = "blank",reference.date = Sys.Date(),Referrer.ID=0, percentage = 1,performance= c(-0.2,-0.1,-0.04,0.1,0.2,0.3)) {
  
####Package loading####
##--------------------- Loading of necessary packages used in data manipulation------------------------------------##
  library("dplyr")
  library("tidyr")
  
  
#### Year Formatting ####
#---------------------------------------------------------------------------------------------------------------#
#Response to changes in the reference.date option                                                               #
#Takes the date (given or default) and converts it into a REGEX format and match variable format respectively   #
#This step is key to handling dates in a robust manner and selecting the correct reference year and performance #
#variable.                                                                                                      #
#---------------------------------------------------------------------------------------------------------------#
  if(reference.date == Sys.Date()) {
    reference.date <- as.POSIXlt(reference.date)
    reference.date$year <- reference.date$year - 1
  }else {
    reference.date <- as.POSIXlt(as.Date(reference.date), origin = "1990-01-01")
    reference.date$year <- reference.date$year - 1
  }
  regex.yr <- paste(strsplit((as.character(reference.date)),"-")[[1]][1],"$",sep = "")
  if( is.na(match(HR.performance,names(HR)))) {HR.performance <- paste("X",strsplit((as.character(reference.date)),"-")[[1]][1],performance.var,sep = "")}
#---------------------------------------------------------------------------------------------------------------#
#this statement can be modified to match any consistent naming convention by modifying the paste statement      #
  HR.performance <- paste("x",strsplit((as.character(reference.date)),"-")[[1]][1],".Perf.Rating",sep = "")     #
#---------------------------------------------------------------------------------------------------------------#  
  
####Transform Dataset####
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# To understand this block of code please consult the dplyr and tidyr packages created by Hadley Wickham                                                            #
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------#
  HR.F <- HR %>%  mutate(Performance = extract_numeric(.[,match(HR.performance,names(.))])) %>% 
                  group_by(Market,Job, Job.level,Age) %>% 
                  mutate(mean.salary = mean(Annual.Salary), int.var.c = mean.salary / mean(HR.F$Annual.Salary)) %>%
                  ungroup()

#### Main Syntax ####
#---------------------------------------------------------------------------------------------------------------------------------------------------------#
# This block filters down to the Business Units (Business.Unit) which the ERP intends to use and then generates the variable x using a for loop                    #
# Bounds for x in the for loop can be set to any amounts between 0 and infinity, but research suggests that 100:3200 are reasonable low and high ends for #
# payout amounts.                                                                                                                                         #
  if(ERP.BUs == "blank") {ERP.Payout <- HR.F}else{
  ERP.Payout <- HR.F %>% filter(Business.Unit %in% ERP.BUs)}
  i <- 1
  pp <- list()
  for (x in 100:3200) {
    if ((sum(ERP.Payout %>% filter(grepl(regex.yr,Hire.Date), Applicant == 1) %>% mutate(worth = int.var.c * x) %>% select(worth)))< (total.spent*percentage)) {
      i <- i + 1
      pp[[i]] <- x
    }else {next}
  }
  #---------------------------------------------------------------------------------------------------------------------------------------------------------#
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------# 
  # This next block of code uses if then else statements to determine whether the function is dealing with one or multiple applicant(s) and referrer(s) assuming that no    #
  # applicant could have more than one referrer at the same time.                                                                                                           #
  # Next, x is multiplied by int.var.c to create the "pre-payout" amount, the payouts are created along with suggested paid vacation days based on a 261 day work year      #
  # NOTE: that al V.D. variables (Vacation Days) are reduced because there is an assumed loss of productivity due to an employee absence                                    #
  if (length(applicant.id)==1 & length(Referrer.ID)==1) {
    px2 <- max(as.vector(unlist(pp)))
    ERP.Applicant <- ERP.Payout %>% mutate(worth = ((int.var.c * px2)  ), Applicant.ID = ID) %>% filter(Applicant.ID == as.integer(applicant.id)) %>%
      select(Applicant.ID, Job,   worth, Performance)
    Payout.1 <- as.numeric((ERP.Applicant %>% select(worth))[1,1]/2)
    emp.performance <- as.numeric((ERP.Applicant %>% select(Performance))[1,1])
    prepay <- HR.F %>% mutate(Payout.1 = Payout.1, Payout.2 = Payout.1 + (Payout.1 * performance[emp.performance +1]), Vacation.1 = round((Payout.1/(Annual.Salary/261))*0.7,0),
                                 Vacation.2 = round((Payout.2/(Annual.Salary/261))*0.6,0),Vacation.App = round((Payout.2/(Annual.Salary/261))*0.4,0))
    if (Referrer.ID != 0){
      # Here prepay is calculated and then entered as the output as payout.2 because there is only one value for applicant.id and Referrer.ID                                         #
      payout.2 <- cbind((prepay %>% filter(ID == Referrer.ID) %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App)),ERP.Applicant)
      # Here prepay is calculated and then entered as the output as payout.2 because Referrer.ID was left as its default                                                              #
    }else {payout.2 <- cbind((prepay %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App) %>% summarise(m.Payout.1 = mean(Payout.1), m.Payout.2 = mean(Payout.2),
                                                                                   m.Vacation.1 = mean(Vacation.1), m.Vacation.2 = mean(Vacation.2), m.Vacation.App = mean(Vacation.App))),ERP.Applicant)}
    
  }else if (length(applicant.id)>1 & length(Referrer.ID)>1) {
    for (d in 1:length(applicant.id)) {
      px2 <- max(as.vector(unlist(pp)))
      ERP.Applicant <- ERP.Payout %>% mutate(worth = ((int.var.c * px2)  ), Applicant.ID = ID) %>% filter(Applicant.ID == as.integer(applicant.id[d])) %>%
        select(Applicant.ID, Job,   worth, Performance)
      Payout.1 <- as.numeric((ERP.Applicant %>% select(worth))[1,1]/2)
      emp.performance <- as.numeric((ERP.Applicant %>% select(Performance))[1,1])
      prepay <- HR.F %>% filter(ID == Referrer.ID[d]) %>% mutate(Payout.1 = Payout.1, Payout.2 = Payout.1 + (Payout.1 * performance[emp.performance +1]), Vacation.1 = round((Payout.1/(Annual.Salary/261))*0.7,0),
                                   Vacation.2 = round((Payout.2/(Annual.Salary/261))*0.6,0),Vacation.App = round((Payout.2/(Annual.Salary/261))*0.4,0))
      if (nrow((prepay %>% filter(ID == Referrer.ID[d]) %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App))) == nrow(ERP.Applicant)) {
        # Here prepay is calculated and then entered as the output as payout.2 because there is only one value for Referrer.ID and multiple for applicant.id                            #
        payout <- cbind((prepay %>% filter(ID == Referrer.ID[d]) %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App)),ERP.Applicant)
        
        if (exists("payout.2")){
          # Here prepay is calculated and then output multiple times. Each time being bound to payout.2. This accounts for multiple applicant-referrer interactions                 #
          payout.2 <- rbind(payout.2,payout)}else {payout.2 <- payout}
      }else {warning(paste(ERP.Applicant$Applicant.ID,"or",Referrer.ID[d],"returns no rows"))} 
    } 
    
  }else if (length(applicant.id)>1 & length(Referrer.ID)==1){
    for (d in 1:length(applicant.id)) {
      px2 <- max(as.vector(unlist(pp)))
      ERP.Applicant <- ERP.Payout %>% mutate(worth = ((int.var.c * px2)  ), Applicant.ID = ID) %>% filter(Applicant.ID == as.integer(applicant.id[d])) %>%
          select(Applicant.ID, Job,   worth, Performance)
      Payout.1 <- as.numeric((ERP.Applicant %>% select(worth))[1,1]/2)
      emp.performance <- as.numeric((ERP.Applicant %>% select(Performance))[1,1])
      prepay <- HR.F %>% mutate(Payout.1 = Payout.1, Payout.2 = Payout.1 + (Payout.1 * performance[emp.performance +1]), Vacation.1 = round((Payout.1/(Annual.Salary/261))*0.7,0),
                                      Vacation.2 = round((Payout.2/(Annual.Salary/261))*0.6,0),Vacation.App = round((Payout.2/(Annual.Salary/261))*0.4,0))
      if (nrow((prepay %>% filter(ID == Referrer.ID) %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App))) == nrow(ERP.Applicant)) {
        # Here prepay is calculated and then entered as the output as payout.2 because there is only one value for Referrer.ID and multiple for applicant.id                            #
        payout <- cbind((prepay %>% filter(ID == Referrer.ID) %>% select(ID,Payout.1,Payout.2,Vacation.1,Vacation.2,Vacation.App)),ERP.Applicant)
        
        if (exists("payout.2")){
          # Here prepay is calculated and then output multiple times. Each time being bound to payout.2. This accounts for multiple applicant-referrer interactions                 #
          payout.2 <- rbind(payout.2,payout)}else {payout.2 <- payout}
      }else {warning(paste(ERP.Applicant$Applicant.ID,"or",Referrer.ID,"returns no rows"))} 
    }
  }else {warning("Invalid entry")}
  return(payout.2)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
}