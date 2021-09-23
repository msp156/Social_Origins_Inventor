#Figure out where your directory is 
#getwd()

setwd("/Your Directory/")
getwd()

install.packages("dplyr")
install.packages("readstata13")
install.packages("usmap")
install.packages("ggplot2")
install.packages("regtools")
#install.packages("tidyverse")


library("dplyr")
library("readstata13")
library("usmap")
library("ggplot2")
library("regtools")
#library("tidyverse")

#Load Data Frame
us.states.social.origins.csv <- read.csv("/Your Directory/Data1")
us.states.social.origins.csv

#check data type for each variable
sapply(us.states.social.origins.csv, class)


#Declare Columns as vectors
par_cz<-us.states.social.origins.csv[1]
par_czname<-us.states.social.origins.csv[2]
par_state<-us.states.social.origins.csv[3]
par_stateabbrv<-us.states.social.origins.csv[4]
kid_count<-us.states.social.origins.csv[5]
inventor<-us.states.social.origins.csv[6]
inventor_cat_1<-us.states.social.origins.csv[7]
inventor_cat_2<-us.states.social.origins.csv[8]
inventor_cat_3<-us.states.social.origins.csv[9]
inventor_cat_4<-us.states.social.origins.csv[10]
inventor_cat_5<-us.states.social.origins.csv[11]
inventor_cat_6<-us.states.social.origins.csv[12]
inventor_cat_7<-us.states.social.origins.csv[13]
kid_count_g_m<-us.states.social.origins.csv[14]
inventor_g_m<-us.states.social.origins.csv[15]
inventor_cat_1_g_m<-us.states.social.origins.csv[16]
inventor_cat_2_g_m<-us.states.social.origins.csv[17]
inventor_cat_3_g_m<-us.states.social.origins.csv[18]
inventor_cat_4_g_m<-us.states.social.origins.csv[19]
inventor_cat_5_g_m<-us.states.social.origins.csv[20]
inventor_cat_6_g_m<-us.states.social.origins.csv[21]
inventor_cat_7_g_m<-us.states.social.origins.csv[22]
kid_count_g_f<-us.states.social.origins.csv[23]
inventor_g_f<-us.states.social.origins.csv[24]
inventor_cat_1_g_f<-us.states.social.origins.csv[25]
inventor_cat_2_g_f<-us.states.social.origins.csv[26]
inventor_cat_3_g_f<-us.states.social.origins.csv[27]
inventor_cat_4_g_f<-us.states.social.origins.csv[28]
inventor_cat_5_g_f<-us.states.social.origins.csv[29]
inventor_cat_6_g_f<-us.states.social.origins.csv[30]
inventor_cat_7_g_f<-us.states.social.origins.csv[31]

#cleaning and sorting through the data

char.par_state<-as.character(unlist(par_state))
num.par_state<-as.numeric(unlist(par_state))

char.par_stateabbrv<-as.character(unlist(par_stateabbrv))


num.par_cz<-as.numeric(unlist(par_cz))
char.par_czname<-as.character(unlist(par_czname))



num.kid_count<-as.numeric(unlist(kid_count))
num.kid_count_g_m<-as.numeric(unlist(kid_count_g_m))
num.kid_count_g_f<-as.numeric(unlist(kid_count_g_f))
num.inventor<-as.numeric(unlist(inventor))
log.num.inventor<-log(num.inventor)
num.inventor_cat_1<-as.numeric(unlist(inventor_cat_1))
num.inventor_cat_2<-as.numeric(unlist(inventor_cat_2))
num.inventor_cat_3<-as.numeric(unlist(inventor_cat_3))
num.inventor_cat_4<-as.numeric(unlist(inventor_cat_4))
num.inventor_cat_5<-as.numeric(unlist(inventor_cat_5))
num.inventor_cat_6<-as.numeric(unlist(inventor_cat_6))
num.inventor_cat_7<-as.numeric(unlist(inventor_cat_7))
num.inventor_g_m<-as.numeric(unlist(inventor_g_m))
log.num.inventor_g_m<-log(num.inventor_g_m)
num.inventor_cat_1_g_m<-as.numeric(unlist(inventor_cat_1_g_m))
num.inventor_cat_2_g_m<-as.numeric(unlist(inventor_cat_2_g_m))
num.inventor_cat_3_g_m<-as.numeric(unlist(inventor_cat_3_g_m))
num.inventor_cat_4_g_m<-as.numeric(unlist(inventor_cat_4_g_m))
num.inventor_cat_5_g_m<-as.numeric(unlist(inventor_cat_5_g_m))
num.inventor_cat_6_g_m<-as.numeric(unlist(inventor_cat_6_g_m))
num.inventor_cat_7_g_m<-as.numeric(unlist(inventor_cat_7_g_m))
num.inventor_g_f<-as.numeric(unlist(inventor_g_f))
log.num.inventor_g_f<-log(num.inventor_g_f)

num.inventor_cat_1_g_f<-as.numeric(unlist(inventor_cat_1_g_f))
num.inventor_cat_2_g_f<-as.numeric(unlist(inventor_cat_2_g_f))
num.inventor_cat_3_g_f<-as.numeric(unlist(inventor_cat_3_g_f))
num.inventor_cat_4_g_f<-as.numeric(unlist(inventor_cat_4_g_f))
num.inventor_cat_5_g_f<-as.numeric(unlist(inventor_cat_5_g_f))
num.inventor_cat_6_g_f<-as.numeric(unlist(inventor_cat_6_g_f))
num.inventor_cat_7_g_f<-as.numeric(unlist(inventor_cat_7_g_f))

#print vectors to check data
#char.par_czname
#char.par_state
#char.par_stateabbrv
#num.kid_count
#num.kid_count_g_m
#num.kid_count_g_f

#log.num.inventor
#num.inventor_g_m
#log.num.inventor_g_f

log.num.inventor.na<-na_if(log.num.inventor, -Inf)
log.num.inventor.g.m.na<-na_if(log.num.inventor_g_m, -Inf)
log.num.inventor.g.f.na<-na_if(log.num.inventor_g_f, -Inf)

social.origins.df<- data.frame(char.par_state, num.par_state, char.par_stateabbrv, num.par_cz, char.par_czname, 
                               num.kid_count, num.kid_count_g_m, num.kid_count_g_f, num.inventor, log.num.inventor.na, num.inventor_cat_1,
                               num.inventor_cat_2, num.inventor_cat_3, num.inventor_cat_4,num.inventor_cat_5,
                               num.inventor_cat_6, num.inventor_g_m,log.num.inventor.g.m.na, num.inventor_cat_1_g_m, num.inventor_cat_2_g_m,
                               num.inventor_cat_3_g_m, num.inventor_cat_4_g_m, num.inventor_cat_5_g_m, num.inventor_cat_6_g_m, 
                               num.inventor_cat_7_g_m, num.inventor_g_f,log.num.inventor.g.f.na, num.inventor_cat_1_g_f, num.inventor_cat_2_g_f, 
                               num.inventor_cat_3_g_f, num.inventor_cat_4_g_f, num.inventor_cat_5_g_f, num.inventor_cat_6_g_f, 
                               num.inventor_cat_7_g_f)

social.origins.df



social.origins.df.na<-na.omit(social.origins.df)
social.origins.df.na

#dir.create("C:/Users/patel_m/OneDrive - US Department of Labor - BLS/Desktop/R Users Group Presentation/Working Files")
getwd()
setwd("/Your Directory/Working Files")
getwd()


write.csv(social.origins.df.na, 'social.origins.df.na.csv')

#Load new dataset
us.characteristics.social.origins.csv<-read.dta13("/Your Directory/Data2")
us.characteristics.social.origins.csv

#Declare Columns as vectors
cz<-us.characteristics.social.origins.csv[1]
czname<-us.characteristics.social.origins.csv[2]
par_stateabbrv_2<-us.characteristics.social.origins.csv[3]
pop2000<-us.characteristics.social.origins.csv[4]
intersects_msa<-us.characteristics.social.origins.csv[5]
cs_race_black<-us.characteristics.social.origins.csv[6]
cs_race_theil_2000<-us.characteristics.social.origins.csv[7]
cs00_seg_inc<-us.characteristics.social.origins.csv[8]
cs00_seg_inc_pov25<-us.characteristics.social.origins.csv[9]
cs00_seg_inc_aff75<-us.characteristics.social.origins.csv[10]
frac_traveltime<-us.characteristics.social.origins.csv[11]
hhinc00<-us.characteristics.social.origins.csv[12]
gini<-us.characteristics.social.origins.csv[13]
inc_share_1perc<-us.characteristics.social.origins.csv[14]
gini99<-us.characteristics.social.origins.csv[15]
frac_middleclass<-us.characteristics.social.origins.csv[16]
taxrate<-us.characteristics.social.origins.csv[17]
subcty_total_exp<-us.characteristics.social.origins.csv[18]
tax_st_diff_top20<-us.characteristics.social.origins.csv[19]
eitc_exposure<-us.characteristics.social.origins.csv[20]
ccd_exp_total<-us.characteristics.social.origins.csv[21]
ccd_pup<-us.characteristics.social.origins.csv[22]
score_r<-us.characteristics.social.origins.csv[23]
dropout_r<-us.characteristics.social.origins.csv[24]
num_inst_pc<-us.characteristics.social.origins.csv[25]
tuition<-us.characteristics.social.origins.csv[26]
gradrate_r<-us.characteristics.social.origins.csv[27]
cs_labforce<-us.characteristics.social.origins.csv[28]
cs_elf_ind_man<-us.characteristics.social.origins.csv[29]
tradeusch<-us.characteristics.social.origins.csv[30]
frac_worked<-us.characteristics.social.origins.csv[31]
mig_inflow<-us.characteristics.social.origins.csv[32]
mig_outflow<-us.characteristics.social.origins.csv[33]
cs_born_foreign<-us.characteristics.social.origins.csv[34]
scap<-us.characteristics.social.origins.csv[35]
reltotal<-us.characteristics.social.origins.csv[36]
crime<-us.characteristics.social.origins.csv[37]
cs_family<-us.characteristics.social.origins.csv[38]
cs_divorced<-us.characteristics.social.origins.csv[39]
cs_married<-us.characteristics.social.origins.csv[40]
inc_growth<-us.characteristics.social.origins.csv[41]


#check if character or numeric
sapply(us.characteristics.social.origins.csv, class)


#character variables


char.par_czname<-as.character(unlist(czname))

char.par_stateabbrv<-as.character(unlist(par_stateabbrv))
num.par_stateabbrv<-as.numeric(unlist(par_stateabbrv))

char.pop2000<-as.character(unlist(pop2000))
num.pop2000<-as.numeric(unlist(char.pop2000))

num.par_cz<-as.numeric(unlist(cz))
num.intersects_msa<-as.numeric(unlist(intersects_msa))
num.cs_race_black<-as.numeric(unlist(cs_race_black))
num.cs_race_theil_2000<-as.numeric(unlist(cs_race_theil_2000))
num.cs00_seg_inc<-as.numeric(unlist(cs00_seg_inc))
num.cs00_seg_inc_pov25<-as.numeric(unlist(cs00_seg_inc_pov25))
num.cs00_seg_inc_aff75<-as.numeric(unlist(cs00_seg_inc_aff75))
num.frac_traveltime<-as.numeric(unlist(frac_traveltime))
num.hhinc00<-as.numeric(unlist(hhinc00))
num.hhinc002<-as.numeric(unlist(num.hhinc00))
num.gini<-as.numeric(unlist(gini))
num.inc_share_1perc<-as.numeric(unlist(inc_share_1perc))
num.inc_shar_1perc2<-num.inc_share_1perc^2
num.gini99<-as.numeric(unlist(gini99))
num.frac_middleclass<-as.numeric(unlist(frac_middleclass))
num.taxrate<-as.numeric(unlist(taxrate))
num.subcty_total_exp<-as.numeric(unlist(subcty_total_exp))
num.tax_st_diff_top20<-as.numeric(unlist(tax_st_diff_top20))
num.eitc_exposure<-as.numeric(unlist(eitc_exposure))
num.ccd_exp_total<-as.numeric(unlist(ccd_exp_total))
num.ccd_pup<-as.numeric(unlist(ccd_pup))
num.score_r<-as.numeric(unlist(score_r))
num.dropout_r<-as.numeric(unlist(dropout_r))
num.num_inst_pc<-as.numeric(unlist(num_inst_pc))
num.tuition<-as.numeric(unlist(tuition))
num.gradrate_r<-as.numeric(unlist(gradrate_r))
num.cs_labforce<-as.numeric(unlist(cs_labforce))
num.cs_elf_ind_man<-as.numeric(unlist(cs_elf_ind_man))
num.tradeusch<-as.numeric(unlist(tradeusch))
num.frac_worked<-as.numeric(unlist(frac_worked))
num.mig_inflow<-as.numeric(unlist(mig_inflow))
num.mig_outflow<-as.numeric(unlist(mig_outflow))
num.cs_born_foreign<-as.numeric(unlist(cs_born_foreign))
num.scap<-as.numeric(unlist(scap))
num.reltotal<-as.numeric(unlist(reltotal))
num.crime<-as.numeric(unlist(crime))
num.cs_family<-as.numeric(unlist(cs_family))
num.cs_divorced<-as.numeric(unlist(cs_divorced))
num.cs_married<-as.numeric(unlist(cs_married))
num.inc_growth<-as.numeric(unlist(inc_growth))

social.origins.char.df<- data.frame(num.par_cz, char.par_czname, char.par_stateabbrv, 
                                    num.par_stateabbrv, char.pop2000, num.pop2000, num.intersects_msa,
                                    num.cs_race_black,num.cs_race_theil_2000,num.cs00_seg_inc, num.cs00_seg_inc_pov25,
                                    num.cs00_seg_inc_aff75, num.frac_traveltime,num.hhinc00,num.hhinc002, num.gini,
                                    num.inc_share_1perc, num.gini99, num.frac_middleclass,num.inc_shar_1perc2,
                                    num.taxrate, num.subcty_total_exp, num.tax_st_diff_top20, 
                                    num.eitc_exposure, num.ccd_exp_total, num.ccd_pup,
                                    num.score_r, num.dropout_r, num.num_inst_pc, 
                                    num.tuition, num.gradrate_r, num.cs_labforce, 
                                    num.cs_elf_ind_man,num.tradeusch,num.frac_worked, num.mig_inflow,
                                    num.mig_outflow, num.cs_born_foreign,num.scap, 
                                    num.reltotal, num.crime, num.cs_family, 
                                    num.cs_divorced,num.cs_married,num.inc_growth)
                               
social.origins.char.df

setwd("/Your Directory/Working Files")
getwd()

write.csv(social.origins.char.df, 'social.origins.char.df.csv')

setwd("/Your Directory/Working Files")
getwd()

us.social.origins.df<-merge(social.origins.df.na,social.origins.char.df)

us.social.origins.df

write.csv(us.social.origins.df,'us.social.origins.df.csv')



#dir.create("/Your Directory/Results")
setwd("/Your Directory/Results")
getwd()

summary_df<-summary(us.social.origins.df)
summary_df

write.csv(summary_df, 'summary statistics.csv')

summary_inventor<-summary(num.inventor)
summary_inventor

summary_inventor_male<-summary(num.inventor_g_m)
summary_inventor_male


summary_inventor_female<-summary(num.inventor_g_f)
summary_inventor_female

#t-test for inventors, male, and female


t.test.1<-t.test(num.inventor, num.inventor_g_m, paired=TRUE)
t.test.2<-t.test(num.inventor,num.inventor_g_f, paired = TRUE)
t.test.3<-t.test(num.inventor_g_m,num.inventor_g_f, paired = TRUE)

t.test.1
t.test.2
t.test.3



inventor_model<-lm(log.num.inventor.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model))


inventor_model_glm<-glm(log.num.inventor.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model_glm))


inventor_reg_diagnostics<-plot(inventor_model)

inventor_reg_diagnostics


inventor_model_male<-lm(log.num.inventor.g.m.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model_male))


inventor_model_glm_male<-glm(log.num.inventor.g.m.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model_glm_male))

inventor_reg_diagnostics2<-plot(inventor_model_male)

inventor_reg_diagnostics

inventor_model_female<-lm(log.num.inventor.g.f.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model_female))


inventor_model_glm_female<-glm(log.num.inventor.g.f.na ~ num.cs_labforce+num.cs_family+num.tuition+num.cs_married+num.inc_share_1perc
                +num.inc_shar_1perc2+num.gini+num.hhinc00+num.scap
                +num.par_stateabbrv)

print(summary(inventor_model_glm_female))

inventor_reg_diagnostics3<-plot(inventor_model_female)

inventor_reg_diagnostics3

inventor_reg_diagnostics4<-plot(inventor_model_glm)
