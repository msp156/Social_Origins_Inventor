getwd()

setwd("/Your Directory/")
getwd()

#Load Packages. Reloading Packages after running the first file will not affect running the rest of the program

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

#check if character or numeric
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

social.origins.maps.df<- data.frame(char.par_state, num.par_state, char.par_stateabbrv, num.par_cz, char.par_czname, 
                               num.kid_count, num.kid_count_g_m, num.kid_count_g_f, num.inventor, log.num.inventor,num.inventor_cat_1,
                               num.inventor_cat_2, num.inventor_cat_3, num.inventor_cat_4,num.inventor_cat_5,
                               num.inventor_cat_6, num.inventor_g_m,log.num.inventor_g_m, num.inventor_cat_1_g_m, num.inventor_cat_2_g_m,
                               num.inventor_cat_3_g_m, num.inventor_cat_4_g_m, num.inventor_cat_5_g_m, num.inventor_cat_6_g_m, 
                               num.inventor_cat_7_g_m, num.inventor_g_f,log.num.inventor_g_f, num.inventor_cat_1_g_f, num.inventor_cat_2_g_f, 
                               num.inventor_cat_3_g_f, num.inventor_cat_4_g_f, num.inventor_cat_5_g_f, num.inventor_cat_6_g_f, 
                               num.inventor_cat_7_g_f)

social.origins.maps.df


setwd("/Your Directory/Working Files")
getwd()

write.csv(social.origins.maps.df)

#load charaacteristics data set
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
num.hhinc002<-as.numeric(unlist(num.hhinc00^2))
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



social.origins.char.maps.df<- data.frame(num.par_cz, char.par_czname, char.par_stateabbrv, 
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
                               
social.origins.char.maps.df

setwd("/Your Directory/Working Files")
getwd()

write.csv(social.origins.char.maps.df, 'social.origins.char.maps.df.csv')

setwd("/Your Directory/Working Files")
getwd()

us.social.origins.maps.df<-merge(social.origins.maps.df,social.origins.char.maps.df)

us.social.origins.maps.df

write.csv(us.social.origins.maps.df,'us.social.origins.maps.df.csv')


us_map_data<-merge(us.social.origins.maps.df,countypop)
us_map_data

write.csv(us_map_data,'us_map_data.csv')


#For demo purposes, I printed the Northeast region for each of the subpopulations. 
jpeg(file = "/Your Directory/Results",  width = 975, height = 975)

map1<-plot_usmap(data=us_map_data, values="num.inventor", color="black" , include=.northeast_region)+labs(color="num.inventor")+  scale_fill_continuous(
    low = "white", high = "red", name = "Inventor Share")+ theme(legend.position = "right")+ggtitle("Inventor")
map1

dev.off()

map1


jpeg(file = "/Your Directory/Results",  width = 975, height = 975)

map2<-plot_usmap(data=us_map_data, values="num.inventor_g_m", color="black" , include=.northeast_region)+labs(color="num.inventor_g_m")+  scale_fill_continuous(
    low = "white", high = "red", name = "Male Inventor Share")+ theme(legend.position = "right")+ggtitle("Male Inventor")
map2

dev.off()

map2

jpeg(file = "/Your Directory/Results",  width = 975, height = 975)


map3<-plot_usmap(data=us_map_data, values="num.inventor_g_f", color="black" , include=.northeast_region)+labs(color="num.inventor_g_f")+  scale_fill_continuous(
    low = "white", high = "red", name = "Female Inventor Share")+ theme(legend.position = "right")+ggtitle("Female Inventor")
map3

dev.off()

map3
