## code to prepare `DATASET` dataset goes here
library(tidyverse)

####1) Data import and recoding ####
dat <- read.csv("./dyinvarRR.csv")#import csv
####(a) Recoding Sex at Birth #####
dat$sex.birth.1[dat$sex.birth.1==1]<-"Male"#Recode 1 to male
dat$sex.birth.1[dat$sex.birth.1==2]<-"Female"#Recode 2 to female

dat$sex.birth.2[dat$sex.birth.2==1]<-"Male"#Recode 1 to male
dat$sex.birth.2[dat$sex.birth.2==2]<-"Female"#Recode 2 to female

table(dat$sex.birth.1)#Frequencies for P1
table(dat$sex.birth.2)#Frequencies for P2

####(b) Recoding Gender Identity #####
dat$gen.id.1[dat$gen.id.1==1]<-"Male"#Recode 1 to male
dat$gen.id.1[dat$gen.id.1==2]<-"Female"#Recode 2 to female
dat$gen.id.1[dat$gen.id.1==3]<-"Indigenous GMI" #Recode 3 to Indigenous GMI
dat$gen.id.1[dat$gen.id.1==4]<-"Something Else" #Recode 4 to Something Else
dat$gen.id.1[dat$gen.id.1==5]<-"Prefer No Answer"#Recode 5 to Prefer No Answer

dat$gen.id.2[dat$gen.id.2==1]<-"Male"#Recode 1 to male
dat$gen.id.2[dat$gen.id.2==2]<-"Female"#Recode 2 to female
dat$gen.id.2[dat$gen.id.2==3]<-"Indigenous GMI" #Recode 3 to Indigenous GMI
dat$gen.id.2[dat$gen.id.2==4]<-"Something Else" #Recode 4 to Something Else
dat$gen.id.2[dat$gen.id.2==5]<-"Prefer No Answer"#Recode 5 to Prefer No Answer

table(dat$gen.id.1)#Frequencies for P1
table(dat$gen.id.2)#Frequencies for P2
####(c) Recoding Sexual Orientation #####
dat$sex.orient.1[dat$sex.orient.1==1]<-"Heterosexual"
dat$sex.orient.1[dat$sex.orient.1==2]<-"Lesbian/Gay"
dat$sex.orient.1[dat$sex.orient.1==3]<-"Bisexual"
dat$sex.orient.1[dat$sex.orient.1==4]<-"Asexual"
dat$sex.orient.1[dat$sex.orient.1==5]<-"Something Else"
dat$sex.orient.1[dat$sex.orient.1==6]<-"Prefer No Answer"

dat$sex.orient.2[dat$sex.orient.2==1]<-"Heterosexual"
dat$sex.orient.2[dat$sex.orient.2==2]<-"Lesbian/Gay"
dat$sex.orient.2[dat$sex.orient.2==3]<-"Bisexual"
dat$sex.orient.2[dat$sex.orient.2==4]<-"Asexual"
dat$sex.orient.2[dat$sex.orient.2==5]<-"Something Else"
dat$sex.orient.2[dat$sex.orient.2==6]<-"Prefer No Answer"

table(dat$sex.orient.1)#Frequencies for P1
table(dat$sex.orient.2)#Frequencies for P2
####(d) Categorizing Couples and Subsetting (sex)####
dat$couple_type_sex <- paste(dat$sex.birth.1,dat$sex.birth.2)
table(dat$couple_type_sex)
#Create subset of LGBTQ relationships
queerdat_sex<-dplyr::filter(dat, couple_type_sex == "Female Female"|
                       couple_type_sex == "Male Male")

#Orientations of LGBTQ couples
table(queerdat_sex$sex.orient.1)#Frequencies for P1
table(queerdat_sex$sex.orient.2)#Frequencies for P2

#Create subset of same-sex relationships
samesexdat_sex<-dplyr::filter(dat, couple_type_sex =="Female Male"|
                         couple_type_sex =="Male Female")

#Orientations of same-sex couples
table(samesexdat_sex$sex.orient.1)#Frequencies for P1
table(samesexdat_sex$sex.orient.2)#Frequencies for P2
####(e) Categorizing Couples and Subsetting (gender) #####
dat$couple_type_gender <- paste(dat$gen.id.1,dat$gen.id.2)

#Create subset of LGBTQ relationships
queerdat_gender<-filter(dat, couple_type_gender == "Female Female"|
                          couple_type_gender == "Male Male"|
                          couple_type_gender == "Female Indigenous GMI"|
                          couple_type_gender == "Indigenous GMI Indigenous GMI"|
                          couple_type_gender == "Indigenous GMI Male")

#Orientations of LGBTQ couples
table(queerdat_gender$sex.orient.1)#Frequencies for P1
table(queerdat_gender$sex.orient.2)#Frequencies for P2

#Create subset of same-sex relationships
samesexdat_gender<-filter(dat, couple_type_gender =="Female Male"|
                            couple_type_gender =="Male Female")

#Orientations of same-sex couples
table(samesexdat_gender$sex.orient.1)#Frequencies for P1
table(samesexdat_gender$sex.orient.2)#Frequencies for P2

#############################################################
####2) Separating and recombining mixed-sex couples####
samesexdat_sex <-select(samesexdat_sex, consent.p1:com.2_5)

#Assign couple ID
samesexdat_sex$couple <- seq(1:nrow(samesexdat_sex))

####(a) Male Partners #####
#Select male participants in P1 and rename variables
samesexdat_sex.male_1<-filter(select(samesexdat_sex, couple, age.1:com.1_5), sex.birth.1=="Male")
#Remove misplaced partner ID delimeter and character
clean_names<- str_replace(names(samesexdat_sex.male_1), ".1","")
#remove _ delimeter between items
clean_names<-str_remove_all(clean_names, "[_]")
#Add dyVarNames-friendly delimeter and character and rename
clean_names<-paste(clean_names, "_m", sep = "")
names(samesexdat_sex.male_1)<-clean_names

#Select male participants in P2
samesexdat_sex.male_2<-filter(select(samesexdat_sex, couple, age.2:com.2_5), sex.birth.2=="Male")
#Remove misplaced partner ID delimeter and character
clean_names.2<- str_replace(names(samesexdat_sex.male_2), ".2","")
#remove _ delimeter between items
clean_names.2<-str_remove_all(clean_names.2, "[_]")
#Add dyVarNames-friendly delimeter and character and rename
clean_names.2<-paste(clean_names.2, "_m", sep = "")
names(samesexdat_sex.male_2)<-clean_names.2

dat_sex_male<-rbind(samesexdat_sex.male_1, samesexdat_sex.male_2)
names(dat_sex_male)[names(dat_sex_male) == "couple_m"] <- "couple"
####(b) Female Partners #####
#Select female participants in P1 and rename variables
samesexdat_sex.female_1<-filter(select(samesexdat_sex, couple, age.1:com.1_5), sex.birth.1=="Female")
#Remove misplaced partner ID delimeter and character
clean_names<- str_replace(names(samesexdat_sex.female_1), ".1","")
#remove _ delimeter between items
clean_names<-str_remove_all(clean_names, "[_]")
#Add dyVarNames-friendly delimeter and character and rename
clean_names<-paste(clean_names, "_f", sep = "")
names(samesexdat_sex.female_1)<-clean_names

#Select female participants in P2
samesexdat_sex.female_2<-filter(select(samesexdat_sex, couple, age.2:com.2_5), sex.birth.2=="Female")
#Remove misplaced partner ID delimeter and character
clean_names.2<- str_replace(names(samesexdat_sex.female_2), ".2","")
#remove _ delimeter between items
clean_names.2<-str_remove_all(clean_names.2, "[_]")
#Add dyVarNames-friendly delimeter and character and rename
clean_names.2<-paste(clean_names.2, "_f", sep = "")
names(samesexdat_sex.female_2)<-clean_names.2

dat_sex_female<-rbind(samesexdat_sex.female_2, samesexdat_sex.female_1)
names(dat_sex_female)[names(dat_sex_female) == "couple_f"] <- "couple"

####(c) Check couple ID and merge #####
#Combine couple id's from men/women to make sure matching correctly
idcheck= data.frame(dat_sex_female$couple, dat_sex_male$couple)
#Is "Couple" ID the same?
idcheck$check <-all.equal(idcheck$dat_sex_female.couple, idcheck$dat_sex_male.couple)
#Are there any "FALSE"?
table(idcheck$check)
#combine
dydat<- cbind(dat_sex_female, dat_sex_male)
#Remove duplicate couple ID variable otherwise dplyr cranky
#https://stackoverflow.com/questions/54932063/using-r-getting-a-cant-bind-data-because-some-arguments-have-the-same-name-u
dydat<- dydat[-c(135)]

commitmentM <- dydat
commitmentQ <- queerdat_sex

imsM <- select(commitmentM,
               sat.g1_f:sat.g5_f,
               qalt.g1_f:qalt.g5_f,
               invest.g1_f:invest.g5_f,
               com1_f:com5_f,
               sat.g1_m:sat.g5_m,
               qalt.g1_m:qalt.g5_m,
               invest.g1_m:invest.g5_m,
               com1_m:com5_m)

usethis::use_data(imsM, overwrite = TRUE)
