# Binary choice models

data = read.csv2(file="data.csv", header=TRUE, sep=";")


# A dataset created from a higher education institution (acquired from several disjoint databases) related to students enrolled in different undergraduate degrees, such as agronomy, design, education, nursing, journalism, management, social service, and technologies.
# 
# The dataset includes information known at the time of student enrollment (academic path, demographics, and social-economic factors) and the students' academic performance at the end of the first and second semesters.
# 
# The data is used to build classification models to predict students' dropout and academic success. The problem is formulated as a three category classification task (dropout, enrolled, and graduate) at the end of the normal duration of the course.


#Sys.setenv(LANG = "en")
options(scipen = 999)

# install.packages("BaylorEdPsych")
# install.packages("htmltools")
# install.packages("LogisticDx")
# install.packages("logistf")

library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("BaylorEdPsych")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf") #Firth's bias reduction method
library(dplyr)
library("stargazer")

#View(data)
data = na.omit(data)

colnames(data)


#######
# ï.¿Marital.status
plot(data$ï.¿Marital.status)
unique(data$ï.¿Marital.status)

# 1 – single
# 2 – married
# 3 – widower
# 4 – divorced
# 5 – facto union
# 6 – legally separated

#View(data[data$ï.¿Marital.status == 6,])

# We combine widower with divorced and legally separated
# and married with factor union
data$ï.¿Marital.status[data$ï.¿Marital.status == 4] <- 3
data$ï.¿Marital.status[data$ï.¿Marital.status == 6] <- 3
data$ï.¿Marital.status[data$ï.¿Marital.status == 5] <- 2

data$ï.¿Marital.status <- as.factor(data$ï.¿Marital.status)

#######
# Application.mode

# 1 - 1st phase - general contingent
# 2 - Ordinance No. 612/93
# 5 - 1st phase - special contingent (Azores Island)
# 7 - Holders of other higher courses
# 10 - Ordinance No. 854-B/99
# 15 - International student (bachelor)
# 16 - 1st phase - special contingent (Madeira Island)
# 17 - 2nd phase - general contingent
# 18 - 3rd phase - general contingent
# 26 - Ordinance No. 533-A/99, item b2) (Different Plan)
# 27 - Ordinance No. 533-A/99, item b3 (Other Institution)
# 39 - Over 23 years old
# 42 - Transfer
# 43 - Change of course
# 44 - Technological specialization diploma holders
# 51 - Change of institution/course
# 53 - Short cycle diploma holders
# 57 - Change of institution/course (International)


plot(data$Application.mode)
unique(data$Application.mode)

table(data$Application.mode)

# Combine all 1st phase to group 1
data$Application.mode[data$Application.mode == 5] <- 1
data$Application.mode[data$Application.mode == 16] <- 1

# All 'changes of institutions' together in group 51
data$Application.mode[data$Application.mode == 57] <- 51
data$Application.mode[data$Application.mode == 42] <- 51

# All 'ordinances' together in group 2
data$Application.mode[data$Application.mode == 10] <- 2
data$Application.mode[data$Application.mode == 26] <- 2
data$Application.mode[data$Application.mode == 27] <- 2

data$Application.mode <- as.factor(data$Application.mode)


#######
# Application.order
plot(data$Application.order)
unique(data$Application.order)

data$Application.order[data$Application.order == 0] <- 1
data$Application.order[data$Application.order == 9] <- 6

data$Application.order <- as.factor(data$Application.order)

#######
# Course
plot(data$Course)
unique(data$Course)

table(data$Course)

#administracyjne 1
data$Course[data$Course == 9238] <- 8014
data$Course[data$Course == 9254] <- 8014  

#artystyczno humanistyczne 2
data$Course[data$Course == 9070] <- 171
data$Course[data$Course == 9773] <- 171 
data$Course[data$Course == 9853] <- 171 

#biologiczno-przyrodnicze 3
data$Course[data$Course == 9003] <- 33
data$Course[data$Course == 9130] <- 33 
data$Course[data$Course == 9119] <- 33 

#ekonomiczne 4
data$Course[data$Course == 9147] <- 9670 
data$Course[data$Course == 9991] <- 9670 

#medyczne 5
data$Course[data$Course == 9085] <- 9556 
data$Course[data$Course == 9500] <- 9556 

# Grouping 
data$Course[data$Course == 8014] <- 1 
data$Course[data$Course == 171] <- 2 
data$Course[data$Course == 33] <- 3 
data$Course[data$Course == 9670] <- 4
data$Course[data$Course == 9556] <- 5

table(data$Course)

data$Course <- as.factor(data$Course)

#######
# Daytime.evening.attendance.
plot(data$Daytime.evening.attendance.)
unique(data$Daytime.evening.attendance.)

table(data$Daytime.evening.attendance.)

data$Daytime.evening.attendance. <- as.factor(data$Daytime.evening.attendance.)


#######
# Previous.qualification
plot(data$Previous.qualification)
unique(data$Previous.qualification)

table(data$Previous.qualification)

# Remove doctorate outlier
data <- data[data$Previous.qualification != 5,]

data$Previous.qualification[data$Previous.qualification == 10] <- 9
data$Previous.qualification[data$Previous.qualification == 12] <- 9
data$Previous.qualification[data$Previous.qualification == 14] <- 9
data$Previous.qualification[data$Previous.qualification == 15] <- 9
data$Previous.qualification[data$Previous.qualification == 19] <- 9
data$Previous.qualification[data$Previous.qualification == 38] <- 9

data$Previous.qualification[data$Previous.qualification == 43] <- 3
data$Previous.qualification[data$Previous.qualification == 4] <- 3
data$Previous.qualification[data$Previous.qualification == 40] <- 3
data$Previous.qualification[data$Previous.qualification == 2] <- 3              
data$Previous.qualification[data$Previous.qualification == 6] <- 3   
     
data$Previous.qualification[data$Previous.qualification == 42] <- 39              


data$Previous.qualification <- as.factor(data$Previous.qualification)


#######
# Previous.qualification..grade.

data$Previous.qualification..grade. <- as.numeric(data$Previous.qualification..grade.)

library(ggplot2)
ggplot(data, aes(x=Previous.qualification..grade.)) + 
  geom_histogram()


data$Previous.qualification <- as.factor(data$Previous.qualification)

# Raczej git zmienna 


#######
# Nacionality
plot(data$Nacionality)
unique(data$Nacionality)

table(data$Nacionality)

data$Nacionality[data$Nacionality != 1] <- "Non Portuguese"
data$Nacionality[data$Nacionality == 1] <- "Portuguese"

data$Nacionality <- as.factor(data$Nacionality)


#######
# Mother.s.qualification       
plot(data$Mother.s.qualification)
unique(data$Mother.s.qualification)

table(data$Mother.s.qualification)

# We remove level 5 (Higher Education - Doctorate) because number of observations was very small
data <- data[data$Mother.s.qualification != 5,]

# All high school and lower not completed to group 9
data$Mother.s.qualification[data$Mother.s.qualification == 10] <- 9
data$Mother.s.qualification[data$Mother.s.qualification == 12] <- 9
data$Mother.s.qualification[data$Mother.s.qualification == 14] <- 9
#data$Mother.s.qualification[data$Mother.s.qualification == 15] <- 9
data$Mother.s.qualification[data$Mother.s.qualification == 19] <- 9
data$Mother.s.qualification[data$Mother.s.qualification == 38] <- 9

# All higher education in group 3
data$Mother.s.qualification[data$Mother.s.qualification == 43] <- 3
data$Mother.s.qualification[data$Mother.s.qualification == 4] <- 3
data$Mother.s.qualification[data$Mother.s.qualification == 40] <- 3
data$Mother.s.qualification[data$Mother.s.qualification == 2] <- 3              
data$Mother.s.qualification[data$Mother.s.qualification == 6] <- 3   

# All special courses to group 39
data$Mother.s.qualification[data$Mother.s.qualification == 42] <- 39     
data$Mother.s.qualification[data$Mother.s.qualification == 18] <- 39     
data$Mother.s.qualification[data$Mother.s.qualification == 22] <- 39
data$Mother.s.qualification[data$Mother.s.qualification == 41] <- 39

# All high school and lower not completed to group 9
data$Mother.s.qualification[data$Mother.s.qualification == 26] <- 11
data$Mother.s.qualification[data$Mother.s.qualification == 27] <- 11 
data$Mother.s.qualification[data$Mother.s.qualification == 29] <- 11 
data$Mother.s.qualification[data$Mother.s.qualification == 30] <- 11 
data$Mother.s.qualification[data$Mother.s.qualification == 11] <- 9

# Remove Can't read or write and Can read without having a 4th year of schooling
data <- data[!data$Mother.s.qualification %in% c(35, 36),] 

data$Mother.s.qualification[data$Mother.s.qualification == 44] <- 3


table(data$Mother.s.qualification)

data$Mother.s.qualification <- as.factor(data$Mother.s.qualification)


#######
# Father.s.qualification  
plot(data$Father.s.qualification)
unique(data$Father.s.qualification)

table(data$Father.s.qualification)

data <- data[data$Father.s.qualification != 5,]

data$Father.s.qualification[data$Father.s.qualification == 10] <- 9
data$Father.s.qualification[data$Father.s.qualification == 12] <- 9
data$Father.s.qualification[data$Father.s.qualification == 14] <- 9
data$Father.s.qualification[data$Father.s.qualification == 15] <- 9
data$Father.s.qualification[data$Father.s.qualification == 19] <- 9
data$Father.s.qualification[data$Father.s.qualification == 38] <- 9

data$Father.s.qualification[data$Father.s.qualification == 43] <- 3
data$Father.s.qualification[data$Father.s.qualification == 4] <- 3
data$Father.s.qualification[data$Father.s.qualification == 40] <- 3
data$Father.s.qualification[data$Father.s.qualification == 2] <- 3              
data$Father.s.qualification[data$Father.s.qualification == 6] <- 3   

data$Father.s.qualification[data$Father.s.qualification == 42] <- 39     
data$Father.s.qualification[data$Father.s.qualification == 18] <- 39     
data$Father.s.qualification[data$Father.s.qualification == 22] <- 39
data$Father.s.qualification[data$Father.s.qualification == 41] <- 39

data$Father.s.qualification[data$Father.s.qualification == 26] <- 11
data$Father.s.qualification[data$Father.s.qualification == 27] <- 11 
data$Father.s.qualification[data$Father.s.qualification == 29] <- 11 
data$Father.s.qualification[data$Father.s.qualification == 30] <- 11 
data$Father.s.qualification[data$Father.s.qualification == 11] <- 9

# Remove Can't read or write and Can read without having a 4th year of schooling
data <- data[!data$Father.s.qualification %in% c(35, 36),] 

data$Father.s.qualification[data$Father.s.qualification == 44] <- 3

data$Father.s.qualification[data$Father.s.qualification == 13] <- 39
data$Father.s.qualification[data$Father.s.qualification == 20] <- 39
data$Father.s.qualification[data$Father.s.qualification == 25] <- 39
data$Father.s.qualification[data$Father.s.qualification == 31] <- 39
data$Father.s.qualification[data$Father.s.qualification == 33] <- 39

table(data$Father.s.qualification)

data$Father.s.qualification <-as.factor(data$Father.s.qualification)

          
#######
# Mother.s.occupation_is
plot(data$Mother.s.occupation)
unique(data$Mother.s.occupation)

table(data$Mother.s.occupation)

data$Mother.s.occupation[data$Mother.s.occupation >= 110 & data$Mother.s.occupation < 120] <- 1
data$Mother.s.occupation[data$Mother.s.occupation >= 120 & data$Mother.s.occupation < 130] <- 2
data$Mother.s.occupation[data$Mother.s.occupation >= 130 & data$Mother.s.occupation < 140] <- 3
data$Mother.s.occupation[data$Mother.s.occupation >= 140 & data$Mother.s.occupation < 150] <- 4
data$Mother.s.occupation[data$Mother.s.occupation >= 150 & data$Mother.s.occupation < 160] <- 5
data$Mother.s.occupation[data$Mother.s.occupation >= 160 & data$Mother.s.occupation < 170] <- 6
data$Mother.s.occupation[data$Mother.s.occupation >= 170 & data$Mother.s.occupation < 180] <- 7
data$Mother.s.occupation[data$Mother.s.occupation >= 180 & data$Mother.s.occupation < 190] <- 8
data$Mother.s.occupation[data$Mother.s.occupation >= 190 & data$Mother.s.occupation < 200] <- 9
data$Mother.s.occupation[data$Mother.s.occupation >= 100 & data$Mother.s.occupation < 110] <- 10

# # Proste prace
# easy <- c(4, 5, 9, 141, 144, 151, 152, 153, 175, 191, 192, 193, 194)
# data$Mother.s.occupation[data$Mother.s.occupation %in% easy] <- "Easy"
# # Nieproste prace
# non_easy <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 122, 123, 125, 131, 132, 134, 143, 171, 173)
# data$Mother.s.occupation[data$Mother.s.occupation %in% non_easy] <- "Non easy"

data <- data[!data$Mother.s.occupation %in% c(90, 99),]                       

unique(data$Mother.s.occupation)

# data$Mother.s.occupation[data$Mother.s.occupation == "Easy"] <- 1
# data$Mother.s.occupation[data$Mother.s.occupation == "Non easy"] <- 0

table(data$Mother.s.occupation)

data$Mother.s.occupation <- as.factor(data$Mother.s.occupation)


#######
# Father.s.occupation
plot(data$Father.s.occupation)
unique(data$Father.s.occupation)

table(data$Father.s.occupation)

# # Proste prace
# easy <- c(4, 5, 9, 102, 103, 141, 144, 151, 152, 153, 154, 163, 175, 191, 192, 193, 194)
# data$Father.s.occupation[data$Father.s.occupation %in% easy] <- "Easy"
# # Nieproste prace
# non_easy <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 101, 112, 114, 121, 122, 123, 124, 131, 132, 134, 135, 143, 161, 171, 172, 174, 173)
# data$Father.s.occupation[data$Father.s.occupation %in% non_easy] <- "Non easy"

data$Father.s.occupation[data$Father.s.occupation >= 110 & data$Father.s.occupation < 120] <- 1
data$Father.s.occupation[data$Father.s.occupation >= 120 & data$Father.s.occupation < 130] <- 2
data$Father.s.occupation[data$Father.s.occupation >= 130 & data$Father.s.occupation < 140] <- 3
data$Father.s.occupation[data$Father.s.occupation >= 140 & data$Father.s.occupation < 150] <- 4
data$Father.s.occupation[data$Father.s.occupation >= 150 & data$Father.s.occupation < 160] <- 5
data$Father.s.occupation[data$Father.s.occupation >= 160 & data$Father.s.occupation < 170] <- 6
data$Father.s.occupation[data$Father.s.occupation >= 170 & data$Father.s.occupation < 180] <- 7
data$Father.s.occupation[data$Father.s.occupation >= 180 & data$Father.s.occupation < 190] <- 8
data$Father.s.occupation[data$Father.s.occupation >= 190 & data$Father.s.occupation < 200] <- 9
data$Father.s.occupation[data$Father.s.occupation >= 100 & data$Father.s.occupation < 110] <- 10


data <- data[!data$Father.s.occupation %in% c(90, 99),]                       

unique(data$Father.s.occupation)

# data$Father.s.occupation[data$Father.s.occupation == "Easy"] <- 1
# data$Father.s.occupation[data$Father.s.occupation == "Non easy"] <- 0

table(data$Father.s.occupation)

data$Father.s.occupation <- as.factor(data$Father.s.occupation)

#######
# Admission.grade
data$Admission.grade <- as.numeric(data$Admission.grade)

library(ggplot2)
ggplot(data, aes(x=Admission.grade)) + 
  geom_histogram()

# Raczej git zmienna 



#######
# Displaced
table(data$Displaced)

data$Displaced <- as.factor(data$Displaced)


#######
# Educational.special.needs
table(data$Educational.special.needs)

data$Educational.special.needs <- as.factor(data$Educational.special.needs)


#######
# Debtor
table(data$Debtor)

data$Debtor <- as.factor(data$Debtor)


#######
# Tuition.fees.up.to.date
table(data$Tuition.fees.up.to.date)

data$Tuition.fees.up.to.date <- as.factor(data$Tuition.fees.up.to.date)

#######
# Gender
table(data$Gender)

data$Gender <- as.factor(data$Gender)

#######
# Scholarship.holder
table(data$Scholarship.holder)

data$Scholarship.holder <- as.factor(data$Scholarship.holder)

#######
# Age.at.enrollment
table(data$Age.at.enrollment)   

library(ggplot2)
ggplot(data, aes(x=Age.at.enrollment)) + 
  geom_histogram()

data$Age.at.enrollment[data$Age.at.enrollment %in% c(17, 18, 19)] <- "17-19"
data$Age.at.enrollment[data$Age.at.enrollment >= 20 & data$Age.at.enrollment < 30] <- "20-29"
data$Age.at.enrollment[data$Age.at.enrollment >= 30] <- "30+"

table(data$Age.at.enrollment) 
        
data$Age.at.enrollment <- as.factor(data$Age.at.enrollment)              

#######
# International   
table(data$International)   

# Same variable as Nationality, we remove International
data <- data[,!(names(data) %in% "International")]

data$International <- as.factor(data$International)


#######
# Curricular.units.1st.sem..credited.  
table(data$Curricular.units.1st.sem..credited.)   

data$Curricular.units.1st.sem..credited.[data$Curricular.units.1st.sem..credited. > 0] <- "non_zero"
data$Curricular.units.1st.sem..credited.[data$Curricular.units.1st.sem..credited. == "0"] <- "zero"

data$Curricular.units.1st.sem..credited. <- as.factor(data$Curricular.units.1st.sem..credited.)


#######
# Curricular.units.1st.sem..enrolled.  
table(data$Curricular.units.1st.sem..enrolled.)   

data$Curricular.units.1st.sem..enrolled. <- as.numeric(data$Curricular.units.1st.sem..enrolled.)

data$Curricular.units.1st.sem..enrolled.[data$Curricular.units.1st.sem..enrolled. > 7] <- 8
data$Curricular.units.1st.sem..enrolled.[data$Curricular.units.1st.sem..enrolled. %in% c(0, 1, 2, 3, 4)] <- 0
data$Curricular.units.1st.sem..enrolled.[data$Curricular.units.1st.sem..enrolled. == 5] <- 5
data$Curricular.units.1st.sem..enrolled.[data$Curricular.units.1st.sem..enrolled. == 6] <- 6
data$Curricular.units.1st.sem..enrolled.[data$Curricular.units.1st.sem..enrolled. == 7] <- 7



#######
# Curricular.units.1st.sem..evaluations.            
table(data$Curricular.units.1st.sem..evaluations.)   

data$Curricular.units.1st.sem..evaluations. <- as.numeric(data$Curricular.units.1st.sem..evaluations.)

#glimpse(data)
data$Curricular.units.1st.sem..evaluations.[data$Curricular.units.1st.sem..evaluations. > 16] <- 17
data$Curricular.units.1st.sem..evaluations.[data$Curricular.units.1st.sem..evaluations. %in% c(0, 1, 2, 3, 4)] <- 0



#######
# Curricular.units.1st.sem..approved.            
table(data$Curricular.units.1st.sem..approved.)   
data$Curricular.units.1st.sem..approved. <- as.numeric(data$Curricular.units.1st.sem..approved.)

data$Curricular.units.1st.sem..approved.[data$Curricular.units.1st.sem..approved. > 8] <- 9



#######
# Curricular.units.1st.sem..grade.      
table(data$Curricular.units.1st.sem..grade.)   
data$Curricular.units.1st.sem..grade. <- as.numeric(data$Curricular.units.1st.sem..grade.)

library(ggplot2)
ggplot(data, aes(x=Curricular.units.1st.sem..grade.)) + 
  geom_histogram()


#######
# Curricular.units.1st.sem..without.evaluations.      
table(data$Curricular.units.1st.sem..without.evaluations.)   
data$Curricular.units.1st.sem..without.evaluations. <- as.numeric(data$Curricular.units.1st.sem..without.evaluations.)

data$Curricular.units.1st.sem..without.evaluations.[data$Curricular.units.1st.sem..without.evaluations. > 0] <- 1

       
#######
# Curricular.units.2nd.sem..credited.      
table(data$Curricular.units.2nd.sem..credited.) 

data$Curricular.units.2nd.sem..credited.[data$Curricular.units.2nd.sem..credited. > 0] <- "non_zero"
data$Curricular.units.2nd.sem..credited.[data$Curricular.units.2nd.sem..credited. == "0"] <- "zero"

data$Curricular.units.2nd.sem..credited. <- as.factor(data$Curricular.units.2nd.sem..credited.)


#######
# Curricular.units.2nd.sem..enrolled.      
table(data$Curricular.units.2nd.sem..enrolled.)  

data$Curricular.units.2nd.sem..enrolled. <- as.numeric(data$Curricular.units.2nd.sem..enrolled.)

data$Curricular.units.2nd.sem..enrolled.[data$Curricular.units.2nd.sem..enrolled. > 7] <- 8
data$Curricular.units.2nd.sem..enrolled.[data$Curricular.units.2nd.sem..enrolled. %in% c(0, 1, 2, 3, 4)] <- 0
data$Curricular.units.2nd.sem..enrolled.[data$Curricular.units.2nd.sem..enrolled. == 5] <- 5
data$Curricular.units.2nd.sem..enrolled.[data$Curricular.units.2nd.sem..enrolled. == 6] <- 6
data$Curricular.units.2nd.sem..enrolled.[data$Curricular.units.2nd.sem..enrolled. == 7] <- 7



#######
# Curricular.units.2nd.sem..evaluations.      
table(data$Curricular.units.2nd.sem..evaluations.) 

data$Curricular.units.2nd.sem..evaluations. <- as.numeric(data$Curricular.units.2nd.sem..evaluations.)

data$Curricular.units.2nd.sem..evaluations.[data$Curricular.units.2nd.sem..evaluations. > 16] <- 17
data$Curricular.units.2nd.sem..evaluations.[data$Curricular.units.2nd.sem..evaluations. %in% c(0, 1, 2, 3, 4)] <- 0
data$Curricular.units.2nd.sem..evaluations. <- as.numeric(data$Curricular.units.2nd.sem..evaluations.)



#######
# Curricular.units.2nd.sem..approved.      
table(data$Curricular.units.2nd.sem..approved.) 

data$Curricular.units.2nd.sem..approved. <- as.numeric(data$Curricular.units.2nd.sem..approved.)
data$Curricular.units.2nd.sem..approved.[data$Curricular.units.2nd.sem..approved. > 8] <- 9



#######
# Curricular.units.2nd.sem..grade.      
table(data$Curricular.units.2nd.sem..grade.) 

data$Curricular.units.2nd.sem..grade. <- as.numeric(data$Curricular.units.2nd.sem..grade.)

library(ggplot2)
ggplot(data, aes(x=Curricular.units.2nd.sem..grade.)) + 
  geom_histogram()



#######
# Curricular.units.2nd.sem..without.evaluations.      
table(data$Curricular.units.2nd.sem..without.evaluations.) 

data$Curricular.units.2nd.sem..without.evaluations. <- as.numeric(data$Curricular.units.2nd.sem..without.evaluations.)
data$Curricular.units.2nd.sem..without.evaluations.[data$Curricular.units.2nd.sem..without.evaluations. > 0] <- "non_zero"
data$Curricular.units.2nd.sem..without.evaluations.[data$Curricular.units.2nd.sem..without.evaluations. == "0"] <- "zero"
#data$Curricular.units.2nd.sem..without.evaluations. <- as.factor(data$Curricular.units.2nd.sem..without.evaluations.)



#######
# Unemployment.rate      
table(data$Unemployment.rate) 

data$Unemployment.rate <- as.numeric(data$Unemployment.rate)

library(ggplot2)
ggplot(data, aes(x=Unemployment.rate)) + 
  geom_histogram()



#######
# Inflation.rate    
table(data$Inflation.rate) 

data$Inflation.rate <- as.numeric(data$Inflation.rate)

library(ggplot2)
ggplot(data, aes(x=Inflation.rate)) + 
  geom_histogram()


#######
# GDP    
table(data$GDP) 

data$GDP <- as.numeric(data$GDP)

ggplot(data, aes(x=GDP)) + 
  geom_histogram()                       

                                       
#######
# Target    
table(data$Target) 

data <- data[!data$Target %in% c("Enrolled"),] 
#data$Target[data$Target == "Dropout"] <- 1
#data$Target[data$Target == "Graduate"] <- 0
#data$Target <- as.factor(data$Target)


############
#glimpse(data)

#colnames(data[,select()])
#target <- c("Target")
#variables <- data[, !colnames(data) %in% target]

############

####################################
# To be able to drop categorical variables we encode them to dummy
library(caret)
dmy <- dummyVars(" ~ .", data = data, fullRank = T)
data <- data.frame(predict(dmy, newdata = data))

colnames(data)

colnames(data)[which(names(data) == "TargetGraduate")] <- "Target"

# TARGET
# 1 - if Graduate
# 0 - if Dropout

data[,1:25] <- lapply(data[,1:25], as.factor)
data[,27:57] <- lapply(data[,27:57], as.factor)
data[,59:67] <- lapply(data[,59:67], as.factor)
data[,72:73] <- lapply(data[,72:73], as.factor)

data$Curricular.units.2nd.sem..without.evaluations.zero <- as.factor(data$Curricular.units.2nd.sem..without.evaluations.zero)
data$Target <- as.factor(data$Target)


target <- "Target"


#################################################################
# estimation of linear probability model (OLS with White’s robust matrix), logit model, and probit model
#################################################################
data$Target <- as.numeric(data$Target)
variables  <- colnames(data[ , !(names(data) %in% c("Target"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))
# linear probability model
linear_probity_raw = lm(formula, data=data)
summary(linear_probity_raw)

# specification test
resettest(linear_probity_raw, power=2:3, type="fitted")
# p-value < 0.05, we have to reject the H0
# The model has the inappropriate form

# heteroscedasticity
linear_probity_raw.residuals = linear_probity_raw$residuals
#plot(linear_probity_raw.residuals~log.pop, data=data)
#plot(linear_probity_raw.residuals~log.gdp, data=data)
#bptest(linear_probity_raw.residuals~log.pop, data=data)
bptest(linear_probity_raw.residuals~ï.¿Marital.status.2 + ï.¿Marital.status.3 + Application.mode.2 + 
         Application.mode.7 + Application.mode.15 + Application.mode.17 + 
         Application.mode.18 + Application.mode.39 + Application.mode.43 + 
         Application.mode.44 + Application.mode.51 + Application.mode.53 + 
         Application.order.2 + Application.order.3 + Application.order.4 + 
         Application.order.5 + Application.order.6 + Course.2 + Course.3 + 
         Course.4 + Course.5 + Daytime.evening.attendance..1 + Previous.qualification.3 + 
         Previous.qualification.9 + Previous.qualification.39 + Previous.qualification..grade. + 
         Nacionality.Portuguese + Mother.s.qualification.3 + Mother.s.qualification.9 + 
         Mother.s.qualification.34 + Mother.s.qualification.37 + Mother.s.qualification.39 + 
         Father.s.qualification.3 + Father.s.qualification.9 + Father.s.qualification.34 + 
         Father.s.qualification.37 + Father.s.qualification.39 + Mother.s.occupation.1 + 
         Mother.s.occupation.2 + Mother.s.occupation.3 + Mother.s.occupation.4 + 
         Mother.s.occupation.5 + Mother.s.occupation.6 + Mother.s.occupation.7 + 
         Mother.s.occupation.8 + Mother.s.occupation.9 + Mother.s.occupation.10 + 
         Father.s.occupation.1 + Father.s.occupation.2 + Father.s.occupation.3 + 
         Father.s.occupation.4 + Father.s.occupation.5 + Father.s.occupation.6 + 
         Father.s.occupation.7 + Father.s.occupation.8 + Father.s.occupation.9 + 
         Father.s.occupation.10 + Admission.grade + Displaced.1 + 
         Educational.special.needs.1 + Debtor.1 + Tuition.fees.up.to.date.1 + 
         Gender.1 + Scholarship.holder.1 + Age.at.enrollment.20.29 + 
         Age.at.enrollment.30. + Curricular.units.1st.sem..credited..zero + 
         Curricular.units.1st.sem..enrolled. + Curricular.units.1st.sem..evaluations. + 
         Curricular.units.1st.sem..approved. + Curricular.units.1st.sem..grade. + 
         Curricular.units.1st.sem..without.evaluations. + Curricular.units.2nd.sem..credited..zero + 
         Curricular.units.2nd.sem..enrolled. + Curricular.units.2nd.sem..evaluations. + 
         Curricular.units.2nd.sem..approved. + Curricular.units.2nd.sem..grade. + 
         Curricular.units.2nd.sem..without.evaluations.zero + Unemployment.rate + 
         Inflation.rate + GDP, data=data)

# p-value < 0.05, we have to reject the H0
# The residuals are heteroscedastics

# Because both tests fails (RESEST and Breusch-Pagan) the form of the model is incorrect, and homoscedasticity assumption.
# So, the estimates are biased and standard errors of the estimates are biased and inconsistent.

#View(linear_probity_raw$fitted.values)

# We can fix the heteroscedasticity problem by using White's estimator of the variance-covariance matrix.
# However, it only fixes one of the issues. The form of the model will remain incorrect.\
# What's more, linear model predictions are likely to appear outside the 0 and 1 interval. That's why using linear probability model is incorrect.

# White's estimator of the variance-covariane matrix
robust_vcov = vcovHC(linear_probity_raw, data = data, type = "HC")
robust.linear_probity_raw = coeftest(linear_probity_raw, vcov.=robust_vcov)

data$Target <- as.factor(data$Target)

# We estimate probit and logit on all variables
# Probit
probit_model_raw <- glm(formula, data=data, family=binomial(link="probit"))
# Logit
logit_model_raw <- glm(formula, data=data, family=binomial(link="logit"))
# All 3 models combined
stargazer(robust.linear_probity_raw, probit_model_raw, logit_model_raw,  type="text")

#  Akaike Inf. Crit. suggests that the logit model is the best out of these three.





#################################################################
# LOGIT MODEL
#################################################################


variables  <- colnames(data[ , !(names(data) %in% c("Target"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


# 1st estimation of logit model
logit_model <- glm(formula, data=data, family=binomial(link="logit"))
summary(logit_model)
logit_model$coefficients
summary(logit_model)$coefficients[2,3]



# Joint insignificance of all variables test
null_logit = glm(Target~1, data=data, family=binomial(link="logit"))
lrtest(logit_model, null_logit)
#p-value < 0.05, reject the null hypothesis, at least one of the variable estimator is not equal to zero

# Niby smiga, ale duzo nieistotnych zmiennych

# GENERAL TO SPECIFIC PROCEDURE
# LR-test
# unrest vs. restricted
# H0: te modele sa takie same
# czyli jesli p-value > 0.05, wtedy unrest i restricted mozemy uznac za takie same modele, wiec mozemy zostac przy restricted

# We drop the most insignificat ones (p-value > 0.7)
variables <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted1 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value > 0.05, we can drop selected variables

logit_model <- logit_model_restricted
summary(logit_model)

# Now, we drop p-value > 0.3 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted2 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value > 0.05, we can drop selected variables

logit_model <- logit_model_restricted
summary(logit_model)

# Now, we drop p-value > 0.3 AGAIN
variables <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted3 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.5381 > 0.05, we can drop selected variables

logit_model <- logit_model_restricted
summary(logit_model)


# Now, we drop p-value > 0.3 AGAIN AGAIN
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted4 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.6842 > 0.05, we can drop selected variables

logit_model <- logit_model_restricted
summary(logit_model)



# Now, we drop p-value > 0.3 AGAIN AGAIN AGAIN
variables <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Mother.s.qualification.34",
                                     "Father.s.qualification.34"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted5 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.004787 < 0.05, we cannot drop them jointly

summary(logit_model)

# Now, we drop one by one, with the highest p-value ("Father.s.qualification.341")
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted6 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.3699 > 0.05, we can drop Father.s.qualification.34

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Father.s.occupation.81 0.26
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted7 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.2747 > 0.05, we drop Father.s.occupation.81

logit_model <-logit_model_restricted
summary(logit_model)

# Highest p-value: Father.s.occupation.71 0.30
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted8 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.3115 > 0.05, we drop Father.s.occupation.71

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Father.s.occupation.31 0.35
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted8 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.3565 > 0.05, we drop Father.s.occupation.31

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Curricular.units.1st.sem..without.evaluations.1 0.231849    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations."))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted9 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.2351 > 0.05, we drop Curricular.units.1st.sem..without.evaluations.1

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: GDP 0.29    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted10 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.2889 > 0.05, we drop GDP

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Admission.grade 0.18880    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted11 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.1876 > 0.05, we drop Admission.grade

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Course.51 0.20549    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted12 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.2035 > 0.05, we drop Course.51

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Daytime.evening.attendance..11 0.206702    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted13 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.209 > 0.05, we drop Daytime.evening.attendance..11

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Application.mode.21 0.187412    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted14 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.2028 > 0.05, we drop Application.mode.21

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Mother.s.occupation.71 0.15623    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted15 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.1642 > 0.05, we drop Mother.s.occupation.7

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Father.s.qualification.391 0.15293    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7",
                                     "Father.s.qualification.39"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted17 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.16 > 0.05, we drop Father.s.qualification.391

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Mother.s.occupation.81 0.148721    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7",
                                     "Father.s.qualification.39",
                                     "Mother.s.occupation.8"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted18 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.1597 > 0.05, we drop Mother.s.occupation.8

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Displaced.11 0.105087    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7",
                                     "Father.s.qualification.39",
                                     "Mother.s.occupation.8",
                                     "Displaced.1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted19 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.1035 > 0.05, we drop Displaced.1

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Application.mode.391 0.077376    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7",
                                     "Father.s.qualification.39",
                                     "Mother.s.occupation.8",
                                     "Displaced.1",
                                     "Application.mode.39"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
logit_model_restricted20 <- logit_model_restricted
lrtest(logit_model, logit_model_restricted)
# p-value = 0.07959  > 0.05, we drop Application.mode.391

logit_model <- logit_model_restricted
summary(logit_model)


# Highest p-value: Application.mode.511 0.050175    
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Previous.qualification..grade.",
                                     "Father.s.qualification.3",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.6",
                                     "Educational.special.needs.1",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.7",
                                     "Application.mode.43",
                                     "Application.order.2",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Age.at.enrollment.20.29",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "Inflation.rate",
                                     "Application.mode.53",
                                     "Father.s.qualification.37",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.3",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.10",
                                     "Mother.s.qualification.37",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.5",
                                     "Father.s.occupation.5",
                                     "Father.s.qualification.34",
                                     "Father.s.occupation.8",
                                     "Father.s.occupation.7",
                                     "Father.s.occupation.3",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "GDP",
                                     "Admission.grade",
                                     "Course.5",
                                     "Daytime.evening.attendance..1",
                                     "Application.mode.2",
                                     "Mother.s.occupation.7",
                                     "Father.s.qualification.39",
                                     "Mother.s.occupation.8",
                                     "Displaced.1",
                                     "Application.mode.39",
                                     "Application.mode.51"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


logit_model_restricted <- glm(formula, data=data, 
                              family=binomial(link="logit"))
lrtest(logit_model, logit_model_restricted)
# p-value = 0.05553   > 0.05, we drop Application.mode.511

logit_model <- logit_model_restricted
summary(logit_model)


# SUCCESS ALL VARIABLES ARE SIGNIFICANT!!!

# FINAL MODEL after dropping insignificant variables
summary(logit_model)
stargazer(logit_model_restricted5, logit_model_restricted15, logit_model,  type="text")



# DIAGNOSTICS

# LINKTEST
source("linktest.R")
linktest_result = linktest(logit_model)
summary(linktest_result)
# yhat = 0 < 0.05 (significant)
# yhat^2 = 0.00745  < 0.05 (significant)

# 1: add Curricular.units.1st.sem..approved.*Curricular.units.2nd.sem..approved and remove Debtor (we think it is correlated with Tuition.fees.up.to.date.11)
logit_model_test <- glm(Target~Application.mode.15+
                          Application.mode.17+
                          Application.mode.44+
                          Course.2+
                          Course.3+
                          Course.4+
                          Previous.qualification.3+
                          Mother.s.qualification.34+
                          Father.s.qualification.9+
                          Father.s.occupation.4+
                          Father.s.occupation.9+
                          Tuition.fees.up.to.date.1+
                          Gender.1+
                          Scholarship.holder.1+
                          Curricular.units.1st.sem..credited..zero+
                          Curricular.units.1st.sem..enrolled.+
                          Curricular.units.1st.sem..approved.+
                          Curricular.units.2nd.sem..enrolled.+
                          Curricular.units.2nd.sem..evaluations.+
                          Curricular.units.2nd.sem..approved.+
                          Curricular.units.2nd.sem..grade.+
                          Unemployment.rate+
                          Curricular.units.1st.sem..approved.*Curricular.units.2nd.sem..approved.,
                        data=data, family=binomial(link="logit"))
summary(logit_model_test)
linktest_result = linktest(logit_model_test)

# 2. Remove insignificant Curricular.units.2nd.sem..grade. variable
logit_model_test2 <- glm(Target~Application.mode.15+
                           Application.mode.17+
                           Application.mode.44+
                           Course.2+
                           Course.3+
                           Course.4+
                           Previous.qualification.3+
                           Mother.s.qualification.34+
                           Father.s.qualification.9+
                           Father.s.occupation.4+
                           Father.s.occupation.9+
                           Tuition.fees.up.to.date.1+
                           Gender.1+
                           Scholarship.holder.1+
                           Curricular.units.1st.sem..credited..zero+
                           Curricular.units.1st.sem..enrolled.+
                           Curricular.units.1st.sem..approved.+
                           Curricular.units.2nd.sem..enrolled.+
                           Curricular.units.2nd.sem..evaluations.+
                           Curricular.units.2nd.sem..approved.+
                           Unemployment.rate+
                           Curricular.units.1st.sem..approved.*Curricular.units.2nd.sem..approved.,
                         data=data, family=binomial(link="logit"))
summary(logit_model_test2)
lrtest(logit_model_test, logit_model_test2)
# p-value > 0.05, We can remove Curricular.units.2nd.sem..grade.

#3. add Curricular.units.1st.sem..credited..zero*Scholarship.holder.1
logit_model_test3 <- glm(Target~Application.mode.15+
                           Application.mode.17+
                           Application.mode.44+
                           Course.2+
                           Course.3+
                           Course.4+
                           Previous.qualification.3+
                           Mother.s.qualification.34+
                           Father.s.qualification.9+
                           Father.s.occupation.4+
                           Father.s.occupation.9+
                           Tuition.fees.up.to.date.1+
                           Gender.1+
                           Scholarship.holder.1+
                           Curricular.units.1st.sem..credited..zero+
                           Curricular.units.1st.sem..enrolled.+
                           Curricular.units.1st.sem..approved.+
                           Curricular.units.2nd.sem..enrolled.+
                           Curricular.units.2nd.sem..evaluations.+
                           Curricular.units.2nd.sem..approved.+
                           Unemployment.rate+
                           Curricular.units.1st.sem..approved.*Curricular.units.2nd.sem..approved.+
                           Curricular.units.1st.sem..credited..zero*Scholarship.holder.1,
                         data=data, family=binomial(link="logit"))
summary(logit_model_test3)

#4. remove insignificant Previous.qualification.31
logit_model_test4 <- glm(Target~Application.mode.15+
                           Application.mode.17+
                           Application.mode.44+
                           Course.2+
                           Course.3+
                           Course.4+
                           Mother.s.qualification.34+
                           Father.s.qualification.9+
                           Father.s.occupation.4+
                           Father.s.occupation.9+
                           Tuition.fees.up.to.date.1+
                           Gender.1+
                           Scholarship.holder.1+
                           Curricular.units.1st.sem..credited..zero+
                           Curricular.units.1st.sem..enrolled.+
                           Curricular.units.1st.sem..approved.+
                           Curricular.units.2nd.sem..enrolled.+
                           Curricular.units.2nd.sem..evaluations.+
                           Curricular.units.2nd.sem..approved.+
                           Unemployment.rate+
                           Curricular.units.1st.sem..approved.*Curricular.units.2nd.sem..approved.+
                           Curricular.units.1st.sem..credited..zero*Scholarship.holder.1,
                         data=data, family=binomial(link="logit"))
summary(logit_model_test4)
lrtest(logit_model_test4, logit_model_test3)
# p-value > 0.05, We can remove Previous.qualification.31

logit_main_model <- logit_model_test4


# LINKTEST AGAIN
linktest_result = linktest(logit_main_model)
summary(linktest_result)
# yhat = 0 < 0.05 (significant)
# yhat^2 = 0.0616   > 0.05 (insignificant)


# LIKELIHOOD test - PASSED
lrtest(logit_main_model, null_logit)
# p-value = 0
# We reject the H0
# DZIA£A

# Hosmer-Lemeshow - PASSED
gof.results = gof(logit_main_model)
gof.results$gof
# HL chiSq p-value < 0.05 it suggests that our model is inappropriate, Hosmer-Lemeshow test can be incorrect, we employ Osius-Rojek test
# Osius-Rojek p-value=0.95 > 0.05 cannot reject the H0 - the model is appropriate
# DZIA£A


PseudoR2(logit_main_model)
# McFadden 0.6479259        
# McKelvey.Zavoina 0.8619838 if latent variable was observed, our model explains 86% of total its variation
# Count 0.9131805, it means that 91% of observations are correctly predicted  
# Adj.Count 0.7711480, it means that 77% of predictions are correct because of the variation of the variable


stargazer(logit_model, logit_main_model,  type="text")
#######################################
# INTERPRETATION OF THE OUTPUT
# RAW MODEL
summary(logit_main_model)

# Interpretation:
# If Application.mode.15 (International student), then probability of a student being a Graduate increases in comparison to base level (applied in 1st phase)
# If Application.mode.17 (2nd phase - general contingent), then probability of a student being a Graduate decreases in comparison to base level (applied in 1st phase)
# If Application.mode.44 (Technological specialization diploma holders), then probability of a student being a Graduate increases in comparison to base level (applied in 1st phase)
# If Course.2 (Arts/Social studies), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Course.3 (Biology/Nature), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Course.4 (Economics), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Mother.s.qualification.34 (Unknown), then probability of a student being a Graduate decreases in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.qualification.9 (12th Year of Schooling - Not Completed), then probability of a student being a Graduate increases in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.occupation.4 (Administrative staff), then probability of a student being a Graduate decreases in comparison to base level (Student)
# If Father.s.occupation.9 (Unskilled Workers), then probability of a student being a Graduate decreases in comparison to base level (Student)
# If Tuition.fees.up.to.date.1 (If pays on time), then probability of a student being a Graduate increases in comparison to base level (Do not pay in time)
# If Gender.1 (if male), then probability of a student being a Graduate decreases in comparison to base level (is a female)
# If Scholarship.holder.1 (if holds a scholarship), then probability of a student being a Graduate increases in comparison to base level (does not hold scholarship)
# If Curricular.units.1st.sem..credited..zero (Number of curricular units credited in the 1st semester is zero), then probability of a student being a Graduate increases by 29 p.p. in comparison to base level (Number of curricular units credited in the 1st semester is not zero)
# If Curricular.units.1st.sem..enrolled. increases by 1 unit (Number of curricular units enrolled in the 1st semester increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 5.946705)
# If Curricular.units.1st.sem..approved. increases by 1 unit (Number of Curricular.units.1st.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases in comparison to average level (average level: 4.639542)
# If Curricular.units.2nd.sem..enrolled. increases by 1 unit (Number of Curricular.units.2nd.sem..enrolled. increases by 1 unit), then probability of a student being a Graduate decreases comparison to average level (average level: 6.039542)
# If Curricular.units.2nd.sem..evaluations. increases by 1 unit (Number of Curricular.units.2nd.sem..evaluations. increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 7.733238)
# If Curricular.units.2nd.sem..approved. increases by 1 unit (Number of Curricular.units.2nd.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases in comparison to average level (average level: 4.461032)
# If Unemployment.rate. increases by 1 percentage point (Unemployment.rate. increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 11.71544)



# MARGINAL EFFECTS
# Base levels:
# Application.mode
unique(data$Application.mode)
# Levels: 1 2 7 15 17 18 39 43 44 51 53
# 1 - all '1st phases'
# 2 - all 'ordinances' together
# 51 - all 'changes of institutions' together 

# Course
unique(data$Course)
# Levels: 1 2 3 4 5
# 1 - administracyjne
# 2 - artystyczno humanistyczne
# 3 - biologiczno-przyrodnicze
# 4 - ekonomiczne
# 5 - medyczne

# Mother.s.qualification
unique(data$Mother.s.qualification)
# Levels: 1 3 9 34 37 39
# Base lvl: 1 - Secondary Education - 12th Year of Schooling or Eq. (finished)
# 9 - all high school and lower not completed
# 3 - all higher education 
# 39 - all special courses

# Father.s.qualification
unique(data$Father.s.qualification)
# Levels: 1 3 9 34 37 39
# Base lvl: 1 - Secondary Education - 12th Year of Schooling or Eq. (finished)
# 9 - all high school and lower not completed
# 3 - all higher education 
# 39 - all special courses

# Father.s.occupation
unique(data$Father.s.occupation)
# Levels: 0 1 2 3 4 5 6 7 8 9 10
# Base lvl: 0 - student
# 1 - Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers
# 2 - Specialists in Intellectual and Scientific Activities
# 3 - Intermediate Level Technicians and Professions
# 4 - Administrative staff
# 5 - Personal Services, Security and Safety Workers and Sellers
# 6 - Farmers and Skilled Workers in Agriculture, Fisheries and Forestry
# 7 - Skilled Workers in Industry, Construction and Craftsmen
# 8 - Installation and Machine Operators and Assembly Workers
# 9 - Unskilled Workers
# 10 - Armed Forces Professions

# Tuition.fees.up.to.date
unique(data$Tuition.fees.up.to.date)
#Levels: 0 1
# Base lvl: 0
# 1 if up to date, 0 if not

# Gender
unique(data$Gender)
# Levels: 0 1
# Base lvl: 1 - 1 if male, 0 if female

# Scholarship.holder
unique(data$Scholarship.holder)
# Levels: 0 1
# Base lvl: 1 - 1 if holder, 0 if not

# Curricular.units.1st.sem..credited
unique(data$Curricular.units.1st.sem..credited)
# Levels: non_zero zero
# Base lvl: 1 - 1 if zero, 0 if otherwise


# CALCULATING MARGINAL EFFECTS
logitmfx(logit_main_model, data=data)
# Interpretation:
# If Application.mode.15 (International student), then probability of a student being a Graduate increases by 44 p.p. in comparison to base level (applied in 1st phase)
# If Application.mode.17 (2nd phase - general contingent), then probability of a student being a Graduate decreases by 8 p.p. in comparison to base level (applied in 1st phase)
# If Application.mode.44 (Technological specialization diploma holders), then probability of a student being a Graduate increases by 21 p.p. in comparison to base level (applied in 1st phase)
# If Course.2 (Arts/Social studies), then probability of a student being a Graduate decreases by 27 p.p. in comparison to base level (Administration)
# If Course.3 (Biology/Nature), then probability of a student being a Graduate decreases by 26 p.p. in comparison to base level (Administration)
# If Course.4 (Economics), then probability of a student being a Graduate decreases by 24 p.p. in comparison to base level (Administration)
# If Mother.s.qualification.34 (Unknown), then probability of a student being a Graduate decreases by 29 p.p. in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.qualification.9 (12th Year of Schooling - Not Completed), then probability of a student being a Graduate increases by 8 p.p. in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.occupation.4 (Administrative staff), then probability of a student being a Graduate decreases by 12 p.p. in comparison to base level (Student)
# If Father.s.occupation.9 (Unskilled Workers), then probability of a student being a Graduate decreases by 8 p.p. in comparison to base level (Student)
# If Tuition.fees.up.to.date.1 (If pays on time), then probability of a student being a Graduate increases by 52 p.p. in comparison to base level (Do not pay in time)
# If Gender.1 (if male), then probability of a student being a Graduate decreases by 8 p.p. in comparison to base level (is a female)
# If Scholarship.holder.1 (if holds a scholarship), then probability of a student being a Graduate increases by 50 p.p. in comparison to base level (does not hold scholarship)
# If Curricular.units.1st.sem..credited..zero (Number of curricular units credited in the 1st semester is zero), then probability of a student being a Graduate increases by 29 p.p. in comparison to base level (Number of curricular units credited in the 1st semester is not zero)
# If Curricular.units.1st.sem..enrolled. increases by 1 unit (Number of curricular units enrolled in the 1st semester increases by 1 unit), then probability of a student being a Graduate decreases by 13 p.p. in comparison to average level (average level: 5.946705)
# If Curricular.units.1st.sem..approved. increases by 1 unit (Number of Curricular.units.1st.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases by 24 p.p. in comparison to average level (average level: 4.639542)
# If Curricular.units.2nd.sem..enrolled. increases by 1 unit (Number of Curricular.units.2nd.sem..enrolled. increases by 1 unit), then probability of a student being a Graduate decreases by 21 p.p. in comparison to average level (average level: 6.039542)
# If Curricular.units.2nd.sem..evaluations. increases by 1 unit (Number of Curricular.units.2nd.sem..evaluations. increases by 1 unit), then probability of a student being a Graduate decreases by 2 p.p. in comparison to average level (average level: 7.733238)
# If Curricular.units.2nd.sem..approved. increases by 1 unit (Number of Curricular.units.2nd.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases by 33 p.p. in comparison to average level (average level: 4.461032)
# If Unemployment.rate. increases by 1 percentage point (Unemployment.rate. increases by 1 unit), then probability of a student being a Graduate decreases by 2 p.p. in comparison to average level (average level: 11.71544)

# Curricular.units.1st.sem..approved.:Curricular.units.2nd.sem..approved - we cannot interpret interactions
# Scholarship.holder.11:Curricular.units.1st.sem..credited..zero - we cannot interpret interactions





#################################################################
# PROBIT MODEL
#################################################################


# 1st estimation of probit model
probit_model <- glm(formula, data=data, family=binomial(link="probit"))
summary(probit_model)
probit_model$coefficients
summary(probit_model)$coefficients[2,3]

# Joint insignificance of all variables test
null_probit = glm(Target~1, data=data, family=binomial(link="probit"))
lrtest(probit_model, null_probit)
#p-value < 0.05, reject the null hypothesis, at least one of the variable estimator is not equal to zero

# Niby smiga, ale duzo nieistotnych zmiennych

# GENERAL TO SPECIFIC PROCEDURE
# LR-test
# unrest vs. restricted
# H0: te modele sa takie same
# czyli jesli p-value > 0.05, wtedy unrest i restricted mozemy uznac za takie same modele, wiec mozemy zostac przy restricted


# We drop the most insignificat ones (p-value > 0.7)
variables <- colnames(data[ , !(names(data) %in%
                                  c("Target",
                                    "Application.mode.18",
                                    "Application.order.4",
                                    "Application.order.5",
                                    "Previous.qualification.9",
                                    "Mother.s.occupation.6",
                                    "Father.s.occupation.2",
                                    "Father.s.occupation.6",
                                    "Father.s.occupation.10",
                                    "Educational.special.needs.1",
                                    "Age.at.enrollment.20.29"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))


probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables


probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop p-value > 0.5 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop p-value > 0.3 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop p-value > 0.2 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop p-value > 0.1 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop two highest p-values Course.51 0.12783 and Displaced.11 0.10823
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Daytime.evening.attendance..11 0.12258
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.9 0.09042
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.11  0.343015 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.41 0.367924  
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.31 0.406068  
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.3"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.21 0.387066 
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.3",
                                     "Mother.s.occupation.2"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.51 0.405121
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.3",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.5"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.71 0.151254
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.3",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.5",
                                     "Mother.s.occupation.7"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)

# Now, we drop one by one with the highest p-value: Mother.s.occupation.81 0.11482
variables  <- colnames(data[ , !(names(data) %in%
                                   c("Target",
                                     "Application.mode.18",
                                     "Application.order.4",
                                     "Application.order.5",
                                     "Previous.qualification.9",
                                     "Mother.s.occupation.6",
                                     "Father.s.occupation.2",
                                     "Father.s.occupation.6",
                                     "Father.s.occupation.10",
                                     "Educational.special.needs.1",
                                     "Age.at.enrollment.20.29",
                                     "ï.¿Marital.status.2",
                                     "Application.mode.7",
                                     "Application.order.2",
                                     "Previous.qualification..grade.",
                                     "Mother.s.qualification.9",
                                     "Mother.s.qualification.39",
                                     "Mother.s.occupation.10",
                                     "Father.s.occupation.1",
                                     "Curricular.units.1st.sem..grade.",
                                     "Curricular.units.2nd.sem..credited..zero",
                                     "Previous.qualification.39",
                                     "Nacionality.Portuguese",
                                     "Mother.s.qualification.3",
                                     "Father.s.qualification.3",
                                     "Father.s.qualification.37",
                                     "Father.s.occupation.5",
                                     "Age.at.enrollment.30.",
                                     "Curricular.units.1st.sem..without.evaluations.",
                                     "Curricular.units.2nd.sem..without.evaluations.zero",
                                     "ï.¿Marital.status.3",
                                     "Application.mode.43",
                                     "Application.mode.53",
                                     "Application.order.3",
                                     "Application.order.6",
                                     "Mother.s.qualification.37",
                                     "Father.s.occupation.8",
                                     "Admission.grade",
                                     "Application.mode.2",
                                     "Application.mode.39",
                                     "Father.s.qualification.34",
                                     "Father.s.qualification.39",
                                     "Father.s.occupation.3",
                                     "Father.s.occupation.7",
                                     "Curricular.units.1st.sem..evaluations.",
                                     "Inflation.rate",
                                     "GDP",
                                     "Course.5",
                                     "Displaced.1",
                                     "Daytime.evening.attendance..1",
                                     "Mother.s.occupation.9",
                                     "Mother.s.occupation.1",
                                     "Mother.s.occupation.4",
                                     "Mother.s.occupation.3",
                                     "Mother.s.occupation.2",
                                     "Mother.s.occupation.5",
                                     "Mother.s.occupation.7",
                                     "Mother.s.occupation.8"))])
paste(target, paste(variables, collapse=" + "), sep=" ~ ")
formula <- as.formula(paste (target, paste(variables, collapse=" + "), sep=" ~ "))

probit_model_restricted <- glm(formula, data=data, 
                               family=binomial(link="probit"))
lrtest(probit_model, probit_model_restricted)
# p-value > 0.05, we can drop selected variables

probit_model <- probit_model_restricted
summary(probit_model)


# SUCCESS ALL VARIABLES ARE SIGNIFICANT!!!

# FINAL MODEL after dropping insignificant variables
summary(probit_model)

# DIAGNOSTICS

# LINKTEST
source("linktest.R")
linktest_result = linktest(probit_model)
summary(linktest_result)
# yhat = 0 < 0.05 (significant)
# yhat^2 = 0.00745  < 0.05 (significant)


# 1: add Unemployment.rate_2
data$Unemployment.rate_2 <- data$Unemployment.rate^2

probit_model_test <- glm(Target~Application.mode.15+
                           Application.mode.17+
                           Application.mode.44+
                           Application.mode.51+
                           Course.2+
                           Course.3+
                           Course.4+
                           Previous.qualification.3+
                           Mother.s.qualification.34+
                           Father.s.qualification.9+
                           Father.s.occupation.4+
                           Father.s.occupation.9+
                           Debtor.1+
                           Tuition.fees.up.to.date.1+
                           Scholarship.holder.1+
                           Curricular.units.1st.sem..credited..zero+
                           Curricular.units.1st.sem..enrolled.+
                           Curricular.units.1st.sem..approved.+
                           Curricular.units.2nd.sem..enrolled.+
                           Curricular.units.2nd.sem..evaluations.+
                           Curricular.units.2nd.sem..approved.+
                           Curricular.units.2nd.sem..grade.+
                           Unemployment.rate+
                           Unemployment.rate_2,
                         data=data, family=binomial(link="probit"))
summary(probit_model_test)
linktest_result = linktest(probit_model_test)


# 2: remove debtor
data$Curricular.units.2nd.sem..grade._2 <- data$Curricular.units.2nd.sem..grade.^2

probit_model_test2 <- glm(Target~Application.mode.15+
                            Application.mode.17+
                            Application.mode.44+
                            Application.mode.51+
                            Course.2+
                            Course.3+
                            Course.4+
                            Previous.qualification.3+
                            Mother.s.qualification.34+
                            Father.s.qualification.9+
                            Father.s.occupation.4+
                            Father.s.occupation.9+
                            Tuition.fees.up.to.date.1+
                            Scholarship.holder.1+
                            Curricular.units.1st.sem..credited..zero+
                            Curricular.units.1st.sem..enrolled.+
                            Curricular.units.1st.sem..approved.+
                            Curricular.units.2nd.sem..enrolled.+
                            Curricular.units.2nd.sem..evaluations.+
                            Curricular.units.2nd.sem..approved.+
                            Curricular.units.2nd.sem..grade.+
                            Unemployment.rate+
                            Unemployment.rate_2,
                          data=data, family=binomial(link="probit"))
summary(probit_model_test2)
linktest_result = linktest(probit_model_test2)


# 3: remove Application.mode.51

probit_model_test3 <- glm(Target~Application.mode.15+
                            Application.mode.17+
                            Application.mode.44+
                            Course.2+
                            Course.3+
                            Course.4+
                            Previous.qualification.3+
                            Mother.s.qualification.34+
                            Father.s.qualification.9+
                            Father.s.occupation.4+
                            Father.s.occupation.9+
                            Tuition.fees.up.to.date.1+
                            Scholarship.holder.1+
                            Curricular.units.1st.sem..credited..zero+
                            Curricular.units.1st.sem..enrolled.+
                            Curricular.units.1st.sem..approved.+
                            Curricular.units.2nd.sem..enrolled.+
                            Curricular.units.2nd.sem..evaluations.+
                            Curricular.units.2nd.sem..approved.+
                            Curricular.units.2nd.sem..grade.+
                            Unemployment.rate+
                            Unemployment.rate_2,
                          data=data, family=binomial(link="probit"))
summary(probit_model_test3)
linktest_result = linktest(probit_model_test3)


# 4: remove Application.mode.17

probit_model_test4 <- glm(Target~Application.mode.15+
                            Application.mode.44+
                            Course.2+
                            Course.3+
                            Course.4+
                            Previous.qualification.3+
                            Mother.s.qualification.34+
                            Father.s.qualification.9+
                            Father.s.occupation.4+
                            Father.s.occupation.9+
                            Tuition.fees.up.to.date.1+
                            Scholarship.holder.1+
                            Curricular.units.1st.sem..credited..zero+
                            Curricular.units.1st.sem..enrolled.+
                            Curricular.units.1st.sem..approved.+
                            Curricular.units.2nd.sem..enrolled.+
                            Curricular.units.2nd.sem..evaluations.+
                            Curricular.units.2nd.sem..approved.+
                            Curricular.units.2nd.sem..grade.+
                            Unemployment.rate+
                            Unemployment.rate_2,
                          data=data, family=binomial(link="probit"))
summary(probit_model_test4)
linktest_result = linktest(probit_model_test4)


# 5: add Curricular.units.1st.sem..credited..zero*Scholarship.holder.1

probit_model_test5 <- glm(Target~Application.mode.15+
                            Application.mode.44+
                            Course.2+
                            Course.3+
                            Course.4+
                            Previous.qualification.3+
                            Mother.s.qualification.34+
                            Father.s.qualification.9+
                            Father.s.occupation.4+
                            Father.s.occupation.9+
                            Tuition.fees.up.to.date.1+
                            Scholarship.holder.1+
                            Curricular.units.1st.sem..credited..zero+
                            Curricular.units.1st.sem..enrolled.+
                            Curricular.units.1st.sem..approved.+
                            Curricular.units.2nd.sem..enrolled.+
                            Curricular.units.2nd.sem..evaluations.+
                            Curricular.units.2nd.sem..approved.+
                            Curricular.units.2nd.sem..grade.+
                            Unemployment.rate+
                            Unemployment.rate_2+
                            Curricular.units.1st.sem..credited..zero*Scholarship.holder.1,
                          data=data, family=binomial(link="probit"))
summary(probit_model_test5)
linktest_result = linktest(probit_model_test5)

# 6: remove Previous.qualification.3

probit_model_test6 <- glm(Target~Application.mode.15+
                            Application.mode.44+
                            Course.2+
                            Course.3+
                            Course.4+
                            Mother.s.qualification.34+
                            Father.s.qualification.9+
                            Father.s.occupation.4+
                            Father.s.occupation.9+
                            Tuition.fees.up.to.date.1+
                            Scholarship.holder.1+
                            Curricular.units.1st.sem..credited..zero+
                            Curricular.units.1st.sem..enrolled.+
                            Curricular.units.1st.sem..approved.+
                            Curricular.units.2nd.sem..enrolled.+
                            Curricular.units.2nd.sem..evaluations.+
                            Curricular.units.2nd.sem..approved.+
                            Curricular.units.2nd.sem..grade.+
                            Unemployment.rate+
                            Unemployment.rate_2+
                            Curricular.units.1st.sem..credited..zero*Scholarship.holder.1,
                          data=data, family=binomial(link="probit"))
summary(probit_model_test6)
linktest_result = linktest(probit_model_test6)

probit_main_model <- probit_model_test6

# LINKTEST - PASSED
linktest_result = linktest(probit_main_model)
summary(linktest_result)
# yhat = 0 < 0.05 (significant)
# yhat^2 = 0.0556 > 0.05 (insignificant)

# LIKELIHOOD test - PASSED
lrtest(probit_main_model, null_probit)
# p-value = 0
# We reject the H0

# Hosmer-Lemeshow - PASSED
gof.results = gof(probit_main_model)
gof.results$gof
# HL chiSq p-value < 0.05 it suggests that our model is inappropriate, Hosmer-Lemeshow test can be incorrect, we employ Osius-Rojek test
# Osius-Rojek p-value=0.99 > 0.05 cannot reject the H0 - the model is appropriate


PseudoR2(probit_main_model)
# McFadden 0.6426915                
# McKelvey.Zavoina 0.8485157 if dropout, our model explains 85% of total its variation
# Count 0.9126074, it means that 91% of observations are correctly predicted  
# Adj.Count 0.7696375, it means that 77% of predictions are correct because of the variation of the variable



#######################################
# INTERPRETATION OF THE OUTPUT
# RAW MODEL
summary(probit_main_model)

# Interpretation:
# If Application.mode.15 (International student), then probability of a student being a Graduate increases in comparison to base level (applied in 1st phase)
# If Application.mode.44 (Technological specialization diploma holders), then probability of a student being a Graduate increases in comparison to base level (applied in 1st phase)
# If Course.2 (Arts/Social studies), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Course.3 (Biology/Nature), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Course.4 (Economics), then probability of a student being a Graduate decreases in comparison to base level (Administration)
# If Mother.s.qualification.34 (Unknown), then probability of a student being a Graduate decreases in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.qualification.9 (12th Year of Schooling - Not Completed), then probability of a student being a Graduate increases in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.occupation.4 (Administrative staff), then probability of a student being a Graduate decreases in comparison to base level (Student)
# If Father.s.occupation.9 (Unskilled Workers), then probability of a student being a Graduate decreases in comparison to base level (Student)
# If Tuition.fees.up.to.date.1 (If pays on time), then probability of a student being a Graduate increases in comparison to base level (Do not pay in time)
# If Scholarship.holder.1 (if holds a scholarship), then probability of a student being a Graduate increases in comparison to base level (does not hold scholarship)
# If Curricular.units.1st.sem..credited..zero (Number of curricular units credited in the 1st semester is zero), then probability of a student being a Graduate increases in comparison to base level (Number of curricular units credited in the 1st semester is not zero)
# If Curricular.units.1st.sem..enrolled. increases by 1 unit (Number of curricular units enrolled in the 1st semester increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 5.946705)
# If Curricular.units.1st.sem..approved. increases by 1 unit (Number of Curricular.units.1st.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases in comparison to average level (average level: 4.639542)
# If Curricular.units.2nd.sem..enrolled. increases by 1 unit (Number of Curricular.units.2nd.sem..enrolled. increases by 1 unit), then probability of a student being a Graduate decreases comparison to average level (average level: 6.039542)
# If Curricular.units.2nd.sem..evaluations. increases by 1 unit (Number of Curricular.units.2nd.sem..evaluations. increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 7.733238)
# If Curricular.units.2nd.sem..approved. increases by 1 unit (Number of Curricular.units.2nd.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases in comparison to average level (average level: 4.461032)
# If Curricular.units.2nd.sem..grade. increases by 1 unit (Number of Curricular.units.2nd.sem..grade. increases by 1 unit), then probability of a student being a Graduate increases in comparison to average level (average level: 4.461032)
# If Unemployment.rate. increases by 1 percentage point (Unemployment.rate. increases by 1 unit), then probability of a student being a Graduate decreases in comparison to average level (average level: 10.15163)

# Scholarship.holder.11:Curricular.units.1st.sem..credited..zero - we cannot interpret interactions

# MARGINAL EFFECTS
# Base levels:
# Application.mode
unique(data$Application.mode)
# Levels: 1 2 7 15 17 18 39 43 44 51 53
# 1 - all '1st phases'
# 2 - all 'ordinances' together
# 51 - all 'changes of institutions' together 

# Course
unique(data$Course)
# Levels: 1 2 3 4 5
# 1 - administracyjne
# 2 - artystyczno humanistyczne
# 3 - biologiczno-przyrodnicze
# 4 - ekonomiczne
# 5 - medyczne

# Mother.s.qualification
unique(data$Mother.s.qualification)
# Levels: 1 3 9 34 37 39
# Base lvl: 1 - Secondary Education - 12th Year of Schooling or Eq. (finished)
# 9 - all high school and lower not completed
# 3 - all higher education 
# 39 - all special courses

# Father.s.qualification
unique(data$Father.s.qualification)
# Levels: 1 3 9 34 37 39
# Base lvl: 1 - Secondary Education - 12th Year of Schooling or Eq. (finished)
# 9 - all high school and lower not completed
# 3 - all higher education 
# 39 - all special courses

# Father.s.occupation
unique(data$Father.s.occupation)
# Levels: 0 1 2 3 4 5 6 7 8 9 10
# Base lvl: 0 - student
# 1 - Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers
# 2 - Specialists in Intellectual and Scientific Activities
# 3 - Intermediate Level Technicians and Professions
# 4 - Administrative staff
# 5 - Personal Services, Security and Safety Workers and Sellers
# 6 - Farmers and Skilled Workers in Agriculture, Fisheries and Forestry
# 7 - Skilled Workers in Industry, Construction and Craftsmen
# 8 - Installation and Machine Operators and Assembly Workers
# 9 - Unskilled Workers
# 10 - Armed Forces Professions

# Tuition.fees.up.to.date
unique(data$Tuition.fees.up.to.date)
#Levels: 0 1
# Base lvl: 0
# 1 if up to date, 0 if not

# Gender
unique(data$Gender)
# Levels: 0 1
# Base lvl: 1 - 1 if male, 0 if female

# Scholarship.holder
unique(data$Scholarship.holder)
# Levels: 0 1
# Base lvl: 1 - 1 if holder, 0 if not

# Curricular.units.1st.sem..credited
unique(data$Curricular.units.1st.sem..credited)
# Levels: non_zero zero
# Base lvl: 1 - 1 if zero, 0 if otherwise


# CALCULATING MARGINAL EFFECTS
logitmfx(probit_main_model, data=data)
# Interpretation:
# If Application.mode.15 (International student), then probability of a student being a Graduate increases by 43 p.p. in comparison to base level (applied in 1st phase)
# If Application.mode.44 (Technological specialization diploma holders), then probability of a student being a Graduate increases by 25 p.p. in comparison to base level (applied in 1st phase)
# If Course.2 (Arts/Social studies), then probability of a student being a Graduate decreases 28 p.p. in comparison to base level (Administration)
# If Course.3 (Biology/Nature), then probability of a student being a Graduate decreases 31 p.p. in comparison to base level (Administration)
# If Course.4 (Economics), then probability of a student being a Graduate decreases by 26 p.p. in comparison to base level (Administration)
# If Mother.s.qualification.34 (Unknown), then probability of a student being a Graduate decreases by 31 p.p. in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.qualification.9 (12th Year of Schooling - Not Completed), then probability of a student being a Graduate increases by 8 p.p. in comparison to base level (Secondary Education - 12th Year of Schooling or Eq. (finished))
# If Father.s.occupation.4 (Administrative staff), then probability of a student being a Graduate decreases by 11 p.p. in comparison to base level (Student)
# If Father.s.occupation.9 (Unskilled Workers), then probability of a student being a Graduate decreases by 7 p.p. in comparison to base level (Student)
# If Tuition.fees.up.to.date.1 (If pays on time), then probability of a student being a Graduate increases by 54 p.p. in comparison to base level (Do not pay in time)
# If Scholarship.holder.1 (if holds a scholarship), then probability of a student being a Graduate increases by 52 p.p. in comparison to base level (does not hold scholarship)
# If Curricular.units.1st.sem..credited..zero (Number of curricular units credited in the 1st semester is zero), then probability of a student being a Graduate increases by 34 p.p. in comparison to base level (Number of curricular units credited in the 1st semester is not zero)
# If Curricular.units.1st.sem..enrolled. increases by 1 unit (Number of curricular units enrolled in the 1st semester increases by 1 unit), then probability of a student being a Graduate decreases by 10 p.p. in comparison to average level (average level: 5.946705)
# If Curricular.units.1st.sem..approved. increases by 1 unit (Number of Curricular.units.1st.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases by 15 p.p. in comparison to average level (average level: 4.639542)
# If Curricular.units.2nd.sem..enrolled. increases by 1 unit (Number of Curricular.units.2nd.sem..enrolled. increases by 1 unit), then probability of a student being a Graduate decreases by 21 p.p. comparison to average level (average level: 6.039542)
# If Curricular.units.2nd.sem..evaluations. increases by 1 unit (Number of Curricular.units.2nd.sem..evaluations. increases by 1 unit), then probability of a student being a Graduate decreases by 3 p.p. in comparison to average level (average level: 7.733238)
# If Curricular.units.2nd.sem..approved. increases by 1 unit (Number of Curricular.units.2nd.sem..approved. increases by 1 unit), then probability of a student being a Graduate increases by 22 p.p. in comparison to average level (average level: 4.461032)
# If Curricular.units.2nd.sem..grade. increases by 1 unit (Number of Curricular.units.2nd.sem..grade. increases by 1 unit), then probability of a student being a Graduate increases by 4 p.p. in comparison to average level (average level: 4.461032)
# If Unemployment.rate. increases by 1 percentage point (Unemployment.rate. increases by 1 unit), then probability of a student being a Graduate increases by 2 p.p. (=-0.1254605+2*0.0045632*11.71544) in comparison to average level (average level: 11.71544)

# Scholarship.holder.11:Curricular.units.1st.sem..credited..zero - we cannot interpret interactions

stargazer(probit_main_model, logit_main_model, type="text")

