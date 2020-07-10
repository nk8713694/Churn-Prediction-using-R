# Get and print current working directory.
print(getwd())
data <- read.csv("DataSet.csv")
print(data)
dim(data)
summary(data)
table(data$Churn)
W <- read.csv("Churn_DataSet.xlsx")
print(W)
data <- read.xlsx("Churn_DataSet.xlsx", sheetIndex = 1)
print(data)
data <- read.xlsx("Input.xlsx", sheetIndex = 1)
print(data)
             # STEP1: READ AND DISPLAY DATA SET
data <- read.csv("Telco-Customer-Churn.csv")
print(data)
dim(data)
             # STEP2: BASIC STUDY OF DATA SET USING BAR PLOT AND MEAN MEDIAN MODE ANALYSIS
#Q1-> HOW MANY CHRUN HAPPENED

U<-table(data$Churn)
barplot(U,xlab="Churn Happened?", ylab="Count",col="blue", main="Churn Count")
table(data$Churn)

#Q2-> HOW MANY TOTAL MALE AND FEMALE CUSOMERS ARE THERE

B<-table(data$gender)
barplot(B, xlab="Gender", ylab="Count",col="green",main="Gender Count")
table(data$gender)

# Q3-> TOTAL SENIOR CITIZEN CUSTOMERS

C<- table(data$SeniorCitizen)
barplot(C,xlab="Senior Citizen?", ylab="Count",main="Senior Citizen Count",col="red")
table(data$SeniorCitizen)

#Q4-> HOW MANY OF THEM ARE PARTERS

D<- table(data$Partner)
barplot(D,xlab="Partners?", ylab="Count",main="Partner Count",col="red")
table(data$Partner)

#Q5-> HOW MANY OF THEM ARE DEPENDENT

E<- table(data$Dependents)
barplot(E,xlab="Dependents?", ylab="Count",main="Dependency Count",col="red")
table(data$Dependents)

#Q6-> HOW MANY ARE USING TELECOM SERVICE FOR MOBILE PHONES

G<- table(data$PhoneService)
barplot(G,xlab="Using OhoneService?", ylab="Count",main="Count On Using Phone Service Facility",col="red")
table(data$PhoneService)


#Q7-> HOW MANY OF THEM ARE USING MULTIPLE CONNECTIONS

H<- table(data$MultipleLines)
barplot(H,xlab="Multiple Connections?", ylab="Count",main="Count On Multiple Lines Users",col="red")
table(data$MultipleLines)


#Q8-> COUNT OF DIFFERENT TYPES OF INTERNET SERVICES THEY HAVE USED

I<- table(data$InternetService)
barplot(I,xlab="DIFFERENT INTERNET SERVICES", ylab="Count",main="Diffrent Types of Internet Users",col="red")
table(data$InternetService)


#Q9-> HOW MANY ARE HAVING ONLINE SECURITY 

J<- table(data$OnlineSecurity)
barplot(J,xlab="Having Online Security?", ylab="Count",main="User Online Security Analysis",col="red")
table(data$OnlineSecurity)


#Q10-> HOW MANY ARE HAVING ONLINE BACKUP


K<- table(data$OnlineBackup)
barplot(K,xlab="Having Online Backup?", ylab="Count",main="User's Online Backup Analysis",col="red")
table(data$OnlineBackup)


#Q11-> HOW MANY ARE HAVING DEVICE PROTECTION

L<- table(data$DeviceProtection)
barplot(L,xlab="Having Device Protection?", ylab="Count",main="User's Device Protection Analysis",col="red")
table(data$DeviceProtection)

#Q12-> HOW MANY ARE HAVING TECHNICAL SUUPORT

M<- table(data$TechSupport)
barplot(M,xlab="Having Technical Support?", ylab="Count",main="User's Technical Support Analysis",col="red")
table(data$TechSupport)

#Q13-> HOW MANY ARE STREAMING TELEVISION

N<- table(data$StreamingTV)
barplot(N,xlab="Streaming Television?", ylab="Count",main="User's Streaming Tevelesion Analysis",col="red")
table(data$StreamingTV)

#Q14-> HOW MANY ARE STREAMING MOVIES

O<- table(data$StreamingMovies)
barplot(O,xlab="Streaming Movies?", ylab="Count",main="User's Streaming Movies Analysis",col="red")
table(data$StreamingMovies)


#Q15-> WHAT ARE DIFFERENT CONTRACT PERIODS

P<- table(data$Contract)
barplot(P,xlab="Contract Periods", ylab="Count",main="Users Period Of Contract",col="red")
table(data$Contract)

#Q16-> HOW MANY ARE USING PAPERLESS BILLING SCHEMES

Q<- table(data$PaperlessBilling)
barplot(Q,xlab="Having Paperless Billing Option?", ylab="Count",main="Paperless Billing Option Analysis",col="red")
table(data$PaperlessBilling)

#Q17-> COUNT ON THE DIFFERENT BILLING MODES USED BY THEM

R<- table(data$PaymentMethod)
barplot(R,xlab="Different Payment Methods", ylab="Count",main="User's Payment Method Analysis",col="red")
table(data$PaymentMethod)

#Q18-> [MEAN,MEDIAN,MODE]ANALYSIS OF TENURES

F1<-mean(data$tenure)
print(F1)

F2<-median(data$tenure)
print(F2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

F3<- getmode(data$tenure)
print(F3)

F4<-c(F1,F2,F3)
barplot(F4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Tenure In Months", col="blue")

#Q19-> [MEAN,MEDIAN,MODE]ANALYSIS MONTHLY CHARGING
S1<-mean(data$MonthlyCharges)
print(S1)

S2<-median(data$MonthlyCharges)
print(S2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

S3<- getmode(data$MonthlyCharges)
print(S3)

S4<-c(S1,S2,S3)
barplot(S4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Monthly Charges", col="blue")



#Q20-> [MEAN,MEDIAN,MODE]ANALYSIS OF TOTAL CHARGING

T1<-mean(data$TotalCharges,na.rm=TRUE)
print(T1)

T2<-median(data$TotalCharges,na.rm=TRUE)
print(T2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

T3<- getmode(data$TotalCharges)
print(T3)

T4<-c(T1,T2,T3)
barplot(T4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Total Charges", col="blue")


              # STEP3: ANALYSIS OF VARIOUS PROPERTIES OF A CUSTOMER HAVING CHURN 

#Q21-> WHAT IS THE TOTAL COUNT
install.packages("dplyr")

#TOTAL CHURN COUNT
library(dplyr)
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
t<-select(temp,MonthlyCharges)
print(t)

#Q21-> HOW MANY FEMALE CUSTOMER HAS A CHURN 

#TOTAL FEMALE CHURN COUNT
info<-subset(data,gender =="Female" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT FEMALE
a<-(fc2/fc1)*100
print(a)
#TOTAL MALE CHURN COUNT
info<-subset(data,gender =="Male" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT FEMALE
a<-(fc3/fc1)*100
print(a)
H<-c(fc1,fc2,fc3)
M<-c("ALL","FEMALE","MALE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Vs. Male Churn Analysis",col="blue")

#CONCLUSION: 50.24% OF THEM ARE FEMALE

#Q23-> HOW MANY YOUNG CITIZEN HAS CHURN 

info<-subset(data,SeniorCitizen==0 & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,SeniorCitizen==1 & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100


H<-c(fc1,fc2,fc3)
M<-c("ALL","NOT SENIOR CITIZEN","SENIOR CITIZEN")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Young Citizen Churn",col="blue")

# CONCLUSION: 74.53% OF THEM ARE NOT SENIOR CITIZENS

#Q24-> HOW MANY DEPENDENTS HAS A CHURN

info<-subset(data,Dependents=="No" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,Dependents=="Yes" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

H<-c(fc1,fc2,fc3)
M<-c("ALL","INDEPENDENTS","DEPENDENTS")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Independents Churn",col="blue")

# CONCLUSION: 82.55% OF THEM ARE INDEPENDENTS

#Q25-> HOW MANY OF THEM ARE USING PHONE SERVICE

info<-subset(data,PhoneService=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,PhoneService=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100


H<-c(fc1,fc2,fc3)
M<-c("ALL","USING PHONE SERVICE","NOT USING PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")


# CONCLUSION: 90% OF THEM ARE USING PHONE SERVICE

#Q26-> HOW MANY OF THEM ARE USING MULTIPLE LINES

info<-subset(data,MultipleLines=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,MultipleLines=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100

info<-subset(data,MultipleLines=="No phone service" & Churn=="Yes")
print(info)
fc4<-nrow(info)
print(fc4)

# PERCENTAGE OF CHURN COUNT
a<-(fc4/fc1)*100

H<-c(fc1,fc2,fc3,fc4)
M<-c("ALL","MULTIPLE LINES","SINGLE LINE","NO SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 45.47%(No=45.42%,No Service=9.095%) OF THEM ARE USING MULTIPLE SERVICE

#Q27-> MOST INTERNET SERVICE HAVING CHURN

info<-subset(data,InternetService=="DSL" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,InternetService=="Fiber optic"& Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)
info<-subset(data,InternetService=="No"& Churn=="Yes")
print(info)
fc4<-nrow(info)
print(fc4)

# PERCENTAGE OF CHURN COUNT
a<-(fc4/fc1)*100

H<-c(fc1,fc2,fc3,fc4)
M<-c("ALL","DSL","FIBER OPTIC","NO SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 69.39% OF THEM ARE USING FIBER OPTIC LINES

#Q28-> HOW MANY OF THEM HAVING ONLINE SECURITY

info<-subset(data,OnlineSecurity=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,OnlineSecurity=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","SECURITY","NO SECURITY")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 78.17% DOES NOT HAVE ONLINE SECURITY

#Q29-> HOW MANY OF THEM HAVING ONLINE BACKUP

info<-subset(data,OnlineBackup=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,OnlineBackup=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","BACKUP","NO BACKUP")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 65.97% DOES NOT HAVE ONLINE BACKUP

#Q30-> HOW MANY OF THEM ARE HAVING DEVICE PROTECTION

info<-subset(data,DeviceProtection=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,DeviceProtection=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","PROTECTION","NO PROTECTION")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 64.79% DOES NOT HAVE DEVICE PROTECTION

#Q31-> HOW MANY OF THEM ARE HAVING TECHNICAL SUPPORT

info<-subset(data,TechSupport=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,TechSupport=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","TECHSUPPORT","NO SUPPORT")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 77.36% DOES NOT HAVE TECHNICAL SUPPORT

#Q32-> HOW MANY OF THEM STREAMING TV

info<-subset(data,StreamingTV=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,StreamingTV=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","STREAMING TV","NOT STREAMING TV")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 50.40% ARE NOT STREAMING TV

#Q33-> HOW MANY OF THEM ARE STREAMING MOVIE

info<-subset(data,StreamingMovie=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100

info<-subset(data,StreamingMovie=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","STREAMING MOVIE","NOT STREAMING MOVIE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 50.40% OF THEM ARE NOT STREAMING MOVIE

#Q34-> HOW MANY OF THEM ARE USING PAPERLESSBILLING

info<-subset(data,PaperlessBilling=="Yes" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

info<-subset(data,PaperlessBilling=="No" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)

H<-c(fc1,fc2,fc3)
M<-c("ALL","PAPERLESS BILLING","PAPER BILLING")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 74.90% OF THEM ARE USING PAPERLESS BILLING SCHEME


#Q35-> MOST ACCUTE PAYMENT METHOD

info<-subset(data,PaymentMethod=="Electronic check" & Churn=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)
# CONCLUSION: 45.47 OF THEM ARE USING electronic check

info<-subset(data,PaymentMethod=="Mailed check" & Churn=="Yes")
print(info)
fc3<-nrow(info)
print(fc3)

# PERCENTAGE OF CHURN COUNT
a<-(fc3/fc1)*100
print(a)
# CONCLUSION: 16.47 OF THEM ARE USING electronic check

info<-subset(data,PaymentMethod=="Bank transfer (automatic)" & Churn=="Yes")
print(info)
fc4<-nrow(info)
print(fc4)

# PERCENTAGE OF CHURN COUNT
a<-(fc4/fc1)*100
print(a)
info<-subset(data,PaymentMethod=="Bank transfer (automatic)" & Churn=="Yes")
print(info)
fc4<-nrow(info)
print(fc4)
# CONCLUSION: 13.80 OF THEM ARE USING electronic check

# PERCENTAGE OF CHURN COUNT
a<-(fc4/fc1)*100
print(a)

info<-subset(data,PaymentMethod=="Credit card (automatic)" & Churn=="Yes")
print(info)
fc5<-nrow(info)
print(fc5)

# PERCENTAGE OF CHURN COUN
a<-(fc5/fc1)*100
print(a)
# CONCLUSION: 12.41 OF THEM ARE USING credit card check


H<-c(fc1,fc2,fc3,fc4,fc5)
M<-c("ALL","ELECTRONIC CHECK","MAILED CHECK","BANK TRANSFER","CREDIT CARD")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Total Churn Vs. Female Churn Analysis",col="blue")

# CONCLUSION: 57.30% OF THE PAYMENT METHOD IS ELECTRONIC TRANSFER

#Q36-> MEAN MEDIAN MODE VALUE OF THE MONTHLY CHARGERS CAUSES CHURN

library(dplyr)  # LIBRARY FOR SELECT QUERY

info<-subset(data,Churn=="Yes")
t<-select(info,MonthlyCharges)

S1<-mean(t$MonthlyCharges)
print(S1)

S2<-median(t$MonthlyCharges)
print(S2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

S3<-getmode(t$MonthlyCharges)
print(S3)

S4<-c(S1,S2,S3)
barplot(S4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Monthly Charges", col="blue")

#Q37-> MEAN MEDIAM MODE VALUE OF THE TOTAL CHARGERS CAUSES CHURN

library(dplyr)  # LIBRARY FOR SELECT QUERY

info<-subset(data,Churn=="Yes")
t<-select(info,TotalCharges)

S1<-mean(t$TotalCharges)
print(S1)

S2<-median(t$TotalCharges)
print(S2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

S3<-getmode(t$TotalCharges)
print(S3)

S4<-c(S1,S2,S3)
barplot(S4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Total Charges", col="blue")

#Q38-> MEAN MEDIAM MODE VALUE OF THE TENURE CAUSING CHURN

library(dplyr)  # LIBRARY FOR SELECT QUERY

info<-subset(data,Churn=="Yes")
t<-select(info,tenure)

S1<-mean(t$tenure)
print(S1)

S2<-median(t$tenure)
print(S2)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

S3<-getmode(t$tenure)
print(S3)

S4<-c(S1,S2,S3)
barplot(S4, xlab="Mean-Median-Mode values", ylab="Count",main="Statistical Analysis Of Tenure", col="blue")

#Q39-> ORDER OF MOST EFFECTIVE FACTORS CAUSING CHURN 

S1<- c(50.24,74.53,82.55,90,45.47,69.39,78.17,65.97,64.79,77.36,50.40,50.40,74.90,57.30)
S2<- c("FEMALE","YOUTH","INDEPENDENT","PHONE SERVICE","MULTIPLE LINES","FIBER OPTIC",
"NO ONLINE SECURITY","NO BACKUP","NO DEVICE PROTECTION","NO TECHNICAL SUPPORT" 
"NOT STREAMING TV","NOT STREAMING MV","PAPERLESSBILLING","ELECTRONIC TRANSFER")

S3<- c("PS","INDP","NOS","NOT","PB", 
       "YO","FO","NB","NDP","ET","NSV",
       "NSMV","FM","ML")
print(S1)
m<- sort(S1,decreasing=TRUE)
print(m)

barplot(m,names.arg=S3,xlab="OVERALL CHURN", ylab="Count",main="Descending order of Churn Percentages",col="blue")

#Q40-> INDEPENDENT CUSTOMERS USING PHONE SERVICE CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Dependents=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)
#conclusion :74.69 using phn service & independent
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Independent Phoneservice Users",col="blue")

#Q41-> CUSTOMER USING PHONE SERVICE BUT NOT HAVING ONLINE SECURITY CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & OnlineSecurity=="No")
print(info)
fc2<-nrow(info)
print(fc2)
#conclusion :70.49 using phn service & independent
# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

H<-c(fc1,fc2)
M<-c("ALL","NO SECURITY+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Without Online Security",col="blue")


#Q42-> CUSTOMER USING PHONE SERVICE BUT NOT HAVING tech support CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & TechSupport=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion 69.76

H<-c(fc1,fc2)
M<-c("ALL","PHONE SERVICE+ NO TECH SUPPORT")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Without Technical Support",col="blue")

#Q43-> CUSTOMER USING PHONE SERVICE HAVING PAPERLESSBILLING CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & PaperlessBilling=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 68.592

H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILLING+ PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users With PaperlessBilling",col="blue")

#Q44-> CUSTOMER USING PHONE SERVICE AND ARE NOT SENIORCITIZEN CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & SeniorCitizen==0)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 67.79

H<-c(fc1,fc2)
M<-c("ALL","YOUNG + PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Young Phoneservice Users",col="blue")


#Q45-> CUSTOMER USING PHONE SERVICE AND USING Contract CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 80.36


H<-c(fc1,fc2)
M<-c("ALL","FIBER OPTIC + PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users With Fiber Optic Internet Service",col="blue")

#Q47-> CUSTOMER USING PHONE SERVICE WITHOUT ONLINE BACKUP CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & OnlineBackup=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 59.604

H<-c(fc1,fc2)
M<-c("ALL","NO BACKUP+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Without Online Backup",col="blue")

#Q48-> CUSTOMER USING PHONE SERVICE WITHOUT DEVICE PROTECTION CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & DeviceProtection=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 58.31

H<-c(fc1,fc2)
M<-c("ALL","NO DEVICE PROTECTION+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Without Device Protection",col="blue")

#Q49-> CUSTOMER USING PHONE SERVICE WITH PAYMENT METHOD ELECTRONIC TRANSFER CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & PaymentMethod=="Electronic check")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 52.4

H<-c(fc1,fc2)
M<-c("ALL","ELECTRONIC CHEQ+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users With Electronic Cheque",col="blue")

#Q50-> CUSTOMER USING PHONE SERVICE NOT STREAMING MOVIE OR TV

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & StreamingTV=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 44.72


H<-c(fc1,fc2)
M<-c("ALL","NOTSTREAMINGMV+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Neither Streaming TV or MV",col="blue")

#Q51-> FEMALE PHONE SERVICE USERS CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & gender=="Female")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

H<-c(fc1,fc2)
M<-c("ALL","FEMALE+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Female Phoneservice Users",col="blue")

#Q52-> PHONE SERVICE USERS USING MULTIPLE LINES CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & MultipleLines=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 45.47


H<-c(fc1,fc2)
M<-c("ALL","MULTIPLE LINES+PHONE SERVICE")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Phoneservice Users Using Multiple Lines",col="blue")

******************************************************************************************************

#Q53-> CUSTOMER USING PHONE SERVICE NOT STREAMING MOVIE

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & StreamingMovies=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

*******************************************************************************************************
54 #Q53-> CUSTOMER USING PHONE SERVICE and partner neeeded
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Partner=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 58.37
*******************************************************************************************************

55-> CUSTOMER USING PHONE SERVICE and tenure
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 81.59

*******************************************************************************************************

56-> CUSTOMER USING PHONE SERVICE and MonthlyCharges
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & MonthlyCharges<=100)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 77.36

*******************************************************************************************************

57-> CUSTOMER USING PHONE SERVICE and TotalCharges
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & TotalCharges<=1500)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 58.855

*******************************************************************************************************
3rd level :phone service and tenure mila uppar se

*******************************************************************************************************

#Q40-> INDEPENDENT CUSTOMERS USING PHONE SERVICE and tenure < =50 and  CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Dependents=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)
#conclusion :67.46


*******************************************************************************************************
-> CUSTOMER USING PHONE SERVICE BUT NOT HAVING ONLINE SECURITY  and tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & OnlineSecurity=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)
#conclusion :70.49 using phn service & independent
# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion:65.008

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE BUT NOT HAVING tech support   and tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & TechSupport=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion 63.99144

******************************************************************************************************

#Q43-> CUSTOMER USING PHONE SERVICE HAVING PAPERLESSBILLING and & tenure<=50 CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & PaperlessBilling=="Yes" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 61.36972

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE AND ARE NOT SENIORCITIZEN and & tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & SeniorCitizen==0 & tenure<=50 )
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 61.04869

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE AND USING Contract and tenure<=50 CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 76.99304

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE WITHOUT ONLINE BACKUP  and tenure<=50 CAUING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & OnlineBackup=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 56.66132

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE WITHOUT DEVICE PROTECTION and tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & DeviceProtection=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 55.21669

******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE WITH PAYMENT METHOD ELECTRONIC TRANSFER  and tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & PaymentMethod=="Electronic check" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 47.83

******************************************************************************************************
-> CUSTOMER USING PHONE SERVICE NOT STREAMING TV  & tenure<=50

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & StreamingTV=="No"  & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion :  43.07116


******************************************************************************************************
-> FEMALE PHONE SERVICE USERS and tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & gender=="Female" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 42.16158

******************************************************************************************************

-> PHONE SERVICE USERS USING MULTIPLE LINES & tenure<=50 CAUSING CHURN

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & MultipleLines=="Yes" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 37.6672


******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE NOT STREAMING MOVIE and & tenure<=50

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & StreamingMovies=="No" & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 43.23167

*******************************************************************************************************
-> CUSTOMER USING PHONE SERVICE and partner neeeded and & tenure<=50
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Partner=="No" & tenure<=50 )
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 55.37721
*******************************************************************************************************


-> CUSTOMER USING PHONE SERVICE and MonthlyCharges and tenure<=50
info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & MonthlyCharges<=100  & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 73.19422

*******************************************************************************************************

-> CUSTOMER USING PHONE SERVICE and TotalCharges and tenure<=50
info<-subset(data,Churn=="Yes") 
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & TotalCharges<=1500 & tenure<=50)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion: 58.53398


*******************************************************************************************************

4 th level me phone service tenure contract(month to month) ....................

********************************************************************************************************

Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50  & TotalCharges

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & TotalCharges<=1500)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 57.3033
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+TotalCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

**************************************************************************************************************
Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50 & monthlyCharges

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 69.8769
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*****************************************************************************************************************

Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50 & Partner

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & Partner=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 53.18352
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+PARTNER")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************

Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50 & Not StreamingMovies

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & StreamingMovies=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 42.1616
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+StreamingMovies")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************

Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50 CAUING CHURN & MultipleLines

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MultipleLines=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 35.5805
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+MultipleLines")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

************************************************************************************************************************

Q-> FEMALE PHONE SERVICE USER USING PHONE SERVICE ,USING Contract ,tenure<=50 

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & gender=="Female")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 39.70
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+FEMALE USER")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

**********************************************************************************************************************

Q-> Customer USING PHONE SERVICE ,USING Contract ,tenure<=50 & No StreamingTV

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & StreamingTV=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion :41.94 
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+No StreamingTV")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*************************************************************************************************************************

Q-> Customer USING PHONE SERVICE ,USING Contract ,tenure<=50 & PAY METHOD ELECTRONIC

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & PaymentMethod=="Electronic check")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 46.3349
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+PAYMENT_METHOD")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*************************************************************************************************************************************

Q-> Customer USING PHONE SERVICE ,USING Contract ,tenure<=50 & PAY METHOD ELECTRONIC without DEVICE PROTECTION

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 &  DeviceProtection=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 53.39754
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+DEV_protection")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***********************************************************************************************************************************
Q-> Customer USING PHONE SERVICE ,USING Contract ,tenure<=50 & PAY METHOD ELECTRONIC & no online_backup

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & OnlineBackup=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 54.3606
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+online_backup")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*********************************************************************************************************************************
Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50  ,No Senior CItizen

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 &  SeniorCitizen==0)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 56.9823
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+tenure+NSC")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*********************************************************************************************************************

Q-> CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50  & PaperlessBilling

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 &  PaperlessBilling=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 58.2129
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+PaperlessBilling")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*****************************************************************************************************************************
Q-> CUSTOMER USING PHONE SERVICE not having tech service ,USING Contract ,tenure<=50  

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & TechSupport=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion :62.386
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+TechSupport")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

************************************************************************************************************************
Q-> CUSTOMER USING PHONE SERVICE NOT HAVING ONLINE SECURITY,USING Contract ,tenure<=50  

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & OnlineSecurity=="No" )
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 62.386
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+OnlineSecurity")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*************************************************************************************************************************
Q-> INDEPENDENT CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50  

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & Dependents=="No" )
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 64.847
H<-c(fc1,fc2)
M<-c("ALL","INDEPENDENT+ PHONE SERVICE+Contract+Dependents")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*******************************************************************************************************************
5TH Level :PHONE SERVICE+Contract+tenure+MonthlyCharges
*******************************************************************************************************************

Q-> INDEPENDENTS CUSTOMER USING PHONE SERVICE ,USING Contract ,tenure<=50 & monthlyCharges

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & Dependents="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 100
H<-c(fc1,fc2)
M<-c("ALL","Independents+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

****************************************************************************************************************************************
Q->CUSTOMER USING PHONE SERVICE NOT HAVING ONLINE SECURITY,USING Contract ,tenure<=50 and monthly charges

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & OnlineSecurity=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 56.92
H<-c(fc1,fc2)
M<-c("ALL","no_onlineSecurity+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

*****************************************************************************************************************************************
Q->CUSTOMER USING PHONE SERVICE NOT HAVING TECH SERVICE,USING Contract ,tenure<=50 and monthly charges

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & TechSupport=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 57.2499
H<-c(fc1,fc2)
M<-c("ALL","Techsup+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

**************************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & PaperlessBilling=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion :52.059 
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

**********************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & SeniorCitizen==0)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion :52.3809 
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***********************************************************************************************************************


info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & OnlineBackup=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 51.257
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

**************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & DeviceProtection=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 51.6318
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************


info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & PaymentMethod=="Electronic check")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 41.519
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & StreamingTV=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 41.947
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & gender=="Female")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 36.436
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & MultipleLines=="Yes")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 29.106
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************


info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & StreamingMovies=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 42.0545
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************


info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & Partner=="No")
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 49.331
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************

info<-subset(data,Churn=="Yes")
print(info)
fc1<-nrow(info)
print(fc1)
   
info<-subset(data,Churn=="Yes" & PhoneService=="Yes" & Contract=="Month-to-month"  & tenure<=50 & MonthlyCharges<=100 & TotalCharges<=1500)
print(info)
fc2<-nrow(info)
print(fc2)

# PERCENTAGE OF CHURN COUNT
a<-(fc2/fc1)*100
print(a)

#conclusion : 54.8956
H<-c(fc1,fc2)
M<-c("ALL","PAPERLESSBILL+PHONE SERVICE+Contract+tenure+MonthlyCharges")
barplot(H,names.arg=M,xlab="OVERALL CHURN", ylab="Count",main="Churn Calculation",col="blue")

***************************************************************************************************************************
6TH level: 100% churning ->Independents+PHONE SERVICE+Contract+tenure+MonthlyCharges
***************************************************************************************************************************


























