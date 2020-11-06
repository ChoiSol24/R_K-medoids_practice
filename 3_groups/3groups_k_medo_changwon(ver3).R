#k_medo_3groups
library(writexl)
library(fpc)
library(readxl)
library(cluster)
library(dplyr)
reading <- readxl::read_excel(path = "rate_change_ver2.xlsx",
                              sheet = "Sheet1",
                              col_names = TRUE)

#전처리 과정

names(reading) = c('동','노인수','노인비율','문화','버스','경로당','급식','독거','의료')

reading$노인수 = as.numeric(reading$노인수)
reading$노인비율 = as.numeric(reading$노인비율)
reading$문화 = as.numeric(reading$문화)
reading$버스 = as.numeric(reading$버스)
reading$급식 = as.numeric(reading$급식)
reading$독거 = as.numeric(reading$독거)
reading$의료 = as.numeric(reading$의료)
#reading$경로당 = as.numeric(reading$경로당)

reading$경로당 <- NULL
local_name = reading$동

#모든 요소

read <- subset(reading, select=c(노인수,문화, 버스, 급식,독거,노인비율,의료))
pamk.best <-pam(read, 3)
ans = table(pamk.best$clustering,local_name)
plot(pamk.best)
ans = data.frame(ans)
names(ans) = c('group','local','all_num')


#문화, 노인수, 노인비율, 독거
read <- subset(reading, select=c(노인수,문화,독거,노인비율))
pamk.best <-pam(read, 3)
cul = table(pamk.best$clustering,local_name)
plot(pamk.best)
cul = data.frame(cul)
ans = cbind(ans,cul$Freq)
names(ans)[4] = c('문화')

#버스, 노인수, 노인비율,독거
read <- subset(reading, select=c(노인수,버스,독거,노인비율))
pamk.best <-pam(read, 3)
bus = table(pamk.best$clustering,local_name)
plot(pamk.best)
bus = data.frame(bus)
ans = cbind(ans,bus$Freq)
names(ans)[5] = c('버스')

#급식, 노인수, 노인비율, 독거
read <- subset(reading, select=c(노인수,급식,독거,노인비율))
pamk.best <-pam(read, 3)
food = table(pamk.best$clustering,local_name)
plot(pamk.best)
food = data.frame(food)
ans = cbind(ans,food$Freq)
names(ans)[6] = c('급식')

#의료, 노인수, 노인비율, 독거
read <- subset(reading, select=c(노인수,의료,노인비율))
pamk.best <-pam(read, 3)
medi = table(pamk.best$clustering,local_name)
plot(pamk.best)
medi = data.frame(medi)
ans = cbind(ans,medi$Freq)
names(ans)[7] = c('의료')

write_xlsx(ans,path = "200929_k_medo_ver3.xlsx")

#의료, 노인수, 노인비율

read <- subset(reading, select=c(노인수,의료,노인비율))
pamk.best <-pam(read, 3)
medi_1 = table(pamk.best$clustering,local_name)
plot(pamk.best)
medi_1 = data.frame(medi_1)
medi_1

#버스, 노인수, 노인비율

read <- subset(reading, select=c(노인수,노인비율))
pamk.best <-pam(read, 2)
bus_1 = table(pamk.best$clustering,local_name)
plot(pamk.best)
bus_1 = data.frame(bus_1)
medi_1 = cbind(medi_1,bus_1$Freq)
names(medi_1) = c('group','local','medi','bus')
medi_1

write_xlsx(medi_1,path = "200929_k_medo_ver4.xlsx")
