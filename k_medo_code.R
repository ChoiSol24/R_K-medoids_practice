library(fpc)
library(readxl)
library(cluster)
library(dplyr)
reading <- readxl::read_excel(path = "rate_change_ver2.xlsx",
                              sheet = "Sheet1",
                              col_names = TRUE)

reading

#전처리 과정
names(reading)
head(reading)
names(reading) = c('동','노인수','노인비율','문화','버스','경로당','급식','독거','의료')

reading$노인수 = as.numeric(reading$노인수)
reading$노인비율 = as.numeric(reading$노인비율)
reading$문화 = as.numeric(reading$문화)
reading$버스 = as.numeric(reading$버스)
reading$급식 = as.numeric(reading$급식)
reading$독거 = as.numeric(reading$독거)
reading$의료 = as.numeric(reading$의료)
#reading$경로당 = as.numeric(reading$경로당)
reading1 = reading
reading$경로당 <- NULL
local_name = reading$동
class(reading$독거)

reading
#모든 요소

read <- subset(reading, select=c(노인수,문화, 버스, 급식,독거,노인비율,의료))
pamk.best <-pam(read, 3)
ans = table(pamk.best$clustering,local_name)
plot(pamk.best)
ans
ans = data.frame(ans)
ans = ans[!(ans$Var1=="2"),]
ans<- ans[,-1]
names(ans) = c('동','all')
head(ans)

#문화, 노인수, 노인비율, 독거
read <- subset(reading, select=c(노인수,문화,독거,노인비율))
pamk.best <-pamk(read)
cul = table(pamk.best$pamobject$clustering,local_name)
plot(pam(read, pamk.best$nc))
cul
cul = data.frame(cul)
cul = cul[!(cul$Var1=="2"),]
cul<- cul[,-1]
head(cul)
ans = cbind(ans,cul$Freq)
names(ans)[3] = c('문화')
head(ans)


#버스,노인수, 노인비율, 독거
read <- subset(reading, select=c(버스, 노인수,독거,노인비율))
pamk.best<-pamk(read)
bus = table(pamk.best$pamobject$clustering,local_name)
plot(pam(read, pamk.best$nc))
bus = data.frame(bus)
bus = bus[!(bus$Var1=="2"),]
bus <- bus[,-1]
ans = cbind(ans,bus$Freq)
names(ans)[4] = c('버스')
head(ans)

#급식, 노인수, 노인비율,독거
names(reading)
read <- subset(reading, select=c(급식,노인수,독거,노인비율))
pamk.best<-pamk(read)
plot(pam(read, pamk.best$nc))
food = table(pamk.best$pamobject$clustering,local_name)
food = data.frame(food)
food = food[!(food$Var1=="2"),]
food <- food[,-1]
ans = cbind(ans,food$Freq)
names(ans)[5] = c('급식')
head(ans)

#의료, 노인수, 노인비율, 독거
read <-subset(reading, select=c(의료, 노인수, 독거, 노인비율))
pamk.best <- pamk(read)
plot(pam(read, pamk.best$nc))
med = table(pamk.best$pamobject$clustering,local_name)
med = data.frame(med)
med = med[!(med$Var1=="2"),]
med <- med[,-1]
ans = cbind(ans,med$Freq)
names(ans)[6] = c('의료')
head(ans)

reading = data.frame(reading)
reading_rate <- subset(reading, select=c(노인수,문화, 버스, 급식,독거,노인비율,의료))
pam.result <- pamk(reading_rate,3)
a = table(pam.result$pamobject$clustering, local_name)
plot(pam(reading ))

reading

library(writexl)
write_xlsx(reading,path = "200927_sub.xlsx")
write_xlsx(ans, path = "k_medo(ver.rate2).xlsx")
