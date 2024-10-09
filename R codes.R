library(readxl)
d=read_excel("C:\\Users\\mukun\\OneDrive\\Desktop\\ty project.xlsx")
View(d)
gender=d$gender
age=d$age
earning=d$earning
sleep=d$`sleep difficulty`
physical_dis=d$`physical discomfort`
posture=d$`body posture`
exhaust=d$`exhausted feeling`
comparision=d$`life comparison`
media_break=d$`break`
motivation=d$`online motivation`
promotion=d$promotion
adv=d$advertisement
und_concept=d$`understanding concepts`
misinfo=d$misinformation
hiring_dec=d$`hiring decision`
job_search=d$`job search`
personal_branding=d$`personal branding`
personal_info=d$`personal info`
scams=d$scams
communication=d$communication
influence=d$influence
pol_issue=d$`political issue`
pol_opinion=d$`political dec`
time_spend=d$`time spend`
Data=data.frame(cbind(gender,age,earning,sleep,physical_dis,posture,exhaust,comparision,media_break,motivation,promotion,adv,und_concept,misinfo,hiring_dec,job_search,personal_branding,personal_info,scams,communication,influence,pol_issue,pol_opinion,time_spend))
View(Data)
d1=Data[,-c(1,2,3,24)]
View(d1)
as.integer(d1[1,]) 
sum(as.integer(d1[1,c(1,2,3)]))
sum(as.integer(d1[1,c(4,5,6,7)])
sum(as.integer(d1[1,c(8,9)])
d1[1,c(10,11,12)]
d1[1,c(13,14)]
rowSums(d1[1,c(1,2,3)])


