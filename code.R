install.packages("MASS")
install.packages("caret")
install.packages("dplyr")
install.packages("ggplotify")
library(ggplot2)
library(ggplotify)
library(dplyr)
library(caret)
library(class)
d=read.csv("C:\\Users\\hp\\Desktop\\data.csv")
dim(d)
colnames(d)
head(d)
d=d[,-c(1,33)]

#checking the null/missing values in data.
sum(is.na(d))

#Getting summary of entire data.
summary(d)




#checking the traget class.
b=dim(d[which(d$diagnosis=="B"),])[1]
m=dim(d[which(d$diagnosis=="M"),])[1]
barplot(c(b,m),col =c("pink","blue"),names.arg = c("begin","maligant"),main = "Total count of each class",ylab = "counts")
View(d)


#scaling the data because it preqest for machine learning algorithum.
d_sc=scale(d[,-1],center = TRUE,scale = TRUE)
d$diagnosis=as.numeric(as.factor(d$diagnosis))
dd_scale=cbind(d_sc,d$diagnosis)
colnames(dd_scale)[31]="class"
dd_scale=as.data.frame(dd_scale)
summary(dd_scale)
mean(cor(dd_scale))
head(dd_scale)

dim(dd_scale)




#Using PCA since their exist multicollinearity and dimension of data is very large due to which the model result get effected.
library(stats)
install.packages("factoextra")
library(factoextra)
PCA=princomp(dd_scale[,-31])
summary(PCA)
#Using scree plot to select PC's for which it explain most of variance of data.
fviz_eig(PCA,addlabels = TRUE)
pc6=PCA$scores[,c(1,2,3,4,5,6)]
dd_scale1=data.frame(pc6,dd_scale$class)
colnames(dd_scale1)[7]="class"
PCA$loadings[,1:6]
#From above scree plot and cumulative proportion we can consider 6 principle component which about 89% of varition of data.





#determine the no of clustering.
my_pca=data.frame(pc6)
fviz_nbclust(my_pca,FUNcluster = kmeans,method="wss")
#By this we get optimal no of cluster at k=3
Kmeans=kmeans(my_pca,centers=2)
table(Kmeans$cluster)
#evaluation of cluster analysis.
#plotting the clusters.
rownames(my_pca)=paste(d$diagnosis,1:dim(d)[1],sep="_")
fviz_cluster(list(data=my_pca,cluster=Kmeans$cluster))
table(d$diagnosis,Kmeans$cluster)







#spliting the data into train and test dataset.
n=dim(dd_scale1)[1];n
set.seed(700)
ind=sample(1:n,n*0.7)
train_data=dd_scale1[ind,]
test_data=dd_scale1[-ind,]
dim(train_data)


#fitting different models on train data set by using pc's




#fitting naive bayes.
library(e1071)
naive_model=naiveBayes(class~.,data=train_data)
summary(naive_model)
pre=predict(naive_model,test_data[,-7]);pre
cm=confusionMatrix(as.factor(test_data$class),as.factor(pre));cm
f1_score1=cm$table[1]/(cm$table[1]+0.5*(cm$table[3]+cm$table[2]));f1_score







#fitting the KNN model.
k_values = 1:20
f1_scores = numeric(length(k_values))
for (i in k_values) {
    cat("----------------- For k =", i, "-----------------\n")
    knn_model = knn(train_data, test_data, train_data$class, k = i)
    cm = confusionMatrix(knn_model, as.factor(test_data$class))
    f1_score = cm$table[1] / (cm$table[1] + 0.5 * (cm$table[3] + cm$table[2]))
    f1_scores[i] = f1_score
    cat("f1 score is: ", f1_score, "\n")
    cat("\n")
  }
  
# Plot k versus F1 scores
plot(k_values, f1_scores, type = "b", pch = 19, col = "blue",xlab = "k", ylab = "F1 Score", main = "F1 Score vs. k for KNN Model")
abline(v = 3, col = "red", lty = 2)

#For K=3 we get most accuracy and less error.
knn_model=knn(train_data, test_data, train_data$class, k=3)
cm=confusionMatrix(knn_model, as.factor(test_data$class))
f1_score2=cm$table[1]/(cm$table[1]+0.5*(cm$table[3]+cm$table[2]))





#fitting logistic model.
train_data$class=ifelse(train_data$class==1,0,1)
test_data$class=ifelse(test_data$class==1,0,1)
l=glm(class~.,data=train_data);l
summary(l)
pre=predict(l,test_data[,-7]);pre
f=c()

#checking the f1score at different thersold value.
s=seq(0,1,0.05)
for(i in 1:length(s)){
  cat("----------------- For thersold =", s[i], "-----------------\n")
  k=s[i]
  
cm=confusionMatrix(as.factor(test_data$class),as.factor(ifelse(pre>k,1,0)))
f1_score=cm$table[1]/(cm$table[1]+0.5*(cm$table[3]+cm$table[2]))
f[i]=f1_score
cat("f1 score is: ", f1_score, "\n")
cat("\n")
}
#checking for which thersold the f1 score is maximum.
d=data.frame(s,f)
d[which(f==max(d$f)),][1]
#for  0.5 thersold we get maximum f1score
#showing by this graph.
plot(x=s,y=f,col=ifelse(f==max(f),"red","blue"),xlab="thersold",ylab="F1 score",main="Different f1 scores for different thersolds",type="b",lty=3,lwd=3)
abline(v=d[which(f==max(d$f)),][1],lty=2,lwd=2,col="pink")
text(x=s,y=f,labels =round(f,3),pos =1,cex=0.6)

#for thersold 0.5 we get best f1 scorce.
l=glm(class~.,data=train_data);l
pre=predict(l,test_data[,-7]);pre
cm=confusionMatrix(as.factor(test_data$class),as.factor(ifelse(pre>0.5,1,0)))
f1_score3=cm$table[1]/(cm$table[1]+0.5*(cm$table[3]+cm$table[2]))

#confusion:
d=data.frame(Models=c("Naive bayes","Knn","logistic regression"),f1_score=c(f1_score1,f1_score2,f1_score3));d
barplot(c(f1_score1,f1_score2,f1_score3),names.arg = c("Naive bayes","Knn","logistic regression"),col=c("pink","blue","violet"),ylim=c(0,1),main = "F1_score of different models")


library(rpart)
T=rpart::rpart(class~.,data = train_data)
summary(T)


        