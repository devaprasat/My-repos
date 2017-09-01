rm(list=ls(all=T))

library(reshape2)
library(sqldf)
library(tm)
library(magrittr)         

#################
###Approach - 1
#################

#Reading the Train_Data file
setwd("D:/R/Job_recommendation_engine/Job recommendation engine")
train_data<-read.csv("TrainData.csv")

head(train_data)

#Converting into Applicant ~ JobId Matrix
job_cast<-dcast(train_data, ApplicantID ~ JobID, value.var = "Rating",fill = 0)
head(job_cast)
dim(job_cast)
#We get 3027 Applicants and 6270 Job_Ids

#job_cast[1:8,1:5]
rm(train_data)

#Creating SVD of the Applicant~Job_Id matrix
job_svd<-svd(job_cast[,-1])
s<-diag(job_svd[[1]])   #diagonal matrix  
u<-job_svd[[2]]         #user matrix of 3027x3027 dimension
v<-job_svd[[3]]         #Job Matrix

rm(job_svd)

dim(s)
head(u,1)

#Creating a function SimilarUsers to find 'z' similar users for a given Applicant Id 'x'
SimilarUsers<-function(x,z){
  
  library(lsa)
  score= 0
  user = 0  
  applicant = 0
  
  for (i in 1:nrow(u)) {
    score[i] =cosine(u[x,],as.vector(u[i,]))
    user[i] = i
    applicant[i] = job_cast[i,1]
  }
  Distance<-data.frame(user,applicant,score)
  Distance<-Distance[order(-Distance$score),]
  z=z+1
  TopZUsers<-Distance$applicant[1:z][!Distance$user[1:z]%in%c(x)]
  return(TopZUsers)
}

#Taking our first user as an example. Applicant_Id = 42, and finding out the nearest 3 Similar users
User <- 1
Num_of_Similar_Users <- 3
User_Appln_Id <- job_cast[User,]$ApplicantID
TopZUsers <- SimilarUsers(User,Num_of_Similar_Users)
TopZUsers

#Reading MainJobViews.csv file to find out the actual jobs viewed by the Applicant
main_job_views <- read.csv("MainJobViews.csv")
dim(main_job_views)
head(main_job_views)
user_actual_viewed_jobs<- sort(sqldf(paste('select * from main_job_views where "Applicant.ID" =',User_Appln_Id))$Job.ID)

#Finding the jobs which the SimilarUsers have viewed or applied
x<-main_job_views[which(main_job_views$Applicant.ID %in% TopZUsers),]
friends_jobs <- unique(x$Job.ID)

#Finding out the row number in the Job_space matrix fot the ffriends'jobs
job_rows <- which(colnames(job_cast) %in% friends_jobs)-1


job_ids <- as.integer(colnames(job_cast[,-1]))
str(job_ids)

#Creating a function to find the'z' number of SimilarJobs to the job 'x'
SimilarJobs<-function(x,z){
  
  library(lsa)
  score= 0
  job = 0
  for (i in 1:nrow(v)) {
    score[i] =cosine(v[x,],as.vector(v[i,]))
    job[i] = i
  }
  Distance<-data.frame(job,job_ids,score)
  Distance<-Distance[order(-Distance$score),]
  z=z+1
  TopZJobs<-Distance$job_ids[1:z]
  return(TopZJobs)
}

#Finding out the pool of jobs which are similar jobs to the friend's viewed jobs
i=0
pool <- NULL
job_rows_unique <- unique(job_rows)
for(j in job_rows_unique)
{
  k <- SimilarJobs(j,3)  
  pool <- append(pool,c(k))
  i=i+1
  print(i)
}

#pool consists of all the job_ids which are to be suggested
pool <- sort(unique(pool))

#Since we now have a pool of jobs to be recommended we filter out jobs which are in the same City as the Applicant's City

#Reading Main_Info.csv file to find the Applicant's City
main_info <- read.csv("Main_Info.csv")
str(main_info)
main_info$City <- as.character(main_info$City)
user_city <- sqldf(paste('select City from main_info where "Applicant.ID" == ',User_Appln_Id))
user_city<- trimws(user_city$City)

#Reading Combined_Jobs_Final.csv to find out the City o the Jobs
combined_jobs <- read.csv("Combined_Jobs_Final.csv")
str(combined_jobs)
combined_jobs$City <- trimws(as.character(combined_jobs$City))

#Now we filter out the jobs which are in the same City where Applicant belong and these are the recommended jobs
recommended_jobs <- combined_jobs$Job.ID[which(combined_jobs$Job.ID %in%  pool & combined_jobs$City ==user_city)]

#Actual Viewed jobs by the Applicant
user_actual_viewed_jobs
#Recommended jobs based on the location:
recommended_jobs


###################
##Approach - 2
##################

#we first filter jobs first based on the applicant's location
filtered_jobs <- combined_jobs[which(combined_jobs$City ==user_city),]
nrow(filtered_jobs)
combined_jobs <- filtered_jobs


#Now we create a query from Experience File, using the Applicant's Position name,Job_City and Job_description
experience <- read.csv("Experience.csv")
applicant_info <- experience[which(experience$Applicant.ID == User_Appln_Id),]

#We combine all the positions and job descriptions to create the query
if(nrow(applicant_info)>1)
{
  chosen_applicant_text_position = paste(unlist(applicant_info$Position.Name), collapse =" ")
  chosen_applicant_text_jd = paste(unlist(applicant_info$Job.Description), collapse =" ")
}
query <- paste(chosen_applicant_text_position,chosen_applicant_text_jd,applicant_info$City[1])


#Now we create a Corpus of Documents using the job description,Position and City of the Filtered jobs
names(combined_jobs)
combined_jobs$Position <- as.character(combined_jobs$Position)
combined_jobs$Job.Description <- as.character(combined_jobs$Job.Description)
combined_jobs$City <- as.character(combined_jobs$City)
str(combined_jobs)

#Each document is a collection of job_desc,position,city
corpus <-  paste(combined_jobs$Position,combined_jobs$City,combined_jobs$Job.Description)
nrow(corpus)
sum(is.na(corpus))

combined_jobs$Job.ID
job_names<- paste("jobid",combined_jobs$Job.ID)
my_docs <- VectorSource(c(corpus,query))

my_corpus <- Corpus(my_docs)
names(my_corpus) <- c(job_names,"query")

#Applying preprocessing on the Corpus
getTransformations()

my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, tolower)
for(j in seq(my_corpus))   
{   
  my_corpus[[j]] = gsub("/", " ", my_corpus[[j]])   
  my_corpus[[j]] = gsub("@â", " ", my_corpus[[j]])   
  my_corpus[[j]] = gsub("\\|", " ", my_corpus[[j]])
  my_corpus[[j]] = gsub("â-", " ", my_corpus[[j]])
}
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus, stripWhitespace)
my_corpus <- tm_map(my_corpus, stemDocument)
my_corpus <- tm_map(my_corpus, PlainTextDocument)

#Creating Term-Document matrix to find tf-idf for the ters and documents
term.doc.matrix.stm <- TermDocumentMatrix(my_corpus)
head(inspect(term.doc.matrix.stm[,1]),100)

#We make the matrix dense by storing all the zeros
term.doc.matrix <- as.matrix(term.doc.matrix.stm)
N.docs <- nrow(combined_jobs)

#we choose tfidf weights of ((1 + log_2(tf)) times log_2(N/df))
#Note that whenever a term does not occur in a specific document, 
#or when it appears in every document, its weight is zero.

get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}

#We implement this weighting function across entire rows of the term document matrix, 
#and therefore our tfidf function must take a term frequency vector and a document frequency scalar as inputs
#Using apply, we run the tfidf weighting function on every row of the term document matrix
get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.docs+1] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, 1, FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:5, ]

#We may furthermore normalize each column vector in our tfidf matrix so that its norm is one. 
tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))

#Keeping the query alongside the other documents let us avoid repeating the same steps
query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

#We now find the Cosine SImilarity as dot product of query and document using the matrix multiplication, as the vectors have been normalized to unit length
doc.scores <- t(query.vector) %*% tfidf.matrix

#With scores in hand, rank the documents by their cosine similarities with the query vector.
results.df <- data.frame(doc = job_names, score = t(doc.scores))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

#Top 10 recommended jobs: 
results.df$doc[1:10]
