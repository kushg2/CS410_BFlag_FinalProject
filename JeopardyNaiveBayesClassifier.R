#Install reticulate package
if (!requireNamespace("reticulate", quietly = TRUE)) {
  install.packages("reticulate")
}

#Import necessary packages and modules
print("Importing Python libraries from reticulate")
import_start_time = Sys.time()
library(reticulate)
py_install(packages=c("pandas", "scikit-learn"))
pd <- import("pandas", as="pd")
feature_extraction <- import("sklearn.feature_extraction.text")
model_selection <- import("sklearn.model_selection")
metrics <- import("sklearn.metrics")
naive_bayes <- import("sklearn.naive_bayes")
import_end_time = Sys.time()
print(paste("Completed in", difftime(import_end_time, import_start_time, units="secs"), "s.", sep = " "))

# Read Jeopardy data
print("Processing Jeopardy data and splitting into training and test datasets.")
data_processing_start_time = Sys.time()
jeopardy_data <- read.csv("JeopardyQuestions_Labeled.csv")

# Split dataset into training and test datasets
trainAndTestDatasets = model_selection$train_test_split(jeopardy_data$questionClean, 
                                                          jeopardy_data$X10_topic_predictions, 
                                                          test_size=0.25)
X_train = trainAndTestDatasets[1]
X_test = trainAndTestDatasets[2]
y_train = trainAndTestDatasets[3]
y_test = trainAndTestDatasets[4]
data_processing_end_time = Sys.time()
print(paste("Completed in", difftime(data_processing_end_time, data_processing_start_time, units="secs"), "s.", sep = " "))

#Vectorize the Jeopardy questions
print("Vectorizing Jeopardy questions from training and test datasets.")
vectorizing_start_time = Sys.time()
question_vectorizer = feature_extraction$CountVectorizer(stop_words='english', 
                                                         ngram_range=tuple(1L,2L), 
                                                         max_features=2000L)

train_questions = r_to_py(X_train)
train_topics = r_to_py(y_train)
test_questions = r_to_py(X_test)
test_topics = r_to_py(y_test)

vectorized_train_questions = question_vectorizer$fit_transform(train_questions[0L])

vectorized_test_questions = question_vectorizer$transform(test_questions[0L])
vectorizing_end_time = Sys.time()
print(paste("Completed in", difftime(vectorizing_end_time, vectorizing_start_time, units="secs"), "s.", sep = " "))

#create Naive Bayes Classification model
print("Training Naive Bayes Model.")
training_start_time = Sys.time()
naive_bayes_model <- naive_bayes$MultinomialNB(alpha=10L)

#Train model with the questions from the training dataset
naive_bayes_model$fit(vectorized_train_questions, train_topics[0L])
training_end_time = Sys.time()
print(paste("Completed in", difftime(training_end_time, training_start_time, units="secs"), "s.", sep = " "))

#Use test questions to predict their topics using the model
print("Testing and Evaluating Naive Bayes Model.")
test_start_time = Sys.time()
predicted_topics = naive_bayes_model$predict(vectorized_test_questions)

#Evaluate model
model_accuracy <- metrics$accuracy_score(test_topics[0L], predicted_topics)
model_f1 <- metrics$f1_score(test_topics[0L], predicted_topics, average="macro")
test_end_time = Sys.time()
print(paste("Completed in", difftime(test_end_time, test_start_time, units="secs"), "s.", sep = " "))

predict_topic <- function(question_text) {
  question_text_string = paste(question_text, collapse = " ")
  unique_question_words = unique(strsplit(question_text_string, " "))
  unique_question_words_list = list(paste(unique_question_words, collapse = " "))
  vectorized_uniq_words = question_vectorizer$transform(r_to_py(unique_question_words_list))
  naive_bayes_model$predict(vectorized_uniq_words)
}