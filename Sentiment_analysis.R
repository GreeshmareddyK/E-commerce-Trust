#The functionality of each function is mentioned below
# ConnectNSearch() - connects to twitter using the consumer key and retrieves the top 1500 tweets for that ecommerce site
#clean_tweets() - cleans all the data retrieved from connectNsearch fucntion. The comments in function clearly explains the steps followed while cleaning tweets
#sentimentAnalysis() - This is the main method that connects all other methods. This function performs bayes algorithm and plots the emotions,polarity and wordcloud for ecommerce site
#multiplot is used to display 2 plots in single active device window
connectNSearch <- function(ecommerce){
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
customerkey <- 'hMLoozWk0nyKXzigLqM4dNQaD'
customer_secret <- 'bmtirZdFPhy9uRoVU4DsGeIY2f5fUWIiboZadgZWbjFvY3jtWf'
access_token <- '717922534990209024-6mtu5wJFChvQmNQ89Y0bfgEMdFQ4XGF'
access_secret <- 'bVYAyx8jttLuq781gTZyRzUWm9q6vqpXohXz1hztx0Qk7'
setup_twitter_oauth(customerkey,customer_secret,access_token,access_secret)

#setup_twitter_oauth(customerkey,customer_secret,access_token,access_secret)
tweet <- searchTwitter(ecommerce,n = 1500,lang = "en")#searches for 1500 recent tweets above the ecommerce website
return(tweet)
}

clean_tweets <- function(tweet,ecommerce){
		 tweet_text <- sapply(tweet, function(t) t$getText()) 
       
     #write the raw tweets data to file
     #name of the file is dynamically created based on the name of the site 
     	raw_tweet_Date <- data.frame(tweet_text)
      
     	names(paste0(ecommerce,"_raw_tweets"))
     	write.csv(unique(raw_tweet_Date),paste0(ecommerce,"_raw_data.csv"))    

     require(tm)
     clean_tweets <- lapply(tweet_text, function(tweet){
        clean <- gsub("@\\w+", "", tweet)
	  #remove punctuation marks
	      clean <- gsub("[[:punct:]]","",clean)
        #remove digits from tweets
		clean <- gsub("[[:digit:]]","",clean)
		
	  #remove links 
            
		clean <- gsub("http\\w+","", clean)
		clean <- gsub("amazon","",clean)
	  #remove & 	
		clean <- gsub("&amp","",clean)
        #remove the name of the ecommerce site
            clean <- gsub("Amazon","",clean)
	  #removed some of the unnecessary patterns on observation
          clean = gsub("\n","",clean)
	    clean = gsub("@\\w+", "", clean)
	    clean = gsub("[ \t]{2,}", "", clean)
  	    clean <- gsub("^\\s+|\\s+$", "", clean)
          clean <- gsub("<U+","",clean)
          clean <- gsub("<u+","",clean)
          #remove stopwords and create a document term matrix with frequencies of all words
          
	    return(clean)
     })
	 }

sentimentAnalysis <- function(ecommerce){
      Auth <- connectNSearch(ecommerce)
      tweets <- clean_tweets(Auth,ecommerce)
	# classify emotion using bayes algorithm
	identify_emotions = classify_emotion(tweets, algorithm="bayes", prior=1.0)
	names(paste0(ecommerce,"_classified_emotions"))
     	write.csv(unique(identify_emotions),paste0(ecommerce,"_classified_emotions.csv"))   

	# get emotion best fit , the last column has to overall emotion for the tweet
	emotion = identify_emotions[,7]
	# Replace NA's by "unknown"
	emotion[is.na(emotion)] = "unknown"
	# classify polarity
	identify_polarity = classify_polarity(tweets, algorithm="bayes")
	names(paste0(ecommerce,"_classified_polarity"))
     	write.csv(unique(identify_polarity),paste0(ecommerce,"_classified_polarity.csv"))   

	# get best fit for polarity
	polarity = identify_polarity[,4]
	#results are placed in a dataframe
	data_frame = data.frame(tweets,emotion=emotion,polarity=polarity,stringAsFactor=FALSE)     
	data_frame = within(data_frame,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
	map_emotion <- ggplot(data_frame, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2") + labs(x="Emotions Types", y="No.of tweets")+ggtitle(paste0(" Sentiment Analysis for ",ecommerce," Twitter Data\n(Classification by Emotion)"))

	map_polarity <- ggplot(data_frame, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + labs(x="Polarity", y="No. of tweets")+ggtitle(paste0(" Sentiment Analysis for ",ecommerce," Twitter Data\n(Classification by Polarity)"))


	# separating text by emotion
	emotions = levels(factor(data_frame$emotion))
	no_of_emtions = length(emotions)
	emotion.docs= rep("", no_of_emtions)
	for (i in 1:no_of_emtions)
	{
	   tmp = tweets[emotion == emotions[i]]
	   emotion.docs[i] = paste(tmp, collapse=" ")
	}

	# remove stopwords
	emotion.docs= removeWords(emotion.docs,stopwords("english"))
	# create corpus
	tweet_corpus = Corpus(VectorSource(emotion.docs))
	 tweet_corpus <- tm_map(tweet_corpus,tolower)
      tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
      tweet_corpus <- tm_map(tweet_corpus,removeWords,c(stopwords("english"),"rt","amp"))
      #Stemming on the document
      tweet_corpus <- tm_map(tweet_corpus,stemDocument)
      tweet_corpus <- tm_map(tweet_corpus,PlainTextDocument)

	tdm = TermDocumentMatrix(tweet_corpus)
	tdm = as.matrix(tdm)
	colnames(tdm) = emotions

	# comparison word cloud
	multiplot(map_emotion,map_polarity,cols =1)	
	dev.new()
	cloud <- comparison.cloud(tdm, colors = brewer.pal(no_of_emtions, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5)
	cloud

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))

  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
