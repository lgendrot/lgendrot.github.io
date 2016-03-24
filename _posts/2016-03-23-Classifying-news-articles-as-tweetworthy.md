---
layout: post
title: Classifying News Articles as Tweetworthy using Python and R
date: 2016-03-23
---

Recently I attended a data analytics bootcamp hosted by Northeastern university. My capstone project was to do sentiment analysis on some tweets [which you can read about here](http://rpubs.com/lgendrot/sentiment){:target="_blank"}. Prior to attending that bootcamp I had been trying to figure out a way to make the [twitter bot](http://twitter.com/agingnewsie) I run more efficient in terms of the articles it links to. As it turns out, the same technique I used for my sentiment analysis will work for any general classification, provided I have the right training data.

## Training Data: Twitter Analytics

### Downloading the Data

![downloading analytics](http://i.imgur.com/JZQCO7f.png)

Luckily Twitter provides an export link for the analytics it provides. The only caveat here is that there's no programmatic access to this export, and it only offers a month at a time. That just means I had to manually download every month's information and glue the resulting CSV file together. If you're following along I'll leave that as an exercise for the reader.


### So what's in this file? 

![analytics headers](http://i.imgur.com/KuX1b6C.png)

If I open up the resulting file in excel (or textedit or whatever) I can see it contains exactly what I need to classify the articles as "tweetworthy". Namely it has information about engagement, retweets, and likes for every tweet.

Great! Except I want more than just the tweet and impression information. I want the text of the articles each tweet links to! 

### Enter Newspaper

What I need is a way to scrape the text of an article without having to worry about sifting through the HTML of any particular news site. And that's where [Newspaper](http://newspaper.readthedocs.org) comes in. 

{%highlight python%}
from newspaper import Article
first_article = Article(url="http://fakeurl.fake.com/article-name", language='en')
first_article.download()

first_article.text
#u'Blah Blah this is fake text, but you can see how this works can't you?'
{%endhighlight%}

Okay. Well now I just need to run through every tweet in my analytics csv and download the article text and then save it somewhere! I chose mongoDB here but you could just as easily save the text to a CSV or whatever other file type you want.

{%highlight python%}
import newspaper
from newspaper import Article
import pandas as pd
import re
import requests
from urlparse import urlparse
import pymongo
from dateutil import parser

#Import the analytics data
tweets = pd.read_csv("2015_tweet_analytics.csv",
                     delimiter=",", dtype={"Tweet.id":'str'})


#Connect to mongodb
try:
  conn = pymongo.MongoClient()
  print "Connected successfully!"
except pymongo.errors.ConnectionFailure, e:
  print "Could not connect to MongoDB: %s" % e

db = conn.TweetArticles
collection = db.ArticleText


# Get rid of wonky Google redirect urls
# Don't worry about this first part of the for loop,
# it's an artifact of how my twitter bot tweets links.

for i in range(len(tweets['Tweet.text'])):
  tweet = tweets['Tweet.text'][i]
  if re.search("(?P<url>https?://[^\s]+)", tweet) != None:
    link = re.search("(?P<url>https?://[^\s]+)", tweet).group('url')
    response = requests.get(link, allow_redirects=True)
    parsed_uri = urlparse(response.url)
    domain = '{uri.scheme}://{uri.netloc}/'.format(uri=parsed_uri)
    
    if domain == "https://www.google.com/":
      url = re.search("(?P<url>https?://[^\s]+)\"\);",
      response.text).group('url')
    elif domain == "http://paper.li/":
      continue
    else:
      url = response.url

  #Download article text, and save info to the mongoDB database
  try:
    article = Article(url, language='en')
    article.download()
    
    print "Parsing article..  ", i
    article.parse()
    text = article.text
    
    if text != "":
      print "Storing tweet.. ", i

      collection.update(
        {"url": url},
        {
          "url": url,
          "text": text,
          "tweet_id": tweets['Tweet.id'][i],
          "engagements": tweets['engagements'][i],
          "impressions": tweets['impressions'][i],
          "clicks": tweets['url.clicks'][i],
          "created_at": parser.parse(tweets['time'][i])
        },
        upsert=True
      )
  except Exception as e:
    print e
    print "Text extraction failed, continuing"
    continue
{%endhighlight%}

## The Analysis

Great, now I've got the text from ~900 articles that my twitter bot has tweeted out in the last ~6 months (give or take). Switching over to R now I can use the [same functions](http://rpubs.com/lgendrot/sentiment){:target="_blank"} I used for my sentiment analysis. First let's load the data into my R environment:

{%highlight R%}
mongo <- mongo.create()

if(mongo.is.connected(mongo) == TRUE) {
  db <- "TweetArticles"
  collection <- paste(db, "ArticleText", sep=".")
  fields <- mongo.bson.buffer.create()
  mongo.bson.buffer.append(fields, "text", 1L)
  mongo.bson.buffer.append(fields, "tweet_id", 1L)
  mongo.bson.buffer.append(fields, "url", 1L)
  mongo.bson.buffer.append(fields, "_id", 0L)
  mongo.bson.buffer.append(fields, "engagements", 1L)
  mongo.bson.buffer.append(fields, "clicks", 1L)
  mongo.bson.buffer.append(fields, "impressions", 1L)
  mongo.bson.buffer.append(fields, "created_at", 1L)
  fields <- mongo.bson.from.buffer(fields)
  tweets <- mongo.find.all(mongo, collection, fields=fields)
  tweets <- data.frame(matrix(unlist(tweets), nrow=length(tweets), byrow=T), stringsAsFactors = F)
  names(tweets) <- c("url", "text", "created_at", "tweet_id", "impressions", "engagements", "clicks")
  tweets$engagements <- as.numeric(tweets$engagements)
  tweets$clicks <- as.numeric(tweets$clicks)
  tweets$impressions <- as.numeric(tweets$impressions)
  tweets$engaged_with <- ifelse(tweets$engagements > 0, 1, 0)
}

{%endhighlight%}

This gives me a data frame "tweets" whose columns are "url", "text", "created_at", "tweet_id", "impressions", "engagements", "clicks" and a boolean valued "engaged_with". Since my data is relatively sparse (I've got a lot of zeroes, my twitter bot isn't very popular) I'm going to do a simple binary classification on whether or not the tweet has been engaged with at all. In other words, if someone has clicked on, retweeted, liked, or even just expanded my tweet it'll get a 1, else it'll be a 0.


And from here it's as easy as training the models with some training data and trying it out on some testing data. 

{%highlight R%}
article_tokens <- tokenize(tweets$text)
article_features <- get_feature_vectors(article_tokens, corpus_size=5000)
article_features <- add_targets(article_features, tweets)

dependent_names <- names(article_features)[3001:3004]

train <- sample_frac(article_features, .80, replace=FALSE)
test <- setdiff(article_features, train)

form <- as.formula(paste("engaged_bool~", paste(setdiff(names(train), dependent_names), collapse="+")))
m_nnet <- nnet(form, data=train, size=10, MaxNWts=100000)
m_nbayes <- naiveBayes(form, data=train, laplace=1000, threshold=.1)
m_randomforest <- ranger(form, data=train, write.forest=TRUE)
m_logit <- glm(form, data=train, family=binomial(link='logit'))
m_svm <- svm(form, data=train, type="C")


p_nnet <- predict(m_nnet, test, type="class")
p_bayes <- predict(m_nbayes, test)
p_rforest <- predict(m_randomforest, test)$predictions
p_logit <- predict(m_logit, test, type="response")
p_logit <- ifelse(p_logit > .5, 1, 0)
p_svm <- predict(m_svm, test)

p_ensemble <- ensemble(list(p_nnet, p_bayes, p_rforest, p_logit, p_svm))
sensitivity(table(p_ensemble, test$engaged_bool))

## $accuracy
## [1] 0.6222222

## $specificity
## [1] 0.6716418

## $precision
## [1] 0.752809

## $sensitivity
## [1] 0.5929204
##
{%endhighlight%}

Well, that's not the best sensitivity ever, it's barely better than chance! I'll have to keep playing with the model parameters and perhaps a larger corpus size if I want this to actually be useful, but I suppose you can't win them all!
