library(XML)
library(twitteR)
library(RDSTK)
library(httr)

# Stores access tokens from dev.twitter.com
api_key = ""
api_secret =""
access_token = ""
access_token_secret = ""
# Authenticate
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# This was how I got the latitude and longitude for the KP hospitals

# hosp <- read.delim(file ='C:/Users/F445103/Desktop/KPHospitals.txt')
# data <- paste0("[",paste(paste0("\"",hosp$Address,"\""),collapse=","),"]")
# url  <- "http://www.datasciencetoolkit.org/street2coordinates"
# response <- POST(url,body=data)
# json     <- fromJSON(content(response,type="text"))
# geocode  <- do.call(rbind,lapply(json,
#                                  function(x) c(long=x$longitude,lat=x$latitude)))
# geocode <- as.data.frame(geocode)
# create address column
# geocode$Address <- rownames(geocode)
# inner joins on address - R recognizes that there is an 'Address' column in both
# df <- merge(hosp,geocode)
# write.table(df, file = 'C:/Users/F445103/Desktop/KPHospLoc.txt')

# This function takes:
#       1) a txt file with a list of locations, along with latitude and longitude information
#       2) a list of search terms to be searched for each location
#       3) an existing dataset to append results to
#       4) a mile radius to search around each location
# and searches twitter for each location/term combination using two methods:
#       1) Searching for each term within the specified radius
#       2) Concatenating the current location name and search term and searching for both
# The search is limited to tweets that are more recent (i.e. have a greater ID) than the 
# most recent tweet in the dataset to which they are to be appended.

savetweetstodb <- function(locations = 'KPHospLoc.txt', 
                           searchterms = 'twittersearchterms.txt', 
                           appendtodataset = 'dataset.Rda', 
                           mileradius = '25mi') {
        require(twitteR)
        # If no dataset is entered, create a new dataframe
        if (is.null(appendtodataset)) {
                dataset <- data.frame()
                maxid <- NULL
                }
        # Otherwise, load in dataset as dataframe
        else {
                load(appendtodataset)
                # Get ID of most recent tweet in dataset, so that search can be limited to tweets
                # more recent than those already in the dataset
                maxid <- max(dataset$id)
        }
        
        # Load locations
        locs <- read.table(file = locations, stringsAsFactors = FALSE, header = TRUE)
        # Create column that will match the format the twitter API wants for geolocations
        locs$geoloc <- paste(locs$lat,locs$long,mileradius, sep = ',')
        # Read in searchterms file
        terms <- read.delim(file = searchterms, stringsAsFactors = FALSE)
        
        # Loop through locations
        for (i in 1:length(rownames(locs))) {
                # Create column that combines the current/ith location name and the search term
                # This will let us search for non-geocoded tweets which mention the location name and the term of interest
                terms$locterm <- paste(locs[i,'Hospital'],"+", terms$term)
                # Now loop through each term for the current location
                for (j in 1:length(rownames(terms))) {
                        # The first search uses geolocation
                        a <- searchTwitter(
                                #Get the current search term
                                terms[j,'term'],
                                #Limit to 100 tweets per term
                                n = 100,
                                lang = 'en',
                                #get geoloc term which includes lattitude, longitude, and radius
                                geocode = locs[i,'geoloc'],
                                # Only get tweets more recent than the most recent tweet in the specified dataset
                                sinceID = maxid
                                )
        
                        # If the search returned results, convert the results to a dataframe
                        if (length(a) > 0) {
                                tweetsbyterm <- twListToDF(a)
                                # Create column which specifies which term was searched
                                tweetsbyterm$term <- terms[j,'term']
                                # Create column for which location name was searched
                                tweetsbyterm$hospital <- locs[i,'Hospital']
                                # Create column for whether tweet was returned as part of a search by geolocation
                                tweetsbyterm$geocoded <- TRUE
                                # Add tweets to the dataset
                                dataset <- rbind(dataset,tweetsbyterm)
                                }
                        # Wait 10 seconds so the Twitter API doesn't freak out
                        Sys.sleep(10)
                        
                        # The second search uses the location name and doesn't use geolocation (gets non-geocoded tweets)
                        b <- searchTwitter(terms[j,'locterm'],
                                n = 100,
                                lang = 'en',
                                sinceID = maxid
                                )    
                        # If the search returned results, convert the results to a dataframe 
                        if (length(b) > 0) {
                                tweetsbyterm <- twListToDF(b)
                                tweetsbyterm$term <- terms[j,'term']
                                tweetsbyterm$hospital <- locs[i,'Hospital']
                                tweetsbyterm$geocoded <- FALSE
                                dataset <- rbind(dataset,tweetsbyterm)
                                }
                        # Wait 10 seconds so the Twitter API doesn't freak out
                        Sys.sleep(10)
                }
        }
        write.csv(dataset, file = 'dataset.csv')
        save(dataset, file = 'dataset.Rda')
}
savetweetstodb()
load('dataset.Rda')

sentimentanalysis <- function(df = dataset) {
        # Data Cleaning
        library(tm) #text mining
        library(stringr)
        # Tweet columns we care about
        keepvars <- c('text','id','screenName','text','created','longitude','latitude',
                      'isRetweet','favoriteCount','term','hospital')
        df <- dataset[keepvars]
        df$geocoded <- ifelse(is.null(df$longitude),FALSE,TRUE)
        df$created <- as.Date(df$created)

        # remove twitter handles
        df$cleantext <- str_replace_all(df$text, pattern = "@\\w+", replacement = "") #\w = a word after the @
        df$cleantext <- str_replace_all(df$cleantext, pattern = "�", replacement = "") 
        # removing emoticons
        df$cleantext <- sapply(df$cleantext,function(row) iconv(row, "latin1", "ASCII", sub=""))
        
        # Sentiment Analysis
        require(syuzhet) 
        sentiments <- get_nrc_sentiment(df$cleantext)
        df <- cbind(df, sentiments)
        
        save(df, file = 'sentimentanalysis.Rda')
}
sentimentanalysis()
load('sentimentanalysis.Rda')

        library(ggplot2)
        sentimentTotals = data.frame(count = colSums(sentiments)) #sum counts and "count =" column name
        sentimentTotals$sentiment = rownames(sentimentTotals) #adding rownames into dataframe
        
        ggplot(sentimentTotals,aes(x=reorder(sentiment,count), y = count))+
          theme_classic()  +
          geom_bar(stat="identity", aes(fill=sentiment))+
          theme(legend.position = "none") + xlab("Sentiment") + ylab("Count") +
          ggtitle("Twitter Sentiment Analysis")
 
plottweets <- function(locations='KPHospLoc.txt', tweets = df) {
        require(ggmap)  
        require(sqldf)
        require(ggrepel)
        locs <- read.table(file = 'KPHospLoc.txt', stringsAsFactors = FALSE, header = TRUE)

        aggtweets <- sqldf(
                'SELECT
                        longitude
                        , latitude
                        , count(id) as number_of_tweets
                        , sum(anger) as anger
                        , sum(anticipation) as anticipation
                        , sum(disgust) as disgust
                        , sum(fear) as fear
                        , sum(joy) as joy
                        , sum(sadness) as sadness
                        , sum(surprise) as surprise
                        , sum(trust) as trust
                        , sum(negative) as negative
                        , sum(positive) as positive
                FROM df
                GROUP BY
                        longitude, latitude'
        )
        aggtweets <- aggtweets[which(aggtweets$number_of_tweets >= 100),]
        
        locmap <- get_map(location = c(min(locs$long), 
                                       min(locs$lat), 
                                       max(locs$long), 
                                       max(locs$lat)), 
                          maptype = 'satellite')
        ggmap(locmap) + 
                geom_point(data = aggtweets, aes(x = as.numeric(longitude), y = as.numeric(latitude), colour = positive/number_of_tweets), 
                           size = aggtweets$number_of_tweets/sum(aggtweets$number_of_tweets) * 400) +
                scale_colour_gradient(low='red',high='green') + 
                geom_point(data = locs, aes(x = long, y = lat), colour = 'white', size = 2)  +
                geom_text_repel(data=locs, aes(x=long, y=lat, label=Hospital), size = 4, colour='white')
}
plottweets()
