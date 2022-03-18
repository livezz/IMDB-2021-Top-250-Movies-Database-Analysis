library(rvest)
library(XML)
library(xml2)
library(tidyverse)
library(ggplot2)
library(dplyr)

## 1. web scraping data --------------------------------------------------------

url <- "https://www.imdb.com/search/title/?title_type=feature&release_date=2021-01-01,2021-12-31&count=250&ref_=adv_prv"
web <- read_html(url)

# ranking
rank_html <- html_nodes(web, '.text-primary')
rank <- html_text(rank_html)
rank <- as.numeric(rank)

# title
title_html <- html_nodes(web, '.lister-item-header a')
title <- html_text(title_html)

# certificate
certificate_html <- html_nodes(web, '.certificate')
certificate <- html_text(certificate_html)
for (i in c(53,57,65,67,90,110,112,126,128,138,154,157,160,170,174,182,183,202,
            205,206,208,210,215,217,219,225,229,230,233,234,235,237,241,243,245)){
  a<-certificate[1:(i-1)]
  b<-certificate[i:length(certificate)]
  certificate<-append(a,list("Not Rated"))
  certificate<-append(certificate,b)
}
certificate <- as.character(certificate)

# runtime
runtime_html <- html_nodes(web, '.runtime')
runtime <- html_text(runtime_html)
runtime <- gsub("min", "", runtime) # removing"min"
runtime <- as.numeric(runtime)

# genre
genre_html <- html_nodes(web,'.genre')
genre <- html_text(genre_html)
genre <- gsub("\n", "", genre) # removing"\n"
genre <- gsub(" ", "", genre)  # removing blank space
genre <- gsub(",.*", "", genre) # each movie only keep the first genre 
genre <- as.factor(genre)

# rating
rate_html <- html_nodes(web, '.ratings-imdb-rating')
rate <- html_text(rate_html)
rate <- as.numeric(rate)

# director
director_html <- html_nodes(web, '.text-muted+ p a:nth-child(1)')
director <- html_text(director_html)
director <- as.factor(director)

# vote
vote_html <- html_nodes(web, '.sort-num_votes-visible span:nth-child(2)')
vote <- html_text(vote_html)
vote <- gsub(",","",vote)
vote <- as.numeric(vote)

# data companation
movie <- data.frame(Rank=rank,Title=title,Certificate=certificate,Genre=genre,
                    Rate=rate, Director=director,Runtime=runtime,Vote=vote)

movie$Certificate[which(movie$Certificate=="Not Rated")] <- "Unrated"

names(movie) <- tolower(names(movie))


## 2. data analyze -------------------------------------------------------------

# Question: Distribution of movies by genre
genres <- Corpus(VectorSource(movie$genre))
genres_dtm <- DocumentTermMatrix(genres)
genres_freq <- colSums(as.matrix(genres_dtm))
freq <- sort(colSums(as.matrix(genres_dtm)), decreasing=TRUE) 
genres_wf <- data.frame(word=names(genres_freq), freq=genres_freq)

genresplt <- ggplot(genres_wf, aes(x=reorder(word,-freq), y=freq))+ 
  geom_bar(stat="identity")+
  geom_text(aes(label = paste(format(genres_freq))),vjust=1, colour="white",size =3.5)+
  theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="black",),legend.position="none")+
  ggtitle("Distribution of 2021 TOP 250 Movies by Genre")+
  xlab("Genre")+
  ylab("Number of Movies")

ggsave("Distribution of 2021 TOP 250 Movies by Genre.png", genresplt , dpi = 800)

# Question: Which genre had the longest runtimes?
pltgenre_time <- ggplot(movie,aes(x=reorder(genre,runtime,FUN=median),y=runtime)) + 
  geom_boxplot(aes(fill = reorder(genre,runtime,FUN=median)))+
  xlab("genre")+
  scale_fill_discrete(guide = guide_legend(title = "genre"))+
  theme_bw(base_size = 10)+
  ggtitle("Movie Runtimes by Genre")
ggsave("Movie Runtimes by Genre.png", pltgenre_time , dpi = 800)

# Question: Which certificate had the longest runtimes?
pltenre_cert <- ggplot(movie,aes(x=reorder(certificate,runtime,FUN=median),y=runtime)) + 
  geom_boxplot(aes(fill = reorder(certificate,runtime,FUN=median)))+
  xlab("certificate")+
  scale_fill_discrete(guide = guide_legend(title = "certificate"))+
  theme_bw(base_size = 10)+
  ggtitle("Movie Runtimes by Certificate")
ggsave("Movie Runtimes by Certificate.png", pltenre_cert , dpi = 800)

# Question: Which genre has the highest rate?
grouprate <- movie %>% group_by(genre) %>%
  summarise_at(vars(rate), list(rate = mean))

pltrate <- ggplot(grouprate,aes(x=reorder(genre,rate,fun=mean),y=rate,fill=genre))+
  geom_bar(stat='identity') + 
  scale_fill_hue(c=40) + 
  theme(axis.text.x = element_text(angle=90),legend.position="none") +
  xlab("genre")+
  ggtitle("2021 Movie Rate by Genre")
ggsave("2021 Movie Rate by Genre.png", pltrate , dpi = 800)

# Question: Which genre has the highest vote?
groupvote <- movie %>% group_by(genre) %>%
  summarise_at(vars(vote), list(vote = mean))

genrevoate <- ggplot(groupvote,aes(x=reorder(genre,vote,fun=sum),y=vote,fill=genre))+
  geom_bar(stat='identity') + 
  scale_fill_hue(c=40) + 
  theme(axis.text.x = element_text(angle=90),legend.position="none") + 
  xlab("genre")+
  ggtitle("2021 Movie Vote by Genre")
ggsave("2021 Movie Vote by Genre.png", genrevoate , dpi = 800)

# Question: top 10 most popular movies by vote
top10 <- movie %>% drop_na(title,vote)%>%
  arrange(desc(vote)) %>% 
  head(10) %>%  
  ggplot(aes(reorder(title,vote),vote,fill=title))+
  geom_bar(stat="identity")+
  geom_text(aes(label = paste(format(vote))),size =3)+
  theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Black",face="bold"),legend.position="none")+
  scale_y_continuous(labels=scales::comma)+
  labs(x="",y="Total Number of User Votes",title="2021 Top 10 most popular movies")
ggsave("2021 Top 10 most popular movies.png", top10 , dpi = 800)


