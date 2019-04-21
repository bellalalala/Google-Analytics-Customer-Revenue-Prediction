library(tidyverse)
train <- read_csv("C:\\Users\\81468\\OneDrive\\Documents\\DS5220\\project\\train_flat.csv")
test <- read_csv("C:\\Users\\81468\\OneDrive\\Documents\\DS5220\\project\\test_flat.csv")
channel <- unique(train$channelGrouping)
train$transactionRevenue <- log(train$transactionRevenue)
train$transactionRevenue
ggplot(data = train,
       aes(x = channelGrouping)) + 
  geom_bar()
length(train$channelGrouping[which(train$channelGrouping == "(Other)")])
##total 8 categories including 120 others
length(train$channelGrouping[which(is.na(train$date))])
length(train$channelGrouping[which(is.na(train$visitStartTime))])
length(train$channelGrouping[which(is.na(train$fullVisitorId))])
length(train$channelGrouping[which(is.na(train$sessionId))])
length(unique(train$sessionId))
length(train$channelGrouping[which(is.na(train$visitId))])
length(train$channelGrouping[which(is.na(train$visitNumber))])
ggplot(data = train,
       aes(x = visitNumber)) + 
  geom_bar()

ggplot(data = train,
       aes(x = socialEngagementType)) + 
  geom_bar()
##only 1 value; delete
ggplot(data = train,
       aes(x = browser)) + 
  geom_bar()
##Too many levels, group browser with little obs 
unique(train$browser)

ggplot(data = train,
       aes(x = browserVersion)) + 
  geom_bar()
#not available; delete

ggplot(data = train,
       aes(x = browserSize)) + 
  geom_bar()
#not available; delete

ggplot(data = train,
       aes(x = operatingSystem)) + 
  geom_bar()
length(unique(train$operatingSystem))

#group operatingSystem with little obs

ggplot(data = train,
       aes(x = operatingSystemVersion)) + 
  geom_bar()
#delete operatingSystemVersion

ggplot(data = train,
       aes(x = isMobile,
           y = transactionRevenue)) + 
  geom_boxplot()

ggplot(data = train,
       aes(x = mobileDeviceBranding,
           y = transactionRevenue)) + 
  geom_boxplot()
# delete mobileDeviceBranding

ggplot(data = train,
       aes(x = mobileDeviceModel,
           y = transactionRevenue)) + 
  geom_boxplot()
# delete mobileDeviceModel

ggplot(data = train,
       aes(x = mobileInputSelector,
           y = transactionRevenue)) + 
  geom_boxplot()
#delete mobileInputSelector

ggplot(data = train,
       aes(x = mobileDeviceInfo,
           y = transactionRevenue)) + 
  geom_boxplot()
#delete mobileDeviceInfo

ggplot(data = train,
       aes(x = mobileDeviceMarketingName,
           y = transactionRevenue)) + 
  geom_boxplot()
#delete mobileDeviceMarketingName
ggplot(data = train,
       aes(x = flashVersion,
           y = transactionRevenue)) + 
  geom_boxplot()
#delete flashVersion

ggplot(data = train,
       aes(x = language)) + 
  geom_bar()
#delete language
ggplot(data = train,
       aes(x = screenColors)) + 
  geom_bar()
#delete screenColors
unique(train$screenResolution) # delete

ggplot(data = train,
       aes(x = deviceCategory)) + 
  geom_bar()
#almost n

unique(train$deviceCategory)

ggplot(data = train,
       aes(x = continent)) + 
  geom_bar()


ggplot(data = train,
       aes(x = subContinent)) + 
  geom_bar()
unique(train$subContinent)

unique(train$country)
unique(train$region)
unique(train$metro)
unique(train$city)
unique(train$cityId)
unique(train$networkDomain)
unique(train$source)

ggplot(data = train,
       aes(x = networkLocation)) + 
  geom_bar()
  
unique(train$visits)
unique(train$hits)
max(train$hits)
train$hits[is.na(train$hits)]
unique(train$pageviews)
length(train$pageviews[is.na(train$pageviews)])
max(train$pageviews, na.rm = TRUE)

unique(train$bounces)
length(train$pageviews[is.na(train$bounces)])

unique(train$newVisits)
length(train$pageviews[is.na(train$newVisits)])
max(train$pageviews, na.rm = TRUE)

unique(train$transactionRevenue)
length(train$pageviews[is.na(train$transactionRevenue)])

length(test$pageviews[is.na(test$transactionRevenue)])

names(train)

max(train$pageviews, na.rm = TRUE)

ggplot(data = train,
       aes(x = exp(transactionRevenue))) + 
  geom_histogram()+
  labs( x = "transactionRevenue")
  
ggplot(data = train,
       aes(x = transactionRevenue)) + 
  geom_histogram() +
  labs( x = "Log(transactionRevenue)")

unique(train$campaign)

unique(train$medium)

length(unique(train$keyword))

length(unique(train$referralPath))

ggplot(data = train,
       aes(x = pageviews)) + 
  geom_bar()+
  xlim(0,200)

#group countries


unique(train$referralPath)

train_clean <- select(train, channelGrouping, date, fullVisitorId, sessionId, visitId,visitNumber, visitStartTime, browser, 
       operatingSystem, isMobile, deviceCategory, continent, subContinent, country, region, metro, city, networkDomain,
       hits, pageviews, bounces, newVisits, transactionRevenue, campaign, source, medium, keyword, isTrueDirect,
       referralPath, adContent, adwordsClickInfo.page, adwordsClickInfo.slot, adwordsClickInfo.adNetworkType, adwordsClickInfo.isVideoAd)

train_clean$pageviews <- as.numeric(train_clean$pageviews)
train_clean$bounces <- as.numeric(train_clean$bounces)
train_clean$newVisits <- as.numeric(train_clean$newVisits)
train_clean$isTrueDirect <- as.character(train_clean$isTrueDirect)
train_clean$adwordsClickInfo.page <- as.character(train_clean$adwordsClickInfo.page)
train_clean$adwordsClickInfo.slot <- as.character(train_clean$adwordsClickInfo.slot)
train_clean$adwordsClickInfo.adNetworkType <- as.character(train_clean$adwordsClickInfo.adNetworkType)
train_clean$adwordsClickInfo.isVideoAd <- as.character(train_clean$adwordsClickInfo.isVideoAd)


train_clean_add1 <- mutate(train_clean, 
                           pageviews = ifelse(is.na(pageviews),1, pageviews),
                           bounces = ifelse(is.na(bounces),0, bounces),
                           newVisits = ifelse(is.na(newVisits),0, newVisits),
                           isTrueDirect = ifelse(is.na(isTrueDirect),"False", as.character(isTrueDirect)),
                           `adwordsClickInfo.page` = ifelse(is.na(`adwordsClickInfo.page`),1, `adwordsClickInfo.page`),
                           `adwordsClickInfo.slot` = ifelse(is.na(`adwordsClickInfo.slot`),"Others", `adwordsClickInfo.slot`),
                           `adwordsClickInfo.adNetworkType` = ifelse(is.na(`adwordsClickInfo.adNetworkType`),"Others", `adwordsClickInfo.adNetworkType`),
                           `adwordsClickInfo.isVideoAd` = ifelse(is.na(`adwordsClickInfo.isVideoAd`),"Others", `adwordsClickInfo.isVideoAd`))

length(train_clean_add1$networkDomain[train_clean_add1$networkDomain == "google.com"])
networkDomain <- count(train_clean_add1,networkDomain) %>% 
  filter(n>5000)

referalPath <- count(train_clean_add1,referralPath, source) %>% count(referralPath)

library(tidytext)

traffic_pattern <- select(train_clean, source, referralPath)
source <- count(traffic_pattern, source)
source <- count(traffic_pattern, referralPath)
#yt/about from youtue
#analytics web from google
traffic_pattern <- mutate(traffic_pattern, 
                          google_rel = str_detect(source, "google"),
                          baidu_rel = str_detect(source, "baidu"),
                          ads = str_detect(source, "ad"),
                          partner = str_detect(source, "Partner"),
                          direct = str_detect(source, "direct"),
                          siva_rel = str_detect(source,"siliconvalley"),
                          youtu = str_detect(source,"youtube"),
                          bing = str_detect(source,"bing"),
                          facebook = str_detect(source,"facebook"),
                          reddit = str_detect(source,"reddit"),
                          dfa = str_detect(source,"dfa"),
                          quora = str_detect(source, "quora"),
                          yahoo = str_detect(source, "yahoo"),
                          golong = str_detect(source, "golong"),
                          qiita = str_detect(source, "qiita"))

key_word <- select(train_clean, keyword, source) 

key_word <- select(train_clean, adContent, transactionRevenue)  %>% filter(!is.na(adContent))

geo <- select(train_clean, transactionRevenue, continent, subContinent, country, region, metro, city)

geo_count <- group_by(train_clean, continent, country) %>% 
  summarise(totalrev = sum(as.numeric(transactionRevenue), na.rm = TRUE)/1000000, count = n()) 
#country with revenue NA as others and delete others

city_count <- group_by(train_clean, region, city) %>% 
  summarise(totalrev = sum(as.numeric(transactionRevenue), na.rm = TRUE)/1000000, count = n()) 

