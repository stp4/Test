library("httr")
library("XML")
library("stringr")
library("ggplot2")
library("reshape2")

# Define image source
img.url ='http://arbeitszimmer.statistik-peter.at/wp-content/P1080232.jpg'
# Define Microsoft API URL to request data
URL.emoface = 'https://api.projectoxford.ai/emotion/v1.0/recognize'
###   5a2afef67f6743199d635b39bd629421
# Define access key (access key is available via: https://www.microsoft.com/cognitive-services/en-us/emotion-api)
emotionKEY = '5a2afef67f6743199d635b39bd629421'
# 76a353c1bc754b28b5ff2dd9c1f323d1
# Define image
mybody = list(url = img.url)

# Request data from Microsoft
(faceEMO = POST(
  url = URL.emoface,
  content_type('application/json'),
  add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotionKEY)),
  body = mybody,
  encode = 'json'
))

(SelfPortrait = httr::content(faceEMO)[[1]])

# Define results in data frame
o<-melt(as.data.frame(SelfPortrait$scores))
## Using  as id variables
names(o) <- c("Emotion", "Level")
o

# Make plot
ggplot(data=o, aes(x=Emotion, y=Level, fill = Emotion)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Detected Emotions in Self Portrait") +
  theme_bw()
