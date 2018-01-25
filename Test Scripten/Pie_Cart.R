# library(stp5)
# 
# 
# Start("html" )
# MySet(brewer.pal(9,"Set1")[c(3,5)], col.bar=3 )
# windows(8,8)
# show.settings()
# windows(7,4)
# bwplot(yield ~ site|year, barley )
# SaveData()
# End()

#-   http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization

library(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

# Easiest way to transcribe the PDF table
# The slope calculation will enable us to color the lines/points based on up/down
dat <- data_frame(`2014-11-01`=c(0.11, 0.22, 0.35, 0.31, 0.01),
                  `2015-12-01`=c(0.17, 0.30, 0.30, 0.23, 0.00),
                  slope=factor(sign(`2014-11-01` - `2015-12-01`)),
                  fear_level=c("Very worried", "Somewhat worried", "Not too worried",
                               "Not at all", "Don't know/refused"))

# Transform that into something we can use
dat <- gather(dat, month, value, -fear_level, -slope)

# We need real dates for the X-axis manipulation
dat <- mutate(dat, month=as.Date(as.character(month)))

# Since 2 categories have the same ending value, we need to
# take care of that (this is one of a few "gotchas" in slopegraph preparation)
end_lab <- dat %>%
    filter(month==as.Date("2015-12-01")) %>%
    group_by(value) %>%
    summarise(lab=sprintf("%s", paste(fear_level, collapse=", ")))

gg <- ggplot(dat)

# line
gg <- gg + geom_line(aes(x=month, y=value, color=slope, group=fear_level), size=1)
# points
gg <- gg + geom_point(aes(x=month, y=value, fill=slope, group=fear_level),
                      color="white", shape=21, size=2.5)

# left labels
gg <- gg + geom_text(data=filter(dat, month==as.Date("2014-11-01")),
                     aes(x=month, y=value, label=sprintf("%s - %s  ", fear_level, percent(value))),
                     hjust=1, size=3)
# right labels
gg <- gg + geom_text(data=end_lab,
                     aes(x=as.Date("2015-12-01"), y=value,
                         label=sprintf("  %s - %s", percent(value), lab)),
                     hjust=0, size=3)

# Here we do some slightly tricky x-axis formatting to ensure we have enough
# space for the in-panel labels, only show the months we need and have
# the month labels display properly
gg <- gg + scale_x_date(expand=c(0.125, 0),
                        labels=date_format("%bn%Y"),
                        breaks=c(as.Date("2014-11-01"), as.Date("2015-12-01")),
                        limits=c(as.Date("2014-02-01"), as.Date("2016-12-01")))
gg <- gg + scale_y_continuous()

# I used colors from the article
gg <- gg + scale_color_manual(values=c("#f0b35f", "#177fb9"))
gg <- gg + scale_fill_manual(values=c("#f0b35f", "#177fb9"))
gg <- gg + labs(x=NULL, y=NULL, title="Fear of terror attacks (change since last year)n")
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.y=element_blank())
gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.title=element_text(hjust=0.5))
gg


df <- data.frame(
    group = c("Male", "Female", "Child"),
    value = c(25, 25, 50)
)
head(df)
##    group value
## 1   Male    25
## 2 Female    25
## 3  Child    50
#Use a barplot to visualize the data :
    
 #   library(ggplot2)
# Barplot
bp<- ggplot(df, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity")
bp
#ggplot2 pie chart for data visualization in R software

#Create a pie chart :
    
    pie <- bp + coord_polar("y", start=0)
pie

head(PlantGrowth)
##   weight group
## 1   4.17  ctrl
## 2   5.58  ctrl
## 3   5.18  ctrl
## 4   6.11  ctrl
## 5   4.50  ctrl
## 6   4.61  ctrl
#Create the pie chart of the count of observations in each group :
    
    ggplot(PlantGrowth, aes(x=factor(1), fill=group))+
    geom_bar(width = 1)+
    coord_polar("y")

blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )
# Apply the blank theme
# Remove axis tick mark labels
# Add text annotations : The package scales is used to format the labels in percent
# Apply blank theme
 
pie + scale_fill_grey() +  blank_theme +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                  label = percent(value/100)), size=5)
#ggplot2 pie chart for data visualization in R software

# Use brewer palette
pie + scale_fill_brewer("Blues") + blank_theme +
    theme(axis.text.x=element_blank())+
    geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                  label = percent(value/100)), size=5)
