library(ggplot2)
libaray(caret)

data <- read.csv("problemsample.csv")
qplot(manufacturer, data=data)


data <- subset(data, select = -X )
hasvalues <- subset(data, event_type %in% c("Death", "Injury", "Malfunction"))

###not plotted in the thing
g <- ggplot(now2, aes(manufacturer, total))
g+geom_bar(stat ="identity")+facet_grid(.~event_type)

#example
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top 
            auto.key = list(colum
                            ns = 3))

#meaningless dots
featurePlot(x= data[, 4:6], y=data$generic_name), plot="pairs")


#this should be different after mfr names read in, worth plotting?
new2 <- aggregate(data=hasvalues, event_type~manufacturer, FUN="length")
newnew2 <- filter(new2, event_type > 20)
#large number of total events of all types--should be bigger companies?
g <- ggplot(newnew2, aes(manufacturer, event_type))
g+geom_point()+theme(axis.text.x = element_text(angle = 45, hjust=1))

#not plotted
deaths <- subset(moo, event_type=="Death")
deaths$manufacturer <- droplevels(deaths$manufacturer)

#only one mfr with more than one death in this small dataset
g <- ggplot(deaths, aes(manufacturer))
g+geom_histogram()+coord_flip()


##this one has potential but there are still too many mfrs--78 after drop
here <- group_by(hasvalues, manufacturer, event_type) %>% 
      summarise(count=n()) %>% 
      subset(count > 8)
here$manufacturer <- droplevels(here$manufacturer)

##this leaves out deaths
g <- ggplot(here, aes(event_type, count, fill=manufacturer))
g+geom_bar(stat="identity", position="dodge")


now2 <- group_by(hasvalues, manufacturer, event_type) %>% 
                              summarise(total=n()) %>% 
                              mutate(all=sum(total), percent=total/all)

##again may need to change size of subset
##having a death doesn't necessarily mean having a lot of injuries and malfunctions
herenow <- mutate(now2, hasdeath =(event_type=='Death'))
smallnow <- subset(herenow, all>40 | hasdeath==TRUE)


g <- ggplot(smallnow, aes(manufacturer, total, fill=event_type))
g+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 45, hjust=1))


widenow <- spread(now2, event_type, total)
pony <- filter(widenow, Injury & Malfunction >0 )
qplot(Malfunction, Injury, data=widenow)


pony$Death[is.na(pony$Death)]=0
pony$Injury[is.na(pony$Injury)]=0

#many malfunctions don't resuly in injuries but for those that do more is more...
qplot(Malfunction, Injury, data=pony)
qplot(Malfunction, Injury, data=pony, color=manufacturer)

there <- group_by(hasvalues, generic_name, event_type) %>%
                                    summarise(total=n())


widethere <- spread(there, event_type, total)
snowpony <- filter(widethere, Malfunction >0 )
snowpony$Injury[is.na(snowpony$Injury)]=0
qplot(Malfunction, Injury, data=snowpony)
#less obvious relationship here

snowpony2 <- filter(widethere, Injury >0 & Malfunction >0 )
qplot(Malfunction, Injury, data=snowpony2)
snowpony2 <- snowpony2[-1,]
#manually removing top right outlier with blank generic name

kitty <- arrange(ungroup(snowpony2), Malfunction)
kitty2 <- filter(kitty, Malfunction > 20) 
#some major malfunctions have few injuries, some have lots
#time to clean up generic names...

#lots of itmes dealing with some aspect of blood glucose
bloody <- subset(snowpony, grepl("BLOOD GLUCOSE", snowpony$generic_name))
colSums(bloody[2:4])

bigdata <- read.csv("largesample.csv")
bigdata <- subset(bigdata, select = -X)

halfpuppy <- bigdata %>% group_by(class, event_type) %>% summarise(total=n())
widepuppy <- spread(halfpuppy, event_type, total)

g <- ggplot(halfpuppy, aes(event_type, total, fill=class))
> g+geom_bar(stat="identity", position="dodge")

g <- ggplot(halfpuppy, aes(event_type, total))
> g+geom_bar(stat="identity", position="dodge")+facet_grid(.~class)

#adding a level to put all randoms into 
levels(halfpuppy$class) = c(levels(halfpuppy$class), "unknown")


halfpuppy$class[halfpuppy$class=="f"] <- "unknown"
halfpuppy$class[halfpuppy$class=="N"] <- "unknown"
halfpuppy$class[halfpuppy$class=="U"] <- "unknown"
halfpuppy$class[is.na(halfpuppy$class)] <- "unknown"

#now dealing wih event types, make no answer into NA vs other which is an actual thing
halfpuppy$event_type <- factor(halfpuppy$event_type, levels = c("Death","Injury", "Malfunction", "Other"))
#that coerces the unused level to NAs

g <- ggplot(halfpuppy, aes(event_type, total, fill=event_type))
g+geom_bar(stat="identity", position="dodge")+facet_grid(.~class)+theme(axis.text.x = element_text(angle = 45, hjust=1))


tonight <- bigdata %>% group_by(event_type, specialty) %>% summarise(total=n())
tonight2 <- filter(tonight, !(specialty %in% c("NA", "Unknown")))
tonight3 <- filter(tonight2, event_type %in% c("Death","Injury", "Malfunction")
tonight3$event_type <- droplevels(tonight3$event_type)


g <- ggplot(na.omit(tonight2), aes(event_type, total))
g+geom_bar(stat="identity", position="dodge")+facet_wrap(~specialty)+theme(axis.text.x = element_text(angle = 45, hjust=1))

g <- ggplot(na.omit(tonight3), aes(specialty, total, fill=event_type))
> g+geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust=1))
#use prop.table to find out how many malfunctions have an adverse event flag
###what is the meaning of that really?


cbbPalette <- c("#1b9e77","#d95f02","#7570b3","#e7298a")
g <- ggplot(na.omit(tonight3), aes(specialty, total, fill=event_type))
g+geom_bar(stat="identity", position="dodge")+theme(axis.text.x = element_text(angle = 45, hjust=1))+scale_fill_manual(values=cbbPalette)

###need this off of new data with flag! 
newthere <- group_by(hasvalues, generic_name, event_type) %>% mutate(total_adverse = sum(adverse=="Y")) %>% summarise(total=n())

