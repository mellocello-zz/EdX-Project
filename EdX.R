## Access Data
# Load the csv-formatted file and rename to make it easier to work with. Then check for proper loading.

edx <- read.csv(choose.file())
dim(edx)
head(edx)
tail(edx)

# Assess variables and data types within file, by accessing a summary of the entire file
# Rename columns/vars to improve usability as needed. Log in data dictionary.

summary(edx)

attach(edx)

#Create table 'A' with renamed vars.

A <- read.table(file.choose(), sep="," , 
     col.names=c("id", "user", "registered", "viewed", "explored", "certified", 
     "country", "NA1", "NA2", "NA3", "NA4", "enroll_date", "last_event", "clicks", 
     "days_active", "videos_played", "chapters_read", "posts", "NA5", "incomplete"))

#Format date info
A$enroll_date <- as.Date(A$enroll_date, format="%Y-%m-%d")
A$last_event <- as.Date(A$last_event, format="%Y-%m-%d")
date$start_time_DI <- as.Date(date$start_time_DI, format="%Y-%m-%d")

#Have a look around
qplot(A$days_active, A$chapters_read, data = A)
qplot(enroll_date, last_event, data=A, alpha = 1/3, fill=certified)
qplot(nevents, ndays_act, data=edx, alpha = 1/3, fill=certified)
qplot(ndays_act, log10(nevents), data=edx, alpha = 1/3, fill=certified)

# Review missing values

is.na(edx) 

# Replace data with NA, where needed for analysis. (Certified status as an example.)

nevents[nevents < 0] <- NA
A[A == 0] <- NA

# Create var 'nevents' that only represents events >= 0. Omit missing values in analysis.

nevents <- subset(nevents, !is.na(nevents))

# Check distributions, means, and medians. Transform data to look for outliers (log10).
# Perform linear regression analysis to identify variation and outliers.


# Plot a crude correlation matrix for massive top-down view of variable relationships.

qplot(final_cc_cname_DI, explored, data = edx)

# Check if outliers are interesting or unusable data. Determine what to do with unusable data.
# (e.g. remove single point, remove all associated data, replace with average)


#### Exploratory Data Analysis: What factors influence certification?
## Location
country <- arrange(count(registered, "final_cc_cname_DI"), desc(freq))
region <- country$final_cc_cname_DI
freq <- country$freq

# Preliminary plot
g <- ggplot(country, aes(reorder(region, freq), freq))
g + geom_bar(stat="identity") + coord_flip()

# Nicer version
g + geom_bar(stat="identity") + coord_flip() 
  + theme_bw(base_family = "Avenir", base_size = 10) 
  + labs(title = "Enrollment in EdX 2012-2013") 
  + labs(x= "Country/Region") 
  + labs(y= "Registrants")
#######change scale of certified plot & show percentages.

#Faceted plots according to certification
g2 <- ggplot(edx, aes(reorder(country$final_cc_cname_DI, country$freq), country$freq))
g2 + geom_bar(stat="identity") + coord_flip() + facet_grid(. ~ certified)

g2 + geom_bar(stat="identity") + coord_flip() + theme_bw(base_family = "Avenir", base_size = 10) + labs(title = "Enrollment in EdX 2012-2013: Registered = 0, Completed = 1") + labs(x= "Country/Region") + labs(y= "Registrants") + facet_grid(. ~ certified)

#Nicer version

## Enrollment date
date <- arrange(count(registered, "start_time_DI"), freq)
enroll <- date$start_time_DI
efreq <- date$freq

date <- arrange(count(A$registered, "A$enroll_date"), freq)

g3 <- ggplot(date, aes(enroll, efreq))

#Preliminary plot
g3 <- ggplot(date, aes(enroll, efreq, group = 1))
g3 + geom_point(alpha = 1/3) + geom_smooth (method="lm", se=TRUE, col= "steelblue")

ggplot(date, aes(x = enroll, y = efreq)) +  geom_point(alpha=1/3, color="purple") 
  + theme_bw() + labs(title= "Enrollment Dates August 2012 - July 2013", 
  x = "Date", y = "Number of Students")

ggplot(date, aes(x = enroll, y = efreq)) +  geom_point(alpha=1/3, color="purple") 
  + theme_bw(base_family = "Avenir") + theme(axis.title.x=element_text(angle=45)) 
  + labs(title= "Enrollment Dates August 2012 - July 2013", x = "Date", y = "Number of Students")

##enrollment.df <- as.data.frame(table(date, frequency)) #must sort.list

###Proper Dates
#Students who registered
ggplot(date, aes(x = date$start_time_DI, y = efreq)) +  geom_point(alpha=1/3, color="blue") 
  + theme_bw(base_family = "Avenir") + theme(axis.title.x=element_text(angle=45)) + 
  labs(title= "Enrollment Dates August 2012 - July 2013", x = "Date", y = "Number of Students")

#Students who completed
ggplot(date2, aes(x = date2$A.enroll_date, y = date2$freq)) +  
  geom_point(alpha=1/3, color="purple") + theme_bw(base_family = "Avenir") + 
  theme(axis.title.x=element_text(angle=45)) + 
  labs(title= "Enrollment Dates August 2012 - July 2013", x = "Date", 
  y = "Number of Students")

#Student who completed with scaled y-axis limit
ggplot(date2, aes(x = date2$A.enroll_date, y = date2$freq)) + 
   geom_point(alpha=1/3, color="purple") + ylim(0,10000) + theme_bw(base_family = "Avenir") + 
   labs(title= "Enrollment Dates August 2012 - July 2013", x = "Date", 
   y = "Number of Students")


## Gah! Really close!!  ggplot(edx, aes(x = start_time_DI, y = registered)) +  geom_point(aes(color = factor(certified))) + scale_color_manual(values = c("orange", "purple")) + theme_bw() + labs(title= "Enrollment Dates August 2012 - July 2013", x = "Date", y = "Number of Students")
enroll2 <- subset(A$enroll_date, A$certified == 1)
date2 <- arrange(count(enroll2, "A$enroll_date"), freq)

#Nicer version


## Course type
type <- arrange(count(A$registered, "A$id"), desc(freq))
##Remove header row
type[type == 1] <- NA
type[type == "course_id"] <- NA

g5 <- ggplot(type, aes(reorder(type$A.id, desc(type$freq)), type$freq))
# g5 var length = 16 by omit course_id as na
g5 <- ggplot(na.omit(type), aes(reorder(na.omit(type$A.id), desc(na.omit(type$freq))), na.omit(type$freq)))

g5 + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1)) 

idc <- subset(A$id, A$certified == 1)
type2 <- arrange(count(idc, "idc"), desc(freq))

#Completion by course id.
g6 <- ggplot(type2, aes(reorder(type2$idc, desc(type2$freq)), type2$freq))



#expanded
g6 + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
#scaled to match registration plot
g6 + geom_bar(stat = "identity") + ylim(0,100000) + theme(axis.text.x=element_text(angle=45, hjust=1))

##Try to get same order as enrollment plot (need to omit row 17 in type)
#Subset 'type' to omit row 17 (course_id), now dfs are same size.
type3 <- subset(type$freq, type$freq >= 0)
type4 <- arrange(count(na.omit(type3), "A$id"), freq)
> g6 <- ggplot(type2, aes(reorder(type2$idc, desc(type$freq)), type2$freq))
> g6 + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

g6 <- ggplot(type2, aes(reorder(type2$idc, desc(omit.na(type$freq)), type2$freq))
## Visualization Iterations (to improve communicability)

### Upload to VCS/Git. Update data dictionary as needed.
# Need more data for predictive modeling to determine which students will complete course,
# and which populations will not given current MOOC format.