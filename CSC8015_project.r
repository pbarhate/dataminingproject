##########################################################################
## Data Mining Project on Big Mart Data
## Prashant Barhate
##########################################################################

### Utility function to load packages in case its not available, install it. 
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
## Load cluster library
usePackage("ggplot2")
bigmart <- read.csv("/Users/pbarhate/mastersInCS/projects/dataScience/data/bigmart.csv"
                    , stringsAsFactors = TRUE)
## See structure of the data
str(bigmart)

summary(bigmart)
# Sales by item type
ggplot(data = bigmart,aes(x = Item_Type,y = Item_Outlet_Sales)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 70,vjust = 0.5)) +xlab("Item Type") +ylab("Sales")
# Sales by year of establishment
ggplot(data = bigmart,aes(x = Outlet_Establishment_Year,y = Item_Outlet_Sales)) + geom_histogram(stat = "identity")  + xlab("Year Of Establishment") + ylab("Sales") 
# 
ggplot(bigmart,aes(x = Item_Outlet_Sales)) + geom_histogram(colour = "Black",fill = "cornflowerblue")

## Box plot showing sales by outlet type
ggplot(bigmart,aes(x = Outlet_Type,y = Item_Outlet_Sales)) + geom_boxplot() + xlab("Outlet Type") + ylab("Item Sales")


## Cha\king missing values
table(is.na(bigmart))
colSums(is.na(bigmart))

## Cleaning of missing values in Item Weight
bigmart <- ddply(bigmart,~Item_Identifier,transform,Item_Weight = ifelse(is.na(Item_Weight), mean(Item_Weight,na.rm = TRUE),Item_Weight))
## missing values in outlet size to revalued as "other"
levels(bigmart$Outlet_Size)
levels(bigmart$Outlet_Size)[1] <- "Other"
## Correction of mismatch level in fat content
bigmart$Item_Fat_Content <- mapvalues(bigmart$Item_Fat_Content,from = c("LF","reg"),to = c("Low Fat","Regular"))
bigmart$Item_Fat_Content <- mapvalues(bigmart$Item_Fat_Content,from = c("low fat"),to = c("Low Fat"))
## If item exists in store, its visibility cannot be zero. hence clean the 0 visibility to median value
bigmart$Item_Visibility[bigmart$Item_Visibility == 0] <- NA
bigmart <- ddply(bigmart,~Item_Identifier,transform,Item_Visibility = 
                    ifelse(is.na(Item_Visibility),median(Item_Visibility,na.rm=TRUE),Item_Visibility))

## On close observation of item_identifier, you can see that items begins with DR, FD or NC. If you check these items under item_type,
## you realize that these items are categorized in terms of "Drinks", "Food" and "Non-Consumables" respectively. 
## categorizing items in  these high level category will be benefitial  hence let's add new attribute to our dataframe. 

## create a new vector and add Drink, Food and Non-Consumables categories to it
temp <- substr(bigmart$Item_Identifier,1,2)
temp <- gsub("DR","Drinks",temp) 
temp <- gsub("FD","Food",temp)
temp <- gsub("NC","Non-Consumables",temp)
## Add this vector to the dataframe
bigmart$Item_Category <- temp
bigmart$Item_Category <- factor(bigmart$Item_Category,levels = c("Drinks","Food","Non-Consumables"))

## Since we realized that there is a category called "Non-Consumable", having fat content for these items won't be correct. 
## Hence lets replace fat content for "Non-Consumable" items to "Not Applicable"..
bigmart$Item_Fat_Content <- as.character(bigmart$Item_Fat_Content)
bigmart$Item_Fat_Content <- ifelse(bigmart$Item_Category == "Non-Consumables", "Not Applicable", bigmart$Item_Fat_Content)
bigmart$Item_Fat_Content <- as.factor(bigmart$Item_Fat_Content)








