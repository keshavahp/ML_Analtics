# Investment Case Study Analyiss Code 
 
# Load libraries 
library(dplyr)
library(tidyr)

#########################################################
##############      CHECK POINT 1         ###############
#########################################################

# Load the companies and rounds datainto two data frames
# and name them companies and rounds2 respectively.
companies <- read.delim("companies.txt", header = T, sep = "\t", dec = ".", stringsAsFactors = F, 
                        na.strings = c("", " ", "NA"), encoding = "Latin-1")

rounds2 <- read.csv("rounds2.csv", header = T, stringsAsFactors = F, 
                         na.strings = c("", " ", "NA"), encoding = "Latin-1")

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# How many unique companies are present in rounds2?
length(unique(rounds2$company_permalink))

# How many unique companies are present in companies?
length(unique(companies$permalink))

# In the companies data frame, which column can be used as
# the unique key for each company? Write the name of the column.
# "permalink" is unique ID of a company. so it can be used as unique key.

# Are there any companies in the rounds2 file which are not present in companies? 
# Answer yes or no
setdiff(companies$permalink, rounds2$company_permalink)
# Merge the two data frames so that all variables (columns) in the companies frame
# are added to the rounds2 data frame. Name the merged frame master_frame.
# How many observations are present in master_frame?
master_frame <- merge (companies, rounds2, by.x = "permalink", by.y = "company_permalink", all = T)

#########################################################
##############      CHECK POINT 2         ###############
#########################################################
# Calculate the average investment amount for each of the four
# funding types (venture, angel, seed, and private equity)
funding_types <- c("venture", "angel", "seed", "private_equity")
average_amount_funding_types <- filter(master_frame , funding_round_type %in% funding_types) %>%
  group_by(funding_round_type) %>%
  summarise(Average_Investment = mean(raised_amount_usd , na.rm = TRUE)) %>%
  arrange(desc(Average_Investment)) 


average_amount_funding_types[which(average_amount_funding_types$Average_Investment > 5000000 &
                        average_amount_funding_types$Average_Investment < 15000000),]

#########################################################
##############      CHECK POINT 3         ###############
#########################################################

country_wise_funding <- na.omit(master_frame, cols = country_code) %>%
  filter(funding_round_type == "venture") %>%
  group_by(country_code) %>%
  summarise(Total_Investment = sum(raised_amount_usd, na.rm = T)) %>%
  arrange(desc(Total_Investment))

top9 <- country_wise_funding[1:9,]

# Top English Speaking : USA
# Top English Speaking : GBR
# Top English Speaking : IND

#########################################################
##############      CHECK POINT 4         ###############
#########################################################
mapping <- read.csv("mapping.csv", header = T, check.names = FALSE, na.strings=c("","NA"))%>%
   subset( !is.na(category_list))

mapping <- gather(mapping , category , value , 
                          "Automotive & Sports":"Social, Finance, Analytics, Advertising")
mapping <- mapping[!(mapping$value == 0),]
# Now we  also remove the last column my_value from the data frame. 
mapping <- mapping[,-3]
mapping$category_list <- gsub("0" , "na" ,mapping$category_list)

master_frame$category_list <- tolower(master_frame$category_list)
mapping$category_list <- tolower(mapping$category_list)

#the vector is made by cross refrencing the names in the provided pdf of english speaking countries
#internet search is used to get the related country codes
english_top9 <- c("USA","GBR","IND","CAN")

#filtering only english speaking countries from top9
top9_english_speaking <- top9[top9$country_code %in%  english_top9,]
top9_english_speaking


#getting top 3 countries most suited for investment with total amount invested there
top3 <- head(top9_english_speaking,3)
top3

# make a seprate coloum for primary sector 
master_frame <- separate(master_frame, category_list, into=c("primary_sector"), sep = "\\|.*", extra = "drop")
merged_frame <- merge(master_frame, mapping, by.x = "primary_sector", by.y = "category_list", all = T, sort = T)

#creating a filtered data frame with the target set
# for funding type venture
#with raised amount from 5M to 15M
#and from top 3 countries i.e. USA,GBR.IND
country_sec_frame <- filter(merged_frame, funding_round_type == "venture",raised_amount_usd >= 5000000, raised_amount_usd <= 15000000,
                            country_code %in% top3$country_code)


#########################################################
##############      CHECK POINT 5         ###############
#########################################################
##### country 1 --- USA
D1 <- filter(merged_frame, country_code == "USA", funding_round_type == "venture",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)
# total inestment(amount) for D1
D1_total_investment <- summarise (D1, sum(raised_amount_usd, na.rm = T))

# total number of investment sectorwise for country 1
D1_no_of_inv <- group_by(D1, category) %>% 
  summarise(no_of_investments = n()) %>% 
  arrange(desc(no_of_investments)) 

D1_total_number_of_investment <- sum(D1_no_of_inv$no_of_investments)

##### country 2 --- GBR
D2 <- filter(merged_frame, country_code == "GBR", funding_round_type == "venture",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)
# total inestment(amount) for D1
D2_total_investment <- summarise (D2, sum(raised_amount_usd, na.rm = T))

#total number of investment sectorwise for country 2
D2_no_of_inv <- group_by(D2, category) %>% 
  summarise(no_of_investments = n()) %>% 
  arrange(desc(no_of_investments)) 

D2_total_number_of_investment <- sum(D2_no_of_inv$no_of_investments)
##### country 3 --- IND

D3 <- filter(merged_frame, country_code == "IND", funding_round_type == "venture",
             raised_amount_usd >= 5000000, raised_amount_usd <= 15000000)
# total inestment(amount) for D3
D3_total_investment <- summarise (D3, sum(raised_amount_usd, na.rm = T))

# total number of investment sectorwise for country 3
D3_no_of_inv <- group_by(D3, category) %>% 
  summarise(no_of_investments = n()) %>% 
  arrange(desc(no_of_investments)) 

D3_total_number_of_investment <- sum(D3_no_of_inv$no_of_investments)

#  For the top sector count-wise (point 3), which company received the highest investment?

 D1_first_company_name <- filter(D1 , category == D1_no_of_inv[1,]$category) %>% 
    group_by(name) %>% 
    summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
    arrange(desc(Tot_amount_raised)) %>% 
    head(n=1) 
 
 D2_first_company_name <- filter(D2 , category == D2_no_of_inv[1,]$category) %>% 
   group_by(name) %>% 
   summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
   arrange(desc(Tot_amount_raised)) %>% 
   head(n=1) 
 
 D3_first_company_name <- filter(D3 , category == D3_no_of_inv[1,]$category) %>% 
   group_by(name) %>% 
   summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
   arrange(desc(Tot_amount_raised)) %>% 
   head(n=1) 
 

  
# For the second-best sector count-wise (point 4), which company received the highest investment?
 D1_second_company_name <- filter(D1 , category == D1_no_of_inv[2,]$category) %>% 
   group_by(name) %>% 
   summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
   arrange(desc(Tot_amount_raised)) %>% 
   head(n=1)
 
 D2_second_company_name <- filter(D2 , category == D2_no_of_inv[2,]$category) %>% 
   group_by(name) %>% 
   summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
   arrange(desc(Tot_amount_raised)) %>% 
   head(n=1)
 
 D3_second_company_name <- filter(D3 , category == D3_no_of_inv[2,]$category) %>% 
   group_by(name) %>% 
   summarise(Tot_amount_raised = sum(raised_amount_usd)) %>% 
   arrange(desc(Tot_amount_raised)) %>% 
   head(n=1)
 
 #writing the files to the disk to be used in Tableau for plotting
 write.csv(merged_frame,"master_frame_final.csv")
 write.csv(country_sec_frame,"country_sec_frame.csv")
 write.csv(top9,"top9.csv")
 
 
 