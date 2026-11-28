library(tidyverse)
library(dplyr) 
library(ggplot2)

#region #Loading both csv files#######
df1 <- read.csv("data/raw/Phishing_validation_emails_v3.csv")
df2 <- read.csv("data/raw/phishing_email_1.csv")
#endregion

#region #Extracting "Categories" into variables##########

#To extract categories str_extract() will be used, with the use of regular expressions
#help from: https://r-statistics.co/stringr-str_extract-in-R.html
#regex table from: https://r-statistics.co/R-Regex-Cheat-Sheet-stringr.html

df1 <- df1 |>
  #Categories I have chosen: Recipient, Money, Buzzwords, Codes, URLs
  #Added comment to explain my logic behind each regex
  mutate(
    Recipient = str_extract(Email.Text, "(?<=Dear |Hi |Hello )\\w+"), #matches a word at the begint of the string that is preceded
                                                      #by the words dear, hi  or hello, //w+ makes sure that it only
                                                      #takes the characters or digits (in the case of 
                                                      #usernames) before a space, dot, coma, etc.
    Money  = str_extract(Email.Text,"\\$\\d+"), #matches any consecutive number of digits preceded by
                                          # a $ sign
    Buzzwords = str_extract(Email.Text,"offer|urgent|limited|verify|confirm|suspended"),
                            #Matches any of the words that are present in the list
    Codes  = str_extract(Email.Text, "(?<!\\$)\\b(?=[A-Za-z0-9]*[0-9])[A-Za-z0-9]+\\b"),
                                #Matches any consecutive number of characters or digits. *[0-9]
                                #makes sure that there will be at least one number  
                                #so that the regex will not match English words. 
                                #(?<!\\$) makes sure it is not a monetary amount
    URLs  = str_extract(Email.Text, "https?://[^ ]+") #Matches any number of any character that is
                                                #preceded by https:// or http:// ([^ ] excludes spaces)
  )

#Changing names of columns in df2 to match df1

df2 <- df2 |>
  rename(Email.Text = text_combined)

df2 <- df2 |>
  rename(Email.Type = label)

df2 <- df2 |>
  mutate(
    Recipient = str_extract(Email.Text, "(?<=Dear |Hi |Hello )\\w+"), 
    Money  = str_extract(Email.Text,"\\$\\d+"), 
    Buzzwords = str_extract(Email.Text,"offer|urgent|limited|verify|confirm|suspended"),
    Codes  = str_extract(Email.Text, "(?<!\\$)\\b(?=[A-Za-z0-9]*[0-9])[A-Za-z0-9]+\\b"),
    URLs  = str_extract(Email.Text, "https?://[^ ]+")
  )


#endregion 

#region #Cleaning the data#################

#NA are not dropped because they can count as useful information(emails without some of the
#variables could be considered safe emails)

#Cleaning up the Recipient variable (the regex I used can confuse "there" from "Hi there"
#as a name. So any theres will be changed to NA)
df1 <- df1 |>
  mutate(Recipient = na_if(Recipient, "there"))#na_if function gotten from https://dplyr.tidyverse.org/reference/na_if.html

#Cleaning the Codes variable (string that haves less than 3 characters may represent dates, like
#"25" in "October 25")
df1 <- df1 |>
  mutate(Codes = replace(Codes, str_length(df1$Codes)<3, NA))
#endregion

#region #Saving new CSVs#############

write.csv(df1, "data/clean/phishing_email1.csv", row.names = F)
write.csv(df2, "data/clean/phishing_email2.csv", row.names = F)
#endregion

#region #Creating chart############################
no_links <- df1 |> 
  filter(is.na(URLs))

safe_obs_nolinks <- no_links |>
  filter(Email.Type == "Safe Email") |>
  count() |>
  as.integer()

phishing_obs_nolinks <- no_links |>
  filter(Email.Type == "Phishing Email") |>
  count() |>
  as.integer()

no_links <- tibble(
  label = c("Safe Email", "Phishing Emial"),
  num = c(safe_obs_nolinks, phishing_obs_nolinks)
)

ggplot(no_links, aes(x = label, y = num)) +
  geom_col(fill = "blue") +
  labs(title = "Observation types for email with no links", x = "Category", y = "Number of emails")

#endregion
