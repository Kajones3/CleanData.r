allleads <- read.csv("cityleads.csv", header = T, stringsAsFactors = T)

library(tidyverse)
summary(allleads)


#Make the dates
allleads$Created.Date <- as.Date(allleads$Created.Date, format="%m/%d/%Y")
allleads$Contacted.Date <- as.Date(allleads$Contacted.Date, format="%m/%d/%Y")
allleads$Scheduled.Date <- as.Date(allleads$Scheduled.Date, format="%m/%d/%Y")
allleads$Showed.Date <- as.Date(allleads$Showed.Date, format="%m/%d/%Y")
allleads$Enrollment.Date <- as.Date(allleads$Enrollment.Date, format="%m/%d/%Y")

#Create a separate look at IDL
IDL <- filter(allleads, Method == "Full IDL" & Enrollment.Date != 0 & Admissions.Status == "Closed - Enrolled")

#What do we need to clean?
summary(IDL$Contact.Mailing.State)

#Upcase variable
#Strip out characters in variable
#Clean state names

#Alabama
levels(military$Contact.Mailing.State) <- sub("alaama", "AL", levels(military$Contact.Mailing.State),ignore.case = T)

#Arizona
levels(military$Contact.Mailing.State) <- sub("az", "AZ", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Arizona", "AZ", levels(military$Contact.Mailing.State), ignore.case = T)

#California
levels(military$Contact.Mailing.State) <- sub("California", "CA", levels(military$Contact.Mailing.State),ignore.case = T)

#Colorado
levels(military$Contact.Mailing.State) <- sub("Colorado", "CO", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Co", "CO", levels(military$Contact.Mailing.State),ignore.case = T)

#DC
levels(military$Contact.Mailing.State) <- sub("Dc", "DC", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("D.C", "DC", levels(military$Contact.Mailing.State))
levels(military$Contact.Mailing.State) <- sub("D.C.", "DC", levels(military$Contact.Mailing.State))
levels(military$Contact.Mailing.State) <- sub("DC.", "DC", levels(military$Contact.Mailing.State))

#Florida
levels(military$Contact.Mailing.State) <- sub("fl", "FL", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Florida", "FL", levels(military$Contact.Mailing.State), ignore.case = T)

#Georgia
levels(military$Contact.Mailing.State) <- sub("Ga", "GA", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Georgia", "GA", levels(military$Contact.Mailing.State), ignore.case = T)

#Idaho
levels(military$Contact.Mailing.State) <- sub("Idaho", "ID", levels(military$Contact.Mailing.State), ignore.case = T)

#Indiana
levels(military$Contact.Mailing.State) <- sub("In", "IN", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Indiana", "IN", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Indianapolis", "IN", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("INdpolis", "IN", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("INpolis", "IN", levels(military$Contact.Mailing.State), ignore.case = T)

#Iowa
levels(military$Contact.Mailing.State) <- sub("Iowa", "IA", levels(military$Contact.Mailing.State), ignore.case = T)

#Kentucky
levels(military$Contact.Mailing.State) <- sub("Ky", "KY", levels(military$Contact.Mailing.State),ignore.case = T)

#Kansas
levels(military$Contact.Mailing.State) <- sub("Kansas", "KS", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("ks", "KS", levels(military$Contact.Mailing.State), ignore.case = T)

#Louisiana
levels(military$Contact.Mailing.State) <- sub("La", "LA", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("La.", "LA", levels(military$Contact.Mailing.State), ignore.case = T)

#Maryland
levels(military$Contact.Mailing.State) <- sub("Md", "MD", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Maryland", "MD", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Md.", "MD", levels(military$Contact.Mailing.State), ignore.case = T)


#Mass
levels(military$Contact.Mailing.State) <- sub("ma", "MA", levels(military$Contact.Mailing.State), ignore.case = T)

#Michigan
levels(military$Contact.Mailing.State) <- sub("mi", "MI", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("michigan", "MI", levels(military$Contact.Mailing.State),ignore.case = T)


#Mississippi
levels(military$Contact.Mailing.State) <- sub("mississippi", "MS", levels(military$Contact.Mailing.State),ignore.case = T)

#Nebraska
levels(military$Contact.Mailing.State) <- sub("ne", "NE", levels(military$Contact.Mailing.State),ignore.case = T)

#Nevada
levels(military$Contact.Mailing.State) <- sub("Nevada", "NV", levels(military$Contact.Mailing.State),ignore.case = T)

#New Jersey
levels(military$Contact.Mailing.State) <- sub("New Jersey", "NJ", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("nj", "NJ", levels(military$Contact.Mailing.State),ignore.case = T)

#New Mexico
levels(military$Contact.Mailing.State) <- sub("New Mexico", "NM", levels(military$Contact.Mailing.State),ignore.case = T)

#New York
levels(military$Contact.Mailing.State) <- sub("New York", "NY", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("New Yotk", "NY", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("ny", "NY", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("nyc", "NY", levels(military$Contact.Mailing.State),ignore.case = T)

#North Carolina
levels(military$Contact.Mailing.State) <- sub("nc", "NC", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("North Carolina", "NC", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("-NC", "NC", levels(military$Contact.Mailing.State))
levels(military$Contact.Mailing.State) <- sub("N.c", "NC", levels(military$Contact.Mailing.State))
levels(military$Contact.Mailing.State) <- sub("n.c.", "NC", levels(military$Contact.Mailing.State))

#North Dakota
levels(military$Contact.Mailing.State) <- sub("north dakota", "ND", levels(military$Contact.Mailing.State),ignore.case = T)

#Ohio
levels(military$Contact.Mailing.State) <- sub("Ohio", "OH", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Oh", "OH", levels(military$Contact.Mailing.State),ignore.case = T)

#Oklahoma
levels(military$Contact.Mailing.State) <- sub("oklaoma", "OK", levels(military$Contact.Mailing.State),ignore.case = T)

#Pennsylvania
levels(military$Contact.Mailing.State) <- sub("pa", "PA", levels(military$Contact.Mailing.State),ignore.case = T)

#South Carolina
levels(military$Contact.Mailing.State) <- sub("sc", "SC", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("south carolina", "SC", levels(military$Contact.Mailing.State),ignore.case = T)

#South Dakota
levels(military$Contact.Mailing.State) <- sub("South Dakota", "SD", levels(military$Contact.Mailing.State),ignore.case = T)

#Texas
levels(military$Contact.Mailing.State) <- sub("tx", "TX", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("Texas", "TX", levels(military$Contact.Mailing.State), ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("TX 77328", "TX", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("tx.", "TX", levels(military$Contact.Mailing.State),ignore.case = T)

#Utah
levels(military$Contact.Mailing.State) <- sub("ut", "UT", levels(military$Contact.Mailing.State),ignore.case = T)

#Virginia
levels(military$Contact.Mailing.State) <- sub("va", "VA", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("virginia", "VA", levels(military$Contact.Mailing.State),ignore.case = T)
levels(military$Contact.Mailing.State) <- sub("virgina", "VA", levels(military$Contact.Mailing.State),ignore.case = T)

#Washington
levels(military$Contact.Mailing.State) <- sub("Washington", "WA", levels(military$Contact.Mailing.State),ignore.case = T)

#West Virginia
levels(military$Contact.Mailing.State) <- sub("wv", "WV", levels(military$Contact.Mailing.State),ignore.case = T)

#Wisconsin
levels(military$Contact.Mailing.State) <- sub("Wisconsin", "WI", levels(military$Contact.Mailing.State),ignore.case = T)

summary(military$Contact.Mailing.State)

#What do we need to clean?
summary(IDL$Contact.Mailing.State)

#Done cleaning state names. Which states are the top 10?
sort(table(IDL$Contact.Mailing.State), decreasing = T)

#Let's look at Texas and see city names.
Texas <- filter(IDL, Contact.Mailing.State == "TX")

#That's not helpful
#Let's look at the top 5 markets.
summary(Texas$Market, maxsum = 5)

write.csv(Texas, file = "TexasLeads.csv")
