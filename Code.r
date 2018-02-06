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

#Clean up the state names.Start with Texas
levels(IDL$Contact.Mailing.State) <- sub("tx", "TX", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Texas", "TX", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("TX 77328", "TX", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("tx.", "TX", levels(IDL$Contact.Mailing.State),ignore.case = T)

#Now North Carolina
levels(IDL$Contact.Mailing.State) <- sub("nc", "NC", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("North Carolina", "NC", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("-NC", "NC", levels(IDL$Contact.Mailing.State))
levels(IDL$Contact.Mailing.State) <- sub("N.c", "NC", levels(IDL$Contact.Mailing.State))
levels(IDL$Contact.Mailing.State) <- sub("n.c.", "NC", levels(IDL$Contact.Mailing.State))


#Now Maryland
levels(IDL$Contact.Mailing.State) <- sub("Md", "MD", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Maryland", "MD", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Md.", "MD", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Now DC
levels(IDL$Contact.Mailing.State) <- sub("Dc", "DC", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("D.C", "DC", levels(IDL$Contact.Mailing.State))
levels(IDL$Contact.Mailing.State) <- sub("D.C.", "DC", levels(IDL$Contact.Mailing.State))
levels(IDL$Contact.Mailing.State) <- sub("DC.", "DC", levels(IDL$Contact.Mailing.State))

#Now Ohio
levels(IDL$Contact.Mailing.State) <- sub("Ohio", "OH", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Oh", "OH", levels(IDL$Contact.Mailing.State),ignore.case = T)


#Indiana
levels(IDL$Contact.Mailing.State) <- sub("In", "IN", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Indiana", "IN", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Indianapolis", "IN", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("INdpolis", "IN", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("INpolis", "IN", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Now Florida
levels(IDL$Contact.Mailing.State) <- sub("fl", "FL", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Florida", "FL", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Georgia
levels(IDL$Contact.Mailing.State) <- sub("Ga", "GA", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("Georgia", "GA", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Kansas
levels(IDL$Contact.Mailing.State) <- sub("Kansas", "KS", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("ks", "KS", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Louisiana
levels(IDL$Contact.Mailing.State) <- sub("La", "LA", levels(IDL$Contact.Mailing.State), ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("La.", "LA", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Mass
levels(IDL$Contact.Mailing.State) <- sub("ma", "MA", levels(IDL$Contact.Mailing.State), ignore.case = T)

#Colorado
levels(IDL$Contact.Mailing.State) <- sub("Colorado", "CO", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Iowa
levels(IDL$Contact.Mailing.State) <- sub("Iowa", "IA", levels(IDL$Contact.Mailing.State), ignore.case = T)
#Kentucky
levels(IDL$Contact.Mailing.State) <- sub("Ky", "KY", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Alabama
levels(IDL$Contact.Mailing.State) <- sub("alaama", "AL", levels(IDL$Contact.Mailing.State),ignore.case = T)
#California
levels(IDL$Contact.Mailing.State) <- sub("California", "CA", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Michigan
levels(IDL$Contact.Mailing.State) <- sub("mi", "MI", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("michigan", "MI", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Mississippi
levels(IDL$Contact.Mailing.State) <- sub("mississippi", "MS", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Nebraska
levels(IDL$Contact.Mailing.State) <- sub("ne", "NE", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Nevada
levels(IDL$Contact.Mailing.State) <- sub("Nevada", "NV", levels(IDL$Contact.Mailing.State),ignore.case = T)
#New Mexico
levels(IDL$Contact.Mailing.State) <- sub("New Mexico", "NM", levels(IDL$Contact.Mailing.State),ignore.case = T)
#New York
levels(IDL$Contact.Mailing.State) <- sub("New York", "NY", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("New Yotk", "NY", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("ny", "NY", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("nyc", "NY", levels(IDL$Contact.Mailing.State),ignore.case = T)
#North Dakota
levels(IDL$Contact.Mailing.State) <- sub("north dakota", "ND", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Oklahoma
levels(IDL$Contact.Mailing.State) <- sub("oklaoma", "OK", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Pennsylvania
levels(IDL$Contact.Mailing.State) <- sub("pa", "PA", levels(IDL$Contact.Mailing.State),ignore.case = T)
#South Carolina
levels(IDL$Contact.Mailing.State) <- sub("sc", "SC", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("south carolina", "SC", levels(IDL$Contact.Mailing.State),ignore.case = T)
#South Dakota
levels(IDL$Contact.Mailing.State) <- sub("South Dakota", "SD", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Utah
levels(IDL$Contact.Mailing.State) <- sub("ut", "UT", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Virginia
levels(IDL$Contact.Mailing.State) <- sub("va", "VA", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("virginia", "VA", levels(IDL$Contact.Mailing.State),ignore.case = T)
levels(IDL$Contact.Mailing.State) <- sub("virgina", "VA", levels(IDL$Contact.Mailing.State),ignore.case = T)
#Washington
levels(IDL$Contact.Mailing.State) <- sub("Washington", "WA", levels(IDL$Contact.Mailing.State),ignore.case = T)
#West Virginia
levels(IDL$Contact.Mailing.State) <- sub("wv", "WV", levels(IDL$Contact.Mailing.State),ignore.case = T)

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
