#to combine two datasets based on a value
songs <- read.csv("J.csv")
songs$SongNumber <- NULL
songs$ArtistLatitude <- NULL
songs$ArtistLongitude <-NULL
songs$SongID <- NULL
songs$ArtistID <- NULL
songs$ArtistLocation <- NULL
songs$ArtistName <- NULL
songs$mbID <- NULL
songs$AlbumID <- NULL

charts <- read.csv("billboard_lyrics_1964-2015.csv")
#convert to lower case
songs$Title <- tolower(songs$Title)
charts$Song <- tolower(charts$Song)
#check if the songs were present on the billboard chart and create two new dataframes
required_df <- songs[songs$Title %in% charts$Song,  ]
required_chart <- charts[charts$Song %in% required_df$Title,c(1,2,4)]
required_df[,"rank"] <- 0
required_df[,"hit"] <- 0
#find the rank and year of the songs
for( i in c(1:nrow(required_df)))
{for(j in c(1:nrow(required_chart)))
{ if(required_df$Title[i] == required_chart$Song[j])
  { required_df$rank[i] = required_chart$Rank[j]
    required_df$Year[i] = required_chart$Year[j]
    required_df$hit[i] = 1
  }
}
}
#print the next line once
#mydata <- required_df

mydata <- rbind(mydata, required_df)
#remove duplicated values
mydata <-mydata[!duplicated(mydata), ]
#write.csv(mydata,'asmi.csv') to save df into csv


