  war_df<-read.csv("war.csv")
  book_df<-read.csv("book1.csv")

  

  
  Player_cnt<-war_df$PLAYER
  Year_cnt<-war_df$YEAR

  player_szn1<-c()
  
  x=1
  y=1
  for(i in 1:200){
    pp<-Player_cnt[x]
    dd<-Year_cnt[y]
    tes231<- paste(pp, dd, sep="-")
    player_szn1<-append(player_szn1, tes231)
    x<-x+1
    y<-y+1  
  }

  war_df$player_szn<-player_szn1
  
  Player2_cnt<-book_df$Player
  Year2_cnt<-book_df$Season
  
  player_szn2<-c()
  
  x=1
  y=1
  for(i in 1:200){
    pp<-Player2_cnt[x]
    dd<-Year2_cnt[y]
    tes231<- paste(pp, dd, sep="-")
    player_szn2<-append(player_szn2, tes231)
    x<-x+1
    y<-y+1  
  }
  
  book_df$player_szn<-player_szn2
  
  
  df <- merge(x = war_df, y = book_df, by.x = "player_szn", by.y = "player_szn", all.x = TRUE)
  
  
  df<-df[!duplicated(df$player_szn), ]
  
  df<-filter(df, Rk!="NA")
  
  group3<-group_by(df,Player, Season, RK, OFF,Rk, OPS._2)
  summar<-summarize(group3)
  
