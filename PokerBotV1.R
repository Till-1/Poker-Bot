install.packages("tidyverse")
library(tidyverse)
#poker as a function
poker <- function(hand,spieler,runde,mitte,vermögen,pot,tocall,sb,bb){
  werte <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  farben <- c("H","S","D","C")
  karten <- c("2H","2S","2D","2C","3H","3S","3D","3C","4H","4S","4D","4C","5H","5S","5D","5C","6H","6S","6D","6C","7H","7S","7D","7C","8H","8S","8D","8C","9H","9S","9D","9C","TH","TS","TD","TC","JH","JS","JD","JC","QH","QS","QD","QC","KH","KS","KD","KC","AH","AS","AD","AC")  
  karten <- karten[karten!=hand[1]]
  karten <- karten[karten!=hand[2]]
  
  
  poker2 <- matrix(,nrow = 20000,ncol = spieler)
 
  for(j in 1:20000){
    if(runde==1){
      karten <- karten[karten!=mitte[1]]
      karten <- karten[karten!=mitte[2]]  
      karten <- karten[karten!=mitte[3]]
    shuffle <- sample(karten,length(karten))
    
    playedCards <- sample(shuffle, size = (spieler*2+5))
    poker2[j,1]<- paste( hand[1], hand[2] , playedCards[1] , playedCards[2], playedCards[3],playedCards[4],playedCards[5])
    for(k in 2:spieler){
    for (l in seq(6,length(playedCards)-1, by= 2)){
    poker2[j,k] <- paste(playedCards[1], playedCards[2] , playedCards[3] , playedCards[4], playedCards[5],playedCards[l],playedCards[l+1])
    }
    }
    }
    if(runde==2){
      karten <- karten[karten!=mitte[1]]
      karten <- karten[karten!=mitte[2]]  
      karten <- karten[karten!=mitte[3]]
      
      
      shuffle <- sample(karten,length(karten))
      
      playedCards <- sample(shuffle, size = (spieler*2+2))
      poker2[j,1]<- paste( hand[1], hand[2] ,  mitte[1], mitte[2],mitte[3],playedCards[1] ,playedCards[2])
      for(k in 2:spieler){
        for (l in seq(3,length(playedCards)-1, by= 2)){
          poker2[j,k] <- paste(mitte[1], mitte[2],mitte[3], playedCards[1], playedCards[2] , playedCards[l] , playedCards[l+1])
        }
      } 
      
      
    }
    if(runde==3){
      karten <- karten[karten!=mitte[1]]
      karten <- karten[karten!=mitte[2]]  
      karten <- karten[karten!=mitte[3]]
      karten <- karten[karten!=mitte[4]]
      karten <- karten[karten!=mitte[5]]
      shuffle <- sample(karten,length(karten))
      
      playedCards <- sample(shuffle, size = (spieler*2+1))
      poker2[j,1]<- paste( hand[1], hand[2] ,  mitte[1], mitte[2],mitte[3],mitte[4] ,playedCards[1])
      for(k in 2:spieler){
        for (l in seq(2,length(playedCards)-1, by= 2)){
          poker2[j,k] <- paste(mitte[1], mitte[2],mitte[3], mitte[4], playedCards[1] , playedCards[l] , playedCards[l+1])
        }
      } 
      
      
    }
    if(runde>3){
      karten <- karten[karten!=mitte[1]]
      karten <- karten[karten!=mitte[2]]  
      karten <- karten[karten!=mitte[3]]
      karten <- karten[karten!=mitte[4]]
      karten <- karten[karten!=mitte[5]]
      shuffle <- sample(karten,length(karten))
      
      playedCards <- sample(shuffle, size = (spieler*2))
      poker2[j,1]<- paste( hand[1], hand[2] ,  mitte[1], mitte[2],mitte[3],mitte[4] ,mitte[5])
      for(k in 2:spieler){
        for (l in seq(1,length(playedCards)-1, by= 2)){
          poker2[j,k] <- paste(mitte[1], mitte[2],mitte[3], mitte[4], mitte[5] , playedCards[l] , playedCards[l+1])
        }
      } 
      
      
    } 
  }
  
  for (i in 1:spieler){
    for(j in 1:length(poker2[,1])){
      a <- str_count(poker2[j,i],werte)
      v <- str_count(poker2[j,i],farben)
      l <- a[a == 2]
      z <- a[a==3]
      b <- which(a!=0)
      
      #royal flush
      
      if(a[9]==1 & a[10]==1 & a[11]==1 & a[12]==1 & a[13]==1 & max(v)>=5) {
        
        flush <- vector()
        platz <- str_replace_all(poker2[j,i], fixed(" "), "")
        m <- sapply(seq(from=1, to=nchar(platz), by=2), function(y) substr(platz, y, y+1))
        
        x <- which.max(v)
        for(k in 1:length(m)){
          
          
          if(which.max(str_count(m[k],farben)) == x){
            flush <- append(flush,m[k])
          }
          
          
        }
        t <- c(which.max(str_count(flush[1],werte)),which.max(str_count(flush[2],werte)),which.max(str_count(flush[3],werte)),which.max(str_count(flush[4],werte)),which.max(str_count(flush[5],werte)))
        t <- sort(t,decreasing = T)
        if((t[1]-t[2])==1 & (t[2]-t[3])==1 & (t[3]-t[4])==1 & (t[4]-t[5])==1 & t[1]==13 ){
          
          poker2[j,i] <- 1000    
          next       
        }
        
        
        
        
        
        
        
        
        
        
      }
      
      
      
      
      # Straight Flush
      
      
      if(max(v) >= 5) {
        flush <- vector()
        platz <- str_replace_all(poker2[j,i], fixed(" "), "")
        m <- sapply(seq(from=1, to=nchar(platz), by=2), function(y) substr(platz, y, y+1))
        
        x <- which.max(v)
        for(k in 1:length(m)){
          
          
          if(which.max(str_count(m[k],farben)) == x){
            flush <- append(flush,m[k])
          }
          
          
        }
        t <- c(which.max(str_count(flush[1],werte)),which.max(str_count(flush[2],werte)),which.max(str_count(flush[3],werte)),which.max(str_count(flush[4],werte)),which.max(str_count(flush[5],werte)))
        t <-sort(t,decreasing = T)
        if((t[1]-t[2])==1 & (t[2]-t[3])==1 & (t[3]-t[4])==1 & (t[4]-t[5])==1  ){
          
          poker2[j,i] <- 950 +  t[1]     
          next       
        }
        
      }            
      
      
      
      #vierling
      if(max(a) == 4 ) {
        
        b <- max(which(a!=0 & a!= 4))
        
        poker2[j,i] <- 800 +  which.max(a)  + max(b)/100 
        
        next   
      }
      
      
      #Full House
      
      if((max(a)==3 & max(a[a!=max(a)]) ==2)| sum(z) == 6){
        poker2[j,i] <- 850 + max(which(a==3)) + min(which(a>=2))/100
        next
      }
      
      #Flush
      
      if(max(v) >= 5) {
        
        
        c <- b[b!= max(b)]
        
        d <- c[c!= max(c)]
        e <-  d[d!= max(d)]
        f <- e[e!= max(e)]
        
        poker2[j,i] <- 800 +  max(b) + max(c)/100 + max(d)/1000 + max(e)/10000 + max(f)/100000
        
        next
      } 
      
      
      
      #Straight  
      if(length(b)>=5){
        if(length(b)==7) {
          
          if((b[7]-b[6])==1 & (b[6]-b[5])==1 & (b[5]-b[4])==1 & (b[4]-b[3])==1  ){
            
            poker2[j,i] <- 750 +  b[7]     
            next       
          }
          if((b[6]-b[5])==1 & (b[5]-b[4])==1 & (b[4]-b[3])==1 & (b[3]-b[2])==1  ){
            
            poker2[j,i] <- 750 + b[6]    
            next        
            
            
          }
          if((b[5]-b[4])==1 & (b[4]-b[3])==1 & (b[3]-b[2])==1 & (b[2]-b[1])==1  ){
            
            poker2[j,i] <- 750 +  b[5]     
            next      
          }  
        }
        if(length(b)==6) {
          
          
          if((b[6]-b[5])==1 & (b[5]-b[4])==1 & (b[4]-b[3])==1 & (b[3]-b[2])==1  ){
            
            poker2[j,i] <- 750 + b[6]    
            next        
            
            
          }
          if((b[5]-b[4])==1 & (b[4]-b[3])==1 & (b[3]-b[2])==1 & (b[2]-b[1])==1  ){
            
            poker2[j,i] <- 750 +  b[5]     
            next      
          }  
        }
        if(length(b)==5) {
          
          
          
          if((b[5]-b[4])==1 & (b[4]-b[3])==1 & (b[3]-b[2])==1 & (b[2]-b[1])==1  ){
            
            poker2[j,i] <- 750 +  b[5]     
            next      
          }  
        } 
        
        
        
        
        
      }
      
      #three of a kind
      
      
      
      if(max(a) == 3 ) {
        
        b <- which(a!=0 & a!= 3) 
        c <- b[b!= max(b)]
        
        poker2[j,i] <- 700 +  which.max(a)  + max(b)/100 + max(c)/1000 
        
        next   
      }
      
      
      
      
      
      
      
      
      
      
      #two times two of a kind
      
      
      
      if(sum(l) >= 4){ # >= 3 paare möglich + max ohne maximun 
        
        b <- which(a!=0 & a!= 2) 
        c <-which(a == 2)
        d <- c[c!=max(c)]
        e <- d[d!=max(d)]
        poker2[j,i] <- 650 +  max(c) + max(d)/100 + max(b,e)/1000 
        
        
        next
      }
      
      
      
      #pair
      
      if(sum(l) == 2 & max(a)==2){
        b <- which(a!=0 & a!= 2) 
        c <- b[b!= max(b)]
        
        d <- c[c!= max(c)]
        
        
        poker2[j,i] <- 600 +  which(a == 2) + max(b)/100+ max(c)/1000 + max(d)/10000  
        next
        
      }
      
      #high card
      if(max(a) == 1) {
        b <- which(a!=0) 
        c <- b[b!= max(b)]
        
        d <- c[c!= max(c)]
        e <-  d[d!= max(d)]
        f <- e[e!= max(e)]
        poker2[j,i] <- 550 +  max(b) + max(c)/100 + max(d)/1000 + max(e)/10000 + max(f)/100000 
        next
      }       
      
      
      
      
    }
  }
  gewinn <- 0  
  for(j in 1 :length(poker2[,1]))  {
    if(which.max(poker2[j,]) == 1){
      gewinn <- gewinn +1      
    }      
  }  
  gewinnwkeit <- gewinn/length(poker2[,1])
  
    raise <- 0
    call <-0
    fold <- 0
    if(sb == F){
    if((tocall/(pot+tocall))<gewinnwkeit){
      jj <- rbinom(10,2,prob = gewinnwkeit)
      for( k in 1:length(jj)){
        if(jj[k] == 2){
          raise <- raise +1
        }
        if(jj[k] == 1){
          call <- call +1
        }
        if(jj[k] == 0){
          fold <- fold +1
        }
        
      }
      taktik <- c(raise,call,fold)
      
      if(which.max(taktik)== 1){
        rgeom(1, prob = (1-gewinnwkeit))
        return(floor(rgeom(1, prob = (1-gewinnwkeit))*(vermögen/(pot+tocall))))
      }
      if(which.max(taktik) == 2){
        
        return(tocall)
      }
      if(which.max(taktik)==3){
        return(0)
      
    }else{
      setz <-sample(c(1,2))
      if(setz==1){
        return(tocall)
      }
      if(setz==2){
        return(0)
      }
    }
    }
    
    }
    lol <- paste(hand[1],hand[2])
    a <- str_count(lol,werte)
    
    
  
  if(sb == T && max(a) == 2){
 
    
      if(which.max(a)==13){
        
        return("ALL INNN")
      }
      if(which.max(a)==12){
        
        return((477/3)*bb)
      }
      if(which.max(a)==11){
        
        return((239/3)*bb)
      }
      if(which.max(a)==10){
        
        return((159.6/3)*bb)
      }
      
      if(which.max(a)==9){
        
        return((119.9/3)*bb)
      }
      if(which.max(a)==8){
        
        return((95.7/3)*bb)
      }
      if(which.max(a)==7){
        
        return((79.6/3)*bb)
      }
      if(which.max(a)==6){
        
        return((67.4/3)*bb)
      }
      if(which.max(a)==5){
        
        return((57.7/3)*bb)
      }
      if(which.max(a)==4){
        
        return((49.3/3)*bb)
      }
      if(which.max(a)==3){
        
        return((41/3)*bb)
      }
      if(which.max(a)==2){
        
        return((32.7/3)*bb)
      }
      if(which.max(a)==1){
        
        return((24.4/3)*bb)
      }
      
      
      
    
    
    
    
    
  }
  
  }
 
  

#hand,spieler,runde,mitte,vermögen,pot,tocall,sb,bb

poker(c("AS", "AD"),2,2,c("3H", "8C" , "9S"),1000,40,10,F,20)
# pot 50 
v
pokergame <- function (spieler ) {
  pot <-0
  rundeaktiv <- 1
  
  Zwischenstand <- rep(1000,spieler)
  gesamt <- sum(Zwischenstand)
 while(max(Zwischenstand)<gesamt){
  while(rundeaktiv == 1){
    for( j in 1:spieler){
      einsatz <- as.numeric(readline("Der spieler setzt"))
    Zwischenstand[j] <- Zwischenstand[j] - einsatz
    print(Zwischenstand[j])
    pot <- pot + einsatz
    print(pot)
    rundeaktiv <- as.numeric(readline("noch aktiv?"))
  }
  
  if(rundeaktiv == 0 ){
    gewinner <- as.numeric(readline("Wer hat gewonnen?"))
    Zwischenstand[gewinner] <- Zwischenstand[gewinner] + pot 
   print(Zwischenstand)
   rundeaktiv <- 1
  }
  
  
  
 }
 }
}
pokergame(3)


