#' A Frequency Analysis Function
#'
#' This function conducts a frequency analysis on text.
#' @param
#' @keywords Caeser Cipher
#' @export
#' @examples
#' freq.analysis()

freq.analysis <- function() {

    readline(cat("Press [Enter]"))
    cat("Enter the text to analyze","\n", 'Enclose text with \" \"')
    text <- scan(file = "", what = "character", nmax = 1, sep = "")

    readline(cat("Press [Enter]"))
    cat("Convert integers to text?","\n","i.e. '10' converted to 'one','zero'","\n","Type: Yes or No")
    convert <- scan(file = "", what = "character", nmax = 1)
    ifelse(convert=='Yes'|convert=='Y'|convert=='y'|convert=="yes",convert<-1,convert<-0)

    if(convert==1){
      text<-unlist(strsplit(text,split = ""))
      text.numbers<-matrix(ncol=10,nrow=2,data=NA)
      text.numbers[1,]<-c(0:9)
      text.numbers[2,]<-c('zero','one','two','three','four','five','six','seven','eight','nine')
      for(i in 1:length(text)){
        for(k in 1:ncol(text.numbers)){
          if(text[i]==text.numbers[1,k]){
            text[i]<-text.numbers[2,k]
          }
        }
      }
      text<-unlist(strsplit(text,split = ""))
      text<-tolower(text)
      text<-text[text%in%letters]
    }else{
      text<-unlist(strsplit(text,split = ""))
      text<-tolower(text)
      text<-text[text%in%letters]
    }
    text.p<-text
    text.collapsed.p<-paste(text,collapse = "")

  tt<-as.data.frame(table(text.p))
  names(tt)<-c('letter','frequency')
  tt$proportion<-round(tt[,2]/sum(tt[,2]),3)

  freq.dist<-lf[,-2]

  ciphertext_s<-unlist(strsplit(text,split = ""))
  tts<-as.data.frame(table(ciphertext_s))
  names(tts)<-c('letter','frequency')
  tts$proportion<-tts[,2]/sum(tts[,2])

  tts$letter <- as.character(tts$letter)
  freq.dist$letter <- as.character(freq.dist$letter)
  freq.dist$cipher.proportion<-0
  for(i in 1:length(tts[,1])){
    for(k in 1:26){
      if(tts[i,1]==freq.dist[k,1]){
        freq.dist[k,3]<-tts[i,3]
      }
    }
  }

  freq.dist<-freq.dist[order(freq.dist$proportion,decreasing = T),]
  freq.dist$rank<-c(1:26)
  freq.dist<-freq.dist[order(freq.dist$cipher.proportion,decreasing = T),]
  freq.dist$cipher.rank<-c(1:26)
  freq.dist<-freq.dist[order(freq.dist$letter),]

  plotCipher<-ggplot(data=freq.dist, aes(x=letter, y=cipher.proportion)) +
    geom_bar(stat="identity", width=.75, fill='steelblue') +
    labs(x="",y="Letter Proportion") +
    geom_text(aes(label=cipher.rank), vjust=-0.2, color="steelblue",
              position = position_dodge(0.9), size=3.5) +
    coord_cartesian(ylim = c(0,max(freq.dist$cipher.proportion)+0.02)) +
    ggtitle("Ciphertext Letter Frequency")

  plotLetterFreq<-ggplot(data=freq.dist, aes(x=letter, y=proportion)) +
    geom_bar(stat="identity", width=.75, fill='springgreen', color='steelblue') +
    labs(x="",y="Letter Proportion") +
    geom_text(aes(label=rank), vjust=-0.2, color="steelblue",
              position = position_dodge(0.9), size=3.5) +
    coord_cartesian(ylim = c(0,max(freq.dist$proportion)+0.02)) +
    ggtitle("Cornell English Language Letter Frequency")

  grid.arrange(plotCipher, plotLetterFreq, nrow=2)

  fdm<-melt(freq.dist[,1:3], id='letter')
  fdm

  plotOverlay<-ggplot(data=fdm, aes(x=letter, y=value, fill=factor(variable, labels=c('English Language','Ciphertext')), color=variable)) +
    geom_bar(stat="identity", width=.75, position='identity', alpha=0.4) +
    coord_cartesian(ylim = c(0,0.15)) +
    scale_fill_manual(values=c('gray',"steelblue")) +
    scale_colour_manual(values=c('gray5',rgb(0,0,0,0))) +
    labs(x="",y="Proportion", fill="Letters") +
    guides(colour=F) +
    ggtitle("Letter Frequency Analysis")

  freq.comparison<-freq.dist[,c(1:3)]
  cp2<-rep(freq.comparison$cipher.proportion,2)

  fcm<-as.data.frame(matrix(ncol=25,nrow=26,data=NA))
  names(fcm)<-paste('shift',c(1:25),sep = ".")
  freq.comparison<-cbind(freq.comparison,fcm)
  freq.comparison

  for(i in 2:26){
    freq.comparison[,i+2]<-cp2[(i:(i+25))]
  }

  diff<-as.data.frame(matrix(ncol = 2, nrow = 26, data = c(0:25,rep(NA,26))))
  names(diff)<-c('shift','difference')
  for(i in 1:26){
    diff[i,2]<-sum(abs(freq.comparison[,(i+2)]-freq.comparison[,2]))
    diff<-diff[order(diff$difference, decreasing = F),]
  }

  cat("Recommended Shift Parameter:", diff[1,1])

  plotFreqAnalysis<-ggplot(data=diff, aes(x=shift, y=difference)) +
    geom_point(stat="identity", color = 'steelblue', size = 4) +
    labs(x="Ciphertext Letter Position Shift", y="Sum Difference in Letter Frequency\nCiphertext Compared to Cornell English Language Letter Frequency") +
    geom_text(aes(label=shift), vjust=-0.75, color="steelblue",
              position = position_dodge(0.9), size=4) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    coord_cartesian(ylim = c(0,(max(diff$difference)+0.1))) +
    ggtitle("Ciphertext Frequency Analysis - Caeser Shift")

print(plotFreqAnalysis)

    shiftparameter<-as.integer(diff[1,1])

    ciphertext<-unlist(strsplit(text.collapsed.p,split = ""))
    aa<-rep(letters,2)
    plaintext<-NA
    for(i in 1:length(ciphertext)){
      for(k in 27:length(aa)){
        if(ciphertext[i]==aa[k]){
          plaintext[i]<-aa[(k-shiftparameter)]
        }
      }
    }
    plaintext.collapsed.p<-paste(plaintext,collapse = "")
    plaintext<-plaintext
    plaintext.collapsed.p <- paste(plaintext,collapse = "")
    list(c('Ciphertext:',text.collapsed.p), head(diff), c("Deciphered plaintext:",plaintext.collapsed.p))
}
