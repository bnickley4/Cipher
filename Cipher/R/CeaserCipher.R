#' A Text Preparation Function
#'
#' This function prepares text for encryption by removing spaces and special characters.
#' @param
#' @keywords text preparation
#' @export
#' @examples
#' prep.text()
prep.text<-function(){

  readline(cat("Press [Enter]"))
  cat("Enter the text:","\n", "Enclose in \" \"")
  text <- scan(file = "", what = "character", nmax = 1)

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
  text.p<<-text
  text.collapsed.p<<-paste(text,collapse = "")
  return(text.collapsed.p)
}

#' A Caeser Cipher Function
#'
#' This function enciphers text using a Caeser Cipher.
#' @param
#' @keywords Caeser cipher
#' @export
#' @examples
#' encipher_c()
encipher_c<-function(){

  readline(cat("Press [Enter]"))
  cat("Type the Plaintext:","\n")
  plaintext <- scan(file = "", what = "character", nmax = 1)

  readline(cat("Press [Enter]"))
  cat("Enter the Shift Parameter:","\n","Must be between 1 and 26 (inclusive)","\n")
  shiftparameter <- scan(file = "", what = "numeric", nmax = 1)
  shiftparameter<-as.integer(shiftparameter)
  cat("Ciphertext:","\n")

  plaintext<-unlist(strsplit(plaintext,split = ""))
  aa<-rep(letters,2)
  ciphertext<-NA
  for(i in 1:length(plaintext)){
    for(k in 1:(length(aa)/2)){
      if(plaintext[i]==aa[k]){
      ciphertext[i]<-aa[(k+shiftparameter)]
      }
    }
  }
  ciphertext<-paste(ciphertext,collapse = "")
  return(ciphertext)
}

#' A Caeser Cipher Function
#'
#' This function deciphers text using a Caeser Cipher.
#' @param
#' @keywords Caeser cipher
#' @export
#' @examples
#' decipher_c()
decipher_c<-function(){

  readline(cat("Press [Enter]"))
  cat("Type the Ciphertext:","\n")
  ciphertext <- scan(file = "", what = "character", nmax = 1)

  readline(cat("Press [Enter]"))
  cat("Enter the Shift Parameter:","\n","Must be between 1 and 26 (inclusive)","\n")
  shiftparameter <- scan(file = "", what = "numeric", nmax = 1)
  shiftparameter<-as.integer(shiftparameter)
  cat("Plaintext:","\n")

  ciphertext<-unlist(strsplit(ciphertext,split = ""))
  aa<-rep(letters,2)
  plaintext<-NA
  for(i in 1:length(ciphertext)){
    for(k in 27:length(aa)){
      if(ciphertext[i]==aa[k]){
        plaintext[i]<-aa[(k-shiftparameter)]
      }
    }
  }
  plaintext<-paste(plaintext,collapse = "")
  return(plaintext)
}
