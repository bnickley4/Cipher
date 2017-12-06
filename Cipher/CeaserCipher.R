#Caeser cipher
#Ben Nickley
#11-29-2017

# The Caeser cipher is a simple encryption technique in which each letter in the Plaintext is replaced by some letter a fixed number of positions down the alphabet.  The number of letters downstream from which the substitution letter is obtained is specified by the Shift Parameter.

# For Example:
# Shift Parameter: 3
# Plain Alphabet:   abcdefghijklmnopqrstuvwxyz
# Cipher Alphabet:  defghijklmnopqrstuvwxyzabc

# In this example, the shift parameter of 3 is specified.  This means the letter 'a' is substituted by 'a' plus 3 character positions in the alphabet. So, 'a' is substituted by 'd', 'b' by 'e', 'c' by 'f' and so on.  The Plain Alphabet is therefor replaced by the Cipher Alphabet.  The Plaintext, or message that is to be enciphered, is enciphered then by substituting letters from the Plain Alphabet for letters in the Cipher Alphabet. This is demonstrated below:
# Plaintext:    caeserusedtheCaesercipherinhispersonalcorrespondence
# Ciphertext:   fhdvhuxvhgwkhfhdvhuflskhulqklvshuvrqdofruuhvsrqghqfh

# The 'prep.text' function created here takes text - with spaces and number etc. - like the text about OSU above, and converts removes spaces and characters, changes letters to lowercase, and either removes or replaces #s with text ('1' to 'one'). This puts the text in a format in which it can be enciphered and by which a frequency analysis (frequency of different letters) can be run.
# Example: 'The Ohio State University, commonly' to 'theohiostateuniversitycommonly'
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

encipher<-function(){
  
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

decipher<-function(){
  
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
