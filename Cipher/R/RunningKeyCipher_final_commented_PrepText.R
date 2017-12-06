#Running Key Cipher (RKC)
#Ben Nickley
#11-27-17

###DESCRIPTION############################################################################

# Running Key Cipher:
# In cryptography, the Running Key Cipher is a type of polyalphabetic substitution Cipher where a text (usually a book) provides a long Keystream. Sender and receiver agree ahead of time which book will be used as a Keystream, while the passage to be used as a Key would be chosen randomly by the sender for each message and secretly indicated (by an Indicator Block) somewhere in the enciphered message (Cipher). The original message (Plaintext) and the Key, which is derived from the book (Keystream), are converted to numbers (a=1,b=2,...,z=26) and paired together to form a sequence of coordinates.  These coordinates can be used to refer to rows and columns of a polyalphabet matrix, such as the Tabula Recta, from which substitution letters can be derived.

#Tabula Recta (tr) - the 26 rows coorespond to 26 'alphabets'

# 1.)   a b c d e f g h i j k l m n o p q r s t u v w x y z
# 2.)   b c d e f g h i j k l m n o p q r s t u v w x y z a
# 3.)   c d e f g h i j k l m n o p q r s t u v w x y z a b
# 4.)   d e f g h i j k l m n o p q r s t u v w x y z a b c
# 5.)   e f g h i j k l m n o p q r s t u v w x y z a b c d
# 6.)   f g h i j k l m n o p q r s t u v w x y z a b c d e
# 7.)   g h i j k l m n o p q r s t u v w x y z a b c d e f
# 8.)   h i j k l m n o p q r s t u v w x y z a b c d e f g
# 9.)   i j k l m n o p q r s t u v w x y z a b c d e f g h
# 10.)  j k l m n o p q r s t u v w x y z a b c d e f g h i
# 11.)  k l m n o p q r s t u v w x y z a b c d e f g h i j
# 12.)  l m n o p q r s t u v w x y z a b c d e f g h i j k
# 13.)  m n o p q r s t u v w x y z a b c d e f g h i j k l
# 14.)  n o p q r s t u v w x y z a b c d e f g h i j k l m
# 15.)  o p q r s t u v w x y z a b c d e f g h i j k l m n
# 16.)  p q r s t u v w x y z a b c d e f g h i j k l m n o
# 17.)  q r s t u v w x y z a b c d e f g h i j k l m n o p
# 18.)  r s t u v w x y z a b c d e f g h i j k l m n o p q
# 19.)  s t u v w x y z a b c d e f g h i j k l m n o p q r
# 20.)  t u v w x y z a b c d e f g h i j k l m n o p q r s
# 21.)  u v w x y z a b c d e f g h i j k l m n o p q r s t
# 22.)  v w x y z a b c d e f g h i j k l m n o p q r s t u
# 23.)  w x y z a b c d e f g h i j k l m n o p q r s t u v
# 24.)  x y z a b c d e f g h i j k l m n o p q r s t u v w
# 25.)  y z a b c d e f g h i j k l m n o p q r s t u v w x
# 26.)  z a b c d e f g h i j k l m n o p q r s t u v w x y

# For example:

# Plaintext - This is the message that is to be enciphered:
# thisisthesecretmessagetosendout
# Plaintext as numbers:
# 20,8,9,19,9,19,20,8,5,19,5,3,18,5,20,13,5,19,19,1,7,5,20,15,19,5,14,4,15,21,20

# Key - This is a passage randomly chosen from the Keystream (a mutually agreed on text/book):
# thekeyisrandomlychosenfromabook
# Key as numbers:
# 20,8,5,11,5,25,9,19,18,1,14,4,15,13,12,25,3,8,15,19,5,14,6,18,15,13,1,2,15,15,11

# Indicator Block - This is a sequence of 5 numbers indicating the page number (###) and line number (##) of the Keystream (text/book) from which the Key is derived.
# page - 012
# line - 10
# Indicator Block:
# 01210

# The first coordinate pair, combining the Plaintext and Key is: [20,20]
# The letter to be substituted from the Tabula Recta is the 20th letter taken from the 20th row (alphabet).  This would be 'm': 20.) tuvwxyzabcdefghijkl[m]nopqrs.
# The second coordinate pair is [8,8], coorespoinding to the 8th letter taken from the 8th row (alphabet) of the Tabula Recta.  This would be 'o': 8.) hijklmn[o]pqrstuvwxyzabcdefg
# The third coordinate pair is [9,5], coorespoinding to the 5th letter taken from the 9th row (alphabet) of the Tabula Recta.  This would be 'm': 9.) ijkl[m]nopqrstuvwxyzabcdefgh.
# ...and so on.

# Completing the substitution for each character of the Plaintext yields the Cipher:
# momcmqbzvsrffqekgzgskryfgqnecid

# The Indicator Block can be enciphered and incorporated into the Cipher at a specified location.

# A simple substitution using the first alphabet of the Tabula Recta would encipher the Indicator Block (0=a,1=b,...,25=z).
# Enciphered Indicator Block by simple substitution:
# abcba

# Using the last 5 letters of the Cipher to specify which rows (alphabets) of the Tabula Recta to use helps conceal the Indicator Block.
# Enciphered Indicator Block by polyalphabetic substitution:
# nfejd

# Inserting the enciphered indicator block into the Cipher at the first position yields the completed Cipher:
# nfejdmomcmqbzvsrffqekgzgskryfgqnecid

# This package also allows the user to encipher the Indicator Block location using polyalphabetic substitution and insert it into the Cipher.  This way the location of the Indicator Block does not need to be communicated separately.


###CODE BOOK###############################################################################

###OBJECTS
#a - a:z alphabet into vector 'a'
#aa - alphabet repeated twice into vector 'aa'
#tr - Tabula Recta
#t - Plaintext to be enciphered
#k - Key
#tn - Plaintext as numbers (a=1,b=2,...,z=26)
#kn - Key as numbers (a=1,b=2,...,z=26)
#ct - Cipher or Ciphertext
#ct.ib - Cipher or Ciphertext with Indicator Block appended
#ct5 - last 5 characters of the Cipher
#ct5n - last 5 characters of the Cipher as numbers (a=1,b=2,...,z=26)
#plain.text - deciphered Cipher (==Plaintext)
#pn - Page Number of the Keystream where the Key is located
#ln - Line Number of the Keystream page where the Key is located
#ib - Indicator Block (a 5 character combination of pn (###) and ln (##))
#ibn - Indicator Block as numbers (a=0,b=1,...,z=25)
#ibl - Indicator Block location (###)
#iblr - upper limit to the range of positions where the Indicator Block can be located within the Cipher
#iblk - Indicator Block location known? If yes, iblk==1, if no, iblk==0
#cib - enciphered Indicator Block

###FUNCTIONS
#encipher() - Encipher Plaintext (t) into Cipher (ct), insert Indicator Block (ib) or Indicator Block (ib) AND Indicator Block location (ibl)

#decipher() - Decipher and remove Indicator Block (ib) or Indicator Block (ib) AND Indicator Block location (ibl) from Cipher (ct), decipher Cipher (ct) to Plaintext (plain.text)

###########################################################################################

###ENCIPHER################################################################################

encipher<-function(){

  #Creates alphabet strings and the Tabula Recta (tr)
  a<-letters
  aa<-rep(letters,2)
  tr<-matrix(data=NA, nrow=26, ncol=26)
  for(i in 1:26){
    tr[i,]<-aa[c(i:(i+25))]
  }

  readline(cat("Press [Enter]"))
  cat("Would you like to prep the text for encryption?","\n", "i.e. convert \'Encipher this text\' to \'encipherthistext\'","\n","Type: \'Yes\' or \'No\'","\n")
  prep_yn <- scan(file = "", what = "character", nmax = 1)
  ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

  if(prep_yn==1){

  # This function prepares the text for encryption
  # i.e. 'I want to make this the plaintext' to 'iwanttomakethistheplaintext'
    readline(cat("Press [Enter]"))
    cat("Enter the text:","\n", "Enclose in \" \"")
    text <- scan(file = "", what = "character", nmax = 1)

    readline(cat("Press [Enter]"))
    cat("Convert integers to text?","\n","i.e. '10' converted to 'one','zero'","\n","Type: \'Yes\' or \'No\'")
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
    text.p <- text
    t <- paste(text,collapse = "")
    cat("Prepped Plaintext:","\n")
    print(t)

    }else{

  #'readline' asks the user to enter the Plaintext that is to be enciphered
  readline(cat("Press [Enter]"))
  cat("Type the Plaintext:","\n")
  t <- scan(file = "", what = "character", nmax = 1)
}
  #The user then enters the page number of the Keystream from which the Key will be derived
  readline(cat("\n","Press [Enter]"))
  cat("Type the Page Number (###):","\n")
  pn <- scan(file = "", what = "character", nmax = 1)
  if(nchar(pn)==1){pn<-paste0('00',pn)}
  if(nchar(pn)==2){pn<-paste0('0',pn)}
  pn<-as.character(pn)

  #The user then enters the line number of the Keystream from which the Key will be derived
  readline(cat("Press [Enter]"))
  cat("Type the Line Number (##):","\n")
  ln <- scan(file = "", what = "character", nmax = 1)
  if(nchar(ln)==1){ln<-paste0('0',ln)}
  ln<-as.character(ln)

  #Indicator Block Number is created by combining the page and line numbers (above)
  ibn<-paste(pn,ln,sep = "")

  tn<-unlist(strsplit(t, ""))

  #The user then enters the Key, as found in the Keystream at a location specified by the user above to create Indicator Block Number (ibn). If the Key is too short (e.g. shorter than the Plaintext), this code will inform the user of how many additional characters are needed.
  readline(cat("Press [Enter]"))
  cat("Would you like to prep the Key for encryption?","\n", "i.e. convert \'This is the Key\' to \'thisisthekey\'","\n","Type: \'Yes\' or \'No\'","\n")
  prep_yn <- scan(file = "", what = "character", nmax = 1)
  ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

  if(prep_yn==1){

    # This function prepares the text for encryption
    # i.e. 'I want to make this the plaintext' to 'iwanttomakethistheplaintext'
    readline(cat("Press [Enter]"))
    cat("Type the Key:","\n", "Enclose in \" \"","\n","# of Characters (excluding spaces):",paste(length(tn)),"\n")
    text <- scan(file = "", what = "character", nmax = 1)

    readline(cat("Press [Enter]"))
    cat("Convert integers to text?","\n","i.e. '10' converted to 'one','zero'","\n","Type: \'Yes\' or \'No\'")
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
    text.p <- text
    k <- paste(text,collapse = "")
    cat("Prepped Key","\n")
    print(k)

  }else{
  readline(cat("\n","Press [Enter]"))
  cat("Type the Key:","\n","# of Characters:",paste(length(tn)),"\n")
  k <- scan(file = "", what = "character", nmax = 1)
  }

  if(nchar(k)<length(tn)){
    paste(k)
    cat("Re-enter key plus",length(tn)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)<length(tn)){
    paste(k)
    cat("Re-enter key plus",length(tn)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)<length(tn)){
    paste(k)
    cat("Re-enter key plus",length(tn)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)>length(tn)){
    k<-unlist(strsplit(k, ""))
    k2<-k[1:length(tn)]
    cat("\n","Key truncated by",length(k)-length(tn),"character(s) to match length of Plaintext")
    k <- paste(k2,collapse = "")}

  #Converts both the Plaintext and Key to numbers (a=1,b=2,c=3,...,z=26)
  kn<-unlist(strsplit(k, ""))
  for(i in 1:26){
    for(z in 1:length(tn)){
      if(a[i]==tn[z]){tn[z]<-i}
      if(a[i]==kn[z]){kn[z]<-i}
    }
  }
  tn<-as.integer(tn)
  kn<-as.integer(kn)

  #Polyalphabetic substitution to encipher the Plaintext (tn) using the Key (kn) and Tabula Recta (tr). Creates the Cipher or Ciphertext (ct)
  ct<-NA
  for(i in 1:length(tn)){
    ct[i]<-tr[tn[i],kn[i]]
  }

  #'ct5' is the last five characters of the Cipher (ct). These are used to determine the row numbers of the Tabula Recta (tr) (e.g. the 'alphabets') to use for polyalphabetic substitution to encipher the Indicator Block (ib or ibn).  Typically, the Indicator Block is substituted using the first alphabet (row 1 of the tr), but this makes finding the Indicator Block much easier for someone trying to crack the Cipher.  For example, if the Indicator Block specifies page 12, line 1 of the Keystream, that make the Indicator Block number (ibn) == 01201. This would be enciphered as 'abcab' using the first alphabet of the Tabula Recta.  'a's would be very common in the enciphered Indicator Block, as they specify 0's. Using the last 5 letters of the Cipher to do a polyalphabetic substitution is a more robust way of concealing the Indicator Block in the Cipher.
  ct5<-ct[(length(ct)-4):length(ct)]

  #Make the last five letters of the Cipher (ct5) numeric (a=1,b=1,...,z=26) -> (ct5n)
  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  #Split the numeric Indicator Block (ibn) into its constituent numbers, then use 'ct5n' (see above) and the Tabula Recta (tr) to encipher the Indicator Block via polyalphabetic substitution. Creates the enciphered indicator block (cib)
  ibn<-unlist(strsplit(ibn, ""))
  ibn<-as.integer(ibn)
  cib<-vector(length=5, mode="character")
  for(i in 1:5){
    cib[i]<-tr[ct5n[i],(ibn[i]+1)]
  }
  cib<-cib

  #Here the user has the option either to include in the Cipher itself information about the location of the Indicator Block hidden within the Cipher, or to leave that information out of the Cipher and communicate it to the receiver by some other means.
  #The answer provided by the user sets up a bifurcation of the code
  readline(cat("\n","Press [Enter]"))
  cat("Would you like to include the Indicator Block location in the Cipher?","\n","Type: \'Yes\' or \'No\'","\n")
  include <- scan(file = "", what = "character", nmax = 1)
  ifelse(include=='Yes'|include=='y'|include=='yes'|include=='Y',include<-1,include<-0)

  #If the user would like to include the location of the Indicator block in the Cipher, then the location of Indicator block will be specified by the 2nd through 4th characters of the Cipher.  A polyalphabetic substitution of the Indicator block location is used to obscure the function of ct[2:4].
  if(include==1){
  ct<-paste(append(ct,rep(NA,3),after=1))

  #'iblr' specifies the Indicator Block location range, which will give the user an upper limit on the range locations to choose from in order to determine where the Indicator Block will be hidden within the Cipher.  The Indicator Block cannot be hidden in the first positions of the Cipher, as the Indicator Block location will be specified by characters in position 2:4.  Nor can it be concealed in the last five positions of the Cipher, as these characters are used to conduct polyalphabetic substitution of both the Indicator Block (ib,ibn) and its location (ibl)
  iblr<-NA
  if((length(ct)-5)<999){
    iblr<-(length(ct)-5)
  }else{iblr<-999
  }

  #The user is asked where to conceal the Indicator Block within the Cipher
  readline(cat("\n","Press [Enter]"))
  cat("Type the Indicator Block location:","\n","e.g. choose a number between 4 and",paste(iblr),"(inclusive)","\n")
  ibl <- scan(file = "", what = "character", nmax = 1)
  if(nchar(ibl)==1){ibl<-paste0('00',ibl)}
  if(nchar(ibl)==2){ibl<-paste0('0',ibl)}
  ibl<-unlist(strsplit(ibl,split = ""))
  ibl<-as.integer(ibl)
  cat("\n","Ciphertext:","\n")

  #The location of the Indicator Block (ibl) is enciphered and inserted at postion 2:4 of the Cipher
  ct
  for(i in 1:3){
    ct[(i+1)]<-tr[ct5n[i],(ibl[i]+1)]
  }
  ct

  #Converts the Indicator Block location from a vector of 3 numbers to a single integer (i.e. "0 0 5" to 5)
  ibl<-as.integer(paste(ibl,collapse = ""))

  #Cipher created by appending the Indicator Block to the Cipher at the location specified. ct.ib stand for Cipher Text Indicator Block.
  ct.ib<-paste(append(ct,cib,after=ibl), collapse = "")
  return(ct.ib)

  }else{

  #If the user decides NOT to include the location of the Indicator Block within the Cipher, then this part of the code is run.  The location of the Indicator Block is still determined by the user, but the Cipher will not contain any information about its location.  This will have to be communicated to the receiver by some other means.
  readline(cat("\n","Press [Enter]"))
  cat("Type the Indicator Block location:","\n","e.g. choose a number between 0 and",paste(length(ct)-5),"(inclusive)","\n")
  ibl <- scan(file = "", what = "character", nmax = 1)
  ibl<-as.integer(ibl)
  cat("Ciphertext:","\n")

  #Cipher with appended Indicator Block created.
  ct.ib<-paste(append(ct,cib,after=ibl), collapse = "")
  cipher<-ct.ib
  return(ct.ib)
  }
}

###DECIPHER###############################################################################

decipher<-function(){

  #Creates alphabet strings and the Tabula Recta (tr)
  a<-letters
  aa<-rep(letters,2)
  tr<-matrix(data=NA, nrow=26, ncol=26)
  for(i in 1:26){
    tr[i,]<-aa[c(i:(i+25))]
  }
  ibn<-NA

  #The user is asked for the Cipher (ct).  The Cipher is then split into it's constituent characters to create a vector of individual letters (i.e. 'ablk' to 'a' 'b' 'l' 'k'). Cipher referred to as 'ct.ib' for Cipher Text Indicator Block, as the indicator block is included in the Cipher.
  readline(cat("Press [Enter]"))
  cat("Type the Ciphertext:", "\n")
  ct.ib <- scan(file = "", what = "character", nmax = 1)
  ct.ib<-unlist(strsplit(ct.ib, ""))

  #User is asked if they were provided the Indicator Block location by the sender.  If they were not, it is assumed the sender included the location information within the Cipher itself. The sender had this option when using the encipher() function.
  readline(cat("Press [Enter]"))
  cat("Were you given an Indicator Block location?", "\n", "Type: \'Yes\' or \'No\'", "\n")
  iblk<-scan(file = "", what = "character", nmax = 1)
  ifelse(iblk=='Yes'|iblk=='y'|iblk=='yes',iblk<-1,iblk<-0)

  #Code bifurcated here.  If the Indicator Block location is known (iblk==1), then the following code is used.
  if(iblk==1){

  #The user specifies the location of the Indicator Block hidden within the Cipher (e.g. After which position does the 5 character Indicator Block begin?)
  readline(cat("Press [Enter]"))
  cat("Type the Indicator Block location:", "\n")
  ibl<-scan(file = "", what = "character", nmax = 1)
  ibl<-as.integer(ibl)

  #'ct5' is the last five characters of the Cipher (ct). These are used to determine the row numbers of the Tabula Recta (tr) (e.g. the 'alphabets') to use for polyalphabetic substitution to encipher the Indicator Block (ib or ibn).  Typically, the Indicator Block is substituted using the first alphabet (row 1 of the tr), but this makes finding the Indicator Block much easier for someone trying to crack the Cipher.  For example, if the Indicator Block specifies page 12, line 1 of the Keystream, that make the Indicator Block number (ibn) == 01201. This would be enciphered as 'abcab' using the first alphabet of the Tabula Recta.  'a's would be very common in the enciphered Indicator Block, as they specify 0's. Using the last 5 letters of the Cipher to do a polyalphabetic substitution is a more robust way of concealing the Indicator Block in the Cipher.
  ct5<-ct.ib[(length(ct.ib)-4):length(ct.ib)]

  #Make 'ct5' numeric (a=1,b=2,...,z=26) -> 'ct5n', n for numeric
  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  #Use the Indicator Block location (ibl) to pull the Indicator Block out of the Cipher and put it into 'cib' (for enciphered indicator block)
  cib<-ct.ib[(ibl+1):(ibl+5)]

  #Decipher the Indicator Block, code it into 'ibn' (for Indicator Block numeric). This code uses the rows/alphabets specified by the last five characters of the Cipher (ct5n) to match the letters in the enciphered Indicator Block (cib) to columns (positions) of the Tablula Recta (tr).
  for(i in 1:26){
    for(z in 1:5){
      if(tr[ct5n[z],i]==cib[z]){cib[z]<-i-1}
    }
  }
  ibn<-as.integer(cib)

  #If the Indicator Block location is NOT known by the user, then its location is assumed to be included in the Cipher at position 2:4 (see encipher() function above).
  }else{

  #Again, taking the last five letters of the Cipher to use them to decipher the Indicator Block (ib, ibn).  Also used to decipher the Indicator Block location (ibl).  This is because these letters were used to specify the rows/alphabets in the Tabula Recta for polyaphabetic substitution during encryption (see enchipher() function above)
  ct5<-ct.ib[(length(ct.ib)-4):length(ct.ib)]

  #Makes the last 5 letters of the Cipher numeric (a=1,b=2,...,z=26)
  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  #This is where the Indicator Block location (ibl) is specified in the Cipher
  ibl<-ct.ib[2:4]
  ibl<-unlist(strsplit(ibl,split = ""))

  #Decipher the Indicator Block location (ibl)
  ibl.l<-NA
  for(i in 1:26){
    for(k in 1:3){
      if(tr[ct5n[k],i]==ct.ib[(k+1)]){
        ibl.l[k]<-i-1}
    }
  }

  #Convert deciphered Indicator Block location (ibl) into a single integer (i.e. '0 0 5' -> 5)
  ibl.l<-as.integer(paste(ibl.l,collapse = ""))
  ibl<-ibl.l

  #Extracts the enciphered Indicator Block (cib) from the Cipher (ct.ib) by using the Indicator Block location (ibl) which was deciphered above
  cib<-ct.ib[(ibl+1):(ibl+5)]

  #Deciphers the Indicator Block (cib) by using the last five characters of the Cipher used for encryption as rows and the enciphered Indicator Block characters as columns in the Tabula Recta (tr)
  for(i in 1:26){
    for(z in 1:5){
      if(tr[ct5n[z],i]==cib[z]){cib[z]<-i-1}
    }
  }
  ibn<-as.integer(cib)
  }

  #Line number and Page number derived from the deciphered Indicator Block (ibn).  This tells the receiver where to find the Key within the Keystream.
  line<-ibn[4:5]
  page<-ibn[1:3]

  #If the Indicator Block location (ibl) is known by the user, then the Key will be 3 characters shorter, as the enciphered location (ibl) is appended to the Cipher at characters 2:4 if the user decides to include that information in the Cipher. This explains the need for the lines 271:291 being repeated but with -8 (-5 characters for the indicator block, -3 for it's location) instead of -5 (-5 for the indicator block only - it is 5 characters) below.  If the Key is too short (e.g. shorter than the Plaintext), this code will inform the user of how many additional characters are needed.
  if(iblk==1){

  readline(cat("Key Location:","\n","Page:",paste(page,collapse=""),"\n","Line:",paste(line,collapse=""),"\n","# of Characters:",paste((length(ct.ib)-5)),"\n","Press [Enter]"))

    readline(cat("Press [Enter]"))
    cat("Would you like to prep the Key for decryption?","\n", "i.e. convert \'This is the Key\' to \'thisisthekey\'","\n","Type: \'Yes\' or \'No\'","\n")
    prep_yn <- scan(file = "", what = "character", nmax = 1)
    ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

    if(prep_yn==1){

      # This function prepares the text for encryption
      # i.e. 'I want to make this the plaintext' to 'iwanttomakethistheplaintext'
      readline(cat("Press [Enter]"))
      cat("Type the Key:","\n", "Enclose in \" \"","\n","# of Characters (excluding spaces):",paste(length(ct.ib)-5),"\n")
      text <- scan(file = "", what = "character", nmax = 1)

      readline(cat("Press [Enter]"))
      cat("Convert integers to text?","\n","i.e. '10' converted to 'one','zero'","\n","Type: \'Yes\' or \'No\'")
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
      text.p <- text
      k <- paste(text,collapse = "")
      cat("Prepped Key","\n")
      print(k)

    }else{

  cat("Enter the Key:", "\n")
  k <- scan(file = "", what = "character", nmax = 1)
    }

  if(nchar(k)<(length(ct.ib)-5)){
    paste(k)
    cat("Re-enter key plus",(length(ct.ib)-5)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)<(length(ct.ib)-5)){
    paste(k)
    cat("Re-enter key plus",(length(ct.ib)-5)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)<(length(ct.ib)-5)){
    paste(k)
    cat("Re-enter key plus",(length(ct.ib)-5)-nchar(k),"additional character(s)")
    k <- scan(file = "", what = "character", nmax = 1)}
  if(nchar(k)>(length(ct.ib)-5)){
    k<-unlist(strsplit(k, ""))
    k2<-k[1:(length(ct.ib)-5)]
    cat("\n","Key truncated by",length(k)-(length(ct.ib)-5),"character(s) to match length of Plaintext")
    k <- paste(k2,collapse = "")}
  cat("\n", "Deciphered Plaintext:", "\n")
  }else{
    readline(cat("Key Location:","\n","Page:",paste(page,collapse=""),"\n","Line:",paste(line,collapse=""),"\n","# of Characters:",paste((length(ct.ib)-8)),"\n", "Press [Enter]"))

    readline("Press [Enter]")
      cat("Would you like to prep the Key for decryption?","\n", "i.e. convert \'This is the Key\' to \'thisisthekey\'","\n","Type: \'Yes\' or \'No\'","\n")
    prep_yn <- scan(file = "", what = "character", nmax = 1)
    ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

    if(prep_yn==1){

      # This function prepares the text for encryption
      # i.e. 'I want to make this the plaintext' to 'iwanttomakethistheplaintext'
      readline(cat("Press [Enter]"))
      cat("Type the Key:","\n", "Enclose in \" \"","\n","# of Characters (excluding spaces):",paste(length(ct.ib)-8),"\n")
      text <- scan(file = "", what = "character", nmax = 1)

      readline(cat("Press [Enter]"))
      cat("Convert integers to text?","\n","i.e. '10' converted to 'one','zero'","\n","Type: \'Yes\' or \'No\'")
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
      text.p <- text
      k <- paste(text,collapse = "")
      cat("Prepped Key","\n")
      print(k)

    }else{

    cat("Enter the Key:", "\n")
    k <- scan(file = "", what = "character", nmax = 1)
    }

    if(nchar(k)<(length(ct.ib)-8)){
      paste(k)
      cat("Re-enter key plus",(length(ct.ib)-8)-nchar(k),"additional character(s)")
      k <- scan(file = "", what = "character", nmax = 1)}
    if(nchar(k)<(length(ct.ib)-8)){
      paste(k)
      cat("Re-enter key plus",(length(ct.ib)-8)-nchar(k),"additional character(s)")
      k <- scan(file = "", what = "character", nmax = 1)}
    if(nchar(k)<(length(ct.ib)-8)){
      paste(k)
      cat("Re-enter key plus",(length(ct.ib)-8)-nchar(k),"additional character(s)")
      k <- scan(file = "", what = "character", nmax = 1)}
    if(nchar(k)>(length(ct.ib)-8)){
      k<-unlist(strsplit(k, ""))
      k2<-k[1:(length(ct.ib)-8)]
      cat("\n","Key truncated by",length(k)-(length(ct.ib)-8),"character(s) to match length of Plaintext")
      k <- paste(k2,collapse = "")}
    cat("\n", "Deciphered Plaintext:", "\n")

  }

  #Again, if the Indicator Block location is entered by the user:
  if(iblk==1){

  #Remove the Indicator Block from the Cipher and create Cipher Text (ct.ib to ct)
  ct<-ct.ib[-c((ibl+1):(ibl+5))]

  #Split the Key (k) into its constituent characters (i.e. 'klp' to 'k' 'l' 'p'), then convert the Key to numbers (kn) (a=1,b=2,...,z=26)
  plain.text<-vector(length=length(ct), mode="character")
  kn<-unlist(strsplit(k, ""))
  for(q in 1:26){
    for(i in 1:length(plain.text)){
      if(a[q]==kn[i]){kn[i]<-q}
    }
  }
  kn<-as.integer(kn)

  #Decipher to Plaintext: Use the Key (kn) to specify the rows/alphabets and the Cipher (ct) to specify the columns/characters of the Tabula Recta (tr).
  for(q in 1:26){
    for(i in 1:length(plain.text)){
      if(tr[kn[i],q]==ct[i]){plain.text[i]<-q}
      if(q==plain.text[i]){plain.text[i]<-a[q]}
    }
  }
  #Show the user the deciphered Plaintext
  return(paste(plain.text,collapse = ""))

  #Slightly different path if the Indicator Block Location is NOT known/specified by the user. The only differnce is the Indicator Block location (ibl) must be removed from the Cipher (ct.ib) as well as the Indicator Block (ib or ibn) itself before deciphering
  }else{

    #Remove both the Indicator Block location (ibl) and the Indicator Block (ib, ibn) from the Cipher (ct.ib) and create the Cipher text (ct)
    ct<-ct.ib[-c(2:4,(ibl+1):(ibl+5))]

    #Make the Key (k) numeric (kn) (a=1,b=2,...,z=26)
    plain.text<-vector(length=length(ct), mode="character")
    kn<-unlist(strsplit(k, ""))
    for(q in 1:26){
      for(i in 1:length(plain.text)){
        if(a[q]==kn[i]){kn[i]<-q}
      }
    }
    kn<-as.integer(kn)

    #Decipher to Plaintext: Use the Key (kn) to specify the rows/alphabets and the Cipher (ct) to specify the columns/characters of the Tabula Recta (tr).
    for(q in 1:26){
      for(i in 1:length(plain.text)){
        if(tr[kn[i],q]==ct[i]){plain.text[i]<-q}
        if(q==plain.text[i]){plain.text[i]<-a[q]}
      }
    }
    #Show the user the deciphered Plaintext
    return(paste(plain.text,collapse = ""))
  }
}
