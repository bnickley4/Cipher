#' A Running Key Encipher Function
#'
#' This function enciphers text using a Running Key Cipher.
#' @param
#' @keywords running key cipher
#' @export
#' @examples
#' encipher()
encipher<-function(){

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

  readline(cat("Press [Enter]"))
  cat("Type the Plaintext:","\n")
  t <- scan(file = "", what = "character", nmax = 1)
}

  readline(cat("\n","Press [Enter]"))
  cat("Type the Page Number (###):","\n")
  pn <- scan(file = "", what = "character", nmax = 1)
  if(nchar(pn)==1){pn<-paste0('00',pn)}
  if(nchar(pn)==2){pn<-paste0('0',pn)}
  pn<-as.character(pn)

  readline(cat("Press [Enter]"))
  cat("Type the Line Number (##):","\n")
  ln <- scan(file = "", what = "character", nmax = 1)
  if(nchar(ln)==1){ln<-paste0('0',ln)}
  ln<-as.character(ln)

  ibn<-paste(pn,ln,sep = "")

  tn<-unlist(strsplit(t, ""))

  readline(cat("Press [Enter]"))
  cat("Would you like to prep the Key for encryption?","\n", "i.e. convert \'This is the Key\' to \'thisisthekey\'","\n","Type: \'Yes\' or \'No\'","\n")
  prep_yn <- scan(file = "", what = "character", nmax = 1)
  ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

  if(prep_yn==1){

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

  kn<-unlist(strsplit(k, ""))
  for(i in 1:26){
    for(z in 1:length(tn)){
      if(a[i]==tn[z]){tn[z]<-i}
      if(a[i]==kn[z]){kn[z]<-i}
    }
  }
  tn<-as.integer(tn)
  kn<-as.integer(kn)

  ct<-NA
  for(i in 1:length(tn)){
    ct[i]<-tr[tn[i],kn[i]]
  }

  ct5<-ct[(length(ct)-4):length(ct)]

  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  ibn<-unlist(strsplit(ibn, ""))
  ibn<-as.integer(ibn)
  cib<-vector(length=5, mode="character")
  for(i in 1:5){
    cib[i]<-tr[ct5n[i],(ibn[i]+1)]
  }
  cib<-cib

  readline(cat("\n","Press [Enter]"))
  cat("Would you like to include the Indicator Block location in the Cipher?","\n","Type: \'Yes\' or \'No\'","\n")
  include <- scan(file = "", what = "character", nmax = 1)
  ifelse(include=='Yes'|include=='y'|include=='yes'|include=='Y',include<-1,include<-0)

  if(include==1){
  ct<-paste(append(ct,rep(NA,3),after=1))

  iblr<-NA
  if((length(ct)-5)<999){
    iblr<-(length(ct)-5)
  }else{iblr<-999
  }

  readline(cat("\n","Press [Enter]"))
  cat("Type the Indicator Block location:","\n","e.g. choose a number between 4 and",paste(iblr),"(inclusive)","\n")
  ibl <- scan(file = "", what = "character", nmax = 1)
  if(nchar(ibl)==1){ibl<-paste0('00',ibl)}
  if(nchar(ibl)==2){ibl<-paste0('0',ibl)}
  ibl<-unlist(strsplit(ibl,split = ""))
  ibl<-as.integer(ibl)
  cat("\n","Ciphertext:","\n")

  ct
  for(i in 1:3){
    ct[(i+1)]<-tr[ct5n[i],(ibl[i]+1)]
  }
  ct

  ibl<-as.integer(paste(ibl,collapse = ""))

  ct.ib<-paste(append(ct,cib,after=ibl), collapse = "")
  return(ct.ib)

  }else{

  readline(cat("\n","Press [Enter]"))
  cat("Type the Indicator Block location:","\n","e.g. choose a number between 0 and",paste(length(ct)-5),"(inclusive)","\n")
  ibl <- scan(file = "", what = "character", nmax = 1)
  ibl<-as.integer(ibl)
  cat("Ciphertext:","\n")

  ct.ib<-paste(append(ct,cib,after=ibl), collapse = "")
  cipher<-ct.ib
  return(ct.ib)
  }
}

#' A Running Key Encipher Function
#'
#' This function deciphers text using a Running Key Cipher.
#' @param
#' @keywords running key cipher
#' @export
#' @examples
#' decipher()
decipher<-function(){

  a<-letters
  aa<-rep(letters,2)
  tr<-matrix(data=NA, nrow=26, ncol=26)
  for(i in 1:26){
    tr[i,]<-aa[c(i:(i+25))]
  }
  ibn<-NA

  readline(cat("Press [Enter]"))
  cat("Type the Ciphertext:", "\n")
  ct.ib <- scan(file = "", what = "character", nmax = 1)
  ct.ib<-unlist(strsplit(ct.ib, ""))

  readline(cat("Press [Enter]"))
  cat("Were you given an Indicator Block location?", "\n", "Type: \'Yes\' or \'No\'", "\n")
  iblk<-scan(file = "", what = "character", nmax = 1)
  ifelse(iblk=='Yes'|iblk=='y'|iblk=='yes',iblk<-1,iblk<-0)

  if(iblk==1){

  readline(cat("Press [Enter]"))
  cat("Type the Indicator Block location:", "\n")
  ibl<-scan(file = "", what = "character", nmax = 1)
  ibl<-as.integer(ibl)

  ct5<-ct.ib[(length(ct.ib)-4):length(ct.ib)]

  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  cib<-ct.ib[(ibl+1):(ibl+5)]

  for(i in 1:26){
    for(z in 1:5){
      if(tr[ct5n[z],i]==cib[z]){cib[z]<-i-1}
    }
  }
  ibn<-as.integer(cib)

  }else{

  ct5<-ct.ib[(length(ct.ib)-4):length(ct.ib)]

  for(i in 1:26){
    for(z in 1:length(ct5)){
      if(a[i]==ct5[z]){ct5[z]<-i}
    }
  }

  ct5n<-as.integer(ct5)
  ct5n

  ibl<-ct.ib[2:4]
  ibl<-unlist(strsplit(ibl,split = ""))

  ibl.l<-NA
  for(i in 1:26){
    for(k in 1:3){
      if(tr[ct5n[k],i]==ct.ib[(k+1)]){
        ibl.l[k]<-i-1}
    }
  }

  ibl.l<-as.integer(paste(ibl.l,collapse = ""))
  ibl<-ibl.l

  cib<-ct.ib[(ibl+1):(ibl+5)]

  for(i in 1:26){
    for(z in 1:5){
      if(tr[ct5n[z],i]==cib[z]){cib[z]<-i-1}
    }
  }
  ibn<-as.integer(cib)
  }

  line<-ibn[4:5]
  page<-ibn[1:3]

  if(iblk==1){

  readline(cat("Key Location:","\n","Page:",paste(page,collapse=""),"\n","Line:",paste(line,collapse=""),"\n","# of Characters:",paste((length(ct.ib)-5)),"\n","Press [Enter]"))

    readline(cat("Press [Enter]"))
    cat("Would you like to prep the Key for decryption?","\n", "i.e. convert \'This is the Key\' to \'thisisthekey\'","\n","Type: \'Yes\' or \'No\'","\n")
    prep_yn <- scan(file = "", what = "character", nmax = 1)
    ifelse(prep_yn=='Yes'|prep_yn=='Y'|prep_yn=='y'|prep_yn=="yes",prep_yn<-1,prep_yn<-0)

    if(prep_yn==1){

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

  if(iblk==1){

  ct<-ct.ib[-c((ibl+1):(ibl+5))]

  plain.text<-vector(length=length(ct), mode="character")
  kn<-unlist(strsplit(k, ""))
  for(q in 1:26){
    for(i in 1:length(plain.text)){
      if(a[q]==kn[i]){kn[i]<-q}
    }
  }
  kn<-as.integer(kn)

  for(q in 1:26){
    for(i in 1:length(plain.text)){
      if(tr[kn[i],q]==ct[i]){plain.text[i]<-q}
      if(q==plain.text[i]){plain.text[i]<-a[q]}
    }
  }

  return(paste(plain.text,collapse = ""))

  }else{

    ct<-ct.ib[-c(2:4,(ibl+1):(ibl+5))]

    plain.text<-vector(length=length(ct), mode="character")
    kn<-unlist(strsplit(k, ""))
    for(q in 1:26){
      for(i in 1:length(plain.text)){
        if(a[q]==kn[i]){kn[i]<-q}
      }
    }
    kn<-as.integer(kn)

    for(q in 1:26){
      for(i in 1:length(plain.text)){
        if(tr[kn[i],q]==ct[i]){plain.text[i]<-q}
        if(q==plain.text[i]){plain.text[i]<-a[q]}
      }
    }
    return(paste(plain.text,collapse = ""))
  }
}
