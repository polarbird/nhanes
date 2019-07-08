
install.packages("Hmisc")
#install.packages("xlsx")

#before load files
library(foreign)
library(Hmisc)

#set the working directory
setwd("D:/work/nhanes/origin")

xpt_files <- list.files(path = ".", pattern = "XPT",full.names = FALSE)


full_str <- c("DEMO_!demo_obj<-temp_var!tp_var  <- data.frame(seqn=xpt_var$seqn,gender=xpt_var$riagendr,age=xpt_var$ridageyr,race=xpt_var$ridreth3,education=xpt_var$dmdeduc2,pregnancy=xpt_var$ridexprg,weightqu=xpt_var$wtint2yr,weightex=xpt_var$wtmec2yr,sdmvpsu=xpt_var$sdmvpsu,sdmvstra=xpt_var$sdmvstra)",
              "DR1TOT_!dr1tot_obj<-temp_var!tp_var <- data.frame(seqn=xpt_var$seqn,energy1=xpt_var$dr1tkcal)",
              "DR2TOT_!dr2tot_obj<-temp_var!tp_var <- data.frame(seqn=xpt_var$seqn,energy2=xpt_var$dr2tkcal)",
              "BPX_!bpx_obj<-temp_var!tp_var <- data.frame(seqn=xpt_var$seqn,bpsy1=xpt_var$bpxsy1,bpdi1=xpt_var$bpxdi1,bpsy2=xpt_var$bpxsy2,bpdi2=xpt_var$bpxdi2,bpsy3=xpt_var$bpxsy3,bpdi3=xpt_var$bpxdi3)",
              "BMX_!bmx_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,bmi=xpt_var$bmxbmi)",
              "ALB_!alb_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,acr=xpt_var$urdact)",
              "PBCD_!pbcd_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,wtsh2yr=xpt_var$wtsh2yr,pb=xpt_var$lbxbpb,cd=xpt_var$lbxbcd,hg=xpt_var$lbxthg,se=xpt_var$lbxbse,mn=xpt_var$lbxbmn)",
              "GHB_!ghb_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,hba1c=xpt_var$lbxgh)",
              "BIOPRO_!biopro_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,alb=xpt_var$lbdsalsi,alt=xpt_var$lbxsatsi,ast=xpt_var$lbxsassi,bun=xpt_var$lbxsbu,scr=xpt_var$lbxscr,ua=xpt_var$lbxsua,chl=xpt_var$lbxsch,tri=xpt_var$lbxstr)",
              "ALQ_!alq_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,alq101=xpt_var$alq101,alq110=xpt_var$alq110,alq120q=xpt_var$alq120q)",
              "DIQ_!diq_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,insulin=xpt_var$diq050,hypoglu=xpt_var$diq070)",
              "INQ_!inq_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,income=xpt_var$indfmmpc)",
              "SMQ_!smq_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,smq020=xpt_var$smq020,smq040=xpt_var$smq040)",
              "BPQ_!bpq_obj<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,bpq050a=xpt_var$bpq050a)",
              "UCPREG_!ucpreg_class<-temp_var!tp_var<- data.frame(seqn=xpt_var$seqn,pregnant=xpt_var$urxpreg)"
)


for(single_str in full_str){
  data_str <- strsplit(single_str,"!")
  print(data_str)
  file_str <- data_str[[1]][1]
  obj_str <- data_str[[1]][2]
  action_str <- data_str[[1]][3]
  
  file_var <- xpt_files[grep(file_str,xpt_files)]
  
  temp_var <- 0L
  for( f in file_var){
    xpt_var <-sasxport.get(f)
    summary(xpt_var)
    eval(parse(text = action_str))
    if (is.integer(temp_var)) {
      temp_var <- tp_var
    } else {
      temp_var <- rbind(temp_var,tp_var)
    }
  }
  
  eval(parse(text = obj_str))
  
}

#COTNAL COT
cot_i<-sasxport.get("COT_I.XPT")
cot_h<-sasxport.get("COT_h.XPT")
cot_g<-sasxport.get("COTNAL_G.XPT")
final_results <- rbind(data.frame(seqn=cot_i$seqn,cot=cot_i$lbxcot),data.frame(seqn=cot_h$seqn,cot=cot_h$lbxcot),data.frame(seqn=cot_g$seqn,cot=cot_g$lbxcot))

for( o in ls(pattern="_obj")){
  final_results <- merge(eval(parse(text = o)),final_results,by="seqn")
  print(nrow(final_results))
}
final_results <- merge(final_results,ucpreg_class,by="seqn",all.x=TRUE)

write.table(final_results ,"../mydata.csv",sep=",",row.names = FALSE)

#library(xlsx) 
#write.xlsx(final_results, "d:/mydata.xlsx")

print(full_str)
