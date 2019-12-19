#--------
#----Afrodatae wave 1-7を読み込んだデータを使って各種処理
#----2019/12/10 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　


#install.packages(c("dplyr","haven","memisc","biglm","nlme"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形
library(biglm)	#bigdata lm,glm
library(nlme)	#multi-level
library(lattice)	#lattice plot

#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")

load("0Afrodat.img");ls()

#Afrodat1-Afrodat7　やそれらの共通の変数をまとめたAfrodatAllなどが入っている

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開



repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA02<-function(x){
	x[x<0|x>2]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA03<-function(x){
	x[x<0|x>3]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
#0-4　以外をNAに　
repNA04<-function(x){
	x[x<0|x>4]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA05<-function(x){
	x[x<0|x>5]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

#0-1　以外をNAに　
repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

#NAを除いて平均計算
mean2<-function(x){
	m<-mean(x,na.rm=T)
	return(m)
	}

max2<-function(x){
	m<-max(x,na.rm=T)
	return(m)
	}

min2<-function(x){
	m<-min(x,na.rm=T)
	return(m)
	}

#
table2<-function(x1,x2,...){ 
	st<-table(x1,x2,...) 
	Ic<-matrix(1,nrow(st),1) 
	Ir<-matrix(1,ncol(st),1) 
		sr<-st%*%Ir					#行和 
		sc<-t(st)%*%Ic			#列和 
		s0<-t(sr)%*%Ic			#総和 
	#----------------------- 
	ss0<-st 
 	sst<-rbind(st,t(sc)) 		 #行和、列和を入れた行列に 
	sc<-rbind(sc,s0) 		#列和にさらに総計を入れておく 
	sr<-rbind(sr,s0) 		#行和にさらに総計を入れておく
	 sst<-cbind(sst,sr)   #もとの表に列和、行和、総和がついたもの
	 print(sst)
 #-------------------------------- 行和を100% 
 pr<-sst 
	 for (j in seq(1:ncol(sst))){ 
 	pr[,j]<-pr[,j]/sr*100 
 	} 
 #小数点1桁までで丸めて出力 
 pr<-round( pr,1) 
	 pr<-cbind(pr,sr)		#和のベクトルをつけて返す
			colnames(pr)[length(colnames(pr))-1]<-"%"
			colnames(pr)[length(colnames(pr))]<-"N"
 #列和を100% 
 pc<-sst 
 for (i in seq(1:nrow(sst))){ 
	 pc[i,]<-pc[i,]/t(sc)*100 
	 } 
 pc<-round( pc,1) 
 #総和を100% 
 p0<-sst 
 for (i in seq(1:nrow(sst))){ 
 	for (j in seq(1:ncol(sst))){ 
		 p0[i,j]<-p0[i,j]/s0*100 
	 } 
	 } 
 p0<-round( p0,1) 
	chisq<-chisq.test(st)
	return(list(pr,chisq))
 } 
 
#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　変数名は固定　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
#dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
#	group_trend_plot(dat,lab="Radio")


#-------Round 6で遊ぶ
#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)

m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))


#"dlang_English"                   "dlang_French"                    "dlang_Portuguese"               "dlang_Swahili"                   "dlang_Arabic"                    "dlang_Afrikaans"                 "dlang_Chichewa"                 "dlang_Akan"                      "dlang_Other"                     "dlang_Egyptian_Arabic"           "dlang_Crioulo"                   "dlang_Kirund"                    "dlang_Sesotho"                   "dlang_Sudanese_Arabic"           "dlang_Creole"                   "dlang_siSwati"                   "dlang_Shona"                     "dlang_Algerian_Arabic" 

#"dOccupation_Never"               "dOccupation_Student"            "dOccupation_Housewife_homemaker" "dOccupation_primary"             "dOccupation_Trader"              "dOccupation_Retail"              "dOccupation_Unskilled"           "dOccupation_skilled"             "dOccupation_Clerical"            "dOccupation_Supervisor"         "dOccupation_police"              "dOccupation_Mid_level"           "dOccupation_Upper_level"         "dOccupation_Other" 

#"dRace_BAf"                       "dRace_Wh"                        "dRace_Col"                       "dRace_Arab"                      "dRace_SAs"                       "dRace_EAs"                       "dRace_Oth"                      


#単純にfactorだと変数が多い
#res<-glm(Own_TV~Age+Gender_f+Education+as.factor(Employment_status)+as.factor(Race)+as.factor(Occupation)+Mem_religious+Mem_voluntary+gone_food+gone_cash+as.factor(Language)+COUNTRY2,family="binomial",data=Afrodat6)
	summary(res)

table2(Afrodat6$Mem_religious,Afrodat6$Mem_voluntary,exclude=NULL)
#        0    1    2    3 <NA>   %     N
#0    78.6  8.8  9.4  3.0  0.2 100 28141
#1    49.8 34.1 12.4  3.2  0.5 100  8986
#2    46.2 11.7 35.9  5.6  0.6 100 13739
#3    37.9  8.5 26.7 26.4  0.5 100  2718
#<NA> 11.7  3.4  5.7  1.7 77.5 100   351
#     63.1 13.7 17.5  4.9  0.9 100 53935


table2(Afrodat6$Own_Radio,Afrodat6$News_Radio,exclude=NULL)
#radioなしでもニュースソースにしている者も
#        0   1   2    3    4 <NA>   %     N
#0    45.5 5.9 9.8 19.7 18.8  0.2 100 15825
#1     7.0 2.5 5.9 22.9 61.5  0.1 100 38038
#<NA> 13.9 4.2 8.3 16.7 44.4 12.5 100    72
#     18.3 3.5 7.1 21.9 49.0  0.2 100 53935
table2(Afrodat6$Own_TV,Afrodat6$News_Television,exclude=NULL)
#        0   1   2    3    4 <NA>   %     N
#0    70.9 5.3 5.7  8.3  9.5  0.2 100 27034
#1     7.0 2.3 5.1 18.2 67.3  0.2 100 26812
#<NA> 37.1 5.6 3.4 10.1 29.2 14.6 100    89
#     39.1 3.8 5.4 13.2 38.3  0.2 100 53935

cor(Afrodat6[,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV","News_Radio","News_Television","News_Internet","News_Social_media")],use="complete")
	 
#ダミー変数で
v<-as.formula('~gone_food+gone_cash+
Mem_religious+Mem_voluntary+
Age+Gender_f+Education+
dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dOccupation_Never+dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM')
##dOccupation_Other +dRace_Oth 



res_oAuto<-glm(formula(paste("Own_Auto~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oAuto)
res_oMbphone<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oMbphone)

res_oRadio<-glm(formula(paste("Own_Radio~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oRadio)
res_nRadio<-lm(formula(paste("News_Radio~",v,sep="")[2]),data=Afrodat6)
	summary(res_nRadio)

res_oTelevision<-glm(formula(paste("Own_TV~",v,sep="")[2]),family="binomial",data=Afrodat6)
	summary(res_oTelevision)
res_nTelevision<-lm(formula(paste("News_Television~",v,sep="")[2]),data=Afrodat6)
	summary(res_nTelevision)

res_nInternet<-lm(formula(paste("News_Internet~",v,sep="")[2]),data=Afrodat6)
	summary(res_nInternet)
res_nSocial_media<-lm(formula(paste("News_Social_media~",v,sep="")[2]),data=Afrodat6)
	summary(res_nSocial_media)

mtable(res_oAuto,res_oMbphone,res_oRadio,res_nRadio,res_oTelevision,res_nTelevision,res_nInternet,res_nSocial_media)


#保有状況でクラスタ
set.seed(12345)
d<-complete.cases(Afrodat6[,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV","News_Radio","News_Television","News_Internet","News_Social_media")])
	sum(d);dim(Afrodat6)[1]	#52496  53935
ocl3<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],3)
	ocl3$size
	ocl3$centers

ocl4<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],4)
	ocl4$size	# 18870  9615  1639 22372
	ocl4$centers
data.frame(ocl4$centers,ocl4$size)
#    Own_Auto Own_Mbphone Own_Radio    Own_TV ocl4.size
#1 0.56550079           1 0.5002650 0.6043455     18870	(4)全体的に高い
#2 0.02953718           0 0.4284971 0.0000000      9615	(1)ラジオ
#3 0.12751678           0 0.7156803 1.0000000      1639	(3)ラジオ､TV､自動車
#4 0.00000000           1 1.0000000 0.5910066     22372	(2)携帯､ラジオ､TV

ocl5<-kmeans(Afrodat6[d,c("Own_Auto","Own_Mbphone","Own_Radio","Own_TV")],5)
	data.frame(ocl5$centers,ocl5$size)
#    Own_Auto Own_Mbphone Own_Radio     Own_TV ocl5.size
#1 0.02054329   0.0000000 0.0000000 0.06706282      5890	(1)全体的に低い
#2 0.01228335   0.6895252 1.0000000 0.00000000     13270	(2)携帯とラジオ
#3 0.00000000   1.0000000 0.6172448 0.74230895     21421	(3)携帯とラジオ､TV
#4 0.16800643   0.0000000 0.9429260 1.00000000      1244	(4)クルマ､ラジオ､TV
#5 1.00000000   1.0000000 0.8846406 0.81763659     10671	(5)全体的に高い

#ニュースソース
ucl3<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],3)
	ucl3$size
	ucl3$centers

ucl4<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],4)
	ucl4$size
	ucl4$centers

cor(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")])
ucl5<-kmeans(Afrodat6[d,c("News_Radio","News_Television","News_Internet","News_Social_media")],5)
data.frame(ucl5$centers,ucl5$size)
#  News_Radio News_Television News_Internet News_Social_media ucl5.size
#1  0.1755152      1.05501979     0.1099381        0.08750381      9851	(1)全体的に利用せず
#2  3.5655794      3.51169832     0.1138866        0.05229838     14532	(3)ラジオ､TVのみ
#3  3.4360568      3.49609236     2.5367673        2.28952043      5630	(4)利用するが　ネットは若干低い
#4  2.9646811      3.58895625     3.8278861        3.85859251      7588	(5)全体的に利用
#5  3.4563948      0.09405841     0.0537093        0.05176234     14895	(2)ラジオのみ

dat<-data.frame(ocl5$cluster,ucl5$cluster)
names(dat)<-c("own","usage")
	dat$usage2<-ifelse(dat$usage==2,3,	#usageの番号うちなおし
				ifelse(dat$usage==3,4,
				ifelse(dat$usage==4,5,
				ifelse(dat$usage==5,2,dat$usage))))
				
	table(dat$own,dat$usage2,exclude=NULL)
	table2(dat$own,dat$usage2,exclude=NULL)
#     1    2    3    4    5   %     N
#1 60.6 27.3  8.8  1.4  1.8 100  5890
#2  7.3 70.8 14.3  4.1  3.5 100 13270
#3 19.5 10.7 36.3 14.4 19.2 100 21421
#4 12.5  9.5 65.5  8.0  4.5 100  1244
#5  9.1 14.0 33.1 17.1 26.7 100 10671
#  18.8 28.4 27.7 10.7 14.5 100 52496





#multivariate probit  #処理遅いので
#resmP<-mvProbit(formula(paste("cbind(Own_Auto,Own_Mbphone,Own_Radio,Own_TV)~",v,sep="")[2]),data=Afrodat6[d,])
#resmP<-mvProbit(cbind(Own_Auto,Own_Mbphone,Own_Radio,Own_TV)~gone_food+gone_cash+Mem_religious+Mem_voluntary+Age+Gender_f+Education+dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+dOccupation_Never+dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM,data=Afrodat6[d,])	#dlang_Egyptian_Arabic+	とdCOUNTRY_EGYが相関
#resmP

#---------------
#			7
#---------------
Afrodat7 <- read_sav("r7_merged_data_34ctry.release.sav")
#View(Afrodat7)
summary(Afrodat7)
names(Afrodat7)	
dim(Afrodat7)	#[1] 53935   364
	Afrodat7$wave<-7
	Afrodat7$year<-2018

table(Afrodat7$COUNTRY)
Afrodat7$COUNTRY2<-substr(Afrodat7$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat7$COUNTRY2)
# BEN  BFO  BOT  CAM  CDI  CVE  GAB  GAM  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOR  MOZ  NAM  NGR  NIG  SAF  SEN  SRL  STP  SUD  SWZ  TAN  TOG  TUN  UGA  ZAM  ZIM 
#1200 1200 1198 1202 1200 1200 1199 1200 2400 1194 1599 1200 1200 1200 1200 1200 1200 1200 2392 1200 1200 1600 1840 1200 1200 1200 1200 1200 2400 1200 1199 1200 1200 1200 


#2019/11/現在　codebookがないので､ボツワナの調査票を参照　https://www.afrobarometer.org/sites/default/files/questionnaires/Round%207/bot_r7_questionnaire_062018.pdf
#これを使って欠損値処理
#12. How often do you get news from the following sources? 
#A.Radio
#B. Television
#C. Newspapers
#D.Internet
#E. Social media such as Facebook or Twitter

Afrodat7$News_Radio<-repNA04(Afrodat7$Q12A)
Afrodat7$News_Television<-repNA04(Afrodat7$Q12B)
Afrodat7$News_Newspaper<-repNA04(Afrodat7$Q12C)
Afrodat7$News_Internet<-repNA04(Afrodat7$Q12D)
Afrodat7$News_Social_media<-repNA04(Afrodat7$Q12E)

#保存
save(Afrodat7,file=“0Afrodat7.rda”)







#------全ラウンド必要部分をまとめる　　とりあえずニュースの利用
v<-c("wave","year","COUNTRY2","News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")
AfrodatAll<-rbind(Afrodat1[,v],Afrodat2[,v],Afrodat3[,v],Afrodat4[,v],Afrodat5[,v],Afrodat6[,v],Afrodat7[,v])
	dim(AfrodatAll)	#[1] 204464      8

table(AfrodatAll$COUNTRY2,AfrodatAll$wave,exclude=NULL)
#AfrodatAllg<-group_by(AfrodatAll,c("year","COUNTRY2"))
AfrodatAllg<-group_by(AfrodatAll, AfrodatAll$COUNTRY2,AfrodatAll$year)	#国､年別に集計することを指定

(m<-as.data.frame(summarise(AfrodatAllg,mNews_Radio<-mean2(News_Radio),mNews_Television<-mean2(News_Television),mNews_Newspaper<-mean2(News_Newspaper),mNews_Internet<-mean2(News_Internet),mNews_Social_media<-mean2(News_Social_media))))
names(m)<-c("COUNTRY2","year","mNews_Radio","mNews_Television","mNews_Newspaper","mNews_Internet","mNews_Social_media")
edit(m)

m2<-m
	dd<-rep(NA,dim(m)[2])
m2<-rbind(dd,m2)		#1行下にずらす
m2<-m2[-dim(m2)[1],]	#最後の行は除く
	edit(m2)
names(m2)<-paste("l",names(m2),sep="_")
m2<-cbind(m,m2)

m2$l_COUNTRY2[1]<-""	#NAだと次の処理がエラーになるので
m2[m2$COUNTRY2!=m2$l_COUNTRY2,c("l_COUNTRY2","l_year","l_mNews_Radio","l_mNews_Television","l_mNews_Newspaper","l_mNews_Internet","l_mNews_Social_media")]<-NA
	edit(m2)

dim(AfrodatAll)
AfrodatAll2<-merge(AfrodatAll,m2,by=c("COUNTRY2","year"))	#各国の平均をつける
dim(AfrodatAll2)
names(AfrodatAll2)
	save(AfrodatAll2,file="0AfrodatAll2.rda")


#-------
a<-xyplot(News_Radio~year|COUNTRY2,data=AfrodatAll,
           panel=function(x,y){
                      panel.xyplot(x,y)
                      panel.abline(lsfit(x,y))
					  }
		)

b<-xyplot(News_Television~year|COUNTRY2,data=AfrodatAll,
           panel=function(x,y){
                      panel.xyplot(x,y)
                      panel.abline(lsfit(x,y))
					  }
		)


plot(a+b)


#----
a<-xyplot(mNews_Radio~year|COUNTRY2,data=m2,
           panel=function(x,y){
                      panel.xyplot(x,y)
                      panel.abline(lsfit(x,y))
					  }
		)

b<-xyplot(mNews_Television~year|COUNTRY2,data=m2,
           panel=function(x,y){
                      panel.xyplot(x,y)
                      panel.abline(lsfit(x,y))
					  }
		)


plot(a+b)



#国による違いの有無
res0<-lme(mNews_Radio~1,random=~1|COUNTRY2,data=AfrodatAll2)
	summary(res0)
	VarCorr(res0)
#        AIC       BIC   logLik
#  -65863.42 -65832.12 32934.71
#            Variance   StdDev   
#(Intercept) 0.19136549 0.4374534　　国による違いのVarが大きい
#Residual    0.04493447 0.2119775


res1<-update(res0,.~.+year)	#時間に比例､ランダム切片
	summary(res1)
#        AIC       BIC   logLik
#  -133062.1 -133020.4 66535.04
#               Value Std.Error     DF   t-value p-value
#(Intercept) 42.56237 0.15894351 250249  267.7830       0
#year        -0.01980 0.00007129 250249 -277.6865       0

res2<-update(res1,.~.,random=~year|COUNTRY2)	#傾きランダム
	summary(res2)
#        AIC     BIC   logLik
#  -286796.6 -286734 143404.3
#               Value Std.Error     DF   t-value p-value
#(Intercept) 52.79823  36.52757 250249  1.445435  0.1483
#year        -0.02487   0.01814 250249 -1.371302  0.1703

res2c<-update(res2,.~.,correlation=corAR1())	#系列相関
	summary(res2c)
#Error: 'sumLenSq := sum(table(groups)^2)' = 2.37085e+09 is too large.
# Too large or no groups in your correlation structure?

#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Radio")
dat<-m[,c(1:2,4)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="TV")

dat<-m[,c(1:2,5)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Newspaper")

dat<-m[,c(1:2,6)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Internet")

dat<-m[,c(1:2,7)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Social_media ")


#-------



save.image("0Afrodat.img")
