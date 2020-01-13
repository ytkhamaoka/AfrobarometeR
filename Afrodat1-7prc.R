#--------
#----Afrodatae wave 1-7を読み込んだデータを使って各種処理
#----2019/12/10 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　


#install.packages(c("dplyr","haven","memisc","biglm","nlme","ICC","lavaan","semTools","biglm","car"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形		lmerオブジェクトは処理不可能
library(stargazer)	#lmの出力整形  lmerオブジェクトも処理可能
library(biglm)	#bigdata lm,glm
library(nlme)	#multi-level
library(lme4)	#multi-level
library(mgcv)	#GAM(multi-level for bigdata)

library(car)	#vif

library(lattice)	#lattice plot
library(ICC)	#級内相関
library(lavaan)	
library(semTools)	
library(biglm)

#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")

#load("0Afrodat.img");ls()

#Afrodat1-Afrodat7　やそれらの共通の変数をまとめたAfrodatAllなどが入っている

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開


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

# naの数をカウント
sum.NA<-function(x){
	m<-sum(is.na(x))
	return(m)
	}

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
		print(dat[,c("t","y","cnam")])		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax+1),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}


#----メディア間の比較 時系列プロット
plot_media_usage<-function(dat0,country_nam,...){
#datの中にCOUNTRY2
dat<-dat0[dat0$COUNTRY2==country_nam,]
	print(list(country_nam,dim(dat)))
plot(dat$year,dat$mNews_Radio,type="l",xlim=c(2000,2018),ylim=c(0,4),...)	#ylab="Usage as News Source"
	lines(dat$year,dat$mNews_Television,type="l",col="Red")
	lines(dat$year,dat$mNews_Newspaper,type="l",col="Purple")
	lines(dat$year,dat$mNews_Internet,type="l",col="Blue")
	lines(dat$year,dat$mNews_Social_media,type="l",col="Green")
#	legend("topleft",c(dat$COUNTRY2[1],"Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Black","Red","Purple","Blue","Green"))
	legend("topleft",c(country_nam))	#,"Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Black","Red","Purple","Blue","Green"))
}



#--------------

load(file="0AfrodatAllN.rda");names(AfrodatAllN)	#afro1-7の使う変数データにWbankの国レベルデータ(1次らぐつき)をつけたもの


#----------------平均値		国別､年別
#AfrodatAllN		"Own_Radio","Own_TV","Own_Auto","Own_Mbphone",
AfrodatAllNg<-group_by(AfrodatAllN, AfrodatAllN$COUNTRY2,AfrodatAllN$year)	#国､年別に集計することを指定

(mdat<-summarise(AfrodatAllNg,N=n(),
mean2(wave),mean2(year),mean2(COUNTRY2),
mean2(dUrban),mean2(Age),mean2(Gender_f),mean2(Language),mean2(Race),
mean2(Cond_econ),mean2(Cond_your_liv),mean2(Relative_live),
mean2(gone_food),mean2(gone_water),mean2(gone_med),mean2(gone_fuel),mean2(gone_cash),mean2(gone_electricity),
mean2(Interest_pubaff),
mean2(Discuss_politics),mean2(dDiscuss_politics),
mean2(Mem_religious),mean2(Mem_voluntary),
mean2(Cit_action_Attend_meeting),mean2(Cit_action_raise_issue),
mean2(Diss_request_government),mean2(Diss_Contact_official),mean2(Diss_Attend_demonstration),
mean2(Democ_pref),mean2(dDemoc_pref),
mean2(Democ_nation),mean2(Democ_satis),
mean2(Trust_president),mean2(Trust_parliament),mean2(Trust_police),mean2(Trust_traditional_leaders),mean2(Trust_religious_leaders),
mean2(Use_Inet),mean2(Use_Mbphone),
#mean2(Employment_status),
#mean2(Occupation),
mean2(Education),
mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
	mOwn_Radio=mean2(Own_Radio),mOwn_TV=mean2(Own_TV),mOwn_Auto=mean2(Own_Auto),mOwn_Mbphone=mean2(Own_Mbphone)
))

edit(mdat)

mdat<-as.data.frame(mdat)
	names(mdat)
# [1] "AfrodatAllN$COUNTRY2"             "AfrodatAllN$year"                 "N"                                "mean2(wave)"                     
# [5] "mean2(year)"                      "mean2(COUNTRY2)"                  "mean2(dUrban)"                    "mean2(Age)"                      
# [9] "mean2(Gender_f)"                  "mean2(Language)"                  "mean2(Race)"                      "mean2(Cond_econ)"                
#[13] "mean2(Cond_your_liv)"             "mean2(Relative_live)"             "mean2(gone_food)"                 "mean2(gone_water)"               
#[17] "mean2(gone_med)"                  "mean2(gone_fuel)"                 "mean2(gone_cash)"                 "mean2(gone_electricity)"         
#[21] "mean2(Interest_pubaff)"           "mean2(Discuss_politics)"          "mean2(dDiscuss_politics)"         "mean2(Mem_religious)"            
#[25] "mean2(Mem_voluntary)"             "mean2(Cit_action_Attend_meeting)" "mean2(Cit_action_raise_issue)"    "mean2(Diss_request_government)"  
#[29] "mean2(Diss_Contact_official)"     "mean2(Diss_Attend_demonstration)" "mean2(Democ_pref)"                "mean2(dDemoc_pref)"              
#[33] "mean2(Democ_nation)"              "mean2(Democ_satis)"               "mean2(Trust_president)"           "mean2(Trust_parliament)"         
#[37] "mean2(Trust_police)"              "mean2(Trust_traditional_leaders)" "mean2(Trust_religious_leaders)"   "mean2(Use_Inet)"                 
#[41] "mean2(Use_Mbphone)"               "mean2(Education)"                 "mNews_Radio"                      "mNews_Television"                
#[45] "mNews_Newspaper"                  "mNews_Internet"                   "mNews_Social_media"               "mOwn_Radio"                      
#[49] "mOwn_TV"                          "mOwn_Auto"                        "mOwn_Mbphone"                    


names(mdat)[1:2]<-c("COUNTRY2","year")


#	ニュースソースとしての利用状況　　すべての国についてメディア毎
dat<-mdat[,c("COUNTRY2","year","mNews_Radio")];names(dat)<-c("cnam","t","y")		#低下傾向
	group_trend_plot(dat,lab="Radio")
dat<-mdat[,c("COUNTRY2","year","mNews_Television")];names(dat)<-c("cnam","t","y")	#ほぼ横ばい　7から低下国も
	group_trend_plot(dat,lab="TV")

dat<-mdat[,c("COUNTRY2","year","mNews_Newspaper")];names(dat)<-c("cnam","t","y")	#低下傾向
	group_trend_plot(dat,lab="Newspaper")

dat<-mdat[,c("COUNTRY2","year","mNews_Internet")];names(dat)<-c("cnam","t","y")		#増加
	group_trend_plot(dat,lab="Internet")

dat<-mdat[,c("COUNTRY2","year","mNews_Social_media")];names(dat)<-c("cnam","t","y")	#増加
	group_trend_plot(dat,lab="Social_media ")

#国毎にメディア比較
(cnam<-unique(mdat$COUNTRY2))
length(cnam)	#37
	par(mar=c(0.,0.,0.,0.),mfrow=c(5,8))	#c(0.5,0.5,0.5,0.5)
	for(i in seq(1,length(cnam))){
		plot_media_usage(mdat,cnam[i],xaxt="n",yaxt="n")	#軸の目盛り消す
		}

#6wave 以上ある国のみ
(d<-as.data.frame(table(mdat$COUNTRY2)))
sum(d$Freq>=6)	#[1] 12
	cnam<-as.character(d[,1])
	par(mar=c(0.,0.,0.,0.),mfrow=c(4,4))	#c(0.5,0.5,0.5,0.5)
	jj<-0
	for(ii in seq(1,length(cnam))){
		if(d$Freq[ii]>=6){
			if(jj==1){
					plot_media_usage(mdat,cnam[ii])	#軸の目盛り消さず
				}else{
					plot_media_usage(mdat,cnam[ii],xaxt="n",yaxt="n")	#軸の目盛り消す
					}
		if(jj==1){
		legend("bottomleft",c("Radio","TV","Newspaper","Internet","Social_media"),text.col=c("Black","Red","Purple","Blue","Green"))
			}
			jj<-jj+1
		}
		}

#----所有　　　財毎に
dat<-mdat[,c("COUNTRY2","year","mOwn_Radio")];names(dat)<-c("cnam","t","y")	##40-90%   w3から大きくはかわらず｡　w7では低下の国が多い(が質問方法がかわった)
	group_trend_plot(dat,lab="own Radio")

dat<-mdat[,c("COUNTRY2","year","mOwn_TV")];names(dat)<-c("cnam","t","y")	#10-90%   w3から微増傾向　w7では低下の国が多い(が質問方法がかわった)
	group_trend_plot(dat,lab="own TV")

dat<-mdat[,c("COUNTRY2","year","mOwn_Auto")];names(dat)<-c("cnam","t","y")	#0-40%s
	group_trend_plot(dat,lab="own Auto")

dat<-mdat[,c("COUNTRY2","year","mOwn_Mbphone")];names(dat)<-c("cnam","t","y")	#w4　だと30%程度の国もあるが､多くは50%以上｡　W6だと､多くの国で8割を超えている
	group_trend_plot(dat,lab="own Mbphone")


##----------変数定義 
AfrodatAllN$PopDens<-AfrodatAllN$Population__total2/AfrodatAllN$Surface_area2
	AfrodatAllN$lgPopDens<-log(AfrodatAllN$PopDens)
#人口密度 対数
AfrodatAllN$PopDens<-AfrodatAllN$Population__total2/AfrodatAllN$Surface_area2
	AfrodatAllN$lgPopDens<-log(AfrodatAllN$PopDens)
	AfrodatAllN$lgPopDensc<-AfrodatAllN$lgPopDens-ave(AfrodatAllN$lgPopDens,AfrodatAllN$COUNTRY2)
#モバイル普及率
AfrodatAllN$MobDifusion<-AfrodatAllN$Mobile_cellular_subscriptions2/AfrodatAllN$Population__total2	#人口はそんなに変わらないので､あまり意味なし

#国レベル中心化  GDPとMobile_cellular_subscriptions2cについては対数をとる
AfrodatAllN$yearc<-AfrodatAllN$year-ave(AfrodatAllN$year,AfrodatAllN$COUNTRY2)
	AfrodatAllN$lgyear<-log(AfrodatAllN$year)
	AfrodatAllN$lgyearc<-AfrodatAllN$yearc-ave(AfrodatAllN$year,AfrodatAllN$COUNTRY2)
AfrodatAllN$lgGDP_per_capita2<-log(AfrodatAllN$GDP_per_capita2)
	AfrodatAllN$lgGDP_per_capita2c<-AfrodatAllN$lgGDP_per_capita2-ave(AfrodatAllN$lgGDP_per_capita2,AfrodatAllN$COUNTRY2)
AfrodatAllN$Access_to_electricity2c<-AfrodatAllN$Access_to_electricity2-ave(AfrodatAllN$Access_to_electricity2,AfrodatAllN$COUNTRY2)

AfrodatAllN$lgMobile_cellular_subscriptions2<-log(AfrodatAllN$Mobile_cellular_subscriptions2)
	AfrodatAllN$lgMobile_cellular_subscriptions2c<-AfrodatAllN$lgMobile_cellular_subscriptions2-ave(AfrodatAllN$lgMobile_cellular_subscriptions2,AfrodatAllN$COUNTRY2)

attach(AfrodatAllN,warn=F)
cor(data.frame(year,lgPopDens,MobDifusion,lgGDP_per_capita2c,lgMobile_cellular_subscriptions2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,lgMobile_cellular_subscriptions2c*yearc,Access_to_electricity2c*yearc),use="complete")


summary(AfrodatAllN)
#+dlang_Sudanese_Arabic+dlang_Afrikaans+dlang_Kirund+dlang_Algerian_Arabic+#dRace_Col+dRace_SAs+dRace_EAs	+  割合が低い項目は除く

edit(mdat)

#国､年別の所有割合
dd<-mdat[,c("COUNTRY2","year","mOwn_Mbphone")]
(d0<-reshape(dd,direction = "wide", v.names =c("mOwn_Mbphone"),
				idvar=c("COUNTRY2"),timevar="year"))

#列を年順に並べ替える
names(d0);dim(d0)
#[1] "COUNTRY2"          "mOwn_Mbphone.2012" "mOwn_Mbphone.2014" "mOwn_Mbphone.2005" "mOwn_Mbphone.2008" "mOwn_Mbphone.2017" "mOwn_Mbphone.2000"
#[8] "mOwn_Mbphone.2002"
(d0<-d0[,c(1,order(names(d0)[2:8])+1)])

(d0$nobs<-7-apply(d0[,2:8],1,sum.NA))
d0[d0$nobs>=4,]
dim(d0[d0$nobs>=4,])	#[1] 20  9	20ヵ国
names(d0)

mdat<-merge(mdat,d0[,c("COUNTRY2","nobs")],by.x="COUNTRY2",by.y="COUNTRY2")
dat<-mdat[mdat$nobs>=4,c("COUNTRY2","year","mOwn_Mbphone")];names(dat)<-c("cnam","t","y")	#w4　だと30%程度の国もあるが､多くは50%以上｡　W6だと､多くの国で8割を超えている
	group_trend_plot(dat,lab="own Mbphone")

dim(AfrodatAllN)
AfrodatAllN2<-merge(AfrodatAllN,d0[,c("COUNTRY2","nobs")],by.x="COUNTRY2",by.y="COUNTRY2")
	names(AfrodatAllN2);dim(AfrodatAllN2)


#Mobile_cellular_subscriptions2やMobDifusionはyearなどと相関高い
#yearc*GDP_per_capita2c+yearc*Access_to_electricity2c+yearc*Mobile_cellular_subscriptions2c+


##		Wave毎に分析
v<-as.formula('~Own_TV+Age+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs+
dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_none+dReligion_Lutheran+dReligion_Methodist+dReligion_Independent+dReligion_SeventhDay+
as.factor(COUNTRY2)')	#dRace_Oth Religion+as.factor(Education)+

res_oOwn_Mbphone<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family= binomial(link = "logit"), data=AfrodatAllN)
	summary(res_oOwn_Mbphone);vif(res_oOwn_Mbphone)
res_oOwn_Mbphone_w4<-glm(formula(paste("Own_Mbphone~",v,sep="")[2]),family= binomial(link = "logit"), data=AfrodatAllN[AfrodatAllN$wave==4,])
	summary(res_oOwn_Mbphone_w4);vif(res_oOwn_Mbphone_w4)
res_oOwn_Mbphone_w5<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==5,])
	summary(res_oOwn_Mbphone_w5);vif(res_oOwn_Mbphone_w5)
res_oOwn_Mbphone_w6<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==6,])
	summary(res_oOwn_Mbphone_w6);vif(res_oOwn_Mbphone_w6)
res_oOwn_Mbphone_w7<-update(res_oOwn_Mbphone_w4,.~.,data=AfrodatAllN[AfrodatAllN$wave==7,])
	summary(res_oOwn_Mbphone_w7);vif(res_oOwn_Mbphone_w7)

mtable(res_oOwn_Mbphone,res_oOwn_Mbphone_w4,res_oOwn_Mbphone_w5,res_oOwn_Mbphone_w6,res_oOwn_Mbphone_w7,
	summary.stats=c("N","AIC"))


#------単純にプール   中心か
res_oOwn_Mbphone_2<-glm(Own_Mbphone~yearc+lgGDP_per_capita2c+lgMobile_cellular_subscriptions2c+Access_to_electricity2c+lgPopDensc+	#PopDens+ VIF22なので除外
Age+Gender_f+Education+	dUrban+	#dUrbanがすべて欠損の国は分析対象に含まれない
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+	#dRace_SAs+dRace_EAs　VIFを高める可能性があるので除外
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Sesotho+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+	#dlang_Egyptian_Arabic+ Egyptのみなので除外	
dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_none+dReligion_Lutheran+dReligion_Methodist+dReligion_Independent+dReligion_SeventhDay+	#dlang_Crioulo+ VIF1100　　dlang_Kirund+　12なので除外
as.factor(COUNTRY2),family= binomial(link = "logit"), data=AfrodatAllN)	#+Mobile_cellular_subscriptions2+dlang_Sudanese_Arabic+
	summary(res_oOwn_Mbphone_2);vif(res_oOwn_Mbphone_2)

res_oOwn_Mbphone_2.1<-update(res_oOwn_Mbphone_2,.~.+lgMobile_cellular_subscriptions2c:yearc)	#yearc:Mobile_cellular_subscriptions2c 　マイナス　　VIF問題なし
	summary(res_oOwn_Mbphone_2.1);vif(res_oOwn_Mbphone_2.1)

res_oOwn_Mbphone_2.1.2<-update(res_oOwn_Mbphone_2,.~.+Access_to_electricity2c:yearc)	#+yearc:Access_to_electricity2c 
	summary(res_oOwn_Mbphone_2.1.2);vif(res_oOwn_Mbphone_2.1.2)

res_oOwn_Mbphone_2.1.3<-update(res_oOwn_Mbphone_2,.~.+GDP_per_capita2c:yearc)	#+yearc:Access_to_electricity2c 
	summary(res_oOwn_Mbphone_2.1.3);vif(res_oOwn_Mbphone_2.1.3)


res_oOwn_Mbphone_2.1.4<-update(res_oOwn_Mbphone_2,.~.+Mobile_cellular_subscriptions2c:yearc+Access_to_electricity2c:yearc+GDP_per_capita2c:yearc)	#+yearc:Access_to_electricity2c 
	summary(res_oOwn_Mbphone_2.1.4)

mtable(res_oOwn_Mbphone_2,res_oOwn_Mbphone_2.1,res_oOwn_Mbphone_2.1.2,res_oOwn_Mbphone_2.1.3,res_oOwn_Mbphone_2.1.4,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p


#-----------------
#-----w4-7まで参加している国(携帯の所有
#-----------------
summary(AfrodatAllN2[AfrodatAllN2$nobs>=4,])

#----マルチレベル##yearにランダム切片と傾き
mres_oOwn_Mbphone<-glmer(Own_Mbphone~log(year)+	#log(GDP_per_capita2)+log(Mobile_cellular_subscriptions2)+Access_to_electricity2c+PopDens+
log(Age)+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+	#dRace_Arab+	
dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent+	#dReligion_Lutheran+dReligion_Methodist+dReligion_SeventhDay+
(1|COUNTRY2),family= binomial(link = "logit"), data=AfrodatAllN2[AfrodatAllN2$nobs>=4,])	
	summary(mres_oOwn_Mbphone)

#国レベルの変数導入(個人レベル平均に)	log(Mobile_cellular_subscriptions2)
mres_oOwn_Mbphone.1<-update(mres_oOwn_Mbphone,.~.+lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2)	
	summary(mres_oOwn_Mbphone.1)

mres_oOwn_Mbphone.2<-update(mres_oOwn_Mbphone.1,.~.+(lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2|COUNTRY2))
	summary(mres_oOwn_Mbphone.2)

#個人レベルとの交互作用  
mres_oOwn_Mbphone.3<-update(mres_oOwn_Mbphone.2,.~.+lgGDP_per_capita2c:log(year)+Mem_religious:log(year))
	summary(mres_oOwn_Mbphone.3)

mtable(mres_oOwn_Mbphone,mres_oOwn_Mbphone.1,mres_oOwn_Mbphone.2,mres_oOwn_Mbphone.3,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p

#-------------
#-----携帯の所有のうちw4での普及率が低かった5ヵ国(w4-7まで参加している国)
#Zimbabweであったが､その後は急速に普及が進み､2017年時点では84%に達している｡Burkina Faso､Maliも同様に､2008年時点では低かったものの､2017年時点にかけて急速に普及している｡一方で､2008年時点で普及率が30%程度であった､Malawi､Madagascarは増加傾向にあるものの､それぞれ2017年時点で50.8%､46.2%にとどまっている｡
#-------------
summary(AfrodatAllN2[AfrodatAllN2$COUNTRY2=="ZIM"|AfrodatAllN2$COUNTRY2=="BFO"|AfrodatAllN2$COUNTRY2=="MLI"|AfrodatAllN2$COUNTRY2=="MLW"|AfrodatAllN2$COUNTRY2=="MAD"&AfrodatAllN2$wave>=4,])

dat<-mdat[mdat$COUNTRY2=="ZIM"|mdat$COUNTRY2=="BFO"|mdat$COUNTRY2=="MLI"|mdat$COUNTRY2=="MLW"|mdat$COUNTRY2=="MAD"&mdat$year>=2007,c("COUNTRY2","year","mOwn_Mbphone")];names(dat)<-c("cnam","t","y")	#w4　だと30%程度の国もあるが､多くは50%以上｡　W6だと､多くの国で8割を超えている
	group_trend_plot(dat,lab="own Mbphone")

#------2008の低普及5ヵ国別にロジット	共通変数
v<-as.formula('~yearc+log(Age)+Gender_f+Education+	+dUrban+
dEmployment_status_no+
dRace_BAf+dRace_Wh+	dRace_Arab+	
dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent+
gone_water+gone_cash+
Mem_religious+Mem_voluntary
')	#dRace_Oth Religion+as.factor(Education)+

#国レベルは　Access_to_electricity2cだとトートロジーなのでAccess_to_electricity2cにする
#ZIM　　　分散がない変数は除外　
dat<-AfrodatAllN2[AfrodatAllN2$COUNTRY2=="ZIM"&AfrodatAllN2$wave>=4,]
attach(dat,warn=F)
#lgGDP_per_capita2c とAccess_to_electricity2c の相関高い  yearc とAccess_to_electricity2c*yearcも
cor(data.frame(yearc,PopDens,MobDifusion,lgGDP_per_capita2c,Access_to_electricity2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,Access_to_electricity2c*yearc,Access_to_electricity2c*yearc),use="complete")

res_oOwn_Mbphone.ZIM<-glm(formula(paste("Own_Mbphone~",v,"-dRace_Arab-dlang_Swahili-dlang_Afrikaans-dlang_Chichewa-dlang_Akan-dlang_Crioulo-dlang_Sesotho",sep="")[2]),family= binomial(link = "logit"), data=dat)	
	summary(res_oOwn_Mbphone.ZIM);as.data.frame(vif(res_oOwn_Mbphone.ZIM))

#国レベルの導入
res_oOwn_Mbphone.ZIM.1<-update(res_oOwn_Mbphone.ZIM,.~.+Access_to_electricity2c)	# +lgGDP_per_capita2cAccess_to_electricity2c+入れるとNA
	summary(res_oOwn_Mbphone.ZIM.1);as.data.frame(vif(res_oOwn_Mbphone.ZIM.1))	#　lgGDP_per_capita2c　VIF603なので除外 lgPopDensc+
#国レベル×年の導入
res_oOwn_Mbphone.ZIM.2<-update(res_oOwn_Mbphone.ZIM.1,.~.+yearc:Access_to_electricity2c)	#log(year):Access_to_electricity2c+log(year):lgGDP_per_capita2c
	summary(res_oOwn_Mbphone.ZIM.2);as.data.frame(vif(res_oOwn_Mbphone.ZIM.2))	#yearc:Access_to_electricity2c            VIF 133.198992 となる
#
mtable(res_oOwn_Mbphone.ZIM,res_oOwn_Mbphone.ZIM.1,res_oOwn_Mbphone.ZIM.2,
	summary.stats=c("N","Deviance","AIC","BIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p
#res_oOwn_Mbphone.ZIM.2

#---BFO
dat<-AfrodatAllN2[AfrodatAllN2$COUNTRY2=="BFO"&AfrodatAllN2$wave>=4,]
attach(dat,warn=F)
cor(data.frame(yearc,PopDens,MobDifusion,lgGDP_per_capita2c,Access_to_electricity2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,Access_to_electricity2c*yearc,Access_to_electricity2c*yearc),use="complete")

res_oOwn_Mbphone.BFO<-glm(formula(paste("Own_Mbphone~",v,"-dRace_Arab-dlang_Swahili-dlang_Afrikaans-dlang_Chichewa-dlang_Akan-dlang_Crioulo-dlang_Sesotho-dlang_Shona",sep="")[2]),family= binomial(link = "logit"), data=dat)	
	summary(res_oOwn_Mbphone.BFO);as.data.frame(vif(res_oOwn_Mbphone.BFO))
res_oOwn_Mbphone.BFO.1<-update(res_oOwn_Mbphone.BFO,.~.+Access_to_electricity2c)		#VIF15
	summary(res_oOwn_Mbphone.BFO.1);as.data.frame(vif(res_oOwn_Mbphone.BFO.1))
res_oOwn_Mbphone.BFO.2<-update(res_oOwn_Mbphone.BFO.1,.~.+yearc:Access_to_electricity2c)		#922.458971
	summary(res_oOwn_Mbphone.BFO.2);as.data.frame(vif(res_oOwn_Mbphone.BFO.2))

mtable(res_oOwn_Mbphone.BFO,res_oOwn_Mbphone.BFO.1,res_oOwn_Mbphone.BFO.2,	#res_oOwn_Mbphone.BFO.3,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat")
#res_oOwn_Mbphone.BFO

#---MLI
dat<-AfrodatAllN2[AfrodatAllN2$COUNTRY2=="MLI"&AfrodatAllN2$wave>=4,]
attach(dat,warn=F)
cor(data.frame(yearc,PopDens,MobDifusion,lgGDP_per_capita2c,Access_to_electricity2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,Access_to_electricity2c*yearc,Access_to_electricity2c*yearc),use="complete")

res_oOwn_Mbphone.MLI<-glm(formula(paste("Own_Mbphone~",v,"-dRace_Arab-dlang_English-dlang_Swahili-dlang_Afrikaans-dlang_Chichewa-dlang_Akan-dlang_Crioulo-dlang_Sesotho-dlang_Shona",sep="")[2]),family= binomial(link = "logit"), data=dat)	
	summary(res_oOwn_Mbphone.MLI);as.data.frame(vif(res_oOwn_Mbphone.MLI))
res_oOwn_Mbphone.MLI.1<-update(res_oOwn_Mbphone.MLI,.~.+Access_to_electricity2c)	
	summary(res_oOwn_Mbphone.MLI.1);as.data.frame(vif(res_oOwn_Mbphone.MLI.1))	#Access_to_electricity2c                   10.409493
res_oOwn_Mbphone.MLI.2<-update(res_oOwn_Mbphone.MLI.1,.~.+yearc:Access_to_electricity2c)	
	summary(res_oOwn_Mbphone.MLI.2);as.data.frame(vif(res_oOwn_Mbphone.MLI.2))	#Access_to_electricity2c                        206.777052


mtable(res_oOwn_Mbphone.MLI,res_oOwn_Mbphone.MLI.1,res_oOwn_Mbphone.MLI.2,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat")
#res_oOwn_Mbphone.MLI


#---MLW
dat<-AfrodatAllN2[AfrodatAllN2$COUNTRY2=="MLW"&AfrodatAllN2$wave>=4,]
attach(dat,warn=F)
cor(data.frame(yearc,PopDens,MobDifusion,lgGDP_per_capita2c,Access_to_electricity2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,Access_to_electricity2c*yearc,Access_to_electricity2c*yearc),use="complete")

res_oOwn_Mbphone.MLW<-glm(formula(paste("Own_Mbphone~",v,"-dRace_Arab-dlang_Afrikaans-dlang_Akan-dlang_Crioulo-dlang_Sesotho-dlang_Shona",sep="")[2]),family= binomial(link = "logit"), data=dat)	
	summary(res_oOwn_Mbphone.MLW);as.data.frame(vif(res_oOwn_Mbphone.MLW))
res_oOwn_Mbphone.MLW.1<-update(res_oOwn_Mbphone.MLW,.~.+Access_to_electricity2c)		#Access_to_electricity2c　　VIF11
	summary(res_oOwn_Mbphone.MLW.1);as.data.frame(vif(res_oOwn_Mbphone.MLW.1))
res_oOwn_Mbphone.MLW.2<-update(res_oOwn_Mbphone.MLW.1,.~.+yearc:Access_to_electricity2c)	#VIF290などAccess_to_electricity2c                         17.965412
	summary(res_oOwn_Mbphone.MLW.2);as.data.frame(vif(res_oOwn_Mbphone.MLW.2))

mtable(res_oOwn_Mbphone.MLW,res_oOwn_Mbphone.MLW.1,res_oOwn_Mbphone.MLW.2,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat")
#res_oOwn_Mbphone.MLW.1だがVIF14

#---MAD
dat<-AfrodatAllN2[AfrodatAllN2$COUNTRY2=="MAD"&AfrodatAllN2$wave>=4,]
attach(dat,warn=F)
cor(data.frame(yearc,PopDens,MobDifusion,lgGDP_per_capita2c,Access_to_electricity2c,Access_to_electricity2c,lgGDP_per_capita2c*yearc,Access_to_electricity2c*yearc,Access_to_electricity2c*yearc),use="complete")

res_oOwn_Mbphone.MAD<-glm(formula(paste("Own_Mbphone~",v,"-dlang_Swahili-dlang_Afrikaans-dlang_Chichewa-dlang_Akan-dlang_Other-dlang_Crioulo-dlang_Sesotho-dlang_Shona",sep="")[2]),family= binomial(link = "logit"), data=dat)	
	summary(res_oOwn_Mbphone.MAD);as.data.frame(vif(res_oOwn_Mbphone.MAD))
res_oOwn_Mbphone.MAD.1<-update(res_oOwn_Mbphone.MAD,.~.+Access_to_electricity2c)	
	summary(res_oOwn_Mbphone.MAD.1);as.data.frame(vif(res_oOwn_Mbphone.MAD.1))
res_oOwn_Mbphone.MAD.2<-update(res_oOwn_Mbphone.MAD.1,.~.+yearc:Access_to_electricity2c)	#Access_to_electricity2c                       1388.275822.
	summary(res_oOwn_Mbphone.MAD.2);as.data.frame(vif(res_oOwn_Mbphone.MAD.2))

mtable(res_oOwn_Mbphone.MAD,res_oOwn_Mbphone.MAD.1,res_oOwn_Mbphone.MAD.2,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat")
#res_oOwn_Mbphone.MAD


#各国の最良モデル
mtable(res_oOwn_Mbphone.ZIM.2,res_oOwn_Mbphone.BFO,res_oOwn_Mbphone.MLI,res_oOwn_Mbphone.MLW.1,res_oOwn_Mbphone.MAD,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat",signif.symbols=c("***"=.01,"**"=.05,"*"=.10))	#検定統計量を出力　coef.style="all" 係数､ses,t,p

write.mtable(res_oOwn_Mbphone.ZIM.2,res_oOwn_Mbphone.BFO.1,res_oOwn_Mbphone.MLI.1,res_oOwn_Mbphone.MLW.1,res_oOwn_Mbphone.MAD.1,
	summary.stats=c("Deviance","N","AIC","BIC"),coef.style="stat",signif.symbols=c("***"=.01,"**"=.05,"*"=.10),file="0out.txt",
             format=c("delim"))


#-------------------------
#-----以下は検討中
#-------------------------
#----マルチレベル##yearにランダム切片と傾き
mres_oOwn_Mbphone<-glmer(Own_Mbphone~log(year)+	#log(GDP_per_capita2)+log(Mobile_cellular_subscriptions2)+Access_to_electricity2c+PopDens+
log(Age)+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+	#dRace_Arab+	
dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent+	#dReligion_Lutheran+dReligion_Methodist+dReligion_SeventhDay+
(1|COUNTRY2),family= binomial(link = "logit"), data=dat)	
	summary(mres_oOwn_Mbphone)

#国レベルの変数導入(個人レベル平均に)	log(Mobile_cellular_subscriptions2)
mres_oOwn_Mbphone.1<-update(mres_oOwn_Mbphone,.~.+lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2)	
	summary(mres_oOwn_Mbphone.1)

mres_oOwn_Mbphone.2<-update(mres_oOwn_Mbphone.1,.~.+(lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2c|COUNTRY2))
	summary(mres_oOwn_Mbphone.2)

#個人レベルとの交互作用  
mres_oOwn_Mbphone.3<-update(mres_oOwn_Mbphone.2,.~.+lgGDP_per_capita2c:log(year)+Mem_religious:log(year))
	summary(mres_oOwn_Mbphone.3)


stargazer(mres_oOwn_Mbphone,mres_oOwn_Mbphone.1,mres_oOwn_Mbphone.2,mres_oOwn_Mbphone.3,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat",type="text")	#検定統計量を出力　coef.style="all" 係数､ses,t,p

mtable(mres_oOwn_Mbphone,mres_oOwn_Mbphone.1,mres_oOwn_Mbphone.2,mres_oOwn_Mbphone.3,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p


#最も早い時点のw4について比較
mres_oOwn_Mbphone_w4<-glmer(Own_Mbphone~#log(year)+	#log(GDP_per_capita2)+log(Mobile_cellular_subscriptions2)+Access_to_electricity2c+PopDens+
log(Age)+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+	#dRace_Arab+	
dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent+	#dReligion_Lutheran+dReligion_Methodist+dReligion_SeventhDay+
(1|COUNTRY2),family= binomial(link = "logit"), data=AfrodatAllN2[AfrodatAllN2$nobs>=4&AfrodatAllN2$wave==4,])	
#fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
	summary(mres_oOwn_Mbphone_w4)

#国レベルの変数導入(個人レベル平均に)	log(Mobile_cellular_subscriptions2)
mres_oOwn_Mbphone_w4.1<-update(mres_oOwn_Mbphone_w4,.~.+lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2)	
	summary(mres_oOwn_Mbphone_w4.1)

mres_oOwn_Mbphone_w4.2<-update(mres_oOwn_Mbphone_w4.1,.~.+(lgGDP_per_capita2c+Access_to_electricity2c+lgMobile_cellular_subscriptions2|COUNTRY2))
	summary(mres_oOwn_Mbphone_w4.2)

#個人レベルとの交互作用  経済×教育　　　普及×集団　　
mres_oOwn_Mbphone_w4.3<-update(mres_oOwn_Mbphone_w4.2,.~.+lgGDP_per_capita2c:Education+Mem_religious:lgMobile_cellular_subscriptions2c)
	summary(mres_oOwn_Mbphone_w4.3)


mtable(mres_oOwn_Mbphone_w4,mres_oOwn_Mbphone_w4.1,mres_oOwn_Mbphone_w4.2,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p


#w7について
mres_oOwn_Mbphone_w7<-update(mres_oOwn_Mbphone_w4,.~., data=AfrodatAllN2[AfrodatAllN2$nobs>=4&AfrodatAllN2$wave==7,])	
	summary(mres_oOwn_Mbphone_w7)

mres_oOwn_Mbphone_w7.1<-update(mres_oOwn_Mbphone_w4.1,.~., data=AfrodatAllN2[AfrodatAllN2$nobs>=4&AfrodatAllN2$wave==7,])	
	summary(mres_oOwn_Mbphone_w7.1)


mtable(mres_oOwn_Mbphone_w4,mres_oOwn_Mbphone_w7,
	summary.stats=c("Deviance","N","AIC"),coef.style="stat")	#検定統計量を出力　coef.style="all" 係数､ses,t,p


attach(AfrodatAllN2,warn=F)
fg<-is.na(Own_Mbphone+log(year)+	#log(GDP_per_capita2)+log(Mobile_cellular_subscriptions2)+Access_to_electricity2c+PopDens+
log(Age)+Gender_f+Education+	#dUrban+
Mem_religious+
gone_water+
dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dRace_BAf+dRace_Wh+	#dRace_Arab+	
dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent)
	sum(fg)#[1] 29818



#最も早い時点のw4について比較
mres_oOwn_Mbphone_ba<-bam(Own_Mbphone~	#log(year)+	#log(GDP_per_capita2)+log(Mobile_cellular_subscriptions2)+Access_to_electricity2c+PopDens+
log(Age)+Gender_f+Education+	#dUrban+
#Mem_religious+
#gone_water+
#dEmployment_status_no+   #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
#dRace_BAf+dRace_Wh+	#dRace_Arab+	
#dlang_English+dlang_Swahili+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Crioulo+dlang_Sesotho+dlang_Shona+
#dReligion_none+dReligion_Muslim+dReligion_RomanCatholic+dReligion_Christian+dReligion_Pentecostal+dReligion_Anglican+dReligion_Evangelical+dReligion_Independent+
	s(COUNTRY2, bs = 're'),family= binomial(link = "logit"),method = 'REML', data=AfrodatAllN2[!fg&wave==4,])	#+Mobile_cellular_subscriptions2+dlang_Sudanese_Arabic+dlang_Algerian_Arabic++dlang_Egyptian_Arabic#GDP_per_capita2c+Mobile_cellular_subscriptions2c+Access_to_electricity2c+PopDens+
 #これらはNA2.3万dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+#dUrban+
	summary(mres_oOwn_Mbphone_ba)
#Error in names(dat) <- object$term : 
#  'names' attribute [1] must be the same length as the vector [0]




#-------参考)Round 6で遊ぶ
#---SEM?
names(AfrodatAllNg)
summary(AfrodatAll)

cor(dat<-Afrodat6[,c("gone_food","gone_water","gone_med","gone_cash","gone_fuel","Electric_connection","Mem_religious","Mem_voluntary","Cit_action_Attend_meeting","Cit_action_raise_issue","Ele_campaign_rally","Ele_campaign_meeting","Ele_Attend_persuade","Ele_Attend_Work")],use="complete")

factanal(dat[complete.cases(dat),],2,rotation="promax")
factanal(dat[complete.cases(dat),],3,rotation="promax")
factanal(dat[complete.cases(dat),],4,rotation="promax")
factanal(dat[complete.cases(dat),],5,rotation="promax")

Model.cfa0<- '
	f1lack_food=~gone_food +gone_water+gone_med+gone_cash+gone_fuel #
	f2connect_Elec=~Electric_connection #
	f3community=~Mem_religious+Mem_voluntary #
	f4action=~Cit_action_Attend_meeting +Cit_action_raise_issue #
	f5Election=~Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work#
	'
res.cfa0<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE)
	summary(res.cfa0, fit.measures=TRUE)		
	standardizedSolution(res.cfa0, type = "std.all")

#多母集団
res.cfa0g<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE,group="COUNTRY2")
	summary(res.cfa0g, fit.measures=TRUE)		


#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)
m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))

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
dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Swahili+dlang_Arabic+dlang_Afrikaans+dlang_Chichewa+dlang_Akan+dlang_Other+dlang_Egyptian_Arabic+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM')
##dOccupation_Never+ dOccupation_Other +dRace_Oth 



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


#---
cor(dat<-Afrodat6[,c("gone_food","gone_water","gone_med","gone_cash","gone_fuel","Electric_connection","Mem_religious","Mem_voluntary","Cit_action_Attend_meeting","Cit_action_raise_issue","Ele_campaign_rally","Ele_campaign_meeting","Ele_Attend_persuade","Ele_Attend_Work")],use="complete")

factanal(dat[complete.cases(dat),],2,rotation="promax")
factanal(dat[complete.cases(dat),],3,rotation="promax")
factanal(dat[complete.cases(dat),],4,rotation="promax")
factanal(dat[complete.cases(dat),],5,rotation="promax")

#                          Factor1 Factor2 Factor3 Factor4 Factor5
#gone_food                  0.637                          -0.132 
#gone_water                 0.657                                 
#gone_med                   0.742                                 
#gone_cash                  0.594                          -0.280 
#gone_fuel                  0.679                           0.171 
#Electric_connection       -0.132                           0.547 
#Mem_religious                                      0.555  -0.106 
#Mem_voluntary                                      0.583         
#Cit_action_Attend_meeting                  0.822                 
#Cit_action_raise_issue                     0.753                 
#Ele_campaign_rally                 0.658                  -0.116 
#Ele_campaign_meeting               0.773                         
#Ele_Attend_persuade                0.638                         
#Ele_Attend_Work                    0.664                         
#
#               Factor1 Factor2 Factor3 Factor4 Factor5
#SS loadings      2.225   1.882   1.268   0.689   0.474
#Proportion Var   0.159   0.134   0.091   0.049   0.034
#Cumulative Var   0.159   0.293   0.384   0.433   0.467
#
#Factor Correlations:
#        Factor1 Factor2 Factor3 Factor4 Factor5
#Factor1   1.000 -0.1270  0.3666  0.5700  0.2879
#Factor2  -0.127  1.0000 -0.0769 -0.0925 -0.3653
#Factor3   0.367 -0.0769  1.0000  0.3790  0.0651
#Factor4   0.570 -0.0925  0.3790  1.0000  0.1447
#Factor5   0.288 -0.3653  0.0651  0.1447  1.0000
#
#Test of the hypothesis that 5 factors are sufficient.
#The chi square statistic is 2846.64 on 31 degrees of freedom.
#The p-value is 0 


Model.cfa0<- '
	f1lack_food=~gone_food +gone_water+gone_med+gone_cash+gone_fuel #
	f2connect_Elec=~Electric_connection #
	f3community=~Mem_religious+Mem_voluntary #
	f4action=~Cit_action_Attend_meeting +Cit_action_raise_issue #
	f5Election=~Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work#
	'
res.cfa0<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE)
	summary(res.cfa0, fit.measures=TRUE)		
	standardizedSolution(res.cfa0, type = "std.all")

#多母集団
res.cfa0g<-lavaan(Model.cfa0, data=Afrodat6,auto.var=TRUE,  auto.fix.first=TRUE,auto.fix.single=T,auto.cov.lv.x=TRUE,group="COUNTRY2")
	summary(res.cfa0g, fit.measures=TRUE)		
#	standardizedSolution(res.cfa0g, type = "std.all")	#時間がかかるのでやめる
#lavaan 0.6-5 ended normally after 2234 iterations
#
#  Estimator                                         ML
#  Optimization method                           NLMINB
#  Number of free parameters                       1332
#                                                      
#  Number of observations per group:               Used       Total
#    ALG                                            962        1200
#    BDI                                            845        1200
#    BEN                                            862        1200
#    BFO                                            769        1200
#    BOT                                            967        1200
#    CAM                                            743        1182
#    CDI                                            817        1199
#    CVE                                           1058        1200
#    EGY                                           1044        1198
#    GAB                                            929        1198
#    GHA                                           2004        2400
#    GUI                                            750        1200
#    KEN                                           2140        2397
#    LES                                            602        1200
#    LIB                                            884        1199
#    MAD                                           1030        1200
#    MAU                                           1132        1200
#    MLI                                            922        1200
#    MLW                                            871        2400
#    MOR                                            995        1200
#    MOZ                                           1724        2400
#    NAM                                           1108        1200
#    NGR                                            886        1200
#    NIG                                           2020        2400
#    SAF                                           2153        2390
#    SEN                                            831        1200
#    SRL                                            877        1191
#    STP                                            989        1196
#    SUD                                            904        1200
#    SWZ                                            983        1200
#    TAN                                           1739        2386
#    TOG                                            771        1200
#    TUN                                           1078        1200
#    UGA                                           2011        2400
#    ZAM                                           1015        1199
#    ZIM                                           2083        2400
#                                                                  
#Model Test User Model:
#                                                        
#  Test statistic                              129170.680
#  Degrees of freedom                                2952
#  P-value (Chi-square)                             0.000
#  Test statistic for each group:
#    ALG                                       3637.671
#    BDI                                       2853.361
#    BEN                                       2697.105
#    BFO                                       2287.498
#    BOT                                       2645.085
#    CAM                                       2220.190
#    CDI                                       2669.851
#    CVE                                       3377.988
#    EGY                                       4457.356
#    GAB                                       3140.012
#    GHA                                       5091.285
#    GUI                                       2596.040
#    KEN                                       5875.835
#    LES                                       1615.200
#    LIB                                       2388.104
#    MAD                                       2671.431
#    MAU                                       6738.084
#    MLI                                       2489.523
#    MLW                                       2665.149
#    MOR                                       4941.983
#    MOZ                                       4352.193
#    NAM                                       2730.346
#    NGR                                       2770.312
#    NIG                                       5620.855
#    SAF                                       6504.080
#    SEN                                       2660.956
#    SRL                                       2405.757
#    STP                                       3083.422
#    SUD                                       2422.518
#    SWZ                                       2540.061
#    TAN                                       4846.536
#    TOG                                       2686.043
#    TUN                                       3620.984
#    UGA                                       5987.098
#    ZAM                                       2712.112
#    ZIM                                       7168.656
#
#Model Test Baseline Model:
#
#  Test statistic                            157501.037
#  Degrees of freedom                              3276
#  P-value                                        0.000
#
#User Model versus Baseline Model:
#
#  Comparative Fit Index (CFI)                    0.182
#  Tucker-Lewis Index (TLI)                       0.092
#
#Loglikelihood and Information Criteria:
#
#  Loglikelihood user model (H0)            -659189.336
#  Loglikelihood unrestricted model (H1)             NA
#                                                      
#  Akaike (AIC)                             1321042.672
#  Bayesian (BIC)                           1332542.362
#  Sample-size adjusted Bayesian (BIC)      1328309.258
#
#Root Mean Square Error of Approximation:
#
#  RMSEA                                          0.193
#  90 Percent confidence interval - lower         0.192
#  90 Percent confidence interval - upper         0.193
#  P-value RMSEA <= 0.05                          0.000
#
#Standardized Root Mean Square Residual:
#
#  SRMR                                           1.806
#
#Parameter Estimates:
#
#  Information                                 Expected
#  Information saturated (h1) model          Structured
#  Standard errors                             Standard


res.cfa0gmi<-measurementInvariance(model=Model.cfa0, data=Afrodat6,group="COUNTRY2")

#Measurement invariance models:#
#Chi-Squared Difference Test
#                 Df     AIC     BIC Chisq Chisq diff Df diff Pr(>Chisq)    
#fit.configural 2448 1205049 1220900 12169                                  
#fit.loadings   2763 1210476 1223608 18226       6057     315  < 2.2e-16 ***
#fit.intercepts 3078 1233377 1243789 41758      23531     315  < 2.2e-16 ***
#fit.means      3253 1272171 1281072 80901      39144     175  < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Fit measures:
#                 cfi rmsea cfi.delta rmsea.delta
#fit.configural 0.937 0.059        NA          NA
#fit.loadings   0.900 0.070     0.037       0.011
#fit.intercepts 0.749 0.104     0.151       0.035
#fit.means      0.497 0.144     0.253       0.039



#----
attach(Afrodat6,warn=F)
Afrodat6$f1lack_food<-(gone_food +gone_water+gone_med+gone_cash+gone_fuel)/5 #
Afrodat6$f2connect_Elec<-Electric_connection #
Afrodat6$f3community<-(Mem_religious+Mem_voluntary )/2#
Afrodat6$f4action<-(Cit_action_Attend_meeting +Cit_action_raise_issue)/2 #
Afrodat6$f5Election<-(Ele_campaign_rally +Ele_campaign_meeting+Ele_Attend_persuade+ Ele_Attend_Work)/4 	#

	summary(Afrodat6)	#  f4action は　　NA　11373サンプル

#ダミー変数と上で定義した変数で
v<-as.formula('~f1lack_food+f2connect_Elec+
f3community+f4action+
Age+Gender_f+Education+
dEmployment_status_looking+dEmployment_status_part_time+dEmployment_status_full_time+
dOccupation_Student+dOccupation_Housewife_homemaker+dOccupation_primary+dOccupation_Trader+dOccupation_Retail+dOccupation_Unskilled+dOccupation_skilled+dOccupation_Clerical+dOccupation_Supervisor+dOccupation_police+dOccupation_Mid_level+dOccupation_Upper_level+	
dRace_BAf+dRace_Wh+dRace_Col+dRace_Arab+dRace_SAs+dRace_EAs	+
dlang_English+dlang_French+dlang_Portuguese+dlang_Other+dlang_Crioulo+dlang_Kirund+dlang_Sesotho+dlang_Sudanese_Arabic+dlang_Creole+dlang_siSwati+dlang_Shona+dlang_Algerian_Arabic+
dCOUNTRY_ALG+dCOUNTRY_BDI+dCOUNTRY_BFO+dCOUNTRY_CAM+dCOUNTRY_CDI+dCOUNTRY_EGY+dCOUNTRY_GAB+dCOUNTRY_GHA+dCOUNTRY_GUI+dCOUNTRY_KEN+dCOUNTRY_LES+dCOUNTRY_LIB+dCOUNTRY_MAD+dCOUNTRY_MAU+dCOUNTRY_MLI+dCOUNTRY_MLW+dCOUNTRY_MOR+dCOUNTRY_MOZ+dCOUNTRY_NAM+dCOUNTRY_NGR+dCOUNTRY_NIG+dCOUNTRY_SAF+dCOUNTRY_SEN+dCOUNTRY_SRL+dCOUNTRY_STP+dCOUNTRY_SUD+dCOUNTRY_SWZ+dCOUNTRY_TAN+dCOUNTRY_TOG+dCOUNTRY_TUN+dCOUNTRY_UGA+dCOUNTRY_ZAM')
##dOccupation_Never+ dOccupation_Other +dRace_Oth 



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


#ーーーーーーー保有状況でクラスタ
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


#----
res0<-lme(News_Radio~1,random=~1|COUNTRY2,na.action=na.omit,data=AfrodatAll2)

AfrodatAll2g<-groupedData(News_Radio~year|COUNTRY2,data=AfrodatAll2)
	plot(AfrodatAll2g)
	reslm.g<-lmList(News_Radio~year|COUNTRY2,data=AfrodatAll2,na.action=na.omit)
	summary(reslm.g)
	plot(intervals(reslm.g))

	dat0<-AfrodatAll2[AfrodatAll2$COUNTRY2=="BEN",]
	table(dat0$year,dat0$News_Internet,exclude=NULL)	#	2012=wave5		それぞれ測定前については0にしてダミー定義
	table(dat0$year,dat0$News_Social_media,exclude=NULL)	#2014= wave 6
		dat0$News_Internet[dat0$year<2012]<-0	
			dat0$fg.News_Internet<-ifelse(dat0$year<2012,0,1)		
		dat0$News_Social_media[dat0$year<2014]<-0
			dat0$fg.News_Social_media<-ifelse(dat0$year<2014,0,1)		
	
	d<-complete.cases(dat0[,c("year","News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")])
	dat<-dat0[d,];dim(dat0);dim(dat)
	summary(dat)
res<-lm(News_Radio~year,data=dat)
	summary(res)
	plot(News_Radio~year,data=dat)
	lines(res$fitted.values~year,col="red",data=dat)

res2<-lm(News_Radio~year+News_Television+News_Newspaper+News_Internet+News_Social_media+fg.News_Internet+fg.News_Social_media,data=dat)
	summary(res2)
	plot(News_Radio~year,data=dat)
	lines(res2$fitted.values~year,col="red",data=dat)



library(ICC)
ICCest(as.factor(COUNTRY2),News_Radio,data=AfrodatAll2[AfrodatAll2$wave==1,],alpha=0.05,CI.type="Smith")
#[1] 0.0517132と低いが


#国による違いの有無､
res0<-lme(News_Radio~1,random=~1|COUNTRY2,na.action=na.omit,data=AfrodatAll2)
	summary(res0)
	VarCorr(res0)
#        AIC       BIC   logLik
#  884816.3 884847.6 -442405.2

#(Intercept) 2.735488 0.07191879 250250 38.0358       0

#            Variance   StdDev   
#(Intercept) 0.1909259 0.4369507
#Residual    2.0192435 1.4210009


res1<-update(res0,.~.+year)	#時間に比例､ランダム切片
	summary(res1)
#        AIC       BIC   logLik
#  883515.7 883557.4 -441753.9
#               Value Std.Error     DF   t-value p-value
#(Intercept) 42.56237 0.15894351 250249  267.7830       0
#year        -0.01980 0.00007129 250249 -277.6865       0

res2<-update(res1,.~.,random=~year|COUNTRY2)	#傾きランダム
	summary(res2)
#        AIC     BIC   logLik
#  883515.1 883577.7 -441751.6
#               Value Std.Error     DF   t-value p-value
#(Intercept) 42.76453 1.0962271 249800  39.01065       0
#year        -0.01990 0.0005459 249800 -36.44408       0

res2c<-update(res2,.~.,correlation=corAR1())	#系列相関 このデータは個人が同一ではないのでできない
	summary(res2c)
#Error: 'sumLenSq := sum(table(groups)^2)' = 2.37085e+09 is too large.
# Too large or no groups in your correlation structure?

res2.1<-update(res2,.~.+l_mNews_Radio+News_Television,random=~year|COUNTRY2)	
	summary(res2.1)


#--TV
res0tv<-update(res0,News_Television~.)
	summary(res0tv)
#       AIC      BIC    logLik
#  915710.2 915741.5 -457852.1

res1tv<-update(res1,News_Television~.)
	summary(res1tv)
#       AIC      BIC    logLik
#  914765.4 914807.1 -457378.7
#                Value Std.Error     DF   t-value p-value
#(Intercept) -35.67483 1.2268400 247074 -29.07863       0
#year          0.01874 0.0006042 247074  31.01131       0

res2tv<-update(res2,News_Television~.)
	summary(res2tv)
#       AIC      BIC    logLik
#  914766.6 914829.1 -457377.3


save.image("0Afrodat.img")
