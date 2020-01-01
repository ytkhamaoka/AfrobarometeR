#--------
#----Afrodatae wave 1-7の処理例  読み込んで欠損値をNA(Rでの欠損値)として国別集計､回帰分析
#----2019/11/6 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　

options(stringsAsFactors=FALSE)

#install.packages(c("dplyr","haven","memisc","openxlsx","simputation"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形
library(openxlsx)
library(simputation)
#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")
#load("0Afrodat.img");ls()
#unique(AfrodatAll$COUNTRY2)
#table(AfrodatAll$COUNTRY2)

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開

#----------------サブルーチン
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


#----------1列目に国記号､行方向に各国(の平均)がはいったデータについて、国別に折れ線グラフを作成
plot.by.group<-function(x, ...){
	#x<-as.data.frame(m)
	plot(x[1,-1],type="l")#,...)
	par(srt=90,adj=0)
	text(x[1,1],x[,1],cex=2)
	for (i in seq(2,dim(x)[1])) {lines(x[i,],col=i)}
}


#---World Bankの国データから必要な時点のみ取り出す→すべて使うことにしたのでこのルーチンは使わない
toLong0<-function(dat,vnam){
	#vnam="Population, total"
	dd<-dat[dat$Series.Name==vnam,c(1,4,5,7,10,13,17,19,20)]
	names(dd);dim(dd)	#[1] 219  19
# [1] "Country.Name"  "Country.Code"  "2000.[YR2000]" "2002.[YR2002]" "2005.[YR2005]" "2008.[YR2008]" "2012.[YR2012]" "2014.[YR2014]" "2015.[YR2015]"
	names(dd)[3:9]<-c("y.2000","y.2002","y.2005","y.2008","y.2012","y.2014","y.2015")
#	print(head(dd))

	d0<-reshape(dd,direction = "long",varying=c("y.2000","y.2002","y.2005","y.2008","y.2012","y.2014","y.2015"),
				idvar=c("Country.Name","Country.Code"))	#,timevar=c(2000,2002,2005,2008,2012,2014,2015)
	vnam2<-strsplit(vnam," (",fixed=T)	# (以降の単位部分は除く
	vnam2<-gsub(" |,|%","_",vnam2[[1]])	#　,や%､スペースは_に
	names(d0)[3:4]<-c("year",vnam2)
		d0[,4]<-as.numeric(d0[,4])	#characterになっているので数値に
	print(head(d0))
	return(d0)
	}

#	NAの数をカウント
n_NA<-function(x){
		sum(is.na(x))
		}

#---World Bankの国データにある時点データをすべてとりだす  
toLong<-function(dat,vnam){
	#vnam<-"Population, total";dat<-WBdat
	dd<-dat[dat$Series.Name==vnam,c(1,2,5,6:34)]	#2019まで列はあるが､ほとんどデータはないので除外
	names(dd);dim(dd)
# [1] "WB_Code"       "Afro_Code"     "Country.Name"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]" "1994.[YR1994]" "1995.[YR1995]"
#[10] "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]" "2003.[YR2003]" "2004.[YR2004]"
#[19] "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]" "2012.[YR2012]" "2013.[YR2013]"
#[28] "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 37 33

	names(dd)[4:32]<-paste("y",seq(1990,2018),sep=".")		#変数名 .数字の後からtimeを類推する
	d0<-reshape(dd,direction = "long",varying=names(dd)[4:32],		
				idvar=c("Country.Name","WB_Code","Afro_Code"))		#long形式に
				head(d0)
#                           WB_Code Afro_Code  Country.Name time        y
#Burundi.BDI.BDI.1990           BDI       BDI       Burundi 1990  5438957
#Benin.BEN.BEN.1990             BEN       BEN         Benin 1990  4978496
#Burkina Faso.BFA.BFO.1990      BFA       BFO  Burkina Faso 1990  8811034

	vnam2<-strsplit(vnam," (",fixed=T)	# (以降の単位部分は除く
	vnam2<-gsub(" |,|%","_",vnam2[[1]])	#　,や%､スペースは_に
	names(d0)[4:5]<-c("year",vnam2)
#		d0[,5]<-as.numeric(d0[,5])	#characterになっているので数値に　　既にそうしてある
#		d0<-d0[order(d0[,c("Country.Name","year")]),]
	#trend plot
	dd<-d0[,c(3:5)]
	names(dd)<-c("cnam","t","y")
	group_trend_plot(dd,vnam)

#	print(by(d0[,5],d0$Country.Name,n_NA))
datg<-group_by(dd,dd$cnam)
	print(sm<-summarize(datg,N=n(),n_NA=sum(is.na(y)),mn=min2(y),mx=max2(y)))	#

	print(head(d0))
	return(d0)
	}

#-----yとtをあたえてトレンド推定　　tnew=seq(a,b)について内挿値を算出
fit_trend<-function(t,y,tnew){
	#y<-WBdat[WBdat$Country.Name=="Algeria","Access_to_electricity.x"];t<-WBdat[WBdat$Country.Name=="Algeria","year"];tnew<-seq(2000,2018)
	#y<-WBdat[WBdat$Country.Name=="Algeria","Literacy_rate__adult_total"]
	#y<-WBdat[WBdat$Country.Name=="Algeria","Income_share_held_by_lowest_20_"]
	
	t2<-t^2
	dt<-data.frame(t,t2,y)
	dt$no<-seq(1,dim(dt)[1])
	dt$fg<-complete.cases(dt)
			print(sum(dt$fg))
	dt2<-dt[dt$fg,]
	print(summary(data.frame(t,y)))

	res<-lm(y~t+t2,data=dt2)
#	res<-lm(y~t,data=dt2)		#2乗を入れると"Algeria","Access_to_electricity.xがおかしくなるので1乗に
		print(summary(res))

	tnew2<-tnew^2
	newdat<-data.frame(tnew,tnew2)
#	newdat<-data.frame(tnew)
	names(newdat)<-c("t","t2")
#	names(newdat)<-c("t")
	yhat<-predict(res,newdat)

	dtnew<-data.frame(newdat,yhat)
	(dtnew<-merge(dtnew,dt2[,c("t","y")],by.x="t",by.y="t",all=T))
	dtnew$y2<-ifelse(is.na(dtnew$y),dtnew$yhat,dtnew$y)	#データがある場合はそれを､欠損の場合は内挿値を
	dtnew$fg.interpolate<-ifelse(is.na(dtnew$y),1,0)	#データがある場合はそれを､欠損の場合は内挿値を
	
#	plot(t,y,xlim=c(min(t),max(tnew)),ylim=c(min(y),max(yhat)))
	plot(t,y,xlim=c(1990,2018),ylim=c(min2(y),max2(y)))
	lines(tnew,yhat,col="red")
	return(dtnew[,c("t","y2","fg.interpolate")])	#y2　観測値もしくは内挿値､fg.interpolate=内挿値の場合に1
	}




#------------------
#		国コード対応表　
#		下記からDL　ただし､国名やコードの最後にスペースが入っているので要注意
#		https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm
#------------------
Cnam<-read.xlsx("Afro_countryCode.xlsx",sheet=1)
	names(Cnam);dim(Cnam)	#[1] "Country.Name" "Afro_Code"   [1] 37  2
dat<-read.xlsx("WBcountry_code.xlsx",sheet=1)
	names(dat);dim(dat)	#[1] "Country.Name0" "Country.Name"  "WB_Code"       "WB_nCode"    [1] 264   3
dat<-dat[,2:3]

Cnam<-merge(Cnam,dat,by.x="Country.Name","Country.Name",all=T)
 names(Cnam)
#[1] "Country.Name"  "Afro_Code"     "Country.Name0" "WB_Code"       "WB_nCode"   
Cnam<-Cnam[order(Cnam$Afro_Code),]
	edit(Cnam)
#対応できたことがわかったので不要部分を捨てる
(Cnam<-Cnam[!is.na(Cnam$Afro_Code),])
	dim(Cnam)	#[1] 37  5

#------------------
#		国レベルデータ  2000 2002 2005 2008   2012  2014  2015のみを残す  Afroは最新2018だがないので､2015を使う
#------------------
#Data from database: World Development Indicators Last Updated: 12/20/2019
#https://databank.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG/1ff4a498/Popular-Indicators
#にいくつか加えた   データは1990年からに

dat0<-read.xlsx("WBPopular Indicators2.xlsx",sheet=1)	#, stringsAsFactors=FALSE
	summary(dat0)
	head(dat0)
	names(dat0);dim(dat0)	#[1] 11937    201		Country.CodeがWBの国略号3文字
# [1] "Series.Name"   "Series.Code"   "Country.Name"  "Country.Code"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]" "1994.[YR1994]"
#[10] "1995.[YR1995]" "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]" "2003.[YR2003]"
#[19] "2004.[YR2004]" "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]" "2012.[YR2012]"
#[28] "2013.[YR2013]" "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 13456    34
	unique(dat0$Country.Code)
	unique(dat0$Country.Name)

#characterになっているので数値に
for (i in seq(5,34)){
	dat0[,i]<-as.numeric(dat0[,i])
	}
summary(dat0)

	

#コード対応表とマージして対象国のみに
WBdat<-merge(Cnam[,c( "Afro_Code","WB_Code")],dat0,by.x="WB_Code",by.y="Country.Code",all=T)
	dim(WBdat[is.na(WBdat$Afro_Code),])
	WBdat<-WBdat[!is.na(WBdat$Afro_Code),]
	unique(WBdat$WB_Code)
	length(unique(WBdat$WB_Code))
	unique(WBdat$Afro_Code)		#37ヵ国
	length(unique(WBdat$Afro_Code))
head(WBdat)
	names(WBdat);dim(WBdat)
edit(WBdat)
# [1] "WB_Code"       "Afro_Code"     "Series.Name"   "Series.Code"   "Country.Name"  "1990.[YR1990]" "1991.[YR1991]" "1992.[YR1992]" "1993.[YR1993]"
#[10] "1994.[YR1994]" "1995.[YR1995]" "1996.[YR1996]" "1997.[YR1997]" "1998.[YR1998]" "1999.[YR1999]" "2000.[YR2000]" "2001.[YR2001]" "2002.[YR2002]"
#[19] "2003.[YR2003]" "2004.[YR2004]" "2005.[YR2005]" "2006.[YR2006]" "2007.[YR2007]" "2008.[YR2008]" "2009.[YR2009]" "2010.[YR2010]" "2011.[YR2011]"
#[28] "2012.[YR2012]" "2013.[YR2013]" "2014.[YR2014]" "2015.[YR2015]" "2016.[YR2016]" "2017.[YR2017]" "2018.[YR2018]" "2019.[YR2019]"
#[1] 2294   35

#----指標名を指定してとりだし　ロング形式に(欠損がおおい変数は除外)
#	基礎
WBdatL<-toLong(WBdat,"Population, total")
	dd<-toLong(WBdat,"Population growth (annual %)")
	names(dd)
#[1] "WB_Code"           "Afro_Code"         "Country.Name"      "year"              "Population_growth"
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Surface area (sq. km)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	GNP　GDP
	dd<-toLong(WBdat,"GDP (current US$)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GDP growth (annual %)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"GNI per capita, PPP (current international $)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"GDP per capita (current US$)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

#通信
	dd<-toLong(WBdat,"Individuals using the Internet (% of population)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Mobile cellular subscriptions")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
	dd<-toLong(WBdat,"Fixed telephone subscriptions")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Fixed telephone subscriptions (per 100 people)")		#toLongルーチンでは　(以降を除外するので､上のと同じ変数名になるため除外
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Internet users (per 100 people)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	インフラ
	dd<-toLong(WBdat,"Access to electricity (% of population)")
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	教育
	dd<-toLong(WBdat,"Primary completion rate, total (% of relevant age group)")			#こちらの方がNAが少ない
		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"School enrollment, secondary (% gross)")
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"Literacy rate, adult total (% of people ages 15 and above)")		#欠損が非常に多い国あり
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	格差   いずれもNAが多いので除外
#	dd<-toLong(WBdat,"Income share held by lowest 20%")		#欠損多いが最低一つはデータあり
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))
#	dd<-toLong(WBdat,"GINI index (World Bank estimate)")		#欠損多いが
#		WBdatL<-merge(WBdatL,dd[,-c(1,2)],by.x=c("Country.Name","year"),by.y=c("Country.Name","year"))

	summary(WBdatL)
	head(WBdatL)
	names(WBdatL);dim(WBdatL)

# [1] "Country.Name"                   "year"                           "WB_Code"                        "Afro_Code"                     
# [5] "Population__total"              "Population_growth"              "Surface_area"                   "GDP"                           
# [9] "GDP_growth"                     "GDP_per_capita"                 "Individuals_using_the_Internet" "Mobile_cellular_subscriptions" 
#[13] "Fixed_telephone_subscriptions"  "Access_to_electricity"          "Primary_completion_rate__total"
#[1] 1073   15

	edit(WBdatL)



#----------		cnam毎にyの欠損値を線形モデルで補完
t0<-WBdatL$year
cnam<-WBdatL$Country.Name
	clist<-unique(cnam)

for(jj in seq(5,dim(WBdatL)[2])){	#変数についてループ
	#jj<-5
	y0<-WBdatL[,jj]
	(vnam2<-paste(names(WBdatL)[jj],"2",sep=""))
	print(names(WBdatL[jj]))

	jpeg(file=paste(vnam2,".jpeg",sep=""),width = 960, height = 480)
		par(mfrow=c(5,8),mar=c(0,0,0,0))

	dd<-data.frame(cnam,y0,t0)

		for(ii in seq(1,length(clist))){	#国についてループ
		#ii<-1	
			print(cl<-clist[ii])
			dd2<-dd[dd$cnam==clist[ii],]
			
			if(sum(!is.na(dd2$y0))==0){
					print(list("**********No Data",clist[ii],vnam2))
					}else{
						ydat0<-fit_trend(dd2$t0,dd2$y0,tnew=seq(1990,2018))	#y0の欠損を線形補完しつつ　2018までも外挿
						legend("topleft",cl)
						ydat0$cnam<-cl
							if(ii==1) {
							ydat_all<-ydat0
							}else{
							ydat_all<-rbind(ydat_all,ydat0)
							}
					}
			}
#      t       y2 fg.interpolate       cnam
#1  2000 45.68376              1 Mozambique
#2  2001 46.34044              1 Mozambique
			names(ydat_all)[1]<-"year"
			names(ydat_all)[2]<-vnam2
			names(ydat_all)[3]<-paste(vnam2,"fg.interpolat",sep=".")
			names(ydat_all)[4]<-"Country.Name"
			dev.off()
			x<- readline(list(vnam2,"waiting for pushing a key"))

	if(jj==5) {
		WBdatL2<-ydat_all
			}else{
		WBdatL2<-merge(WBdatL2,ydat_all,by.x=c("Country.Name","year"),by.y=c("Country.Name","year"),all=T)
			}
	}
	
	summary(WBdatL2)
	head(WBdatL2)
	names(WBdatL2);	dim(WBdatL2)

	
summary(WBdatL2)
summary(WBdatL2[WBdatL2$year>=2000,])

names(WBdatL);dim(WBdatL)
names(WBdatL2);dim(WBdatL2)

WBdatL2<-merge(WBdatL[,c( "Country.Name","year","WB_Code","Afro_Code")],WBdatL2,by.x=c("Country.Name","year"),by.y=c("Country.Name","year"),all=T)
summary(WBdatL2)
names(WBdatL2);dim(WBdatL2)

save(WBdatL2,file="0WBdatL2.rda")
edit(WBdatL2)

#NamibiaはPrimary_completion_rate__totalのデータがまったくないので内挿もできない

# [1] "Country.Name"                                  "year"                                          "WB_Code"                                      
# [4] "Afro_Code"                                     "Population__total2"                            "Population__total2.fg.interpolat"             
# [7] "Population_growth2"                            "Population_growth2.fg.interpolat"              "Surface_area2"                                
#[10] "Surface_area2.fg.interpolat"                   "GDP2"                                          "GDP2.fg.interpolat"                           
#[13] "GDP_growth2"                                   "GDP_growth2.fg.interpolat"                     "GDP_per_capita2"                              
#[16] "GDP_per_capita2.fg.interpolat"                 "Individuals_using_the_Internet2"               "Individuals_using_the_Internet2.fg.interpolat"
#[19] "Mobile_cellular_subscriptions2"                "Mobile_cellular_subscriptions2.fg.interpolat"  "Fixed_telephone_subscriptions2"               
#[22] "Fixed_telephone_subscriptions2.fg.interpolat"  "Access_to_electricity2"                        "Access_to_electricity2.fg.interpolat"         
#[25] "Primary_completion_rate__total2"               "Primary_completion_rate__total2.fg.interpolat"
#[1] 1073   26


#ラグ変数つくる
#国→年の順にならんでいる
#WBdatL3<-WBdatL2[order(c(WBdatL2$Country.Name,WBdatL2$year)),]	
#	head(WBdatL3)
	
m2<-WBdatL2[,5:26]
	dd<-rep(NA,dim(m2)[2])
m2<-rbind(dd,m2)		#1行下にずらす
m2<-m2[-dim(m2)[1],]	#最後の行は除く
#	edit(m2)
names(m2)<-paste("l",names(m2),sep="_")

WBdatL3<-cbind(WBdatL2,m2)
	names(WBdatL3)
	for(i in seq(2,dim(WBdatL3)[1])){
		if(WBdatL3$Country.Name[i]!=WBdatL3$Country.Name[i-1]){
			WBdatL3[i,27:48]<-NA
			}
		}

	edit(WBdatL3)
save(WBdatL3,file="0WBdatL3.rda")




#---------------
#			1
#---------------
Afrodat1 <- read_sav("merged_r1_data.sav")
head(Afrodat1)
summary(Afrodat1)
names(Afrodat1)	
dim(Afrodat1)	#
	Afrodat1$wave<-1
	Afrodat1$year<-2000
edit(Afrodat1[,1:2])
table(Afrodat1$country)
#Afrodat1$COUNTRY2<-substr(Afrodat1$casenumb,1,3)	#他のroundだと､はじめの3文字が国名+回答者番号だが､1stは3文字+数字のところと数字から始まるものが混在
(lb<-names(attributes(Afrodat1$country)$labels))
# [1] "Botswana"     "Ghana"        "Lesotho"      "Malawi"       "Mali"         "Namibia"      "Nigeria"      "South Africa" "Tanzania"     "Uganda"      
#[11] "Zambia"       "Zimbabwe"    
lb[4]<-"MLW"	#Malawi	他のwaveと統一
lb[5]<-"MLI"	#Mali	他のwaveと統一
lb[8]<-"SAF"	# "South Africa"はSAF
	(lb<-substr(toupper(lb),1,3))	#大文字にしてはじめの3文字

Afrodat1$COUNTRY2<-lb[Afrodat1$country]	#こちらに入っている
	table(Afrodat1$COUNTRY2)

# BOT  GHA  LES  MAL  NAM  NIG  SAF  TAN  UGA  ZAM  ZIM 
#1200 2004 1177 3297 1183 3603 2200 2198 2271 1198 1200 


#"Variable name: medtv
#Variable label: TV news
#Values: 0-5, 9, 98, 99
#Value Labels: 0=Never, 1=Less than once a month, 2=About once a month, 3=About once a week/few times a month, 4=Several times a week/a few times a week, 5=Every day, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data
#Notes: Not asked in Ghana.
#Question text - SAB

#これを使って欠損値処理
Afrodat1$News_Radio<-repNA05(Afrodat1$medrad)
#    0     1     2     3     4     5  <NA> 
# 2817   746   417  1716  4356 11372   107 

Afrodat1$News_Television<-repNA05(Afrodat1$medtv)
#   0    1    2    3    4    5 <NA> 
#9966 1032  375 1162 2054 4663 2279 


#他の回は0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data なので､5→4　　4→3　　3→2　　とする｡
Afrodat1$News_Radio0<-Afrodat1$News_Radio
Afrodat1$News_Radio<-ifelse(Afrodat1$News_Radio>2,Afrodat1$News_Radio-1,Afrodat1$News_Radio)
	table(Afrodat1$News_Radio0,Afrodat1$News_Radio,exclude=NULL)

Afrodat1$News_Television0<-Afrodat1$News_Television
Afrodat1$News_Television<-ifelse(Afrodat1$News_Television>2,Afrodat1$News_Television-1,Afrodat1$News_Television)
	table(Afrodat1$News_Television0,Afrodat1$News_Television,exclude=NULL)

Afrodat1$News_Newspaper<-NA	#repNA04(Afrodat1$Q12C)	#設問がない場合はNAに
Afrodat1$News_Internet<-NA	#repNA04(Afrodat1$Q12D)
Afrodat1$News_Social_media<-NA	#repNA04(Afrodat1$Q12E)

table(Afrodat1$COUNTRY2,Afrodat1$News_Radio,exclude=NULL)
table(Afrodat1$COUNTRY2,Afrodat1$News_Television,exclude=NULL)


#保存
save(Afrodat1,file=“0Afrodat1.rda”)


#---------------
#			2
#---------------
Afrodat2 <- read_sav("merged_r2_data.sav")
#View(Afrodat2)
summary(Afrodat2)
names(Afrodat2)	
dim(Afrodat2)	#[1] 53935   364
	Afrodat2$wave<-2
	Afrodat2$year<-2002

table(Afrodat2$country)
Afrodat2$COUNTRY2<-substr(Afrodat2$respno,1,3)	#はじめの3文字が国名
	table(Afrodat2$COUNTRY2)
# BOT  cve  CVE  GHA  KEN  LES  MLI  MOZ  MWI  NAM  NIG  SAF  SEN  TNZ  UGA  ZAM  ZIM 
#1200 1250   18 1200 2398 1200 1283 1400 1200 1199 2428 2400 1200 1223 2400 1198 1104 

Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="cve"]<-"CVE"	#大文字に
Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="TNZ"]<-"TAN"	#他のwaveと統一
Afrodat2$COUNTRY2[Afrodat2$COUNTRY2=="MWI"]<-"MLW"	#他のwaveと統一


#これを使って欠損値処理
Afrodat2$News_Radio<-repNA04(Afrodat2$q26a)
Afrodat2$News_Television<-repNA04(Afrodat2$q26b)
Afrodat2$News_Newspaper<-repNA04(Afrodat2$q26c)
Afrodat2$News_Internet<-NA	#repNA04(Afrodat2$Q12D)
Afrodat2$News_Social_media<-NA	#repNA04(Afrodat2$Q12E)

#保存
save(Afrodat2,file=“0Afrodat2.rda”)

#---------------
#			3
#---------------
Afrodat3 <- read_sav("merged_r3_data.sav")
#View(Afrodat3)
summary(Afrodat3)
names(Afrodat3)	
dim(Afrodat3)	#[1]25397   302
	Afrodat3$wave<-3
	Afrodat3$year<-2005

table(Afrodat3$country)
Afrodat3$COUNTRY2<-substr(Afrodat3$respno,1,3)	#はじめの3文字が国名
	table(Afrodat3$COUNTRY2)
# BEN  BOT  CVE  GHA  KEN  LES  MAD  MLI  MOZ  MWI  NAM  NIG  SAF  SEN  TAN  UGA  ZAM  ZIM 
#1198 1200 1256 1197 1278 1161 1350 1244 1198 1200 1200 2363 2400 1200 1304 2400 1200 1048 
#これを使って欠損値処理
Afrodat3$COUNTRY2[Afrodat3$COUNTRY2=="MWI"]<-"MLW"	#他のwaveと統一

Afrodat3$News_Radio<-repNA04(Afrodat3$q15a)
Afrodat3$News_Television<-repNA04(Afrodat3$q15b)
Afrodat3$News_Newspaper<-repNA04(Afrodat3$q15c)
Afrodat3$News_Internet<-NA	#repNA04(Afrodat3$q15d)
Afrodat3$News_Social_media<-NA	#repNA04(Afrodat3$q15e)

#保存
save(Afrodat3,file=“0Afrodat3.rda”)

#---------------
#			4
#---------------
Afrodat4 <- read_sav("merged_r4_data.sav")
#View(Afrodat4)
summary(Afrodat4)
names(Afrodat4)	
dim(Afrodat4)	#[1] 53935   364
	Afrodat4$wave<-4
	Afrodat4$year<-2008

table(Afrodat4$COUNTRY)
Afrodat4$COUNTRY2<-substr(Afrodat4$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat4$COUNTRY2)
# BEN  BFO  BOT  CVE  GHA  KEN  LES  LIb  LIB  MAD  MLI  MLW  MOZ  NAM  NIG  SAF  SEN  TAN  UGA  ZAM  ZIM 
#1200 1200 1200 1264 1200 1104 1200    1 1199 1350 1232 1200 1200 1200 2324 2400 1200 1208 2431 1200 1200
Afrodat4$COUNTRY2[Afrodat4$COUNTRY2=="LIb"]<-"LIB"	#大文字に

#これを使って欠損値処理
Afrodat4$News_Radio<-repNA04(Afrodat4$Q12A)
Afrodat4$News_Television<-repNA04(Afrodat4$Q12B)
Afrodat4$News_Newspaper<-repNA04(Afrodat4$Q12C)
Afrodat4$News_Internet<-NA	#repNA04(Afrodat4$Q12D)
Afrodat4$News_Social_media<-NA	#repNA04(Afrodat4$Q12E)

#保存
save(Afrodat4,file=“0Afrodat4.rda”)

#---------------
#			5
#---------------
Afrodat5 <- read_sav("merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")
##View(Afrodat5)
summary(Afrodat5)
names(Afrodat5)	
dim(Afrodat5)	#[1] 53935   364
	Afrodat5$wave<-5
	Afrodat5$year<-2012

table(Afrodat5$COUNTRY)

Afrodat5$COUNTRY2<-substr(Afrodat5$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat5$COUNTRY2)
# ALG  BDI  BEN  BFO  BOT  CAM  CDI  CVE  EGY  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOZ  MRC  NAM  NGR  NIG  SAF  SEN  SRL  SUD  SWZ  TAN  TOG  TUN 
#1204 1200 1200 1200 1200 1200 1200 1208 1190 2400 1200 2399 1197 1199 1200 1200 1200 2407 2400 1196 1200 1199 2400 2399 1200 1190 1199 1200 2400 1200 1200 
# UGA  ZAM  ZIM 
#2400 1200 2400 
Afrodat5$COUNTRY2[Afrodat5$COUNTRY2=="MRC"]<-"MOR"	#他のwaveと統一

#これを使って欠損値処理
Afrodat5$News_Radio<-repNA04(Afrodat5$Q13A)
Afrodat5$News_Television<-repNA04(Afrodat5$Q13B)
Afrodat5$News_Newspaper<-repNA04(Afrodat5$Q13C)
Afrodat5$News_Internet<-repNA04(Afrodat5$Q13D)
Afrodat5$News_Social_media<-NA	#repNA04(Afrodat5$Q13E)

#保存
save(Afrodat5,file=“0Afrodat5.rda”)


#---------------
#			6
#---------------
Afrodat6 <- read_sav("merged_r6_data_2016_36countries2.sav")
#View(Afrodat6)
summary(Afrodat6)
names(Afrodat6)	
dim(Afrodat6)	#[1] 53935   364
	Afrodat6$wave<-6
	Afrodat6$year<-2014

table(Afrodat6$COUNTRY)
Afrodat6$COUNTRY2<-substr(Afrodat6$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat6$COUNTRY2)
# ALG  BDI  BEN  BFO  BOT  CAM  CDI  CVE  EGY  GAB  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOR  MOZ  NAM  NGR  NIG  SAF  SEN  SRL  STP  SUD  SWZ  TAN  TOG  TUN  UGA  ZAM  ZIM 
#1200 1200 1200 1200 1200 1182 1199 1200 1198 1198 2400 1200 2397 1200 1199 1200 1200 1200 2400 1200 2400 1200 1200 2400 2390 1200 1191 1196 1200 1200 2386 1200 1200 2400 1199 2400 

#これを使って欠損値処理


#    0     1  <NA> 
#22089 31246   600 

#----年齢
#Question Number: Q1
#Question: How old are you?
#Variable Label: Q1. Age
#Values: 18-105, 998-999, -1
#Value Labels: 98=Refused to answer, 999=Don’t know, -1=Missing
Afrodat6$Age<-ifelse(Afrodat6$Q1<0|Afrodat6$Q1>105|Afrodat6$Q1==98,NA,Afrodat6$Q1)
	table(Afrodat6$Age,exclude=NULL)

#"Question Number: Q2
#Question: Which language is your home language?
#Variable Label: Q2. Language of respondent
#Values: 1-35, 101- 107, 141-149, 180- 197, 220-221, 260- 278, 300-315, 340-342, 381-396, 420-421,460-471, 502- 518, 540-553, 581-591, 621- 653, 660-668, 702-710, 740-800, 820- 872, 900, 930- 943, 1100-1105, 1141- 1160, 1180, 1220 -1282, 1300-1305, 1420, 1460,1501,1540,1541,1620,1621,1660,1661,1662, 1700-1707, 2200-2222, 2740-2748, 9998-9999"
#Value Labels: -1 =Missing, 1 =English, 2 =French, 3 =Portuguese, 4 =Swahili, 5 =Arabic, 6 =Adja, 7 =Afrikaans, 8 =Arabe, 9 =Bambara, 10 =Bassa, 11

Afrodat6$Language<-ifelse(Afrodat6$Q2<0|Afrodat6$Q2>=9998,NA,Afrodat6$Q2)
table(Afrodat6$Language,exclude=NULL)

	d<-data.frame(table(Afrodat6$Language,exclude=NULL))
	d[order(d[,2],decreasing=T),]
#    Var1 Freq
#5      5 2199
#3      3 1400
#117  463 1258	Chichewa
#64   260 1210	Akan
#425 9995 1206	Other
#396 1460 1198	Egyptian Arabic
#62   220 1195	Crioulo
#333 1180 1192	Kirund
#96   340 1182	Sesotho
#398 1540 1179	Sudanese Arabic
#303  900 1166	Creole
#400 1620 1166	siSwati
#294  861 1156	Shona
#395 1420 1108	Algerian Arabic
#

Afrodat6$dlang_English<-ifelse(Afrodat6$Language==1,1,0)
Afrodat6$dlang_French<-ifelse(Afrodat6$Language==2,1,0)
Afrodat6$dlang_Portuguese<-ifelse(Afrodat6$Language==3,1,0)
Afrodat6$dlang_Swahili<-ifelse(Afrodat6$Language==4,1,0)
Afrodat6$dlang_Arabic<-ifelse(Afrodat6$Language==5,1,0)
Afrodat6$dlang_Swahili<-ifelse(Afrodat6$Language==6,1,0)
Afrodat6$dlang_Afrikaans<-ifelse(Afrodat6$Language==7,1,0)

Afrodat6$dlang_Chichewa<-ifelse(Afrodat6$Language==463,1,0)
Afrodat6$dlang_Akan<-ifelse(Afrodat6$Language==260,1,0)
Afrodat6$dlang_Other<-ifelse(Afrodat6$Language==9995,1,0)
Afrodat6$dlang_Egyptian_Arabic<-ifelse(Afrodat6$Language==1460,1,0)
Afrodat6$dlang_Crioulo<-ifelse(Afrodat6$Language==220,1,0)
Afrodat6$dlang_Kirund<-ifelse(Afrodat6$Language==1180,1,0)
Afrodat6$dlang_Sesotho<-ifelse(Afrodat6$Language==340,1,0)
Afrodat6$dlang_Sudanese_Arabic<-ifelse(Afrodat6$Language==1540,1,0)
Afrodat6$dlang_Creole<-ifelse(Afrodat6$Language==900,1,0)
Afrodat6$dlang_siSwati<-ifelse(Afrodat6$Language==1620,1,0)
Afrodat6$dlang_Shona<-ifelse(Afrodat6$Language==861,1,0)
Afrodat6$dlang_Algerian_Arabic<-ifelse(Afrodat6$Language==1420,1,0)


#"Question Number: Q4A
#Question: In general, how would you describe: The present economic condition of this country?
#Variable Label: Q4A. Country’s present economic condition
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: NDB, Zambia96"
#"Question Number: Q4B
#Variable Label: Q4B. Your present living conditions
Afrodat6$Cond_econ<-repNA05(Afrodat6$Q4A)
Afrodat6$Cond_your_liv<-repNA05(Afrodat6$Q4B)


#"Question Number: Q5
#Question: In general, how do you rate your living conditions compared to those of other [ENTER NATIONALITY]?
#Variable Label: Q5. Your living conditions vs. others
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"
Afrodat6$Relative_live<-ifelse(Afrodat6$Q5<0|Afrodat6$Q5>=9,NA,Afrodat6$Q5)
	table(Afrodat6$Relative_live,exclude=NULL)		#


#---不足　　
#Variable Label: Q8a. How often gone without food
#Values: 0-4, 9, 98, -1
#Value Labels: 0=Never, 1=Just once or twice, 2=Several times, 3=Many times, 4=Always, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: NDB
#Variable Label: Q8b. How often gone without water
#Variable Label: Q8c.How often gone without medical care
#Variable Label: Q8d. How often gone without cooking fuel
#Variable Label: Q8e. How often gone without a cash income#Source: SAB"
Afrodat6$gone_food<-repNA04(Afrodat6$Q8A)
Afrodat6$gone_water<-repNA04(Afrodat6$Q8B)
Afrodat6$gone_med<-repNA04(Afrodat6$Q8C)
Afrodat6$gone_fuel<-repNA04(Afrodat6$Q8D)
Afrodat6$gone_cash<-repNA04(Afrodat6$Q8E)
Afrodat6$gone_electricity<-NA

# news source
#"Question Number: Q12A 
#Question: How often do you get news from the following sources: Radio? 
#Variable Label: Q12a. Radio news 
#Values: 0-4, 9, 98, -1 
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 98=Refused to answer, -1=Missing 
#Source: Zambia96 "
Afrodat6$News_Radio<-repNA04(Afrodat6$Q12A)
Afrodat6$News_Television<-repNA04(Afrodat6$Q12B)
Afrodat6$News_Newspaper<-repNA04(Afrodat6$Q12C)
Afrodat6$News_Internet<-repNA04(Afrodat6$Q12D)
Afrodat6$News_Social_media<-repNA04(Afrodat6$Q12E)


#----電気　　R6にはなし
# Gone without electricity
# "Question Number: Q94
#Question: Do you have an electric connection to your home from the mains? [If yes] How often is the electricity actually available?
#Variable Label: Q94. Electric connection from mains
#Values: 0-5, 9, 98, -1
#Value Labels: 0= No mains electric supply or connection to the home, [If yes], 1=Never, 2=Occasionally, 3= About half of the time, 4= Most of the time, 5= All of the time, 9=Don’t know , 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"

 
# "Question Number: Q94
#Question: Do you have an electric connection to your home from the mains? [If yes] How often is the electricity actually available?
#Variable Label: Q94. Electric connection from mains
#Values: 0-5, 9, 98, -1
#Value Labels: 0= No mains electric supply or connection to the home, [If yes], 1=Never, 2=Occasionally, 3= About half of the time, 4= Most of the time, 5= All of the time, 9=Don’t know , 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"
Afrodat6$Electric_connection<-repNA05(Afrodat6$Q94)




#---Public affairへの興味
#"Question Number: Q13 
#Question: How interested would you say you are in public affairs? 
#Variable Label: Q13. Interest in public affairs 
#Values: 0-3, 9, 98, -1 
#Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t know, 98=Refused to answer, -1=Missing 
#Source: SAB 
#Note : Interviewer was instructed to prompt if necessary with “You know, in politics and government.” "

Afrodat6$Interest_pubaff<-repNA03(Afrodat6$Q13)


#政治についての会話
#"Question Number: Q14
#Question: When you get together with your friends or family, would you say you discuss political matters:
#Variable Label: Q14. Discuss politics
#Values: 0-2, 9, 98, -1
#Value Labels: 0=Never, 1=Occasionally, 2=Frequently, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Adapted from Zambia96."
Afrodat6$Discuss_politics<-repNA02(Afrodat6$Q14)
Afrodat6$dDiscuss_politics<-ifelse((Afrodat6$Discuss_politics==1)|(Afrodat6$Discuss_politics==2),1,0) 


#"Question Number: Q19A
#Question: Let’s turn to your role in the community. Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A religious group that meets outside of regular worship services? Variable Label: Q19a. Member of religious group
#Values: 0-3, 9, 98, -1
#Value Labels: 0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"
Afrodat6$Mem_religious<-repNA03(Afrodat6$Q19A)

#"Question Number: Q19B
#Variable Label: Q19b. Member of voluntary association or community group
#Source: Afrobarometer Round 4"
Afrodat6$Mem_voluntary<-repNA03(Afrodat6$Q19B)	#


#"Question Number: Q20A
#Question: Here is a list of actions that people sometimes take as citizens. For each of these, please tell me whether you, personally, have done any of these things during the past year. If not, would you do this if you had the chance: Attended a community meeting?
#Variable Label: Q20a. Attend a community meeting
#Values: 0-4, 9, 98, -1
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB"

#"Question Number: Q20B
#Variable Label: Q20b. Join the others to raise an issue

Afrodat6$Cit_action_Attend_meeting<-repNA03(Afrodat6$Q20A)	#
Afrodat6$Cit_action_raise_issue<-repNA03(Afrodat6$Q20B)	#


#"Question Number: Q23A
#Question: Thinking about the last national election in [20xx], did you: Attend a campaign rally? 
#Variable Label: Q23a. Last national election: attend a campaign rally
#Values: 0, 1, 9, 98, -1
#Value Labels: 0=No, 1=Yes, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 5"
#"Question Number: Q23B
#Variable Label: Q23b. Last national election: Attend a campaign meeting?
#Source: Afrobarometer Round 5"
#"Question Number: Q23C
#Variable Label: Q23c. Last national election: persuade others to vote for a certain candidate or party Values: 0, 1, 9, 98, -1
#Source: Afrobarometer Round 5"
#"Question Number: Q23D
#Question: Thinking about the last national election in [20xx], did you: Work for a candidate or party? 
#Variable Label: Q23d. Last national election: work for a candidate or party
#Source: Afrobarometer Round 5"

Afrodat6$Ele_campaign_rally<-repNA01(Afrodat6$Q23A)
Afrodat6$Ele_campaign_meeting<-repNA01(Afrodat6$Q23B)
Afrodat6$Ele_Attend_persuade<-repNA01(Afrodat6$Q23C)
Afrodat6$Ele_Attend_Work<-repNA01(Afrodat6$Q23D)


#"Question Number: Q27A
#Question: Here is a list of actions that people sometimes take as citizens when they are dissatisfied with government performance. For each of these, please tell me whether you, personally, have done any of these things during the past year. If not, would you do this if you had the chance: Joined others in your community to request action from government
#Variable Label: Q27a. Join others to request government action
#Values: 0-4, 9, 98, -1
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Zambia96"
#"Question Number: Q27B
#Variable Label: Q27b. Contact media
#Source: Afrobarometer Round5"
#"Question Number: Q27C
#Variable Label: Q27c. Contact official for help
#Source: Afrobarometer Round5"
#"Question Number: Q27D
#Variable Label: Q27d. Refuse to pay a tax or fee to government
#Source: Zambia96"
#Variable Label: Q27e. Attend a demonstration or protest march
#Source: Afrobarometer Round5"

Afrodat6$Diss_request_government<-repNA04(Afrodat6$Q27A)
Afrodat6$Diss_Contact_media<-repNA04(Afrodat6$Q27B)
Afrodat6$Diss_Contact_official<-repNA04(Afrodat6$Q27C)
Afrodat6$Diss_Refuse2pay<-repNA04(Afrodat6$Q27D)
Afrodat6$Diss_Attend_demonstration<-repNA04(Afrodat6$Q27E)


#"Question Number: Q30
#Question: Which of these three statements is closest to your own opinion?
#Statement 1: Democracy is preferable to any other kind of government.
#Statement 2: In some circumstances, a non-democratic government can be preferable. Statement 3: For someone like me, it doesn’t matter what kind of government we have. Variable Label: Q30. Support for democracy
#Values: 1-3, 9, 98, -1
#  Copyright Afrobarometer
#Value Labels: 1=Statement 3: Doesn’t matter, 2=Statement 2: Sometimes non-democratic preferable, 3=Statement 1: Democracy preferable, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Latinobarometer (LB)
Afrodat6$Democ_pref<-repNA03(Afrodat6$Q30)
Afrodat6$dDemoc_pref<-ifelse((Afrodat6$Democ_pref==3),1,0)


#Variable Label: Q40. Extent of democracy
#Values: 1-4, 8, 9, 98, -1
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Ghana 97
Afrodat6$Democ_nation<-ifelse(Afrodat6$Q40<1|Afrodat6$Q40>4,NA,Afrodat6$Q40)
	table(Afrodat6$Democ_nation,exclude=NULL)		#


#"Question Number: Q41
#Question: Overall, how satisfied are you with the way democracy works in [ENTER COUNTRY]? Are you: Variable Label: Q41. Satisfaction with democracy
#Values: 0-4, 9, 98, -1
#Value Labels: 0=[COUNTRY] is not a democracy, 1=Not at all satisfied, 2=Not very satisfied, 3=Fairly satisfied, 4=Very satisfied, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Eurobarometer
Afrodat6$Democ_satis<-repNA04(Afrodat6$Q41)


#"Question Number: Q52A
#Question: How much do you trust each of the following, or haven’t you heard enough about them to say: The President?
#Variable Label: Q52a. Trust president
#Values: 0-3, 9, 98, -1
#Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t know/Haven’t heard enough, 98=Refused to answer, -1=Missing
#Source: Zambia96

#* The question asked about the most powerful leadership role, whether the President or the Prime Minister. If there was a secondary leader, those are included in country-specific data sets.
#* The following countries asked about their President: ALG, BDI, BEN, BFO, BOT, CAM, CDI, CVE, EGY, GHA, GUI, KEN, LIB, MAD, MLI, MLW, MOZ, NAM, NGR, NIG, SAF, SEN, SRL, SUD, TAN, TOG, UGA, ZAM, ZIM
#* The following countries asked about their Prime Minister: LES, MAU, MRC, TUN
#* Not asked in SWZ"

#Variable Label: Q52b. Trust parliament/national assembly1#Source: Adapted from Zambia96	#*Not asked in EGY"
#Variable Label: Q52h. Trust police#Source: Zambia 96"
#Variable Label: Q52k. Trust traditional leaders	#Source: Zambia 96	#*Not asked in STP, CVE, MAU"
#Variable Label: Q52l. Trust religious leaders	#Source: Zambia 96"
Afrodat6$Trust_president<-repNA04(Afrodat6$Q52A)
Afrodat6$Trust_parliament<-repNA04(Afrodat6$Q52B)
Afrodat6$Trust_police<-repNA04(Afrodat6$Q52H)
Afrodat6$Trust_traditional_leaders<-repNA04(Afrodat6$Q52K)
Afrodat6$Trust_religious_leaders<-repNA04(Afrodat6$Q52L)


#"Question Number: Q54
#Question: In your opinion, over the past year, has the level of corruption in this country increased, decreased, or stayed the same?
#Variable Label: Q54. Level of corruption
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Increased a lot, 2=Increased somewhat, 3=Stayed the same, 4=Decreased somewhat, 5=Decreased a lot, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Transparency International
Afrodat6$corruption<-6-repNA05(Afrodat6$Q54)		#逆転しておく

#"Question Number: Q56
#Question: If you ever paid a bribe for any of the services discussed above, did you report any of the incidents you mentioned to a government official or someone in authority?
#Variable Label: Q56. Reported payment of bribes to government
#Values: 0, 1, 7, 9, 98, -1
#Value Labels: 0=No, 1=Yes, 7=Not Applicable, 9=Don`t know, 98=Refused to answer, -1=Missing Source: Afrobarometer Round 6 and Transparency International
#Note: Interviewer asks the question if respondent ever reported paying a bribe on Q55B, Q55D, Q55F, Q55H,Q 55J or-Q55L"
Afrodat6$report_bribe<-ifelse(Afrodat6$Q56<0|Afrodat6$Q56>7,NA,Afrodat6$Q56)
	table(Afrodat6$report_bribe,exclude=NULL)	#bribeしたことがないと7になる


#-------------所有もあるので濱岡はこれを分析予定
#Source: Afrobarometer Rouund 3
#Value Labels: 0=No (Don’t own), 1=Yes (Do own), 9=Don’t know, 98=Refused to answer, -1=Missing
#Question: Which of these things do you personally own: Radio?
#Variable Label: 
#Q91a. Own radio
#Q91b. Own television
#Q91c. Own motor vehicle, car, or motorcycle
#Q91d. Own mobile phone

Afrodat6$Own_Radio <-repNA01(Afrodat6$Q91A)
Afrodat6$Own_TV <-repNA01(Afrodat6$Q91B)
Afrodat6$Own_Auto <-repNA01(Afrodat6$Q91C)
Afrodat6$Own_Mbphone <-repNA01(Afrodat6$Q91D)

#    0     1  <NA> 
#15825 38038    72 
#27034 26812    89 
#42363 11348   224 
#11698 42129   108 

#Q92A　Question: How often do you use: A mobile phone?
#Values: 0-4, 9, 98, -1
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 4
#Q92a. How often use a mobile phone
#Q92b. How often use the internet

Afrodat6$Use_Mbphone <-repNA04(Afrodat6$Q92A)
Afrodat6$Use_Inet <-repNA04(Afrodat6$Q92B)
#    0     1     2     3     4  <NA> 
# 8978   667  1221  4669 38216   184 
#36352  1545  2312  5087  7733   906 



#----雇用状況
#Question Number: Q95
#Question: Do you have a job that pays a cash income? If yes, is it full-time or part-time? If no, are you presently looking for a job?
#Variable Label: Q95. Employment status
#Values: 0-3, 9, 98, -1
#Value Labels: 0=No (not looking), 1=No (looking), 2=Yes, part time, 3= Yes, full time, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: SAB
Afrodat6$Employment_status<-ifelse(Afrodat6$Q95<0|Afrodat6$Q95>3,NA,Afrodat6$Q95)
	table(Afrodat6$Employment_status,exclude=NULL)

Afrodat6$dEmployment_status_no<-ifelse(Afrodat6$Employment_status==0,1,0)
Afrodat6$dEmployment_status_looking<-ifelse(Afrodat6$Employment_status==1,1,0)
Afrodat6$dEmployment_status_part_time<-ifelse(Afrodat6$Employment_status==2,1,0)
Afrodat6$dEmployment_status_full_time<-ifelse(Afrodat6$Employment_status==3,1,0)

#----職業
#Question Number: Q96A
#Question: What is your main occupation? (If unemployed, retired or disabled, what was your last main occupation?)
#Variable Label: Q96a. Occupation of respondent
#Values: 0-12 95, 99, 98, -1
#Value Labels: 0=Never had a job, 1=Student, 2=Housewife / homemaker, 3=Agriculture / farming / fishing / forestry, 4=Trader / hawker / vendor, 5=Retail / Shop , 6=Unskilled manual worker (e.g., cleaner, laborer, domestic help, unskilled manufacturing worker), 7=Artisan or skilled manual worker (e.g., trades like electrician, mechanic, machinist or skilled manufacturing worker), 8=Clerical or secretarial, 9=Supervisor / Foreman / Senior Manager, 10=Security services (police, army, private security), 11=Mid-level professional (e.g., teacher, nurse, mid-level government officer), 12=Upper-level professional (e.g., banker/finance, doctor, lawyer, engineer, accountant, professor, senior-level government officer), 95=Other , 99=Don’t know, 98=Refused to answer, -1=Missing
#Source: Afrobarometer Round 6

Afrodat6$Occupation<-ifelse(Afrodat6$Q96A<0|Afrodat6$Q96A>95,NA,Afrodat6$Q96A)
	table(Afrodat6$Occupation,exclude=NULL)
#   0     1     2     3     4     5     6     7     8     9    10    11    12    95  <NA> 
# 5953  5344  5540 12375  4864  2195  4850  4074   947   660   931  3080  1150  1766   206 


Afrodat6$dOccupation_Never<-ifelse(Afrodat6$Occupation==0,1,0)
Afrodat6$dOccupation_Student<-ifelse(Afrodat6$Occupation==1,1,0)
Afrodat6$dOccupation_Housewife_homemaker<-ifelse(Afrodat6$Occupation==2,1,0)
Afrodat6$dOccupation_primary<-ifelse(Afrodat6$Occupation==3,1,0)
Afrodat6$dOccupation_Trader<-ifelse(Afrodat6$Occupation==4,1,0)
Afrodat6$dOccupation_Retail<-ifelse(Afrodat6$Occupation==5,1,0)
Afrodat6$dOccupation_Unskilled<-ifelse(Afrodat6$Occupation==6,1,0)
Afrodat6$dOccupation_skilled<-ifelse(Afrodat6$Occupation==7,1,0)
Afrodat6$dOccupation_Clerical<-ifelse(Afrodat6$Occupation==8,1,0)
Afrodat6$dOccupation_Supervisor<-ifelse(Afrodat6$Occupation==9,1,0)
Afrodat6$dOccupation_police<-ifelse(Afrodat6$Occupation==10,1,0)
Afrodat6$dOccupation_Mid_level<-ifelse(Afrodat6$Occupation==11,1,0)
Afrodat6$dOccupation_Upper_level<-ifelse(Afrodat6$Occupation==12,1,0)
Afrodat6$dOccupation_Other<-ifelse(Afrodat6$Occupation==95,1,0)


#----学歴
#Question Number: Q97
#Question: What is your highest level of education?
#Variable Label: Q97. Education of respondent
#Values: 0-9, 99, 98, -1
#Value Labels: 0=No formal schooling, 1=Informal schooling only (including Koranic schooling), 2=Some primary schooling, 3=Primary school completed, 4=Intermediate school or Some secondary school / high school, 5=Secondary school / high school completed , 6=Post-secondary qualifications, other than university e.g. a diploma or degree from a polytechnic or college, 7=Some university, 8=University completed, 9=Post-graduate, 99=Don’t know [Do not read], 98=Refused to answer, -1=Missing
#Source: SAB

Afrodat6$Education<-ifelse(Afrodat6$Q97<0|Afrodat6$Q97>9,NA,Afrodat6$Q97)
	table(Afrodat6$Education,exclude=NULL)


#Question Number: Q101
#Question: Respondent’s gender
#Variable Label: Q101. Gender of respondent
#Values: 1, 2
#Value Labels: 1=Male, 2=Female
#Source: SAB
#Note: Answered by interviewer
#fe_maleダミーに
Afrodat6$Gender_f<-ifelse(Afrodat6$Q101<0|Afrodat6$Q101>2,NA,Afrodat6$Q101-1)
	table(Afrodat6$Gender_f,exclude=NULL)
#   0     1 
#26801 27134 


#-------人種
#Question Number: Q102
#Question: Respondent’s race
#Variable Label: Q102. Race of respondent
#Values: 1-6, 95, -1
#Value Labels: 1=Black/African, 2=White/European, 3=Colored/Mixed Race, 4=Arab/Lebanese/North African, 5=South Asian (Indian, Pakistani, etc.), 6=Eat Asian (Chinese, Korean, Indonesian, etc.), Other=95, -1=Missing
#Source: SAB
#Note: Answered by interviewer
Afrodat6$Race<-ifelse(Afrodat6$Q102<0|Afrodat6$Q102>95,NA,Afrodat6$Q102)
	table(Afrodat6$Race,exclude=NULL)
#    1     2     3     4     5     6    95  <NA> 
#45981   361  1094  4856  1031     8   572    32 
#人種ダミー
Afrodat6$dRace_BAf<-ifelse(Afrodat6$Race==1,1,0)
Afrodat6$dRace_Wh<-ifelse(Afrodat6$Race==2,1,0)
Afrodat6$dRace_Col<-ifelse(Afrodat6$Race==3,1,0)
Afrodat6$dRace_Arab<-ifelse(Afrodat6$Race==4,1,0)
Afrodat6$dRace_SAs<-ifelse(Afrodat6$Race==5,1,0)
Afrodat6$dRace_EAs<-ifelse(Afrodat6$Race==6,1,0)
Afrodat6$dRace_Oth<-ifelse(Afrodat6$Race==95,1,0)



#保存
save(Afrodat6,file=“0Afrodat6.rda”)

#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)

m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))

	
#edit(m)
write.table(m,file="0m.txt",row.names=F,sep="\t")

#plot.by.group(m)

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




#---------------
# データ・コーディング_go
#---------------

#----国
#----Afrodat5
table(Afrodat5$COUNTRY)
#Afrodat5$COUNTRY2<-substr(Afrodat5$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat5$COUNTRY2)
Afrodat5$dCOUNTRY_ALG<-ifelse(Afrodat5$COUNTRY2=="ALG",1,0)    #すべてについてダミーを定義したので使うときはどれかを除く
Afrodat5$dCOUNTRY_BDI<-ifelse(Afrodat5$COUNTRY2=="BDI",1,0)
Afrodat5$dCOUNTRY_BEN<-ifelse(Afrodat5$COUNTRY2=="BEN",1,0) 　　
Afrodat5$dCOUNTRY_BFO<-ifelse(Afrodat5$COUNTRY2=="BFO",1,0)
Afrodat5$dCOUNTRY_CAM<-ifelse(Afrodat5$COUNTRY2=="CAM",1,0)
Afrodat5$dCOUNTRY_CDI<-ifelse(Afrodat5$COUNTRY2=="CDI",1,0)
Afrodat5$dCOUNTRY_CVE<-ifelse(Afrodat5$COUNTRY2=="CVE",1,0)　　
Afrodat5$dCOUNTRY_EGY<-ifelse(Afrodat5$COUNTRY2=="EGY",1,0)
Afrodat5$dCOUNTRY_GHA<-ifelse(Afrodat5$COUNTRY2=="GHA",1,0)
Afrodat5$dCOUNTRY_GUI<-ifelse(Afrodat5$COUNTRY2=="GUI",1,0)
Afrodat5$dCOUNTRY_KEN<-ifelse(Afrodat5$COUNTRY2=="KEN",1,0)
Afrodat5$dCOUNTRY_LES<-ifelse(Afrodat5$COUNTRY2=="LES",1,0)
Afrodat5$dCOUNTRY_LIB<-ifelse(Afrodat5$COUNTRY2=="LIB",1,0)
Afrodat5$dCOUNTRY_MAD<-ifelse(Afrodat5$COUNTRY2=="MAD",1,0)
Afrodat5$dCOUNTRY_MAU<-ifelse(Afrodat5$COUNTRY2=="MAU",1,0)
Afrodat5$dCOUNTRY_MLI<-ifelse(Afrodat5$COUNTRY2=="MLI",1,0)
Afrodat5$dCOUNTRY_MLW<-ifelse(Afrodat5$COUNTRY2=="MLW",1,0)
Afrodat5$dCOUNTRY_MOR<-ifelse(Afrodat5$COUNTRY2=="MOR",1,0)
Afrodat5$dCOUNTRY_MOZ<-ifelse(Afrodat5$COUNTRY2=="MOZ",1,0)
Afrodat5$dCOUNTRY_NAM<-ifelse(Afrodat5$COUNTRY2=="NAM",1,0)
Afrodat5$dCOUNTRY_NGR<-ifelse(Afrodat5$COUNTRY2=="NGR",1,0)
Afrodat5$dCOUNTRY_NIG<-ifelse(Afrodat5$COUNTRY2=="NIG",1,0)
Afrodat5$dCOUNTRY_SAF<-ifelse(Afrodat5$COUNTRY2=="SAF",1,0)
Afrodat5$dCOUNTRY_SEN<-ifelse(Afrodat5$COUNTRY2=="SEN",1,0)
Afrodat5$dCOUNTRY_SRL<-ifelse(Afrodat5$COUNTRY2=="SRL",1,0)
Afrodat5$dCOUNTRY_STP<-ifelse(Afrodat5$COUNTRY2=="STP",1,0)
Afrodat5$dCOUNTRY_SUD<-ifelse(Afrodat5$COUNTRY2=="SUD",1,0)
Afrodat5$dCOUNTRY_SWZ<-ifelse(Afrodat5$COUNTRY2=="SWZ",1,0)
Afrodat5$dCOUNTRY_TAN<-ifelse(Afrodat5$COUNTRY2=="TAN",1,0)
Afrodat5$dCOUNTRY_TOG<-ifelse(Afrodat5$COUNTRY2=="TOG",1,0)
Afrodat5$dCOUNTRY_TUN<-ifelse(Afrodat5$COUNTRY2=="TUN",1,0)
Afrodat5$dCOUNTRY_UGA<-ifelse(Afrodat5$COUNTRY2=="UGA",1,0)
Afrodat5$dCOUNTRY_ZAM<-ifelse(Afrodat5$COUNTRY2=="ZAM",1,0)
Afrodat5$dCOUNTRY_ZIM<-ifelse(Afrodat5$COUNTRY2=="ZIM",1,0)
#34 African countries

#----Afrodat4
table(Afrodat4$COUNTRY)
#Afrodat4$COUNTRY2<-substr(Afrodat4$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat4$COUNTRY2)

Afrodat4$dCOUNTRY_BEN<-ifelse(Afrodat4$COUNTRY2=="BEN",1,0)
Afrodat4$dCOUNTRY_BFO<-ifelse(Afrodat4$COUNTRY2=="BFO",1,0)
Afrodat4$dCOUNTRY_BOT<-ifelse(Afrodat4$COUNTRY2=="BOT",1,0)
Afrodat4$dCOUNTRY_CVE<-ifelse(Afrodat4$COUNTRY2=="CVE",1,0)
Afrodat4$dCOUNTRY_GHA<-ifelse(Afrodat4$COUNTRY2=="GHA",1,0)
Afrodat4$dCOUNTRY_KEN<-ifelse(Afrodat4$COUNTRY2=="KEN",1,0)
Afrodat4$dCOUNTRY_LES<-ifelse(Afrodat4$COUNTRY2=="LES",1,0)
Afrodat4$dCOUNTRY_LIB<-ifelse(Afrodat4$COUNTRY2=="LIB"|Afrodat4$COUNTRY2=="LIb",1,0)
Afrodat4$dCOUNTRY_MAD<-ifelse(Afrodat4$COUNTRY2=="MAD",1,0)
Afrodat4$dCOUNTRY_MLI<-ifelse(Afrodat4$COUNTRY2=="MLI",1,0)
Afrodat4$dCOUNTRY_MLW<-ifelse(Afrodat4$COUNTRY2=="MLW",1,0)
Afrodat4$dCOUNTRY_MOZ<-ifelse(Afrodat4$COUNTRY2=="MOZ",1,0)
Afrodat4$dCOUNTRY_NAM<-ifelse(Afrodat4$COUNTRY2=="NAM",1,0)
Afrodat4$dCOUNTRY_NIG<-ifelse(Afrodat4$COUNTRY2=="NIG",1,0)
Afrodat4$dCOUNTRY_SAF<-ifelse(Afrodat4$COUNTRY2=="SAF",1,0)
Afrodat4$dCOUNTRY_SEN<-ifelse(Afrodat4$COUNTRY2=="SEN",1,0)
Afrodat4$dCOUNTRY_TAN<-ifelse(Afrodat4$COUNTRY2=="TAN",1,0)
Afrodat4$dCOUNTRY_UGA<-ifelse(Afrodat4$COUNTRY2=="UGA",1,0)
Afrodat4$dCOUNTRY_ZAM<-ifelse(Afrodat4$COUNTRY2=="ZAM",1,0)
Afrodat4$dCOUNTRY_ZIM<-ifelse(Afrodat4$COUNTRY2=="ZIM",1,0)
#20 African Countries
#Benin, Botswana, Burkina Faso, Cape Verde, Ghana, Kenya, Lesotho, Liberia, Madagascar, Malawi, Mali, 
#Mozambique, Namibia, Nigeria, Senegal, South Africa, Tanzania, Uganda, Zambia, Zimbabwe)

#----Afrodat3
table(Afrodat3$country)
#Afrodat3$COUNTRY2<-substr(Afrodat3$respno,1,3)    #はじめの3文字が国名
table(Afrodat3$COUNTRY2)

Afrodat3$dCOUNTRY_BEN<-ifelse(Afrodat3$COUNTRY2=="BEN",1,0)
Afrodat3$dCOUNTRY_BOT<-ifelse(Afrodat3$COUNTRY2=="BOT",1,0)
Afrodat3$dCOUNTRY_CVE<-ifelse(Afrodat3$COUNTRY2=="CVE",1,0)
Afrodat3$dCOUNTRY_GHA<-ifelse(Afrodat3$COUNTRY2=="GHA",1,0)
Afrodat3$dCOUNTRY_KEN<-ifelse(Afrodat3$COUNTRY2=="KEN",1,0)
Afrodat3$dCOUNTRY_LES<-ifelse(Afrodat3$COUNTRY2=="LES",1,0)
Afrodat3$dCOUNTRY_MAD<-ifelse(Afrodat3$COUNTRY2=="MAD",1,0)
Afrodat3$dCOUNTRY_MLI<-ifelse(Afrodat3$COUNTRY2=="MLI",1,0)
Afrodat3$dCOUNTRY_MOZ<-ifelse(Afrodat3$COUNTRY2=="MOZ",1,0)
Afrodat3$dCOUNTRY_MWI<-ifelse(Afrodat3$COUNTRY2=="MWI",1,0)
Afrodat3$dCOUNTRY_NAM<-ifelse(Afrodat3$COUNTRY2=="NAM",1,0)
Afrodat3$dCOUNTRY_NIG<-ifelse(Afrodat3$COUNTRY2=="NIG",1,0)
Afrodat3$dCOUNTRY_SAF<-ifelse(Afrodat3$COUNTRY2=="SAF",1,0)
Afrodat3$dCOUNTRY_SEN<-ifelse(Afrodat3$COUNTRY2=="SEN",1,0)
Afrodat3$dCOUNTRY_TAN<-ifelse(Afrodat3$COUNTRY2=="TAN",1,0)
Afrodat3$dCOUNTRY_UGA<-ifelse(Afrodat3$COUNTRY2=="UGA",1,0)
Afrodat3$dCOUNTRY_ZAM<-ifelse(Afrodat3$COUNTRY2=="ZAM",1,0)
Afrodat3$dCOUNTRY_ZIM<-ifelse(Afrodat3$COUNTRY2=="ZIM",1,0)
#18 African Countries
#(Benin, Botswana, Cape Verde, Ghana, Kenya, Lesotho, Madagascar, Malawi, Mali, Mozambique,
#Namibia, Nigeria, Senegal, South Africa, Tanzania, Uganda, Zambia, Zimbabwe)


#----Afrodat2
table(Afrodat2$country)
#Afrodat2$COUNTRY2<-substr(Afrodat2$respno,1,3)    #はじめの3文字が国名
table(Afrodat2$COUNTRY2)

Afrodat2$dCOUNTRY_BOT<-ifelse(Afrodat2$COUNTRY2=="BOT",1,0)
Afrodat2$dCOUNTRY_CVE<-ifelse(Afrodat2$COUNTRY2=="CVE"|Afrodat2$COUNTRY2=="cve",1,0)
Afrodat2$dCOUNTRY_GHA<-ifelse(Afrodat2$COUNTRY2=="GHA",1,0)
Afrodat2$dCOUNTRY_KEN<-ifelse(Afrodat2$COUNTRY2=="KEN",1,0)
Afrodat2$dCOUNTRY_LES<-ifelse(Afrodat2$COUNTRY2=="LES",1,0)
Afrodat2$dCOUNTRY_MLI<-ifelse(Afrodat2$COUNTRY2=="MLI",1,0)
Afrodat2$dCOUNTRY_MOZ<-ifelse(Afrodat2$COUNTRY2=="MOZ",1,0)
Afrodat2$dCOUNTRY_MWI<-ifelse(Afrodat2$COUNTRY2=="MWI",1,0)
Afrodat2$dCOUNTRY_NAM<-ifelse(Afrodat2$COUNTRY2=="NAM",1,0)
Afrodat2$dCOUNTRY_NIG<-ifelse(Afrodat2$COUNTRY2=="NIG",1,0)
Afrodat2$dCOUNTRY_SAF<-ifelse(Afrodat2$COUNTRY2=="SAF",1,0)
Afrodat2$dCOUNTRY_SEN<-ifelse(Afrodat2$COUNTRY2=="SEN",1,0)
Afrodat2$dCOUNTRY_TAN<-ifelse(Afrodat2$COUNTRY2=="TAN",1,0)
Afrodat2$dCOUNTRY_UGA<-ifelse(Afrodat2$COUNTRY2=="UGA",1,0)
Afrodat2$dCOUNTRY_ZAM<-ifelse(Afrodat2$COUNTRY2=="ZAM",1,0)
Afrodat2$dCOUNTRY_ZIM<-ifelse(Afrodat2$COUNTRY2=="ZIM",1,0)

#16 African Countries
#1=Botswana; 2=Ghana; 3=Lesotho; 4=Malawi; 5=Mali; 6=Namibia; 7=Nigeria; 8=South Africa;
#9=Tanzania; 10=Uganda; 11=Zambia; 12=Zimbabwe; 13=Cape Verde; 14=Kenya; 15=Mozambique; 16=Senegal

#----Afrodat1
table(Afrodat1$COUNTRY2)
Afrodat1$dCOUNTRY_BOT<-ifelse(Afrodat1$COUNTRY2=="BOT",1,0)
Afrodat1$dCOUNTRY_GHA<-ifelse(Afrodat1$COUNTRY2=="GHA",1,0)
Afrodat1$dCOUNTRY_LES<-ifelse(Afrodat1$COUNTRY2=="LES",1,0)
Afrodat1$dCOUNTRY_MLI<-ifelse(Afrodat1$COUNTRY2=="MLI",1,0)
Afrodat1$dCOUNTRY_MLW<-ifelse(Afrodat1$COUNTRY2=="MLW",1,0)
Afrodat1$dCOUNTRY_NAM<-ifelse(Afrodat1$COUNTRY2=="NAM",1,0)
Afrodat1$dCOUNTRY_NIG<-ifelse(Afrodat1$COUNTRY2=="NIG",1,0)
Afrodat1$dCOUNTRY_SAF<-ifelse(Afrodat1$COUNTRY2=="SAF",1,0)
Afrodat1$dCOUNTRY_TAN<-ifelse(Afrodat1$COUNTRY2=="TAN",1,0)
Afrodat1$dCOUNTRY_UGA<-ifelse(Afrodat1$COUNTRY2=="UGA",1,0)
Afrodat1$dCOUNTRY_ZAM<-ifelse(Afrodat1$COUNTRY2=="ZAM",1,0)
Afrodat1$dCOUNTRY_ZIM<-ifelse(Afrodat1$COUNTRY2=="ZIM",1,0)
#Botswana, Ghana, Lesotho, Malawi, Mali, Namibia, Nigeria, South Africa, Tanzania, Uganda, Zambia and Zimbabwe Äb0

#----URBRUR
#W4からは
#"Question Number: URBRUR
#Question: PSU/EA
#Variable Label: Urban or Rural Primary Sampling Unit Values: 1-2
#Value Labels: 1=urban, 2=rural
#Note: Answered by interviewer"
#W1-3は
#"Variable name: urbrur
#Variable label: Urban or rural
#Values: 1, 2, 9, 99
#Value Labels: 1=Urban, 2=Rural, 9=Can’t Determine, 99=Missing Data
#Notes: Completed by interviewer."
Afrodat7$dUrban<-ifelse(Afrodat7$URBRUR==1,1,
					ifelse(Afrodat7$URBRUR==2,0,NA))
Afrodat6$dUrban<-ifelse(Afrodat6$URBRUR==1,1,
					ifelse(Afrodat6$URBRUR==2,0,NA))
Afrodat5$dUrban<-ifelse(Afrodat5$URBRUR==1,1,
					ifelse(Afrodat5$URBRUR==2,0,NA))
Afrodat4$dUrban<-ifelse(Afrodat4$URBRUR==1,1,
					ifelse(Afrodat4$URBRUR==2,0,NA))
Afrodat3$dUrban<-ifelse(Afrodat3$urbrur==1,1,
					ifelse(Afrodat3$urbrur==2,0,NA))
Afrodat2$dUrban<-ifelse(Afrodat2$urbrur==1,1,
					ifelse(Afrodat2$urbrur==2,0,NA))
Afrodat1$dUrban<-ifelse(Afrodat1$urbrur==1,1,
					ifelse(Afrodat1$urbrur==2,0,NA))




#----年齢
#Question Number: Q1
#Question: How old are you?
#Value Labels: 98=Refused to answer, 999=Don’t know, -1=Missing
Afrodat7$Age<-ifelse(Afrodat7$Q1<0|Afrodat7$Q1>105|Afrodat7$Q1==98,NA,Afrodat7$Q1)
table(Afrodat7$Age,exclude=NULL)

Afrodat6$Age<-ifelse(Afrodat6$Q1<0|Afrodat6$Q1>105|Afrodat6$Q1==98,NA,Afrodat6$Q1)
table(Afrodat6$Age,exclude=NULL)

Afrodat5$Age<-ifelse(Afrodat5$Q1<0|Afrodat5$Q1>105|Afrodat5$Q1==98,NA,Afrodat5$Q1)
table(Afrodat5$Age,exclude=NULL)

Afrodat4$Age<-ifelse(Afrodat4$Q1<0|Afrodat4$Q1>105|Afrodat4$Q1==98,NA,Afrodat4$Q1)
table(Afrodat4$Age,exclude=NULL)

Afrodat3$Age<-ifelse(Afrodat3$q1<0|Afrodat3$q1>105|Afrodat3$q1==98,NA,Afrodat3$q1)
table(Afrodat3$Age,exclude=NULL)

Afrodat2$Age<-ifelse(Afrodat2$q80<0|Afrodat2$q80>105|Afrodat2$q80==98,NA,Afrodat2$q80)
table(Afrodat2$Age,exclude=NULL)

Afrodat1$Age<-ifelse(Afrodat1$age<0|Afrodat1$age>105|Afrodat1$age==98,NA,Afrodat1$age)
table(Afrodat1$Age,exclude=NULL)

#----言語　
#----Afrodat7
Afrodat7$Language<-ifelse(Afrodat7$Q2A<0|Afrodat7$Q2A>=9998,NA,Afrodat7$Q2A)
table(Afrodat7$Language,exclude=NULL)
d5<-data.frame(table(Afrodat7$Language,exclude=NULL))
d5[order(d5[,2],decreasing=T),]

Afrodat7$dlang_English<-ifelse(Afrodat7$Language==1,1,0)
Afrodat7$dlang_French<-ifelse(Afrodat7$Language==2,1,0)
Afrodat7$dlang_Portuguese<-ifelse(Afrodat7$Language==3,1,0)
Afrodat7$dlang_Swahili<-ifelse(Afrodat7$Language==4,1,0)
Afrodat7$dlang_Arabic<-ifelse(Afrodat7$Language==5,1,0)
Afrodat7$dlang_Adja<-ifelse(Afrodat7$Language==6,1,0)
Afrodat7$dlang_Afrikaans<-ifelse(Afrodat7$Language==7,1,0)

#Afrodat7$dlang_Chichewa<-ifelse(Afrodat7$Language==463,1,0)
Afrodat7$dlang_Akan<-ifelse(Afrodat7$Language==260,1,0)
Afrodat7$dlang_Other<-ifelse(Afrodat7$Language==9995,1,0)
#Afrodat7$dlang_Egyptian_Arabic<-ifelse(Afrodat7$Language==1460,1,0)
Afrodat7$dlang_Crioulo<-ifelse(Afrodat7$Language==220,1,0)
Afrodat7$dlang_Kirund<-ifelse(Afrodat7$Language==1180,1,0)
Afrodat7$dlang_Sesotho<-ifelse(Afrodat7$Language==340,1,0)
Afrodat7$dlang_Sudanese_Arabic<-ifelse(Afrodat7$Language==1540,1,0)
Afrodat7$dlang_Creole<-ifelse(Afrodat7$Language==900,1,0)
Afrodat7$dlang_siSwati<-ifelse(Afrodat7$Language==1620,1,0)
Afrodat7$dlang_Shona<-ifelse(Afrodat7$Language==861,1,0)
Afrodat7$dlang_Algerian_Arabic<-ifelse(Afrodat7$Language==1420,1,0)
#----ここまでR6と同一
Afrodat7$dlang_Akan<-ifelse(Afrodat7$Language==260,1,0)
Afrodat7$dlang_siSwati<-ifelse(Afrodat7$Language==35,1,0)
Afrodat7$dlang_Crioulo<-ifelse(Afrodat7$Language==220,1,0)
Afrodat7$dlang_Kirund<-ifelse(Afrodat7$Language==1180,1,0)
Afrodat7$dlang_Sesotho<-ifelse(Afrodat7$Language==340,1,0)
Afrodat7$dlang_Creole<-ifelse(Afrodat7$Language==900,1,0)
Afrodat7$dlang_Shona<-ifelse(Afrodat7$Language==861,1,0)
Afrodat7$dlang_Hausa<-ifelse(Afrodat7$Language==17,1,0)
Afrodat7$dlang_Malagasy<-ifelse(Afrodat7$Language==421,1,0)
Afrodat7$dlang_Yoruba<-ifelse(Afrodat7$Language==39,1,0)
Afrodat7$dlang_Wolof<-ifelse(Afrodat7$Language==660,1,0)

#----Afrodat5
Afrodat5$Language<-ifelse(Afrodat5$Q2<0|Afrodat5$Q2>=9998,NA,Afrodat5$Q2)
table(Afrodat5$Language,exclude=NULL)
d5<-data.frame(table(Afrodat5$Language,exclude=NULL))
d5[order(d5[,2],decreasing=T),]

Afrodat5$dlang_English<-ifelse(Afrodat5$Language==1,1,0)
Afrodat5$dlang_French<-ifelse(Afrodat5$Language==2,1,0)
Afrodat5$dlang_Portuguese<-ifelse(Afrodat5$Language==3,1,0)
Afrodat5$dlang_Swahili<-ifelse(Afrodat5$Language==4,1,0)
Afrodat5$dlang_Arabic<-ifelse(Afrodat5$Language==5,1,0)
Afrodat5$dlang_Adja<-ifelse(Afrodat5$Language==6,1,0)
Afrodat5$dlang_Afrikaans<-ifelse(Afrodat5$Language==7,1,0)

#Afrodat5$dlang_Chichewa<-ifelse(Afrodat5$Language==463,1,0)
Afrodat5$dlang_Akan<-ifelse(Afrodat5$Language==260,1,0)
Afrodat5$dlang_Other<-ifelse(Afrodat5$Language==9995,1,0)
#Afrodat5$dlang_Egyptian_Arabic<-ifelse(Afrodat5$Language==1460,1,0)
Afrodat5$dlang_Crioulo<-ifelse(Afrodat5$Language==220,1,0)
Afrodat5$dlang_Kirund<-ifelse(Afrodat5$Language==1180,1,0)
Afrodat5$dlang_Sesotho<-ifelse(Afrodat5$Language==340,1,0)
Afrodat5$dlang_Sudanese_Arabic<-ifelse(Afrodat5$Language==1540,1,0)
Afrodat5$dlang_Creole<-ifelse(Afrodat5$Language==900,1,0)
Afrodat5$dlang_siSwati<-ifelse(Afrodat5$Language==1620,1,0)
Afrodat5$dlang_Shona<-ifelse(Afrodat5$Language==861,1,0)
Afrodat5$dlang_Algerian_Arabic<-ifelse(Afrodat5$Language==1420,1,0)
#----ここまでR6と同一
Afrodat5$dlang_Akan<-ifelse(Afrodat5$Language==260,1,0)
Afrodat5$dlang_siSwati<-ifelse(Afrodat5$Language==35,1,0)
Afrodat5$dlang_Crioulo<-ifelse(Afrodat5$Language==220,1,0)
Afrodat5$dlang_Kirund<-ifelse(Afrodat5$Language==1180,1,0)
Afrodat5$dlang_Sesotho<-ifelse(Afrodat5$Language==340,1,0)
Afrodat5$dlang_Creole<-ifelse(Afrodat5$Language==900,1,0)
Afrodat5$dlang_Shona<-ifelse(Afrodat5$Language==861,1,0)
Afrodat5$dlang_Hausa<-ifelse(Afrodat5$Language==17,1,0)
Afrodat5$dlang_Malagasy<-ifelse(Afrodat5$Language==421,1,0)
Afrodat5$dlang_Yoruba<-ifelse(Afrodat5$Language==39,1,0)
Afrodat5$dlang_Wolof<-ifelse(Afrodat5$Language==660,1,0)

#----Afrodat4
Afrodat4$Language<-ifelse(Afrodat4$Q3<0|Afrodat4$Q3>=9998,NA,Afrodat4$Q3)
table(Afrodat4$Language,exclude=NULL)
d4<-data.frame(table(Afrodat4$Language,exclude=NULL))
d4[order(d4[,2],decreasing=T),]

Afrodat4$dlang_English<-ifelse(Afrodat4$Language==1,1,0)
Afrodat4$dlang_French<-ifelse(Afrodat4$Language==2,1,0)
Afrodat4$dlang_Portuguese<-ifelse(Afrodat4$Language==3,1,0)
Afrodat4$dlang_Kiswahili<-ifelse(Afrodat4$Language==4,1,0)

Afrodat4$dlang_Other<-ifelse(Afrodat4$Language==995,1,0)
Afrodat4$dlang_Crioulo<-ifelse(Afrodat4$Language==220,1,0)
Afrodat4$dlang_Sesotho<-ifelse(Afrodat4$Language==340,1,0)
Afrodat4$dlang_Shona<-ifelse(Afrodat4$Language==861,1,0)
Afrodat4$dlang_Setswana<-ifelse(Afrodat4$Language==140,1,0)
Afrodat4$dlang_Malagasy<-ifelse(Afrodat4$Language==421,1,0)
Afrodat4$dlang_Wolof<-ifelse(Afrodat4$Language==660,1,0)
Afrodat4$dlang_Mooré<-ifelse(Afrodat4$Language==182,1,0)
Afrodat4$dlang_Oshivambo<-ifelse(Afrodat4$Language==583,1,0)
Afrodat4$dlang_Bambara<-ifelse(Afrodat4$Language==501,1,0)
Afrodat4$dlang_Akan<-ifelse(Afrodat4$Language==260,1,0)
Afrodat4$dlang_Afrikaans<-ifelse(Afrodat4$Language==700,1,0)
Afrodat4$dlang_Hausa<-ifelse(Afrodat4$Language==620,1,0)
Afrodat4$dlang_Luganda<-ifelse(Afrodat4$Language==780,1,0)
Afrodat4$dlang_Chewa<-ifelse(Afrodat4$Language==463,1,0)
Afrodat4$dlang_Yoruba<-ifelse(Afrodat4$Language==622,1,0)
Afrodat4$dlang_Malagasy<-ifelse(Afrodat4$Language==420,1,0)


#----Afrodat3
Afrodat3$Language<-ifelse(Afrodat3$q114<0|Afrodat3$q114>=9998,NA,Afrodat3$q114)
table(Afrodat3$Language,exclude=NULL)
d3<-data.frame(table(Afrodat3$Language,exclude=NULL))
d3[order(d3[,2],decreasing=T),]

Afrodat3$dlang_English<-ifelse(Afrodat3$Language==1,1,0)
Afrodat3$dlang_French<-ifelse(Afrodat3$Language==2,1,0)
Afrodat3$dlang_Portuguese<-ifelse(Afrodat3$Language==3,1,0)
Afrodat3$dlang_Kiswahili<-ifelse(Afrodat3$Language==4,1,0)

Afrodat3$dlang_Other<-ifelse(Afrodat3$Language==995,1,0)
Afrodat3$dlang_Creole<-ifelse(Afrodat3$Language==165,1,0)
Afrodat3$dlang_Malgasy<-ifelse(Afrodat3$Language==240,1,0)
Afrodat3$dlang_Crioulo<-ifelse(Afrodat3$Language==220,1,0)
Afrodat3$dlang_Oshiwambo<-ifelse(Afrodat3$Language==326,1,0)
Afrodat3$dlang_Chewa<-ifelse(Afrodat3$Language==263,1,0)
Afrodat3$dlang_Wolof<-ifelse(Afrodat3$Language==360,1,0)
Afrodat3$dlang_Bambara<-ifelse(Afrodat3$Language==280,1,0)
Afrodat3$dlang_Shona<-ifelse(Afrodat3$Language==441,1,0)
Afrodat3$dlang_Akan<-ifelse(Afrodat3$Language==180,1,0)
Afrodat3$dlang_Xhosa<-ifelse(Afrodat3$Language==102,1,0)
Afrodat3$dlang_Luganda<-ifelse(Afrodat3$Language==417,1,0)
Afrodat3$dlang_Yoruba<-ifelse(Afrodat3$Language==342,1,0)

#----Afrodat2
Afrodat2$Language<-ifelse(Afrodat2$q83<0|Afrodat2$q83>=9998,NA,Afrodat2$q83)
table(Afrodat2$Language,exclude=NULL)
d2<-data.frame(table(Afrodat2$Language,exclude=NULL))
d2[order(d2[,2],decreasing=T),]

Afrodat2$dlang_English<-ifelse(Afrodat2$Language==1,1,0)
Afrodat2$dlang_French<-ifelse(Afrodat2$Language==2,1,0)
Afrodat2$dlang_Portuguese<-ifelse(Afrodat2$Language==3,1,0)
Afrodat2$dlang_Kiswahili<-ifelse(Afrodat2$Language==4,1,0)

Afrodat2$dlang_Other<-ifelse(Afrodat2$Language==995,1,0)
Afrodat2$dlang_Crioulo<-ifelse(Afrodat2$Language==50,1,0)
Afrodat2$dlang_Sesotho<-ifelse(Afrodat2$Language==180,1,0)
Afrodat2$dlang_Setswana<-ifelse(Afrodat2$Language==240,1,0)
Afrodat2$dlang_Shona<-ifelse(Afrodat2$Language==280,1,0)
Afrodat2$dlang_Akan<-ifelse(Afrodat2$Language==120,1,0)
Afrodat2$dlang_Chewa<-ifelse(Afrodat2$Language==143,1,0)
Afrodat2$dlang_Ovambo<-ifelse(Afrodat2$Language==321,1,0)
Afrodat2$dlang_Hausa<-ifelse(Afrodat2$Language==260,1,0)
Afrodat2$dlang_Wolof<-ifelse(Afrodat2$Language==220,1,0)
Afrodat2$dlang_Yoruba<-ifelse(Afrodat2$Language==261,1,0)
Afrodat2$dlang_Afrikaans<-ifelse(Afrodat2$Language==100,1,0)
Afrodat2$dlang_Bambara<-ifelse(Afrodat2$Language==164,1,0)
Afrodat2$dlang_Kikuyu<-ifelse(Afrodat2$Language==382,1,0)
Afrodat2$dlang_Zulu<-ifelse(Afrodat2$Language==109,1,0)
Afrodat2$dlang_Bemba<-ifelse(Afrodat2$Language==300,1,0)

#----Afrodat1
Afrodat1$Language<-ifelse(Afrodat1$language<0|Afrodat1$language>=9998,NA,Afrodat1$language)
table(Afrodat1$Language,exclude=NULL)
d1<-data.frame(table(Afrodat1$Language,exclude=NULL))
d1[order(d1[,2],decreasing=T),]
Afrodat1$dlang_English<-ifelse(Afrodat1$Language==6,1,0)
Afrodat1$dlang_French<-ifelse(Afrodat1$Language==7,1,0)
Afrodat1$dlang_Portuguese<-ifelse(Afrodat1$Language==12,1,0)
Afrodat1$dlang_Kiswahili<-ifelse(Afrodat1$Language==193,1,0)

Afrodat1$dlang_Other<-ifelse(Afrodat1$Language==995,1,0)
Afrodat1$dlang_Crioulo<-ifelse(Afrodat1$Language==15,1,0)
Afrodat1$dlang_Sesotho<-ifelse(Afrodat1$Language==14,1,0)
Afrodat1$dlang_Akan<-ifelse(Afrodat1$Language==41,1,0)
Afrodat1$dlang_Hausa<-ifelse(Afrodat1$Language==103,1,0)
Afrodat1$dlang_Swahili<-ifelse(Afrodat1$Language==206,1,0)
Afrodat1$dlang_Bambara<-ifelse(Afrodat1$Language==84,1,0)
Afrodat1$dlang_Shona<-ifelse(Afrodat1$Language==18,1,0)
Afrodat1$dlang_Yoruba<-ifelse(Afrodat1$Language==104,1,0)
Afrodat1$dlang_Ibo<-ifelse(Afrodat1$Language==105,1,0)
Afrodat1$dlang_Luganda<-ifelse(Afrodat1$Language==194,1,0)
Afrodat1$dlang_Oshiwambo<-ifelse(Afrodat1$Language==10,1,0)
Afrodat1$dlang_Afrikaans<-ifelse(Afrodat1$Language==1,1,0)
Afrodat1$dlang_Zulu<-ifelse(Afrodat1$Language==23,1,0)

#economic condition、living conditions----------
#Afrodat7----------
#$Question Number: Q3A,B
#Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know, 998=Refused to answer, -1=Missing
#Source: NDB, Zambia96
Afrodat7$Cond_econ<-repNA05(Afrodat7$Q4A)
Afrodat7$Cond_your_liv<-repNA05(Afrodat7$Q4B)


#Afrodat5----------
#$Question Number: Q3A,B
#Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know, 998=Refused to answer, -1=Missing
#Source: NDB, Zambia96
Afrodat5$Cond_econ<-repNA05(Afrodat5$Q3A)
Afrodat5$Cond_your_liv<-repNA05(Afrodat5$Q3B)

#Afrodat4----------
#Question Number: Q4A,B
Afrodat4$Cond_econ<-repNA05(Afrodat4$Q4A)
Afrodat4$Cond_your_liv<-repNA05(Afrodat4$Q4B)

#Afrodat3----------
#Question Number: Q4A,B
Afrodat3$Cond_econ<-repNA05(Afrodat3$q4a)
Afrodat3$Cond_your_liv<-repNA05(Afrodat3$q4b)

#Afrodat2----------
#Question Number: Q1A,B
Afrodat2$Cond_econ<-repNA05(Afrodat2$q1a)
Afrodat2$Cond_your_liv<-repNA05(Afrodat2$q1b)

#//Afrodat1----------
#Value Labels: 1=Much less satisfied/Much worse, 2=Slightly less satisfied/worse, 3=About the same,
#4=Slightly more satisfied/Better, 5=Much more satisfied/Much better, 9=Don’t Know, 98=Refused,99=Missing Data 
Afrodat1$Cond_econ<-repNA05(Afrodat1$pfepas)
Afrodat1$Cond_your_liv<-NA

##living conditions
#Afrodat7----------
#Question Number:Q5
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Relative_live<-ifelse(Afrodat7$Q5<0|Afrodat7$Q5>=9,NA,Afrodat7$Q5)
table(Afrodat7$Relative_live,exclude=NULL)        #

#Afrodat5----------
#Question Number:Q4
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Relative_live<-ifelse(Afrodat5$Q4<0|Afrodat5$Q4>=9,NA,Afrodat5$Q4)
table(Afrodat5$Relative_live,exclude=NULL)        #

#Afrodat4----------
#Question Number: Q5
Afrodat4$Relative_live<-ifelse(Afrodat4$Q5<0|Afrodat4$Q5>=9,NA,Afrodat4$Q5)
table(Afrodat5$Relative_live,exclude=NULL)        #

#Afrodat3----------
#Question Number: Q5
Afrodat3$Relative_live<-ifelse(Afrodat3$q5<0|Afrodat3$q5>=9,NA,Afrodat3$q5)
table(Afrodat3$Relative_live,exclude=NULL)        #

#//Afrodat2----------
#Question Number: Q2B
#Question: In general, how do you rate: Your living conditions compared to those of other {countrymen]?
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t Know, 98=Refused to
Afrodat2$Relative_live<-ifelse(Afrodat2$q2b<0|Afrodat2$q2b>=9,NA,Afrodat2$q2b)
table(Afrodat2$Relative_live,exclude=NULL)        #

#//Afrodat1----------
#Variable name: pfeerd
#Variable label: Own living conditions compared to others
#Value Labels: 1=Much worse, 2=Worse, 3=About the same, 4=Better, 5=Much better, 9=Don’t Know,98=Refused to Answer, 99=Missing Data
Afrodat1$Relative_live<-ifelse(Afrodat1$pfeerd<0|Afrodat1$pfeerd>=9,NA,Afrodat1$pfeerd)
table(Afrodat1$Relative_live,exclude=NULL)        #


##Without xx
#Afrodat7----------
#Question Number: Q8A
#Value Labels: 0=Never, 1=Just once or twice, 2=Several times, 3=Many times, 4=Always, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$gone_food<-repNA04(Afrodat7$Q8A)
Afrodat7$gone_water<-repNA04(Afrodat7$Q8B)
Afrodat7$gone_med<-repNA04(Afrodat7$Q8C)
Afrodat7$gone_fuel<-repNA04(Afrodat7$Q8D)
Afrodat7$gone_cash<-repNA04(Afrodat7$Q8E)
Afrodat7$gone_electricity<-NA


#Afrodat5----------
#Question Number: Q8A
#Value Labels: 0=Never, 1=Just once or twice, 2=Several times, 3=Many times, 4=Always, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$gone_food<-repNA04(Afrodat5$Q8A)
Afrodat5$gone_water<-repNA04(Afrodat5$Q8B)
Afrodat5$gone_med<-repNA04(Afrodat5$Q8C)
Afrodat5$gone_fuel<-repNA04(Afrodat5$Q8D)
Afrodat5$gone_cash<-repNA04(Afrodat5$Q8E)
Afrodat5$gone_electricity<-NA

#Afrodat4----------
Afrodat4$gone_food<-repNA04(Afrodat4$Q8A)
Afrodat4$gone_water<-repNA04(Afrodat4$Q8B)
Afrodat4$gone_med<-repNA04(Afrodat4$Q8C)
Afrodat4$gone_fuel<-repNA04(Afrodat4$Q8D)
Afrodat4$gone_cash<-repNA04(Afrodat4$Q8E)
Afrodat4$gone_electricity<-NA

#Afrodat3----------
Afrodat3$gone_food<-repNA04(Afrodat3$q8a)
Afrodat3$gone_water<-repNA04(Afrodat3$q8b)
Afrodat3$gone_med<-repNA04(Afrodat3$q8c)
Afrodat3$gone_fuel<-repNA04(Afrodat3$q8d)
Afrodat3$gone_cash<-repNA04(Afrodat3$q8e)
Afrodat3$gone_electricity<-NA

#Afrodat2----------
Afrodat2$gone_food<-repNA04(Afrodat2$q9a)
Afrodat2$gone_water<-repNA04(Afrodat2$q9b)
Afrodat2$gone_med<-repNA04(Afrodat2$q9c)
Afrodat2$gone_fuel<-repNA04(Afrodat2$q9e)
Afrodat2$gone_cash<-repNA04(Afrodat2$q9f)
Afrodat2$gone_electricity<-repNA04(Afrodat2$q9d) #R2とR1のみ

#Afrodat1----------
Afrodat1$gone_food<-repNA04(Afrodat1$povfoo)
Afrodat1$gone_water<-repNA04(Afrodat1$povwat) 
Afrodat1$gone_med<-repNA04(Afrodat1$povhth) #Gone without healthcare 
Afrodat1$gone_fuel<-NA	#repNA04(Afrodat2$q8e)
Afrodat1$gone_cash<-repNA04(Afrodat1$povinc)
Afrodat1$gone_electricity<-repNA04(Afrodat1$povelc)　#R2とR1のみ

# news source	上で処理済み
##Afrodat5----------　SNSなし
##Question Number: Q13A,B,C,D
##Question: How often do you get news from the following sources: Radio? 
##Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 998=Refused to answer, -1=Missing 
#Afrodat5$News_Radio<-repNA04(Afrodat5$Q13A)
#Afrodat5$News_Television<-repNA04(Afrodat5$Q13B)
#Afrodat5$News_Newspaper<-repNA04(Afrodat5$Q13C)
#Afrodat5$News_Internet<-repNA04(Afrodat5$Q13D)
#
##Afrodat4----------　インターネット　SNSなし
##Question Number: Q13A,B,C 
##Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 998=Refused to answer, -1=Missing 
#Afrodat4$News_Radio<-repNA04(Afrodat4$Q12A)
#Afrodat4$News_Television<-repNA04(Afrodat4$Q12B)
#Afrodat4$News_Newspaper<-repNA04(Afrodat4$Q12C)
#
##Afrodat3----------　インターネット　SNSなし
##Question Number: Q15A,B,C
#Afrodat3$News_Radio<-repNA04(Afrodat3$q15a)
#Afrodat3$News_Television<-repNA04(Afrodat3$q15b)
#Afrodat3$News_Newspaper<-repNA04(Afrodat3$q15c)
#
##Afrodat2----------　インターネット　SNSなし
##Question Number: Q26A,B,C
#Afrodat2$News_Radio<-repNA04(Afrodat2$q26a)
#Afrodat2$News_Television<-repNA04(Afrodat2$q26b)
#Afrodat2$News_Newspaper<-repNA04(Afrodat2$q26c)
#
##Afrodat1----------　インターネット　SNSなし
##Question Number: medrad,medtv,mednew
#Afrodat1$News_Radio<-repNA04(Afrodat1$medrad)
#Afrodat1$News_Television<-repNA04(Afrodat1$medtv)
#Afrodat1$News_Newspaper<-repNA04(Afrodat1$mednew)

#----電気　　R6あり、R5、R4にはなし


#---Public affairへの興味
#Afrodat7----------
#Question Number: Q13
#Question: How interested would you say you are in public affairs? 
#Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t know, 998=Refused to answer, -1=Missing 
Afrodat7$Interest_pubaff<-repNA03(Afrodat7$Q13)

#Afrodat5----------
#Question Number: Q14 
#Question: How interested would you say you are in public affairs? 
#Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t know, 998=Refused to answer, -1=Missing 
Afrodat5$Interest_pubaff<-repNA03(Afrodat5$Q14)

#Afrodat4----------
#Question Number: Q13
Afrodat4$Interest_pubaff<-repNA03(Afrodat4$Q13)

#Afrodat3----------
#Question Number: Q16
Afrodat3$Interest_pubaff<-repNA03(Afrodat3$q16)

#//Afrodat2----------尋ね方(レベル)が違う
#Question Number: Q27
#Question Number: How interested are you in public affairs?
#Value Labels: 0=Not interested, 1=Somewhat interested, 2=Very interested, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Interest_pubaff<-repNA02(Afrodat2$q27)

#//Afrodat1----------government and public affairsについて尋ねている
#Variable label: Interested in politics
#Value Labels: 1=Very interested/Always/Most of the time, 2=Somewhat interested/Some of the time,
#3=Now and then, 4=Not interested/Hardly, 5=Don’t Know, 98=Refused to Answer, 99=Missing Data
#Some people seem to follow what’s going on in government and public affairs most of the time, whether
#there’s an election going on or not. Others aren’t that interested. Would you say you follow what’s going
#on in government and public affairs:________?
Afrodat1$Interest_pubaff<-repNA04(Afrodat1$scint)


#政治についての会話
#Afrodat7----------
#Question Number: Q13
#Value Labels: 0=Never, 1=Occasionally, 2=Frequently, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Discuss_politics<-repNA02(Afrodat7$Q13)
#政治についての会話 dummy
Afrodat7$dDiscuss_politics<-ifelse((Afrodat7$Discuss_politics==1)|(Afrodat7$Discuss_politics==2),1,0) 

#Afrodat5----------
#Question Number: Q15
#Value Labels: 0=Never, 1=Occasionally, 2=Frequently, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Discuss_politics<-repNA02(Afrodat5$Q15)
#政治についての会話 dummy
Afrodat5$dDiscuss_politics<-ifelse((Afrodat5$Discuss_politics==1)|(Afrodat5$Discuss_politics==2),1,0) 

#Afrodat4----------
#Question Number: Q14
Afrodat4$Discuss_politics<-repNA02(Afrodat4$Q14)
#dummy
Afrodat4$dDiscuss_politics<-ifelse((Afrodat4$Discuss_politics==1)|(Afrodat4$Discuss_politics==2),1,0)

#Afrodat3----------
#Question Number: Q17
Afrodat3$Discuss_politics<-repNA02(Afrodat3$q17)
#dummy
Afrodat3$dDiscuss_politics<-ifelse((Afrodat3$Discuss_politics==1)|(Afrodat3$Discuss_politics==2),1,0) 

#//Afrodat2----------尋ね方（レベル）が異なる
#Question Number: Q25A
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes,several times, 4=Yes, often, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Discuss_politics<-repNA04(Afrodat2$q25a)
#dummy
Afrodat2$dDiscuss_politics<-ifelse((Afrodat2$Discuss_politics==1)|(Afrodat2$Discuss_politics==2)|(Afrodat2$Discuss_politics==3)|(Afrodat2$Discuss_politics==4),1,0) 

#//Afrodat1----------尋ね方（レベル）が異なる
#scdsc
#When you get together with friends, would you say you discuss political matters…?
# Never Occasionally Frequently Don’t know (DNR) 
#Value Labels: 0=Never, 1=Sometimes/Occasionally/Only once, 2=Often/Frequently, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data
Afrodat1$Discuss_politics<-repNA03(Afrodat1$scdsc)
#dummy
Afrodat1$dDiscuss_politics<-ifelse((Afrodat1$Discuss_politics==1)|(Afrodat1$Discuss_politics==2),1,0) 

#コミュニティ
#Afrodat7---------- 1=Inactive Member, 2=Active Member, 3=Official Leader
#Question Number: Q25A
Afrodat7$Mem_religious<-repNA03(Afrodat7$Q20A)

#Afrodat5---------- 1=Inactive Member, 2=Active Member, 3=Official Leader
#Question Number: Q25A
Afrodat5$Mem_religious<-repNA03(Afrodat5$Q25A)

#Afrodat4----------
#Question Number: Q22A
Afrodat4$Mem_religious<-repNA03(Afrodat4$Q22A)

#Afrodat3----------
#Question Number: Q28A
Afrodat3$Mem_religious<-repNA03(Afrodat3$q28a)

#//Afrodat2---------- 
#Question Number: Q24D
#Question: Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A community development or self-help association?
#Value Labels: 0=Not a Member, 1=Inactive Member, 2=Active Member, 3=Official Leader, 9=Don’t Know,98=Refused to Answer, -1=Missing Data
#Afrodat2$Mem_voluntary<-repNA03(Afrodat2$q24d)

#"Question Number: Q24A
#Question: Let’s turn to your role in the community. Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A religious group (e.g. church, mosque)?
#Variable label: Member of religious group
#Values: 0-3, 9, 98, -1
#Value Labels: 0=Not a Member, 1=Inactive Member, 2=Active Member, 3=Official Leader, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
#Source: SAB"
Afrodat2$Mem_religious<-repNA03(Afrodat2$q24a)

#//Afrodat1---------- Yes/No
#Now I am going to read out a list of voluntary organizations. For each one, could you tell me whether you are an official leader, an active member, an inactive member or not a member of that type of
#organization: [Community development association]?
# Official leader Active member Inactive member Not a member  memdev
#Labels value 0 No, not a member/Never attend, 1 Yes, a member/ever attend
#"Variable name: memrel
#Variable label: Member/attends religious group
#Values: 0, 1, 9, 99
#Value Labels: 0=No, not a member/Never attend, 1=Yes, a member/ever attend, 9=Don’t Know, 99=Missing Data
#Question text - SAB
#Afrodat1$Mem_religious<-repNA02(Afrodat1$memdev)
Afrodat1$Mem_religious<-ifelse(Afrodat1$memrel==0,0,
						ifelse(Afrodat1$memrel==2,0,NA))	#リーダーかどうかは不明


#コミュニティvoluntary R6,R5,R4,R3のみ
#Afrodat7----------
#Question Number: Q25B
Afrodat7$Mem_voluntary<-repNA03(Afrodat7$Q20B)    #

#Afrodat5----------
#Question Number: Q25B
Afrodat5$Mem_voluntary<-repNA03(Afrodat5$Q25B)    #

#Afrodat4----------
#Question Number: Q22B
Afrodat4$Mem_voluntary<-repNA03(Afrodat4$Q22B)    #

#//Afrodat3---------- Member of trade union or farmers associationとして聴取
#Question Number: Q28B
Afrodat3$Mem_voluntary<-repNA03(Afrodat3$q28b)

Afrodat2$Mem_voluntary<-NA
Afrodat1$Mem_voluntary<-NA



#actions as citizens Raise an issue
#Afrodat7----------
#Question Number: Q21A
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Cit_action_Attend_meeting<-repNA03(Afrodat7$Q21A)    #
Afrodat7$Cit_action_raise_issue<-repNA03(Afrodat7$Q21B)    #

#Afrodat5----------
#Question Number: Q26A
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Cit_action_Attend_meeting<-repNA03(Afrodat5$Q26A)    #
Afrodat5$Cit_action_raise_issue<-repNA03(Afrodat5$Q26B)    #

#Afrodat4----------
#Question Number: Q23A
Afrodat4$Cit_action_Attend_meeting<-repNA03(Afrodat4$Q23A)    #
Afrodat4$Cit_action_raise_issue<-repNA03(Afrodat4$Q23B)    #

#Afrodat3----------
#Question Number: Q31A
Afrodat3$Cit_action_Attend_meeting<-repNA03(Afrodat3$q31a)    #
Afrodat3$Cit_action_raise_issue<-repNA03(Afrodat3$q31b)    #

#Afrodat2----------
#Question Number: Q25A
Afrodat2$Cit_action_Attend_meeting<-repNA03(Afrodat2$q25a)    #
Afrodat2$Cit_action_raise_issue<-repNA03(Afrodat2$q25b)    #

#Afrodat1----------
#Question Number: parcom
Afrodat1$Cit_action_Attend_meeting<-repNA03(Afrodat1$parcom)    #
Afrodat1$Cit_action_raise_issue<-repNA03(Afrodat1$pariss)    #


#選挙　R5,R6のみ		欠損が多い→選挙が最近行われていなければ質問もされていない　　のでトータルではつかわず
#Request Action
#Afrodat7----------
#Question Number: Q24A
#Question Number: Q24B
#Question Number: Q24D
Afrodat7$Ele_campaign_rally<-repNA02(Afrodat7$Q24A)
Afrodat7$Ele_Attend_persuade<-NA
Afrodat7$Ele_Attend_Work<-repNA02(Afrodat7$Q24B)

#Afrodat5----------
#Question Number: Q29A
#Question Number: Q29B
#Question Number: Q29D
Afrodat5$Ele_campaign_rally<-repNA02(Afrodat5$Q29A)
Afrodat5$Ele_Attend_persuade<-repNA02(Afrodat5$Q29B)
Afrodat5$Ele_Attend_Work<-repNA02(Afrodat5$Q29C)


#request action from government			#要チェック　　4以前
#Afrodat7----------
Afrodat7$Diss_request_government<-repNA04(Afrodat7$Q26A)
#Afrodat5----------
Afrodat5$Diss_request_government<-repNA04(Afrodat5$Q26B)
#Afrodat4----------
Afrodat4$Diss_request_government<-repNA04(Afrodat4$Q25C)
#Afrodat3----------
Afrodat3$Diss_request_government<-repNA04(Afrodat3$q32c)
#Afrodat2----------
Afrodat2$Diss_request_government<-repNA04(Afrodat2$q29c)
Afrodat1$Diss_request_government<-NA

#Contact official 				#要チェック　　4以前
#Afrodat7----------
#"Question Number: Q26C
Afrodat7$Diss_Contact_official<-repNA03(Afrodat7$Q26C)
#Afrodat5----------
#"Question Number: Q30A
Afrodat5$Diss_Contact_official<-repNA03(Afrodat5$Q30C)
#Afrodat4----------
#"Question Number: Q25C
Afrodat4$Diss_Contact_official<-repNA03(Afrodat4$Q25C)
#Afrodat3----------
#"Question Number: Q32C
Afrodat3$Diss_Contact_official<-repNA03(Afrodat3$q32c)
#Afrodat2----------
#"Question Number: Q29C
Afrodat2$Diss_Contact_official<-repNA03(Afrodat2$q29c)
#Afrodat1なし
Afrodat1$Diss_Contact_official<-NA


#Refuse Tax　R6,R5のみ#----			#要チェック　　4以前
#Afrodat7----------
Afrodat7$Diss_Refuse2pay<-repNA04(Afrodat7$Q26D)
#Afrodat5----------
Afrodat5$Diss_Refuse2pay<-repNA04(Afrodat5$Q26C)


#Attend demonstration
#Afrodat7----------
Afrodat7$Diss_Attend_demonstration<-repNA04(Afrodat7$Q26E)
#Afrodat5----------
Afrodat5$Diss_Attend_demonstration<-repNA04(Afrodat5$Q26D)
#Afrodat4----------
Afrodat4$Diss_Attend_demonstration<-repNA04(Afrodat4$Q23C)
#Afrodat3----------
Afrodat3$Diss_Attend_demonstration<-repNA04(Afrodat3$q31c)
#Afrodat2----------
Afrodat2$Diss_Attend_demonstration<-repNA04(Afrodat2$q25d)
#//Afrodat1----------
#Variable name: pardem
#0=Never, 1=Sometimes/Occasionally/Only once, 2=Often/Frequently,
Afrodat1$Diss_Attend_demonstration<-repNA02(Afrodat1$scdsc)


#Democracy pref
#Afrodat7----------
#Question Number: Q28
#Question: Which of these three statements is closest to your own opinion?
#Value Labels: 1=Statement 3: Doesn’t matter, 
#2=Statement 2: Sometimes non-democratic preferable, 3=Statement 1: Democracy preferable, 9=Don’t know, 98=Refused to answer, -1=Missing
Afrodat7$Democ_pref<-repNA04(Afrodat7$Q28)
#dummy
Afrodat7$dDemoc_pref<-ifelse((Afrodat7$Democ_pref==3),1,0)

#Afrodat5----------
#Question Number: Q32
#Question: Which of these three statements is closest to your own opinion?
#Value Labels: 1=Statement 3: Doesn’t matter, 
#2=Statement 2: Sometimes non-democratic preferable, 3=Statement 1: Democracy preferable, 9=Don’t know, 98=Refused to answer, -1=Missing
Afrodat5$Democ_pref<-repNA04(Afrodat5$Q32)
#dummy
Afrodat5$dDemoc_pref<-ifelse((Afrodat5$Democ_pref==3),1,0)

#Afrodat4----------
#Question Number: Q30
#Question: Which of these three statements is closest to your own opinion?
#Value Labels: 1=Statement 3: Doesn’t matter, 
#2=Statement 2: Sometimes non-democratic preferable, 
#3=Statement 1: Democracy preferable, 9=Don’t know, 998=Refused to answer, -1=Missing data
Afrodat4$Democ_pref<-repNA04(Afrodat4$Q30)
#dummy
Afrodat4$dDemoc_pref<-ifelse((Afrodat4$Democ_pref==3),1,0)

#//Afrodat3----------
#Question Number: Q37
#Question: Which of these three statements is closest to your own opinion?
#A: Democracy is preferable to any other kind of government.
#B: In some circumstances, a non-democratic government can be preferable.
#C: For someone like me, it doesn’t matter what kind of government we have.
#Value Labels: 1=Statement C: Doesn’t matter, 
#2=Statement B: Sometimes non-democratic preferable, 
#3=Statement A: Democracy preferable, 9=Don’t Know, 98=Refused to Answer, -1=Missing DataAfrodat4$Democ_pref<-repNA04(Afrodat4$Q30)
Afrodat3$Democ_pref<-repNA04(Afrodat3$q37)
#dummy
Afrodat3$dDemoc_pref<-ifelse((Afrodat3$Democ_pref==3),1,0)

#//Afrodat2----------
#Question Number: Q38
#Value Labels: 1=Statement C: Doesn’t matter, 
#2=Statement B: Sometimes non-democratic preferable, 
#3=Statement A: Democracy preferable, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Democ_pref<-repNA04(Afrodat2$q38)
#dummy
Afrodat2$dDemoc_pref<-ifelse((Afrodat2$Democ_pref==3),1,0)

#//Afrodat1----------
#Question Number: supdem
#Value Labels: 1=Democracy is preferable to any other form of government, 
#2=To people like me, it doesn't matter what form of government, 
#3=In certain situations, a non-democratic government can be preferable, 4=Don't know, 97=Not applicable, 98=Refused to answer, 99=Missing Data
Afrodat1$Democ_pref<-repNA04(Afrodat1$supdem)
#dummy
Afrodat1$dDemoc_pref<-ifelse((Afrodat1$Democ_pref==1),1,0) #反対


#Extent of democracy
#Afrodat7----------
#Variable Label: Q35
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Democ_nation<-ifelse(Afrodat7$Q35<1|Afrodat7$Q35>4,NA,Afrodat7$Q35)
table(Afrodat7$Democ_nation,exclude=NULL)        #

#Afrodat5----------
#Variable Label: Q42
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Democ_nation<-ifelse(Afrodat5$Q42<1|Afrodat5$Q42>4,NA,Afrodat5$Q42)
table(Afrodat5$Democ_nation,exclude=NULL)        #

#Afrodat4----------
#Variable Label: Q42A
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 998=Refused to answer,
Afrodat4$Democ_nation<-ifelse(Afrodat4$Q42A<1|Afrodat4$Q42A>4,NA,Afrodat4$Q42A)
table(Afrodat4$Democ_nation,exclude=NULL)        #

#Afrodat3----------
#Variable Label: Q46
Afrodat3$Democ_nation<-ifelse(Afrodat3$q46<1|Afrodat3$q46>4,NA,Afrodat3$q46)
table(Afrodat3$Democ_nation,exclude=NULL)        #
Afrodat2$Democ_nation<-NA
Afrodat1$Democ_nation<-NA



#Democracy Support  satisfaction
#Afrodat7----------
#Question Number:Q36
#Value Labels: Value Labels: 0=the country is not a democracy, 
#1=Not at all satisfied, 2=Not very satisfied, 3=Fairly satisfied, 4=Very satisfied, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Democ_satis<-repNA04(Afrodat7$Q36)

#Afrodat5----------
#Question Number:Q43
#Value Labels: Value Labels: 0=the country is not a democracy, 
#1=Not at all satisfied, 2=Not very satisfied, 3=Fairly satisfied, 4=Very satisfied, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Democ_satis<-repNA04(Afrodat5$Q43)
#Afrodat4----------
#Question Number:Q43
Afrodat4$Democ_satis<-repNA04(Afrodat4$Q43)
#Afrodat3----------
#Question Number:Q47
Afrodat3$Democ_satis<-repNA04(Afrodat3$q47)
#Afrodat2----------
#Question Number:Q40
Afrodat2$Democ_satis<-repNA04(Afrodat2$q40)
#Afrodat1----------
#//Question Number:dmpsat
#Very dissatisfied  Somewhat dissatisfied  Somewhat satisfied  Very satisfied  Uganda is not a democracy Not applicable
Afrodat1$Democ_satis<-ifelse(Afrodat1$dmpsat<1|Afrodat1$dmpsat>4,NA,Afrodat1$dmpsat)
Afrodat1$Democ_satis<-repNA04(Afrodat1$dmpsat)

#Trust: President R6,R5,R4,R3のみ
#Afrodat7---------- Trust_traditional_leadersなし
Afrodat7$Trust_president<-repNA04(Afrodat7$Q43A)
Afrodat7$Trust_parliament<-repNA04(Afrodat7$Q43B)
Afrodat7$Trust_police<-repNA04(Afrodat7$Q43G)
Afrodat7$Trust_traditional_leaders<-repNA04(Afrodat7$Q43J)
Afrodat7$Trust_religious_leaders<-repNA04(Afrodat7$Q43K)

#Afrodat5---------- Trust_traditional_leadersなし
Afrodat5$Trust_president<-repNA04(Afrodat5$Q59A)
Afrodat5$Trust_parliament<-repNA04(Afrodat5$Q59B)
Afrodat5$Trust_police<-repNA04(Afrodat5$Q59H)
Afrodat5$Trust_traditional_leaders<-NA
Afrodat5$Trust_religious_leaders<-repNA04(Afrodat5$Q59E)

#Afrodat4---------- 
Afrodat4$Trust_president<-repNA04(Afrodat4$Q49A)
Afrodat4$Trust_parliament<-repNA04(Afrodat4$Q49B)
Afrodat4$Trust_police<-repNA04(Afrodat4$Q49G)
Afrodat4$Trust_traditional_leaders<-repNA04(Afrodat4$Q49I)
Afrodat4$Trust_religious_leaders<-repNA04(Afrodat4$Q49D)

#Afrodat3----------
Afrodat3$Trust_president<-repNA04(Afrodat3$q55a)
Afrodat3$Trust_parliament<-repNA04(Afrodat3$q55b)
Afrodat3$Trust_police<-repNA04(Afrodat3$q55h)
Afrodat3$Trust_traditional_leaders<-NA
Afrodat3$Trust_religious_leaders<-repNA04(Afrodat3$q55d)

Afrodat2$Trust_president<-NA
Afrodat2$Trust_parliament<-NA
Afrodat2$Trust_police<-NA
Afrodat2$Trust_traditional_leaders<-NA
Afrodat2$Trust_religious_leaders<-NA

Afrodat1$Trust_president<-NA
Afrodat1$Trust_parliament<-NA
Afrodat1$Trust_police<-NA
Afrodat1$Trust_traditional_leaders<-NA
Afrodat1$Trust_religious_leaders<-NA


#腐敗、賄賂(は参考)　年によって尋ね方が異なる　R3、R2のみ
#//Afrodat3----------
#Question Number:Q65J
#Question: How well or badly would you say the current government is handling the following matters, or haven’t you heard enough about them to say: Fighting corruption in government?
#Variable Label: Handling fighting corruption
#Value Labels: 1=Very Badly, 2=Fairly Badly, 3=Fairly Well, 4=Very Well, 9=Don’t Know/Haven’t heard enough, 98=Refused to Answer, -1=Missing Data
Afrodat3$corruption<-repNA04(Afrodat3$q65j)

#//Afrodat2----------
#Question Number: Q51A
#Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
#about them to say: The President and Officials in his Office?
#Variable label: Corruption: Office of the Presidency
#Values: 0-3, 9, 98, -1
#Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
#-1=Missing Data
#Source: SAB
Afrodat2$corruption<-repNA04(Afrodat2$q51a)

#//Afrodat1----------
#Variable name: pfpcr2
#Variable label: Extent of corruption/bribery
#Values: 1-5, 9, 98, 99
#Value Labels: 1=Strongly disagree/Almost all, 2=Disagree/Most, 3=Agree/A few/Some, 4=Strongly
#agree/Almost none, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data 
Afrodat1$corruption<-repNA04(Afrodat1$pfpcr2)

#所有-------------R6,R5,R4のみ
#Afrodat7----------
#Question Number: Q90A
#Q89A:Radio
#Q89B:Television
#Q89C:Own motor vehicle, car, or motorcycle
Afrodat7$Own_Radio <-repNA01(Afrodat7$Q89A)
Afrodat7$Own_TV <-repNA01(Afrodat7$Q89B)
Afrodat7$Own_Auto <-repNA01(Afrodat7$Q89C)
Afrodat7$Own_Mbphone <-repNA01(Afrodat7$Q89F)

#Afrodat6
#Q91a. Own radio
#Q91b. Own television
#Q91c. Own motor vehicle, car, or motorcycle
#Q91d. Own mobile phone
#Afrodat5----------
#Question Number: Q90A
#Q90A:Radio
#Q90B:Television
#Q90C:Own motor vehicle, car, or motorcycle
Afrodat5$Own_Radio <-repNA01(Afrodat5$Q90A)
Afrodat5$Own_TV <-repNA01(Afrodat5$Q90B)
Afrodat5$Own_Auto <-repNA01(Afrodat5$Q90C)
Afrodat5$Own_Mbphone <-NA

#Afrodat4----------
#Question Number: Q92A
#Q90A:Radio
#Q90B:Television
#Q90C:Own motor vehicle, car, or motorcycle
Afrodat4$Own_Radio <-repNA01(Afrodat4$Q92A)
Afrodat4$Own_TV <-repNA01(Afrodat4$Q92B)
Afrodat4$Own_Auto <-NA
Afrodat4$Own_Mbphone <-NA

#Afrodat3----------
#Question Number: Q93A
#Q93B:Radio
#Q93C:Television
#Q93D:Bicycle
#Q93E:Motorcycle			Value Labels: 0=No (Don’t own), 1=Yes (Do Own),
#Q93F:Motor vehicle or car.
Afrodat3$Own_Radio <-repNA01(Afrodat3$q93b)
Afrodat3$Own_TV <-repNA01(Afrodat3$q93c)
Afrodat3$Own_Auto <-ifelse(Afrodat3$q93e==1|Afrodat3$q93f==1,1,ifelse(Afrodat3$q93e==0&Afrodat3$q93f==0,0,NA))
Afrodat3$Own_Mbphone <-NA


Afrodat2$Own_Radio <-NA
Afrodat2$Own_TV <-NA
Afrodat2$Own_Auto <-NA
Afrodat2$Own_Mbphone <-NA

Afrodat1$Own_Radio <-NA
Afrodat1$Own_TV <-NA
Afrodat1$Own_Auto <-NA
Afrodat1$Own_Mbphone <-NA


#使用-------------R6,R5,R4のみ
#Afrodat7---------- インターネットとコンピュータのみ
#Question Number: Q91B 
#Question: How often do you use: The Internet?
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Everyday, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat7$Use_Inet <-repNA04(Afrodat7$Q91B)
Afrodat7$Use_Mbphone <-repNA03(Afrodat7$Q91A)
Afrodat7$dUse_Mbphone <-ifelse((Afrodat7$Use_Mbphone==1)|(Afrodat7$Use_Mbphone==2),1,0)


#Afrodat5---------- インターネットとコンピュータのみ
#Question Number: Q91B 
#Question: How often do you use: The Internet?
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Everyday, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Use_Inet <-repNA04(Afrodat5$Q91B)

#Question Number: Q92 ##誰のmobileを使ったか
#Question: Do you ever use a mobile phone? If so, who owns the mobile phone that you use most often?
#Value Labels: 0= No, I never use a mobile phone, 1= Yes, I use a mobile phone that I own, 2= Yes, I use a mobile phone owned by someone else in my household, 3= Yes, I use a mobile phone owned by someone outside my household, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Use_Mbphone <-repNA03(Afrodat5$Q92)
Afrodat5$dUse_Mbphone <-ifelse((Afrodat5$Use_Mbphone==1)|(Afrodat5$Use_Mbphone==2),1,0)

#Afrodat4----------
#Q88A:use a cell phone
#Q88B:How often use a computer
#Q88C:How often do you use: The Internet?
Afrodat4$Use_Mbphone <-repNA04(Afrodat4$Q88A)
Afrodat4$Use_Inet <-repNA04(Afrodat4$Q88C)

Afrodat3$Use_Mbphone <-NA
Afrodat3$Use_Inet <-NA

Afrodat2$Use_Mbphone <-NA
Afrodat2$Use_Inet <-NA

Afrodat1$Use_Mbphone <-NA
Afrodat1$Use_Inet <-NA


#----雇用状況
#Afrodat7----------
Afrodat7$Employment_status<-ifelse(Afrodat7$Q94<0|Afrodat7$Q94>3,NA,Afrodat7$Q94)
table(Afrodat7$Employment_status,exclude=NULL)
Afrodat7$dEmployment_status_no<-ifelse(Afrodat7$Employment_status==0,1,0)
Afrodat7$dEmployment_status_looking<-ifelse(Afrodat7$Employment_status==1,1,0)
Afrodat7$dEmployment_status_part_time<-ifelse(Afrodat7$Employment_status==2,1,0)
Afrodat7$dEmployment_status_full_time<-ifelse(Afrodat7$Employment_status==3,1,0)

#Afrodat5----------
Afrodat5$Employment_status<-ifelse(Afrodat5$Q96<0|Afrodat5$Q96>3,NA,Afrodat5$Q96)
table(Afrodat5$Employment_status,exclude=NULL)
Afrodat5$dEmployment_status_no<-ifelse(Afrodat5$Employment_status==0,1,0)
Afrodat5$dEmployment_status_looking<-ifelse(Afrodat5$Employment_status==1,1,0)
Afrodat5$dEmployment_status_part_time<-ifelse(Afrodat5$Employment_status==2,1,0)
Afrodat5$dEmployment_status_full_time<-ifelse(Afrodat5$Employment_status==3,1,0)

#Afrodat4----------
Afrodat4$Employment_status<-ifelse(Afrodat4$Q94<0|Afrodat4$Q94>3,NA,Afrodat4$Q94)
table(Afrodat4$Employment_status,exclude=NULL)
Afrodat4$dEmployment_status_no<-ifelse(Afrodat4$Employment_status==0,1,0)
Afrodat4$dEmployment_status_looking<-ifelse(Afrodat4$Employment_status==1,1,0)
Afrodat4$dEmployment_status_part_time<-ifelse(Afrodat4$Employment_status==2,1,0)
Afrodat4$dEmployment_status_full_time<-ifelse(Afrodat4$Employment_status==3,1,0)

#Afrodat3----------
Afrodat3$Employment_status<-ifelse(Afrodat3$q94<0|Afrodat3$q94>3,NA,Afrodat3$q94)
table(Afrodat3$Employment_status,exclude=NULL)
Afrodat3$dEmployment_status_no<-ifelse(Afrodat3$Employment_status==0,1,0)
Afrodat3$dEmployment_status_looking<-ifelse(Afrodat3$Employment_status==1,1,0)
Afrodat3$dEmployment_status_part_time<-ifelse(Afrodat3$Employment_status==2,1,0)
Afrodat3$dEmployment_status_full_time<-ifelse(Afrodat3$Employment_status==3,1,0)

#Afrodat2----------
Afrodat2$Employment_status<-ifelse(Afrodat2$q89<0|Afrodat2$q89>3,NA,Afrodat2$q89)
table(Afrodat2$Employment_status,exclude=NULL)
Afrodat2$dEmployment_status_no<-ifelse(Afrodat2$Employment_status==0,1,0)
Afrodat2$dEmployment_status_looking<-ifelse(Afrodat2$Employment_status==1,1,0)
Afrodat2$dEmployment_status_part_time<-ifelse(Afrodat2$Employment_status==2,1,0)
Afrodat2$dEmployment_status_full_time<-ifelse(Afrodat2$Employment_status==3,1,0)

#//Afrodat1----------Uemployedかどうか
#Variable name: unemp
#Variable label: Uemployed
#Value Labels: 0=No, 1=Yes, 2=Don’t Know/Don’t Remember, 98=Refused to Answer, 99=Missing Data 
Afrodat1$Employment_status<-ifelse(Afrodat1$unemp<0|Afrodat1$unemp>2,NA,Afrodat1$unemp)
table(Afrodat1$Employment_status,exclude=NULL)
Afrodat1$dEmployment_status_yes<-ifelse(Afrodat1$Employment_status==0,1,0)	#これ特有
Afrodat1$dEmployment_status_no<-ifelse(Afrodat1$Employment_status==1,1,0)	#
Afrodat1$dEmployment_status_looking<-NA
Afrodat1$dEmployment_status_part_time<-NA
Afrodat1$dEmployment_status_full_time<-NA


#----職業
#Afrodat7----------  6,7は同じだが､それ以前はwaveによって異なる
#Q96_ARB
Afrodat7$Occupation<-ifelse(Afrodat7$Q95A<0|Afrodat7$Q95A>95,NA,Afrodat7$Q95A)
table(Afrodat7$Occupation,exclude=NULL)

Afrodat7$dOccupation_Never<-ifelse(Afrodat7$Occupation==0,1,0)
Afrodat7$dOccupation_Farmer<-ifelse((Afrodat7$Occupation==1)|(Afrodat7$Occupation==2)|(Afrodat7$Occupation==3),1,0)
Afrodat7$dOccupation_Farm_worker<-ifelse(Afrodat7$Occupation==4,1,0)
Afrodat7$dOccupation_Fisherman<-ifelse(Afrodat7$Occupation==5,1,0)
Afrodat7$dOccupation_Trader<-ifelse(Afrodat7$Occupation==6,1,0)
Afrodat7$dOccupation_Miner<-ifelse(Afrodat7$Occupation==7,1,0)
Afrodat7$dOccupation_Domestic<-ifelse(Afrodat7$Occupation==8,1,0)
Afrodat7$dOccupation_Armed_Services<-ifelse(Afrodat7$Occupation==9,1,0)
Afrodat7$dOccupation_Skilled<-ifelse((Afrodat7$Occupation==10)|(Afrodat7$Occupation==11),1,0)
Afrodat7$dOccupation_Clerical<-ifelse(Afrodat7$Occupation==12,1,0)
Afrodat7$dOccupation_Unskilled<-ifelse((Afrodat7$Occupation==13)|(Afrodat7$Occupation==14),1,0)
Afrodat7$dOccupation_Businessperson<-ifelse((Afrodat7$Occupation==15)|(Afrodat7$Occupation==16)|(Afrodat7$Occupation==17),1,0)
Afrodat7$dOccupation_Professional<-ifelse(Afrodat7$Occupation==18,1,0)
Afrodat7$dOccupation_Supervisor<-ifelse(Afrodat7$Occupation==19,1,0)
Afrodat7$dOccupation_Teacher<-ifelse(Afrodat7$Occupation==20,1,0)
Afrodat7$dOccupation_Government<-ifelse(Afrodat7$Occupation==21,1,0)
Afrodat7$dOccupation_Retail<-ifelse(Afrodat7$Occupation==22,1,0)
Afrodat7$dOccupation_Student<-ifelse(Afrodat7$Occupation==23,1,0)
Afrodat7$dOccupation_Housewife<-ifelse(Afrodat7$Occupation==24,1,0)
Afrodat7$dOccupation_Other<-ifelse(Afrodat7$Occupation==995,1,0)

#Afrodat5----------
#Q96_ARB
Afrodat5$Occupation<-ifelse(Afrodat5$Q96_ARB<0|Afrodat5$Q96_ARB>95,NA,Afrodat5$Q96_ARB)
table(Afrodat5$Occupation,exclude=NULL)

Afrodat5$dOccupation_Never<-ifelse(Afrodat5$Occupation==0,1,0)
Afrodat5$dOccupation_Farmer<-ifelse((Afrodat5$Occupation==1)|(Afrodat5$Occupation==2)|(Afrodat5$Occupation==3),1,0)
Afrodat5$dOccupation_Farm_worker<-ifelse(Afrodat5$Occupation==4,1,0)
Afrodat5$dOccupation_Fisherman<-ifelse(Afrodat5$Occupation==5,1,0)
Afrodat5$dOccupation_Trader<-ifelse(Afrodat5$Occupation==6,1,0)
Afrodat5$dOccupation_Miner<-ifelse(Afrodat5$Occupation==7,1,0)
Afrodat5$dOccupation_Domestic<-ifelse(Afrodat5$Occupation==8,1,0)
Afrodat5$dOccupation_Armed_Services<-ifelse(Afrodat5$Occupation==9,1,0)
Afrodat5$dOccupation_Skilled<-ifelse((Afrodat5$Occupation==10)|(Afrodat5$Occupation==11),1,0)
Afrodat5$dOccupation_Clerical<-ifelse(Afrodat5$Occupation==12,1,0)
Afrodat5$dOccupation_Unskilled<-ifelse((Afrodat5$Occupation==13)|(Afrodat5$Occupation==14),1,0)
Afrodat5$dOccupation_Businessperson<-ifelse((Afrodat5$Occupation==15)|(Afrodat5$Occupation==16)|(Afrodat5$Occupation==17),1,0)
Afrodat5$dOccupation_Professional<-ifelse(Afrodat5$Occupation==18,1,0)
Afrodat5$dOccupation_Supervisor<-ifelse(Afrodat5$Occupation==19,1,0)
Afrodat5$dOccupation_Teacher<-ifelse(Afrodat5$Occupation==20,1,0)
Afrodat5$dOccupation_Government<-ifelse(Afrodat5$Occupation==21,1,0)
Afrodat5$dOccupation_Retail<-ifelse(Afrodat5$Occupation==22,1,0)
Afrodat5$dOccupation_Student<-ifelse(Afrodat5$Occupation==23,1,0)
Afrodat5$dOccupation_Housewife<-ifelse(Afrodat5$Occupation==24,1,0)
Afrodat5$dOccupation_Other<-ifelse(Afrodat5$Occupation==995,1,0)

#Afrodat4----------なし？
Afrodat4$Occupation<-NA

#Afrodat3----------
#Q96_ARB
Afrodat3$Occupation<-ifelse(Afrodat3$q95<0|Afrodat3$q95>95,NA,Afrodat3$q95)
table(Afrodat3$Occupation,exclude=NULL)

Afrodat3$dOccupation_Never<-ifelse(Afrodat3$Occupation==0,1,0)
Afrodat3$dOccupation_Farmer<-ifelse((Afrodat3$Occupation==1)|(Afrodat3$Occupation==2)|(Afrodat3$Occupation==3),1,0)
Afrodat3$dOccupation_Farm_worker<-ifelse(Afrodat3$Occupation==4,1,0)
Afrodat3$dOccupation_Fisherman<-ifelse(Afrodat3$Occupation==5,1,0)
Afrodat3$dOccupation_Trader<-ifelse(Afrodat3$Occupation==6,1,0)
Afrodat3$dOccupation_Miner<-ifelse(Afrodat3$Occupation==7,1,0)
Afrodat3$dOccupation_Domestic<-ifelse(Afrodat3$Occupation==8,1,0)
Afrodat3$dOccupation_Armed_Services<-ifelse(Afrodat3$Occupation==9,1,0)
Afrodat3$dOccupation_Skilled<-ifelse((Afrodat3$Occupation==10)|(Afrodat3$Occupation==11),1,0)
Afrodat3$dOccupation_Clerical<-ifelse(Afrodat3$Occupation==12,1,0)
Afrodat3$dOccupation_Unskilled<-ifelse((Afrodat3$Occupation==13)|(Afrodat3$Occupation==14),1,0)
Afrodat3$dOccupation_Businessperson<-ifelse((Afrodat3$Occupation==15)|(Afrodat3$Occupation==16)|(Afrodat3$Occupation==17),1,0)
Afrodat3$dOccupation_Professional<-ifelse(Afrodat3$Occupation==18,1,0)
Afrodat3$dOccupation_Supervisor<-ifelse(Afrodat3$Occupation==19,1,0)
Afrodat3$dOccupation_Teacher<-ifelse(Afrodat3$Occupation==20,1,0)
Afrodat3$dOccupation_Government<-ifelse(Afrodat3$Occupation==21,1,0)
Afrodat3$dOccupation_Retail<-ifelse(Afrodat3$Occupation==22,1,0)
Afrodat3$dOccupation_Student<-ifelse(Afrodat3$Occupation==23,1,0)
Afrodat3$dOccupation_Housewife<-ifelse(Afrodat3$Occupation==24,1,0)
Afrodat3$dOccupation_Other<-ifelse(Afrodat3$Occupation==995,1,0)


#Afrodat2----------なし
Afrodat2$Occupation<-ifelse(Afrodat2$q88<0|Afrodat2$q88>95,NA,Afrodat2$q88)
table(Afrodat2$Occupation,exclude=NULL)

Afrodat2$dOccupation_Never<-ifelse(Afrodat2$Occupation==0,1,0)
Afrodat2$dOccupation_Farmer<-ifelse((Afrodat2$Occupation==1)|(Afrodat2$Occupation==2)|(Afrodat2$Occupation==3),1,0)
Afrodat2$dOccupation_Farm_worker<-ifelse(Afrodat2$Occupation==4,1,0)
Afrodat2$dOccupation_Fisherman<-ifelse(Afrodat2$Occupation==5,1,0)
Afrodat2$dOccupation_Trader<-ifelse(Afrodat2$Occupation==6,1,0)
Afrodat2$dOccupation_Businessperson<-ifelse(Afrodat2$Occupation==7,1,0)
Afrodat2$dOccupation_Professional<-ifelse(Afrodat2$Occupation==8,1,0)
Afrodat2$dOccupation_Supervisor<-ifelse(Afrodat2$Occupation==9,1,0)
Afrodat2$dOccupation_Clerical<-ifelse(Afrodat2$Occupation==10,1,0)
Afrodat2$dOccupation_Miner<-ifelse(Afrodat2$Occupation==12,1,0)
Afrodat2$dOccupation_Domestic<-ifelse(Afrodat2$Occupation==14,1,0)
Afrodat2$dOccupation_Teacher<-ifelse(Afrodat2$Occupation==15,1,0)
Afrodat2$dOccupation_Government<-ifelse(Afrodat2$Occupation==16,1,0)
Afrodat2$dOccupation_Armed<-ifelse(Afrodat2$Occupation==17,1,0)
Afrodat2$dOccupation_Student<-ifelse(Afrodat2$Occupation==18,1,0)
Afrodat2$dOccupation_Housewife<-ifelse(Afrodat2$Occupation==19,1,0)
Afrodat2$dOccupation_Retail<-ifelse(Afrodat2$Occupation==22,1,0)
Afrodat2$dOccupation_Student<-ifelse(Afrodat2$Occupation==23,1,0)
Afrodat2$dOccupation_Artisan<-ifelse(Afrodat2$Occupation==24,1,0)
Afrodat2$dOccupation_Unskilled<-ifelse((Afrodat2$Occupation==25)|(Afrodat2$Occupation==26),1,0)
Afrodat2$dOccupation_Other<-ifelse(Afrodat2$Occupation==995,1,0)

#Afrodat1----------
#Identity
Afrodat1$Occupation<-ifelse(Afrodat1$occup<0|Afrodat1$occup>95,NA,Afrodat1$occup)
table(Afrodat1$Occupation,exclude=NULL)

Afrodat1$dOccupation_Never<-ifelse(Afrodat1$Occupation==0,1,0)
Afrodat1$dOccupation_Farmer<-ifelse(Afrodat1$Occupation==1,1,0)
Afrodat1$dOccupation_Informal_marketer<-ifelse(Afrodat1$Occupation==2,1,0)
Afrodat1$dOccupation_Businessperson<-ifelse(Afrodat1$Occupation==3,1,0)
Afrodat1$dOccupation_Clerical<-ifelse(Afrodat1$Occupation==4,1,0)
Afrodat1$dOccupation_Artisan<-ifelse(Afrodat1$Occupation==5,1,0)
Afrodat1$dOccupation_Domestic<-ifelse(Afrodat1$Occupation==6,1,0)
Afrodat1$dOccupation_Miner<-ifelse(Afrodat1$Occupation==7,1,0)
Afrodat1$dOccupation_Technical<-ifelse(Afrodat1$Occupation==8,1,0)
Afrodat1$dOccupation_Teacher<-ifelse(Afrodat1$Occupation==9,1,0)
Afrodat1$dOccupation_Government<-ifelse(Afrodat1$Occupation==10,1,0)
Afrodat1$dOccupation_NGO<-ifelse(Afrodat1$Occupation==11,1,0)
Afrodat1$dOccupation_Professional<-ifelse(Afrodat1$Occupation==12,1,0)
Afrodat1$dOccupation_Retired<-ifelse(Afrodat1$Occupation==13,1,0)
Afrodat1$dOccupation_Housewife<-ifelse(Afrodat1$Occupation==14,1,0)
Afrodat1$dOccupation_Student<-ifelse(Afrodat1$Occupation==15,1,0)
Afrodat1$dOccupation_Other<-ifelse((Afrodat1$Occupation>=16||Afrodat1$Occupation<=95),1,0)

#----学歴
#Afrodat7----------
#Question Number: Q97
#Value Labels: 0=No formal schooling, 1=Informal schooling only (including Koranic schooling), 2=Some primary schooling, 3=Primary school completed, 4=Intermediate school or Some secondary school / high school, 5=Secondary school / high school completed , 6=Post-secondary qualifications, other than university e.g. a diploma or degree from a polytechnic or college, 7=Some university, 8=University completed, 9=Post-graduate, 99=Don’t know [Do not read], 98=Refused to answer, -1=Missing
Afrodat7$Education<-ifelse(Afrodat7$Q97<0|Afrodat7$Q97>9,NA,Afrodat7$Q97)
table(Afrodat7$Education,exclude=NULL)

#Afrodat5----------
#Question Number: Q97
#Value Labels: 3=Primary school completed, 4=Some secondary/high school, 5=High school completed,6=Post secondary qualifications other than university e.g. a diploma or degree from a polytechnic orcollege, 7=Some university, 8=University, completed, 9=Post graduate, -1=Missing
Afrodat5$Education<-ifelse(Afrodat5$Q97<0|Afrodat5$Q97>9,NA,Afrodat5$Q97)
table(Afrodat5$Education,exclude=NULL)

#Afrodat4----------
#Question Number: Q89
Afrodat4$Education<-ifelse(Afrodat4$Q89<0|Afrodat4$Q89>9,NA,Afrodat4$Q89)
table(Afrodat4$Education,exclude=NULL)

#Afrodat3----------
#Question Number: Q90
Afrodat3$Education<-ifelse(Afrodat3$q90<0|Afrodat3$q90>9,NA,Afrodat3$q90)
table(Afrodat3$Education,exclude=NULL)

#Afrodat2----------
#Question Number: Q84
Afrodat2$Education<-ifelse(Afrodat2$q84<0|Afrodat2$q84>9,NA,Afrodat2$q84)
table(Afrodat3$Education,exclude=NULL)

#//Afrodat1----------  他とスケールが異なる
#Question Number: educ
#Value Labels: 0=No formal schooling, 1=Primary only, 2=Secondary, 3=Post-secondary, 10=Don’t Know, 98=Refused to Answer, 99=Missing Data
table(Afrodat1$educ,exclude=NULL)
#   0    1    2    3   10   98   99 
#4345 6944 7627 2527   22    4   62 
#Afrodat1$Education<-ifelse(Afrodat1$educ<0|Afrodat1$educ>3,NA,Afrodat1$educ)	#
Afrodat1$Education<-ifelse(Afrodat1$educ==0,0,
						ifelse(Afrodat1$educ==1,3,
						ifelse(Afrodat1$educ==2,5,
						ifelse(Afrodat1$educ==3,6,NA))))
table(Afrodat1$Education,exclude=NULL)
#   0    3    5    6 <NA> 	多分　3=Post-secondaryに大卒なども入っている
#4345 6944 7627 2527   88 



#----Gender
Afrodat7$Gender_f<-ifelse(Afrodat7$Q101<0|Afrodat7$Q101>2,NA,Afrodat7$Q101)
table(Afrodat7$Gender_f,exclude=NULL)

Afrodat5$Gender_f<-ifelse(Afrodat5$Q101<0|Afrodat5$Q101>2,NA,Afrodat5$Q101)
table(Afrodat5$Gender_f,exclude=NULL)
Afrodat4$Gender_f<-ifelse(Afrodat4$Q101<0|Afrodat4$Q101>2,NA,Afrodat4$Q101)
table(Afrodat4$Gender_f,exclude=NULL)
Afrodat3$Gender_f<-ifelse(Afrodat3$currint<1|Afrodat3$currint>2,NA,Afrodat3$currint)
table(Afrodat3$Gender_f,exclude=NULL)
Afrodat2$Gender_f<-ifelse(Afrodat2$currint<1|Afrodat2$currint>2,NA,Afrodat2$currint)
table(Afrodat2$Gender_f,exclude=NULL)
Afrodat1$Gender_f<-ifelse(Afrodat1$gender<1|Afrodat1$gender>2,NA,Afrodat1$gender)
table(Afrodat1$Gender_f,exclude=NULL)

#-------人種
#Value Labels: 1=Black/African, 2=White/European, 3=Colored/Mixed Race, 4=Arab/Lebanese/North African, 5=South Asian (Indian, Pakistani, etc.), 6=Eat Asian (Chinese, Korean, Indonesian, etc.), Other=95, -1=Missing
#Afrodat5----------
Afrodat7$Race<-ifelse(Afrodat7$Q102<0|Afrodat7$Q102>95,NA,Afrodat7$Q102)
table(Afrodat7$Race,exclude=NULL)
#人種ダミー
Afrodat7$dRace_BAf<-ifelse(Afrodat7$Race==1,1,0)
Afrodat7$dRace_Wh<-ifelse(Afrodat7$Race==2,1,0)
Afrodat7$dRace_Col<-ifelse(Afrodat7$Race==3,1,0)
Afrodat7$dRace_Arab<-ifelse(Afrodat7$Race==4,1,0)
Afrodat7$dRace_SAs<-ifelse(Afrodat7$Race==5,1,0)
Afrodat7$dRace_EAs<-ifelse(Afrodat7$Race==6,1,0)
Afrodat7$dRace_Oth<-ifelse(Afrodat7$Race==95,1,0)



#Afrodat6$Race<-ifelse(Afrodat6$Q102<0|Afrodat6$Q102>95,NA,Afrodat6$Q102)
#table(Afrodat6$Race,exclude=NULL)
#
##人種ダミー
#Afrodat6$dRace_BAf<-ifelse(Afrodat6$Race==1,1,0)
#Afrodat6$dRace_Wh<-ifelse(Afrodat6$Race==2,1,0)
#Afrodat6$dRace_Col<-ifelse(Afrodat6$Race==3,1,0)
#Afrodat6$dRace_Arab<-ifelse(Afrodat6$Race==4,1,0)
#Afrodat6$dRace_SAs<-ifelse(Afrodat6$Race==5,1,0)
#Afrodat6$dRace_EAs<-ifelse(Afrodat6$Race==6,1,0)
#Afrodat6$dRace_Oth<-ifelse(Afrodat6$Race==95,1,0)

#Afrodat5----------
Afrodat5$Race<-ifelse(Afrodat5$Q102<0|Afrodat5$Q102>95,NA,Afrodat5$Q102)
table(Afrodat5$Race,exclude=NULL)
#人種ダミー
Afrodat5$dRace_BAf<-ifelse(Afrodat5$Race==1,1,0)
Afrodat5$dRace_Wh<-ifelse(Afrodat5$Race==2,1,0)
Afrodat5$dRace_Col<-ifelse(Afrodat5$Race==3,1,0)
Afrodat5$dRace_Arab<-ifelse(Afrodat5$Race==4,1,0)
Afrodat5$dRace_SAs<-ifelse(Afrodat5$Race==5,1,0)
Afrodat5$dRace_EAs<-ifelse(Afrodat5$Race==6,1,0)
Afrodat5$dRace_Oth<-ifelse(Afrodat5$Race==95,1,0)

#Afrodat4----------
Afrodat4$Race<-ifelse(Afrodat4$Q102<0|Afrodat4$Q102>95,NA,Afrodat4$Q102)
table(Afrodat4$Race,exclude=NULL)
#人種ダミー
Afrodat4$dRace_BAf<-ifelse(Afrodat4$Race==1,1,0)
Afrodat4$dRace_Wh<-ifelse(Afrodat4$Race==2,1,0)
Afrodat4$dRace_Col<-ifelse(Afrodat4$Race==3,1,0)
Afrodat4$dRace_Arab<-ifelse(Afrodat4$Race==4,1,0)
Afrodat4$dRace_SAs<-ifelse(Afrodat4$Race==5,1,0)
Afrodat4$dRace_EAs<-ifelse(Afrodat4$Race==6,1,0)
Afrodat4$dRace_Oth<-ifelse(Afrodat4$Race==95,1,0)

#Afrodat3----------
Afrodat3$Race<-ifelse(Afrodat3$q102<0|Afrodat3$q102>95,NA,Afrodat3$q102)
table(Afrodat3$Race,exclude=NULL)
#人種ダミー
Afrodat3$dRace_BAf<-ifelse(Afrodat3$Race==1,1,0)
Afrodat3$dRace_Wh<-ifelse(Afrodat3$Race==2,1,0)
Afrodat3$dRace_Col<-ifelse(Afrodat3$Race==3,1,0)
Afrodat3$dRace_Arab<-ifelse(Afrodat3$Race==4,1,0)
Afrodat3$dRace_SAs<-ifelse(Afrodat3$Race==5,1,0)
Afrodat3$dRace_EAs<-ifelse(Afrodat3$Race==6,1,0)
Afrodat3$dRace_Oth<-ifelse(Afrodat3$Race==95,1,0)

#Afrodat2----------
Afrodat2$Race<-ifelse(Afrodat2$q96new<0|Afrodat2$q96new>95,NA,Afrodat2$q96new)
table(Afrodat2$Race,exclude=NULL)
#人種ダミー
Afrodat2$dRace_BAf<-ifelse(Afrodat2$Race==1,1,0)
Afrodat2$dRace_Wh<-ifelse(Afrodat2$Race==2,1,0)
Afrodat2$dRace_Col<-ifelse(Afrodat2$Race==3,1,0)
Afrodat2$dRace_Arab<-ifelse(Afrodat2$Race==4,1,0)
Afrodat2$dRace_SAs<-ifelse(Afrodat2$Race==5,1,0)
Afrodat2$dRace_EAs<-ifelse(Afrodat2$Race==6,1,0)
Afrodat2$dRace_Oth<-ifelse(Afrodat2$Race==95,1,0)

#Afrodat1----------
Afrodat1$Race<-ifelse(Afrodat1$ race<0|Afrodat1$race>95,NA,Afrodat1$race)
table(Afrodat1$Race,exclude=NULL)
#人種ダミー
Afrodat1$dRace_BAf<-ifelse(Afrodat1$Race==1,1,0)
Afrodat1$dRace_Wh<-ifelse(Afrodat1$Race==2,1,0)
Afrodat1$dRace_Col<-ifelse(Afrodat1$Race==3,1,0)
Afrodat1$dRace_Arab<-ifelse(Afrodat1$Race==4,1,0)
Afrodat1$dRace_SAs<-ifelse(Afrodat1$Race==5,1,0)
Afrodat1$dRace_EAs<-ifelse(Afrodat1$Race==6,1,0)
Afrodat1$dRace_Oth<-ifelse(Afrodat1$Race==95,1,0)

 
#参考　R5インターネットだけ別質問
#Question Number: Q88A
#Question: How often do you use: A mobile phone?
#Question: How often do you use: A computer?
#Question: How often do you use: The Internet?
#Variable Label: How often use the internet
#Values: 0-4, 9, 998, -1
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 998=Refused to answer, -1=Missing data
#Source: Afrobarometer Round 4
Afrodat4$Useage_Mobile<-repNA04(Afrodat4$Q88A)
Afrodat4$Useage_Computer<-repNA04(Afrodat4$Q88B)
Afrodat4$Useage_Internet<-repNA04(Afrodat4$Q88C)





#------全ラウンド必要部分をまとめる　　とりあえずニュースの利用
v<-c("wave","year","COUNTRY2","dUrban","Age","Gender_f",
"Language","dlang_English","dlang_French","dlang_Portuguese","dlang_Other","dlang_Crioulo","dlang_Akan","dlang_Shona",
#"dlang_Swahili","dlang_Ibo","dlang_Luganda","dlang_Oshiwambo","dlang_Sesotho","dlang_Afrikaans","dlang_Zulu","dlang_Kiswahili","dlang_Hausa","dlang_Bambara","dlang_Yoruba",
"Race","dRace_BAf","dRace_Wh","dRace_Col","dRace_Arab","dRace_SAs","dRace_EAs","dRace_Oth",
"Cond_econ","Cond_your_liv","Relative_live",
"gone_food","gone_water","gone_med","gone_fuel","gone_cash","gone_electricity",		#gone_electricityは1　2のみ
"Interest_pubaff",
"Discuss_politics",	"dDiscuss_politics",
"Mem_religious",					#round 1は3:リーダーがないので注意
"Mem_voluntary",				#round 1,2は　なし
"Cit_action_Attend_meeting","Cit_action_raise_issue",

"Diss_request_government",		#要チェック　　w1なし
"Diss_Contact_official",	#要チェック　　w1なし
"Diss_Attend_demonstration",	#要チェック　　w1選択肢が異なる
"Democ_pref","dDemoc_pref",
"Democ_nation",				#w1､2なし
"Democ_satis",				
"Trust_president","Trust_parliament","Trust_police","Trust_traditional_leaders","Trust_religious_leaders",	#w1,2なし　Trust_traditional_leadersはw3 5なし
"Own_Radio","Own_TV","Own_Auto","Own_Mbphone",		#w1､2なし　Own_Auto　w4なし　　Own_Mbphoneはw6から　
"Use_Inet","Use_Mbphone",			#w4から
"Employment_status","dEmployment_status_no","dEmployment_status_looking","dEmployment_status_part_time","dEmployment_status_full_time",	#w1は異なる
"Occupation",		#W5までは異なる選択肢　　w4なし?
"Education",		#w1異なる
"News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")

AfrodatAll<-rbind(Afrodat1[,v],Afrodat2[,v],Afrodat3[,v],Afrodat4[,v],Afrodat5[,v],Afrodat6[,v],Afrodat7[,v])
	dim(AfrodatAll)	#[1] ] 250287     68
	save(AfrodatAll,file="0AfrodatAll.rda")

(tb<-table(AfrodatAll$COUNTRY2,AfrodatAll$wave,exclude=NULL))
	dim(tb)
#AfrodatAllg<-group_by(AfrodatAll,c("year","COUNTRY2"))
AfrodatAllg<-group_by(AfrodatAll, AfrodatAll$COUNTRY2,AfrodatAll$year)	#国､年別に集計することを指定

(m<-summarise(AfrodatAllg,mNews_Radio<-mean2(News_Radio),mNews_Television<-mean2(News_Television),mNews_Newspaper<-mean2(News_Newspaper),mNews_Internet<-mean2(News_Internet),mNews_Social_media<-mean2(News_Social_media)))

edit(m)
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



save.image("0Afrodat.img")
