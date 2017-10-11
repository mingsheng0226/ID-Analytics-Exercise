setwd('/Users/riyueyoutu/Desktop/idanalytics/')
library(jsonlite)
library(data.table)
lines = readLines('ida_wrangling_exercise_data.2017-02-13.jsonl')
lst = lapply(lines,fromJSON)
tdf = function(x) as.data.table(t(unlist(x)))
#df = do.call(rbind.fill,lapply(lst,tdf))
dt=rbindlist(lapply(lst,tdf),fill=T)

#1
sort(colnames(dt))

#2(1)
perc = function(col) sum(!is.na(col))/dim(dt)[1]
apply(dt,2,perc)

#2(2)
top5 = function(col) {
  return (dt[!is.na(get(col)),.(count=.N),by=col][order(count,decreasing = T)[1:5],])
}
top5dt = data.table(rank=1:5)
for (col in colnames(dt)) {
  top5dt = cbind(top5dt, top5(col))
}

#3
dt1 = dt
dt1$nn = dt1$name
length(unique(dt1$name.firstname))
sum(is.na(dt1$name.firstname))
dt2 = dt
dt2$nnn = dt2$name
length(unique(dt2[!is.na(name.firstname),name.firstname]))
dt2[is.na(name.firstname),name.firstname:=gsub(fname,"\\2\\3",name,ignore.case = T)]

sum(is.na(dt2$name.firstname))

length(unique(dt[!is.na(name.firstname),name.firstname]))
dt[is.na(name.firstname),name.firstname:=gsub(fname,"\\2\\3",name,ignore.case = T)]
fname = "^(Dr\\.|Mr\\.|Mrs\\.|Ms\\.)\\s+(.*?)\\s+.*|^(.*?)\\s+.*"
dt$name.firstname
#4
dt[is.na(address.street), address.street:=gsub("(.*)\\n.*","\\1",address,ignore.case = T)]
length(unique(dt[!is.na(address.street),address.street]))

#5
AreaCode = ".*\\((\\d{3})\\).*|.*(\\d{3})\\d{7}\\b.*|.*(\\d{3})[-.]\\d{3}[-.]\\d{4}.*"
dt$us.area.code = sub(AreaCode,"\\1\\2\\3",dt$phone)
top5("us.area.code")

dt$address[1:50]
