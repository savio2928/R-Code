library(XML)

ubase = "http://www.cherryblossom.org/"
womenURLs = 
  c("results/1999/cb99f.html","results/2000/Cb003f.htm",
    "results/2001/oof_f.html","results/2002/ooff.htm",
    "results/2003/CB03-F.HTM","results/2004/women.htm",
    "results/2005/CB05-F.htm","results/2006/women.htm",
    "results/2007/women.htm","results/2008/women.htm",
    "results/2009/09cucb-F.htm","results/2010/2010cucb10m-f.htm",
    "results/2011/2011cucb10m-f.htm","results/2012/2012cucb10m-f.htm"
  )

urls = paste(ubase, womenURLs, sep = "")

urls[1:3]

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url, year, file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 1999) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]] 
    } 
    
    else if (year == 2009 & url == "http://www.cherryblossom.org/results/2009/09cucb-M.htm") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

years = 1999:2012
womenTables = mapply(extractResTable, url = urls, year = years)
names(womenTables) = years
sapply(womenTables, length)
womenTables[[3]][2]="PLACE NUM   NAME                  AG HOMETOWN           NET     GUN"
womenTables[[3]][3]="===== ===== ===================== == ================== ======= ======="
womenTables[[3]][1:4]
?mapply
length(womenTables)

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}

extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
      eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

womenResMat = lapply(womenTables, extractVariables)
age = sapply(womenResMat,
             function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")

convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

separatorIdx = grep("^===", womenTables[["2006"]])
separatorRow = womenTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
womenTables[['2006']][separatorIdx] = separatorRowX

womenResMat = sapply(womenTables, extractVariables)
womenDF = mapply(createDF, womenResMat, year = 1999:2012,
               sex = rep("F", 14), SIMPLIFY = FALSE)

sapply(womenDF, function(x) sum(is.na(x$runTime)))

cbWomen = do.call(rbind, womenDF)

dim(cbWomen)

library(RColorBrewer)
ls("package:RColorBrewer")

#Box Plot of Age by Year for Female Runners
boxplot(sapply(womenDF, function(x) x$age), 
        xlab = "Year", ylab = "Age")

#Scatter Plot for Run Times vs. Age for Female Runners
Purples8 = brewer.pal(9, "Purples")[8]
Purples8A = paste(Purples8, "14", sep = "")
plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbWomen, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

#Fit Models to Average Performance
smoothScatter(y = cbWomen$runTime, x = cbWomen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")

#Side-by-Side Boxplots of Female Runners' Run Time vs. Age
ageCat = cut(cbWomen$age, breaks = c(seq(15, 75, 10), 100))
table(ageCat)
plot(cbWomen$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

#Residual Plot from Fitting a Simple Linear Model of Performance to Age
cbWomenSub = cbWomen[cbWomen$runTime > 30 &
                   !is.na(cbWomen$age) & cbWomen$age > 15, ]

lmAge = lm(runTime ~ age, data = cbWomenSub)

lmAge$coefficients

summary(lmAge)

class(lmAge)

smoothScatter(x = cbWomenSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

# Piecewise Linear and Loess Curves Fitted to Run Time vs. Age
resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge),
                                   age = cbWomenSub$age))

age20to80 = 20:80
age20to80

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)

womenRes.lo = loess(runTime ~ age, cbWomenSub)

womenRes.lo.pr = predict(womenRes.lo, data.frame(age = age20to80))

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbWomenSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)
tail(overAge)

lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbWomenSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

tail(overAgeDF)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = womenRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

# Line Plot of the Number of Female Runners by Year
numRunners = with(cbWomen, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")

#Density Curves for the Age of Female Runners for 2 years (smallest and largest year that you analyzed)
age1999 = cbWomenSub[ cbWomenSub$year == 1999, "age" ]
age2012 = cbWomenSub[ cbWomenSub$year == 2012, "age" ]

plot(density(age1999, na.rm = TRUE), 
     ylim = c(0, 0.06), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")

#Loess Curves Fit to Performance for 2 years (smallest and largest year that you analyzed) Female Runners
mR.lo99 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 1999,])
mR.lo.pr99 = predict(mR.lo99, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

plot(mR.lo.pr99 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")

lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")

#Difference between Loess Curves of the predicted run time for 2 years (smallest and largest year that you analyzed)
gap12 = mR.lo.pr12 - mR.lo.pr99

plot(gap12 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)

#extracting Mens Data
menURLs = 
  c("results/1999/cb99m.html","results/2000/Cb003m.htm",
    "results/2001/oof_m.html","results/2002/oofm.htm", 
    "results/2003/CB03-M.HTM","results/2004/men.htm", 
    "results/2005/CB05-M.htm", "results/2006/men.htm", 
    "results/2007/men.htm", "results/2008/men.htm",
    "results/2009/09cucb-M.htm","results/2010/2010cucb10m-m.htm",
     "results/2011/2011cucb10m-m.htm","results/2012/2012cucb10m-m.htm")
  
urls1 = paste(ubase, menURLs, sep = "")
urls1[1:3]

menTables = mapply(extractResTable, url = urls1, year = years)
names(menTables) = years
sapply(menTables, length)

menTables[[11]]=gsub("  "," ",gsub("[^\\x{00}-\\x{7f}]"," ",menTables[[11]],perl=TRUE))

menResMat = lapply(menTables, extractVariables)
age = sapply(menResMat,
             function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")


separatorIdx = grep("^===", menTables[["2006"]])
separatorRow = menTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
menTables[['2006']][separatorIdx] = separatorRowX
menResMat = sapply(menTables, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012,
                 sex = rep("M", 14), SIMPLIFY = FALSE)

sapply(menDF, function(x) sum(is.na(x$runTime)))
cbMen = do.call(rbind, menDF)

#combining men and women tabes
People=rbind(cbMen,cbWomen)

library("ggplot2")
ggplot(People,aes(x=age,y=runTime,color=sex))+
  facet_grid(sex~year)+geom_point(size=0.1)+ylim(40,180)
