# “Is there an association between college major category and income?”
# Rephrasing the question; given a student's major category, is it possible to predict with a
# degree of confidence what income they earn? If such a relationship
# exists, then one can use it to answer the converse question that, given an income,
# then one may predict the student's  major category.
#
# Libraries used in analysis

library(tidyverse)
library(broom)
library(ggfortify)

# Modified from Malarkey@ https://www.r-bloggers.com/ggheat-a-ggplot2-style-heatmap-function/
ggheat=function(m, rescaling='none', clustering='none', labCol=T, labRow=T, border=FALSE,
                brewerPalette="Spectral",revPalette=T) {
  require(reshape)
  require(ggplot2)
  require(RColorBrewer)
  if(is.function(rescaling)) {
    m=rescaling(m)
  } else {
    if(rescaling=='column')
      m=scale(m, center=T)
    if(rescaling=='row')
      m=t(scale(t(m),center=T)) }
  if(is.function(clustering)) {
    m=clustering(m)
  } else {
    if(clustering=='row')
      m=m[hclust(dist(m))$order, ]
    if(clustering=='column')
      m=m[,hclust(dist(t(m)))$order]
    if(clustering=='both')
      m=m[hclust(dist(m))$order ,hclust(dist(t(m)))$order] }
  rows=dim(m)[1]
  cols=dim(m)[2]
  melt.m=cbind(rowInd=rep(1:rows, times=cols), colInd=rep(1:cols, each=rows) ,melt(m))
  g=ggplot(data=melt.m)
  if(border==TRUE)
    g2=g+geom_rect(aes(xmin=colInd-1,xmax=colInd,ymin=rowInd-1,ymax=rowInd, fill=value),colour='white')
  if(border==FALSE)
    g2=g+geom_rect(aes(xmin=colInd-1,xmax=colInd,ymin=rowInd-1,ymax=rowInd, fill=value))
  if(labCol==T)
    g2=g2+scale_x_continuous(breaks=(1:cols)-0.5, labels=colnames(m))
  if(labCol==F)
    g2=g2+scale_x_continuous(breaks=(1:cols)-0.5, labels=rep('',cols))
  if(labRow==T)
    g2=g2+scale_y_continuous(breaks=(1:rows)-0.5, labels=rownames(m))
  if(labRow==F)
    g2=g2+scale_y_continuous(breaks=(1:rows)-0.5, labels=rep('',rows))
  g2=g2+theme(panel.grid.minor=element_line(colour=NA), panel.grid.major=element_line(colour=NA),
              panel.background=element_rect(fill=NA, colour=NA))
  speccol <-
    if(revPalette){
      scale_fill_gradientn(colors=rev(brewer.pal(11, brewerPalette)))
    } else {
      scale_fill_gradientn(colors=brewer.pal(11, brewerPalette))  }
  return(g2+speccol) }

library(collegeIncome)
data(college)

# 1. Visually look at variation in std. deviations of the distribution of the
#    mean of the median incomes.
#    a. Summarise the data to produce the heatmap
byMajorSummary <- {
  dt <-
    dplyr::mutate(college,Major=major_category, MedianIncome=median) %>%
    dplyr::select(Major, MedianIncome) %>%
    dplyr::group_by(Major) %>%
    dplyr::summarise(MeanofMedianIncome=mean(MedianIncome)) %>%
    dplyr::arrange(desc(Major))
  df <- as.data.frame(dt$MeanofMedianIncome, row.names = dt$Major)
  colnames(df) <- c("Mean of Median Incomes")
  df }
#    b. Produce the heatmap
ggheat(byMajorSummary,clustering='none',rescaling = 'column',labCol = F) +
  guides(fill=guide_legend(title=bquote(sigma))) +
  labs(title="Variation, in standard deviations, of Mean Incomes") +
  theme(plot.title =
          element_text(hjust = 0,size=11,colour="black",face="bold"),
        legend.title =
          element_text(hjust = 1,size=10,colour="black"),
        legend.position = c(0.95,0.1),
        legend.justification = c(1,0),
        legend.direction = "horizontal",
        legend.background=element_blank())
#    The heat map shows a wide varion +/- 2 sigma, but the marity appear to
#     be +/- 1 sigma. Promise of outliers...

# Now consider the detail data
byMajorDetail <-
  dplyr::mutate(college,Major=as.factor(major_category), MedianIncome=scale(median,center=T,scale=T)) %>%
  dplyr::select(Major, MedianIncome)
View(byMajorDetail)

# Test normality - Shapiro-Wilks for each group's income data.
# We can only do this where a group has more than three data points - so thie effectively
# excludes 'Interdisciplinary'.
swTests <- data.table("Group"=character(),"N"=numeric(), "Statistic"=numeric(),"P.value"=numeric())
for(group in levels(byMajorDetail$Major)){
  subset <- byMajorDetail[byMajorDetail$Major==group,]$MedianIncome
  l <- length(subset)
  if(l < 4) {
      swTests <- add_row(swTests,
                         Group = group,
                         N = l,
                         Statistic = 0,
                         P.value = 0)
      } else {
      sw <- shapiro.test(subset)
      swTests <- add_row(swTests,
                         Group = group,
                         N = l,
                         Statistic = round(sw$statistic,3),
                         P.value = round(sw$p.value,3)) } }
View(swTests)
# 'Normal':
swTests[swTests$P.value > 0.05]$Group
# 'Not normal'
swTests[swTests$P.value <= 0.05]$Group

# Test for heteroscedasticity
# Fligner Killeen test is a non-parametric test for homogeneity of group variances based on ranks.
# It is useful when the data is non-normal - we have non-normal data for some of the groups.
fligner.test(MedianIncome~Major,byMajorDetail)
# With p-value > 0.05, we can assume homogeneity in the variance of the data across groups.

# Regardless of the normality/variance homogeneity output, (i.e. conditions are
# sometimes not satisfied), linear model with Major as preditor - i.e., given a
# syident's major, can we predict their income?
lmfit <- lm(MedianIncome~Major, byMajorDetail)
# Tidy up putput and augment.
bTidy <- tidy(lmfit,conf.int=T)
View(bTidy)
bAug <- augment(lmfit)
View(bAug)
bSum <- glance(lmfit)
View(bSum)
# From the tidied bSum for the R sequaared - its close to 0, more to the
# point the p-value is >> 0.05, so we can 'generally' conclude that we
# fail to reject the null hypothesis that beta = 0 - that is, there is
# reasonable eveidence to be able to imply that no correlation exists, generally.
# In fact, in the bTidy table, all of the p-values exceed the dignificance level
# so, for each factor, we fail to reject beta = 0 - that is, no correlation
# between any major category and income. Also, from bTidy, we could look at
# the variation in the coefficients (with error bars:
ggplot(bTidy, aes(estimate,term,colour=term)) + geom_point() +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high))

# Diagnostic plots: Just to confirm, visually, we can produce
# some disgnostic plots:
autoplot(lmfit,which=1:6,ncol=3,label.size=2) +
  theme_bw() +
  theme(axis.text.x=element_text(size=rel(0.8), hjust=1, angle=90))
# The leverage for Interdisciplinary (point 110) is extreme and not shown in the plot -
# this is the category which only has one data point. Other than that, there are clearly
# additional outliers with significant leverage - for example point 60, 'Business' and
# these correspond with the spectrum on the heatmap. Normal Q-Q fit is not too good either
# and the Residual vs. Fitted plot shows heteroscedasticity  - it's not symetrically
# balanced.