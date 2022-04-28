---
title: 'ejscreenapi - a report for every site'
output:
  html_document: default
  pdf_document: default
---

### What is this app?

This ejscreenapi web app provides easy access to exactly the same results you would get from EJScreen2.0, but it does it for a list of sites all at once. It provides the EJScreen2.0 environmental and demographic data analysis (and links to reports) on conditions near each site, for any list of sites. 

This app is intended as a very basic (slow, site-by-site) interim tool, while the new Environmental Justice Analysis Multisite (EJAM) tool is developed. EJAM will provide much faster results, but more importantly will provide the details needed for calculating summary statistics for the overall population residing near any of the sites (something this interim app cannot do - see caveats below). 

### How it works

This [R Shiny](https://shiny.rstudio.com/) app provides a very basic user interface that lets a user specify a radius in miles for circular buffering, and then upload a csv file of point locations, with a column each for lat and lon in decimal degrees, and it provides a very simple map of circular buffers at those points. The app then can request from [EPA](https://epa.gov)'s servers an [EJScreen](https://www.epa.gov/ejscreen) standard report on each buffer.

The site-specific results are obtained one at a time (slowly, roughly 1 minute per 100 circular buffers) via the [EJScreen API](https://www.epa.gov/ejscreen/ejscreen-api). Those are compiled into a single table, one row per site (buffered point). Each column provides a different indicator, such as % low income, or the traffic score as a percentile. Any extra columns uploaded along with lat and lon are included as the first set of columns in the results table. 

The interactive results table can be 

- sorted (click on a column header such as % low-income; click again to reverse the order)
- filtered (e.g., type a state name to see only those rows)
- searched (e.g., by facility name)
- downloaded as a csv file to be opened as a spreadsheet

## **Important caveat**

These site-by-site results cannot be summarized accurately as overall statistics on the population overall or average person!

For only a subset of the indicators, and only if buffers do not overlap, these results are sufficient for creating a summary overall. For most indicators, one can only roughly approximate the overall results using population-weighted means, and correctly calculating them is not possible with the information provided by the [EJScreen API](https://www.epa.gov/ejscreen/ejscreen-api) as of early 2022, which is what this simple tool provides access to. This is true for at least three reasons:

1.  One cannot take an arithmetic average across the sites, if the goal is to characterize demographics of the overall population near the set of sites. The average site's percent demographics (e.g., % over age 64) is not actually the same as the overall percent (e.g., % over age 64 among all people near any of the sites). That is because population density varies (often widely) between sites, so a circular buffer of some specified radius will include many people at some sites and very few people at other sites. For example, one site may have 1,000 people nearby while another site may have 5,000 people nearby.

2.  One might think that could be solved by using a population-weighted mean of scores at the various sites, but that only works if the denominator is total population. That means it works for percent over 64 but not for percent low-income, percent less than high school, percent pre 1960, or percent linguistic isolation, because those all use different kinds of denominators, such as the count of households or the count of people aged 25 and up. To correctly calculate percent low-income one needs the sum of counts of low-income divided by the sum of counts of people for whom the poverty income ratio was determined. Those counts, however, are not provided by the [EJScreen API](https://www.epa.gov/ejscreen/ejscreen-api), so the overall summary results cannot be correctly calculated from the site-by-site reports.

3.  Even if the counts were available, circular buffers can overlap, and the site by site reports do not provide information about which residents are in overlapping buffers. This means double-counting cannot be avoided if buffers overlap. Even the correct total population count of unique residents near the set of sites is not necessarily available (the sum of that column would be an overcount). The other indicators would face the same issue.
