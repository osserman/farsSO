---
title: "farsSO_vignette"
author: "Stephen O."
date: "8/31/2018"
output: html_document
---



## Stephen's farsSO package vignette

This package was written to analyze and visualize the traffic fatality data from 
the Fatality Analysis Reporting System <https://www-fars.nhtsa.dot.gov/>.

Included functions allow you to **summarize fatality data from a given set of years**:


```r
fars_summarize_years(list('2013','2015'))
```

And visualize fatalities within a **given state in a given year**:


```r
fars_map_state(13, 2014)
```

