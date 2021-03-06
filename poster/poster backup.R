---
  title: Poster for Vast Challenge 2021 Project
author:
  - name: Li NAN Li Yueting
affil: 1
LinkedIn: linkedin.com/in/li-nan-63b9251a6
email: nan.li.2020@mitb.smu.edu.sg
orcid: 0000-0002-1099-3857
main: true
affiliation:
  - num: 1
address: "Department of Computer and Information Systems,Singapore Management University"
main_findings:
  - "Make **better posters** with RMarkdown + **posterdown**."
- '![](https://raw.githubusercontent.com/brentthorne/posterdown/master/images/betterhexlogo.png){.main_pic}'
logoleft_name: '![](https://raw.githubusercontent.com/brentthorne/posterdown/master/images/qr-code-black.png){.main-img-left}'
logoright_name: '![](https://raw.githubusercontent.com/brentthorne/posterdown/master/images/betterhexlogo.png){.main-img-right}'
output: 
  posterdown::posterdown_betterland:
  self_contained: false
pandoc_args: --mathjax
highlight: haddock
number_sections: false
link-citations: true
bibliography: packages.bib
---
  
  ```{r, include=FALSE}
knitr::opts_chunk$set(results = 'asis',
                      echo = FALSE,
                      warning = FALSE,
                      tidy = FALSE,
                      message = FALSE,
                      
                      out.width = "100%")
options(knitr.table.format = "html") 
```

```{r myplot, include=FALSE}
svg('myplot.svg')
plot(iris$Sepal.Length, iris$Sepal.Width)
dev.off()
```

# Introduction

This is the `poster` which provides an overview of our project which is based on Vast Challenge 2021.
It includes the following sections:    fig.align = 'center',
•	Issues and problems •	Motivation •	Approach •	Results •	Future Work 


```{r, include=FALSE}
knitr::write_bib(c('posterdown', 'rmarkdown','pagedown'), 'packages.bib')
```

## INTRODUCTION

In a fiction scenario,a gas-production company named Tethys-based GAStech has been operating a natural gas production site in the island country of Kronos and it has produced remarkable profits and developed strong relationships with the government of Kronos. However, GAStech has not been as successful in demonstrating environmental stewardship. And in January, 2014, the leaders of GAStech are celebrating their new-found fortune, but in the midst of this celebration, several employees of GAStech go missing. An organization known as the Protectors of Kronos (POK) is suspected in the disappearance, but things may not be what they seem.

# Approach

*1) Network visualization*
  The network visualization shows the connection between GAStech employees. The Department labels allow user to distinguish the department of nodes and the source of emails by different color. User can select the ID of interested person, and the un-correlated nodes will fade to grey. Also user may slide the mouse wheel to zoom in/zoom out to see the person name of each node, hover the mouse to the node can also got the result by reading the tooltip.


*2) Geospatial visualization*
  
  Geospatial visualization in this project is used to create an interactive route map which can track employee’s movement during Jan 6th and Jan 19th . For this visualization, we use multiple R packages. Tmap package offers a flexible, layer-based, and easy to use approach to create thematic maps, such as route map. Shiny package takes a fresh, interactive approach to telling the data story.

*3) Interactive Data table visualization*
  
  To create an interactive transaction data table, DT package in R is used. The R package DT provides an R interface to the JavaScript library DataTables. In this project, loyalty dataset and credit/debit card dataset used by DT package is displayed as tables on HTML pages, and through argument settings, this data table provides filtering and sorting features.


**_Now on to the results!_**
  
  <br>
  
  # Results
  
  *SOLUTIONS*
  
  Solution of issue 1:  Through data manipulation and visualization, this project labeled the stopping points and add tooltip to clarify and dig out more info of car movement and car owners. By input one or multiple Car ID and Tracking Day, users can observation employee’s daily routines and their home locations. Through this improvements, it can also provide the chance to identify their private or professional relationship by observing their daily connection and gathering spots.


Solution of issue 2: Through the stopping points which show info about day and hour and the corresponding transaction record, users can easily see the relationships between car owners and credit card owner, which provides strong support for future detecting work for unusual transaction data.

# Future Work


There are also some future improvements both in application and exploration direction of this project. 

### For application:###

1.	Consider add “select all” action button. This will help to detect each day all the driver’s movement.
2.	Instead of using Car ID as input, consider using Employee title and Employee department as input, which can help to find relationship between different departments.
3.	 Consider Add starting time and end time in the tooltip, which can be more helpful for detecting suspicious gathering groups.

### For future direction:###

1.	Needs to develop more visualizations to summarize employee’s gathering spots to identify their private relationships.
2.	Needs to combine products of social networking visualization and route map visualization to find more insights, like creating a dashboard to combine.

```{r, echo=FALSE,eval=FALSE}
library(pagedown)
```