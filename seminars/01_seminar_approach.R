---
  # title: "Modern Statistical Computing"
  # subtitle: "Seminar 1"
  # title-block-banner: true
  date: "January 19, 2023"
format: 
  pdf: 
  documentclass: report
geometry: 
  - top = 30mm
- left = 20mm
fontfamily: libertinus
colorlinks: true
--- 
  
  \begin{center}\vspace{0.3cm}
\textbf{\Large Modern Statistical Computing} \\
\vspace{5pt} {\large Seminar \#1} \\
  \vspace{5pt} {\large January 19, 2023} \\
  \vspace{5pt} {\large Homework part due on January 26, 2023 (3pm)} 
  \end{center}
  
  \flushleft 
  
  \rule{\linewidth}{0.1mm}
  
  
  
  
  The idea of this seminar is to pull data from [Eurostat](https://ec.europa.eu/eurostat/data/database) on demographic 
  variables, do some data wrangling, and ultimately generate some plots. 
  We will fetch data at the geographic level of NUTS2 regions (e.g. Catalunia in Spain, to get a general sense of NUTS2 regions you can look at maps [here](https://ec.europa.eu/eurostat/web/nuts/nuts-maps)). 
  \section{Exercises}
  
  \subsection{Building the Dataset}
  
  