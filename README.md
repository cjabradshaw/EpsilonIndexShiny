# Shiny app for calculating the ε-index from a user-supplied text file of citation data

<img align="right" src="epsilonindex logo.png" alt="thylacine" width="200" style="margin-top: 20px">

Prof Corey J. A. Bradshaw <br>
<a href="http://globalecologyflinders.com" target="_blank">Global Ecology</a>, <a href="http://flinders.edu.au" target="_blank">Flinders University</a>, Adelaide, Australia <br>
September 2021 <br>
<a href=mailto:corey.bradshaw@flinders.edu.au>e-mail</a> <br>

Existing citation-based indices used to rank research performance do not permit a fair comparison of researchers among career stages or disciplines, nor do they treat women and men equally. We designed the ε-index, which is simple to calculate, based on open-access data, corrects for disciplinary variation, can be adjusted for career breaks, and sets a sample-specific threshold above and below which a researcher is deemed to be performing above or below expectation.

This <a target="_blank" href="https://cran.r-project.org">R</a> <a href="https://shiny.rstudio.com">Shiny</a> app can be accessed via shinyapps.io using the URL <a target="_blank" href="https://cjabradshaw.shinyapps.io/epsilonIndex/">cjabradshaw.shinyapps.io/epsilonIndex</a>. The app is an online interface of the original R code for calculating the the ε-index manually (see this <a href="https://github.com/cjabradshaw/EpsilonIndex">Github repository</a>).

Because third-party scraping of Google Scholar data violates their <a href="https://policies.google.com/terms?hl=en">terms of service</a>, the automatic interface part of the <a href="https://github.com/cjabradshaw/EpsilonIndex">original code</a> using the <a href="https://www.rdocumentation.org/packages/scholar/versions/0.1.7"><strong>scholar</strong></a> library is disabled in the <a target="_blank" href="https://cjabradshaw.shinyapps.io/epsilonIndex/">app</a>.

This app accompanies the article:

<strong><a href="https://globalecologyflinders.com/people/#CJAB" target="_blank">BRADSHAW, CJA</a>, <a href="https://www.chalkerlab.com/jmc" target="_blank">JM CHALKER</a>, <a href="https://stefanicrabtree.com/about-stefani/" target="_blank">SA CRABTREE</a>, <a href="https://researchnow.flinders.edu.au/en/persons/bart-eijkelkamp" target="_blank">BA EIJKELKAMP</a>, <a href="https://en.wikipedia.org/wiki/John_A._Long" target="_blank">JA LONG</a>, <a href="https://www.flinders.edu.au/people/justine.smith" target="_blank">JR SMITH</a>, <a href="https://staffportal.curtin.edu.au/staff/profile/view/K.Trinajstic/" target="_blank">K TRINAJSTIC</a>, <a href="https://researchnow.flinders.edu.au/en/persons/vera-weisbecker" target="_blank">V WEISBECKER</a>. 2021. <a href="http://doi.org/10.1371/journal.pone.0257141">A fairer way to compare researchers at any career stage and in any discipline using open-access citation data</a>. <i><strong>PLoS One</strong></i> 16(9): e0257141. doi:<a href="http://doi.org/10.1371/journal.pone.0257141">10.1371/journal.pone.0257141</a></strong>

-- <br>
<strong>DIRECTIONS</strong>

1. Create a delimited text file of <strong>exactly the same format</strong> as the example file in this repository ('<a href="https://github.com/cjabradshaw/EpsilonIndex/blob/main/datasample.csv">datasample.csv</a>'):

 - <strong>COLUMN 1</strong>: <i>personID</i> — any character identification of an individual researcher (can be a name)
 - <strong>COLUMN 2</strong>: <i>gender</i> — researcher's gender ("F" or "M")
 - <strong>COLUMN 3</strong>: <i>i10</i> — researcher's i10 index (# papers with ≥ 10 citations); <strong>must be > 0</strong>
 - <strong>COLUMN 4</strong>: <i>h</i> — researcher's <i>h</i>-index
 - <strong>COLUMN 5</strong>: <i>maxcit</i> — number of citations of researcher's most cited peer-reviewed paper
 - <strong>COLUMN 6</strong>: <i>firstyrpub</i> — the year of the researcher's first published peer-reviewed paper

2. Access the app at <a target="_blank" href="https://cjabradshaw.shinyapps.io/epsilonIndex/">cjabradshaw.shinyapps.io/epsilonIndex</a>.
  
3. Load your delimited text file in the app by clicking the <i>choose file</i> button.

4. Select whether you want the index to be calculated for women and men separately as well as pooled ('<i>include gender split?</i>'). If there are too few researchers in any gender category, the algorithm will fail.

5. Choose how you want the output file to be ordered by selecting one of the four choices in the drop-down menu:

   <i>ε-index</i>, <i>gender-debiased ε-index</i>, <i>ε′-index</i>, or <i>gender-debiased ε′-index</i>

If there are insufficient individuals per gender to estimate a gender-specific index, we recommmend not using or sorting based on the gender-debiased index. If the individuals in the sample are not all in the same approximate discipline, we recommend not using or sorting based on either of the two normalised (ε′) indices.

6. Click the <i>calculate </i> button.

7. Download the results table as a .csv file by clicking the clicking the <i>download</i> button

The output .csv file includes the following columns:

- <i>person</i>: researcher's ID (specified by user)
- <i>gender</i>: F=female; M=male
- <i>yrs.publ</i>: number of years since first peer-reviewed article
- <i>cM</i>: citation mass (or, <i>cMs</i> = normalised citation mass if you select gender split)
- <i>gender.eindex</i>: <i>ε</i>-index relative to others of the same gender in the sample (not included if you select no gender split)
- <i>expectation</i>: whether above or below expectation based on chosen index (default is 'e' = pooled index)
- <i>m-quotient</i>: <i>h</i>-index ÷ yrs.publ
- <i>h-index</i>: <i>h</i>-index
- <i>debiased.e.prime.index</i>: scaled gender.eindex (gender <i>ε</i>′-index) (not included if you select no gender split)
- <i>gender.rank</i>: rank from gender.eindex (1 = highest) (not included if you select no gender split)
- <i>rnk.debiased</i>: gender-debiased rank (1 = highest) (not included if you select no gender split)
- <i>cM</i>: citation mass (COLUMN 4 if you select no gender split)
- <i>pooled.eindex</i>: <i>ε</i>-index generated from the entire sample (not gender-specific)
- <i>e.prime.index</i>: scaled pooled.eindex (<i>ε</i>′-index)
- <i>pooled.rnk</i>: rank from pooled.eindex (1 = highest)

and

if you sorted based on <i>ε′-index</i>:

- <i>ePRnk</i>: rank from scaled pooled.eindex (<i>ε</i>′-index)

or if you sorte base on <i>gender-debiased ε′-index</i>:

- <i>ePddebRnk</i>: rank from scaled gender.eindex (gender <i>ε</i>′-index)
