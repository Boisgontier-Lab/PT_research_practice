# **What is the current state of physical therapy research practices?**

This repository contains the code and the data for the manuscript: Jabouille et al., (2025). "Transparency and Replicability in Physical Therapy Research: Time to Wake Up".

A preprint of the paper has also been uploaded here as a PDF.

We warmly thank Paul C. Bogdan for his impressive work and for generously sharing his data and code, without which this analysis could not have been carried out. Our p-fragile analysis builds on his work.

## **Repository layout**

#### In the Analysis folder, you will find two main subfolders:

**(1) "PT Research Practices":** containing the dataset with all the data manually extracted from the 1173 articles and the R code to analyze it.

* **PT-research practice validated.xlsx:** entails the individual data extraction of all the coders, and the validated data after concertation. The final dataset used for analysis was the sheet "Data2022+2023+2024".
* **PT research practice analysis.r:** entails all the analyses made on the dataset except the p-fragile calculation.

**(2)** **"PTChange-main":** inspired from the study of Bogdan (2025), containing the R code, python scripts and datasets used for the p-fragile calculation.

* **R\_code:** p\_fragile\_calculation.r contains the code to calculate the p-fragile proportion from scratch.
* **prepare\_fulltexts**: contains the python scripts used in the R code to download the fulltext articles in .HTML and .MHTML in 3 different ways (from doi, pmcid, and ScienceDirect), and the python scripts to retrieve the Results sections of each article and convert them into usable plaintexts (.TXT).
* **make\_dataset:** contains the following:

     	1. make\_p\_z\_df.py that does all of the p-value, statistics, tests extraction and calculate the p\_implied by the tests. It contains much delicate 	regex code. It           produces val\_orig\_quant\_by\_pval.csv where one row = one p-value.

     	2. make\_processed\_p\_df.py that takes the val\_orig\_quant\_by\_pval.csv prepared by make\_p\_z\_df.py and processes it, categorizing each p-value 	(e.g., .01 <= p         < .05), calculates fragile p-values, does the correction for papers that report only p<.05, etc. It produces 	"val\_orig\_quant\_process.csv" where one row = one           paper and "val\_orig\_quant\_processed\_by\_p.csv" where one row = one p-value.


##    **How to run**

I ran this analysis on Windows11.
Software: R (≥4.x) and Python (≥3.x) with the packages used in PT research practice analysis.r and R\_code/p\_fragile\_calculation.r.

**Option A — use existing spreadsheets** 

The spreadsheets have all been uploaded and placed in the "dataframes" folder. Hence, anyone who wants to use the dataset could just skip all of the python scripts and run the code in R\_code/p\_fragile\_calculation.r from #### Fragile p-value calculation #### -> # Upload the final file. You can find the "Final spreadsheet columns" Section further below for instructions on what the spreadsheet columns represent.

**Option B — rebuild**

Alternatively, based on the whole code in R\_code/p\_fragile\_calculation.r, anyone (with a suitable university library subscription) can reproduce the spreadsheets themselves. This would, notably, involve you running the scripts to scrape the fulltexts. I have uploaded as much as I can, but I cannot upload the fulltexts themselves for copyright reasons. If you run the scripts in the prepare\_fulltexts folder with the R code, it will download you the articles in .HTML and .MHTML in the doi\_tree\_html and doi\_tree\_mhtml folders, as well as the .TXT with the Results sections in doi\_tree\_txt and doi\_tree\_mhtml\_txt respectively. Then all you have to do is run the scripts in the make\_dataset folder to obtain the “dataframes” folder and the spreadsheets it contains.

If you choose option B, be sure to fill in the fields marked as requiring your information in the Python scripts.

## **Final spreadsheet columns**

Below are only the columns used in the final spreadsheets shipped in dataframes/.

**'val\_orig\_quant\_by\_pval.csv' (1 row = 1 p-value)**
* sign: reported sign of the p-value: "=", "<", ">".
* p\_val: numeric value of the p-value (for <, this is the threshold).
* sentence: text snippet surrounding the reported p.
* doi\_str: standardized DOI (dash-separated).
* stat\_type: nearby test type (t, F, χ², z, r).
* stat\_value: reported statistic value.
* df, df1, df2: degrees of freedom.
* n: sample size when detected.
* p\_implied: p implied from the test statistic (when computable).

**'val\_orig\_quant\_processed\_by\_p.csv' (1 row = 1 p-value, post-processed)**
Each p-value is labeled by report type (= or <), placed into threshold buckets (.05/.01/.005/.001), with overall counts and fragility.

Exact (=) buckets
* sig\_exact, n05\_exact, n005\_h\_exact, n005\_l\_exact, n001\_exact, num\_ps\_exact.
Less-than (<) buckets
* sig\_less, n05\_less, n005\_h\_less, n005\_l\_less, n001\_less, num\_ps\_less.

Non-significant buckets
* insig\_exact, insig\_less, insig\_over (for > cases).
* num\_ps\_any (any parsable p), n\_exact05 (exactly p = .05).

Aggregates (sign-agnostic)
* sig, n05, n005\_h, n005\_l, n01 (n005\_h ∪ n005\_l), n001, n01\_001 (n01 ∪ n001), num\_ps, insig, cnt.
Article pattern helpers (carried at the p-row for later aggregation)
* lowest\_p\_val: the smallest p\_val seen for that article.
* cond: article’s p-reporting pattern category (e.g., all\_less0.05, all\_equal, equal\_less0.01, eclectic).
* p\_cutoff: threshold implied by cond (e.g., .05, .01).

Fragility labels
* p\_is\_fragile\_reported: FRAGILE by the reported p (true if 0.01 ≤ p < 0.05; inequalities like p < .05 remain ambiguous).
* p\_is\_fragile\_implied: label from implied p: INSIG (≥ .05), FRAGILE (.01–.05), STRONG (< .01).
* ambig\_less05: marks ambiguous p < .05 cases.

**'val\_orig\_quant\_process.csv' (one row = one paper)**
For each article, it sums the per-p flags and report fragility proportions.

Summed flags for all categories:
* sig\_exact, n05\_exact, n005\_h\_exact, n005\_l\_exact, n001\_exact, num\_ps\_exact
* sig\_less, n05\_less, n005\_h\_less, n005\_l\_less, n001\_less, num\_ps\_less
* insig\_exact, insig\_less, insig\_over, num\_ps\_any, n\_exact05
* sig, n05, n005\_h, n005\_l, n01, n001, n01\_001, num\_ps, insig, cnt

Helpers
* lowest\_p\_val: smallest p-val seen for the article.
* cond: article’s p-reporting pattern category (see above).
* p\_cutoff: threshold implied by cond.

Fragility proportions (paper-level)
* p\_fragile: proportion of significant p-values in \[0.01, 0.05].
* p\_fragile\_w\_exact05: as above, counting p = .05.
* p\_fragile\_orig: pre-correction copy.
* p\_fragile\_imp\_implied: computed from implied p’s when available.
* p\_fragile\_prop\_raw: raw retained p-value.
* p\_fragile\_prop\_adj: adjusted p-value use for the analysis. For papers that only reported p < .05 (cond = all\_less0.05):

&nbsp;     - replaced by the implied p-value if available, else by a fixed fallback (~0.51) - (See supplementary material 3 for details on this point).
&nbsp;	   - indicators: fragile\_implied\_applied, fragile\_51\_applied.


Should you have any question, don't hesitate to contact me at fjabouil@uottawa.ca.








