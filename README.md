﻿# Marginal *p*-values in psychology

This project investigates the claims made in Pritschet et al. 2016 (*Psych. Sci.*). HTML articles from APA journals were collected in another project by CHJH (see [here](https://github.com/chartgerink/2016statcheck_data)), which were scanned here for *p*-values. These articles are subject to copyright and are not shared in this repository (local backups exist so more than willing to share for verification purposes).

After neatly ordering the HTMLs into separate folders ([script](extraction-functions/apa_foldering.sh)), these articles were converted from HTML to TXT with the tool `html2text` ([developed by Aaron Swartz](https://github.com/aaronsw/html2text) and now maintained [here](https://github.com/Alir3z4/html2text); be sure to install with `pip install html2text`). Shell command `sh functions/html2text.sh`.

The data were extracted from the articles with regular expressions. Variables included are: 

1. Digital Object Identifier (`doi`); if available

2. Text before the *p*-value in the article (`pre`); 200 characters included. 

3. Raw text of the *p*-value result itself (`result`); e.g., `p=.048`

4. Text after the *p*-value in the article (`post`); 200 characters included. 

5. The sign used in the *p*-value reporting (`comparison`); possibilities include [=><≥≤]

6. The *p*-value reported (`value`)

These data are extracted from each article first and then collated into one final dataset. Each article folder (folder name = doi) contains `fulltext.html`, `fulltext.txt` (after `html2text` was used) and the file `results.csv` (which is the result of the regexes).

Before collecting the *p*-values from the articles, first the metadata for each article are collected. To this end, the following script is run.

```bash
ls apa_articles | grep -v nodoi > data/doi
mkdir data/metadata

for i in $(cat data/doi)
do
 ruby extraction-functions/terrier.rb $i
done

rm data/doi
```

`results.csv` for each article was generated in `R` (see [script](extraction-functions/data_extraction.R)). This is run from a shell with `Rscript extraction-functions/data_extraction.R`. To collate all these files into one big file run

```bash
echo '"doi","journal","year","pre","result","post","comparison","value"' > data/marginal_dataset.csv

for file in apa_articles/*/results.csv; do cat $file | grep -vP '"doi","journal","year","pre","result","post","comparison","value"'>> data/marginal_dataset.csv; echo $file; done
```

# Data location

The data is not in this Github repository because it is too large (~350MB). It is stored on the OSF ([here](https://osf.io/28gxz/)) and will be archived at Zenodo.
