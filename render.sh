#!/bin/sh
depdir=/home/pi/icu_covid_data
jetzt=`date +%Y%m%d`
cd icu_covid_data

git pull

sudo Rscript R/extract.R
sudo Rscript -e "rmarkdown::render('index.Rmd')"

git add .
git commit -m "new render task at $jetzt"
git push -u origin master

cd ..
