#!/bin/sh

markdown results/$1\.txt > results/$1\.body_html
cat templates/header.html results/$1\.body_html templates/footer.html > results/$1\.html
rm results/$1\.body_html
