#!/bin/sh

markdown "EXPERIMENTS/$2/single_results/$1.txt" > "EXPERIMENTS/$2/single_results/$1.body_html"
cat templates/header.html "EXPERIMENTS/$2/single_results/$1.body_html" templates/footer.html > "EXPERIMENTS/$2/single_results/$1.html"
rm "EXPERIMENTS/$2/single_results/$1.body_html"
