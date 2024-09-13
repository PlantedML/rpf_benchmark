#! /bin/bash

quarto render results.qmd --to html

aws s3 cp results.html s3://jemsu/rpf_benchmark/index.html
aws s3 sync results_files s3://jemsu/rpf_benchmark/results_files

quarto render results.qmd --to pdf

aws s3 cp results.pdf s3://jemsu/rpf_benchmark/results.pdf
aws s3 cp results.tex s3://jemsu/rpf_benchmark/results.tex

aws s3 sync data s3://jemsu/rpf_benchmark/data
