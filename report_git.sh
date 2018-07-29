#!/bin/bash

cd /home/pi/git/street_cam
Rscript make_report.R
git add analysis/traffic_report.html
git commit -m 'new report'
git push
