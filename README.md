# street_cam

Raspberry Pi 3 based system that creates automatic traffic reports. This relies heaving on http://github.com/pageauc/speed-camera (OpenCV based traffic logger which also attempts to record speed).

This repository is just a R-based system to pull into the [speed-camera](http://github.com/pageauc/speed-camera) `csv` log file, group the recordings into a single incident (based on time stamps), create the plots, and build the [html page](https://rawgit.com/davemcg/street_cam/master/analysis/traffic_report.html) that is updated every 4 hours by a git push. 

All computation happens on the pi. 
