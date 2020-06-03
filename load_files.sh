#!/bin/sh

cd "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project"
echo "Pulling newest data from github repo:"
git checkout master
git pull

echo "Copying data to AWS"

PTH='ec2-35-153-18-124.compute-1.amazonaws.com'

echo ubuntu@$PTH:/srv/shiny-server/myapp

scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/ui.R" ubuntu@$PTH:/srv/shiny-server/myapp
scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/server.R" ubuntu@$PTH:/srv/shiny-server/myapp
scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/data/ucr_by_state.csv" ubuntu@$PTH:/srv/shiny-server/myapp/data 
scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/data/prison_custody_by_state.csv" ubuntu@$PTH:/srv/shiny-server/myapp/data 
scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/dataPrep.RData" ubuntu@$PTH:/srv/shiny-server/myapp 
scp -i "/home/ewaewaewa/Documents/RR - shiny/stacjonarka.pem" "/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/index.Rmd" ubuntu@$PTH:/srv/shiny-server/rmd-crime

echo All files are loaded!
