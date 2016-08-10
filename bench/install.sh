#!/bin/bash

sudo fallocate -l 512M /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

sudo apt-get update
sudo aptitude install mc htop git
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y
cd "$HOME"
git clone https://github.com/serokell/rscoin.git
cd rscoin
stack setup
stack bench rscoin --no-run-benchmarks

