#!/bin/bash

emacsVersion=emacs-24.5

sudo apt-get install build-essential
sudo apt-get build-dep -y emacs24
sudo apt-get install -y checkinstall
wget -c http://ftp.gnu.org/gnu/emacs/$emacsVersion.tar.gz
tar xfz $emacsVersion.tar.gz
mv $emacsVersion.tar.gz $emacsVersion/.
cd $emacsVersion
./configure
make
sudo checkinstall
cd ..
