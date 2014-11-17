#!/bin/bash

emacsVersion=emacs-24.4

sudo apt-get install -y make libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev
wget -c http://ftp.gnu.org/gnu/emacs/$emacsVersion.tar.gz
tar xfz $emacsVersion.tar.gz
mv $emacsVersion.tar.gz $emacsVersion/.
cd $emacsVersion
./configure
make
sudo checkinstall
cd ..
