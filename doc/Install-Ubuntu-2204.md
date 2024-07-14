# Installation

This describes the setup of the *Bob* building server on Ubunty 22.04 LTS

## Install GHC

```
apt update

apt upgrade -y

apt install -y ghc ghc-prof ghc-doc cabal-install git sudo zlib1g-dev pkg-config build-essential pandoc texlive curl vim
```

## Create the builder user

```
useradd -m -G sudo -s /bin/bash bob

passwd bob

su bob

cd
```

## Create SSH credendtials

As user `bob` do:

```
ssh-keygen -t ed25519
```

Don't forget to upload this to [Bitbucket](https://bitbucket.org).

## Clone the Stacky repo

```
mkdir src

cd src

git config --global user.email "bengtj100@gmail.com"

git config --global user.name "Bengt Johansson"

git clone git@bitbucket.org:bengtj100/stacky.git
```

## Set up the build system

```

cabal update

cabal install cabal-install

~/.cabal/bin/cabal install hasktags
```

## Build a release

```
cd stacky

make release
