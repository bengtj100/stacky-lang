# Installation

## Binary installation

The pre-compiled deliverables are built on a Ubuntu 22.04 machine and should be compatible with most modern Linux distributions. The releases are tested to work on Fedora 40, as well.

### Downloading a TAR-ball

Go to [the releases page]((https://www.dropbox.com/scl/fo/w3r8zo3och43dybheyspl/AHpmO0q0heWiFuOWt2gSwCE?rlkey=pwz1f8j1yuqd00grj88re745o&st=xx54wmnf&dl=0)) and fetch the latest build

### Unpacking the TAR-ball

When downloaded unpack the file using the `tar` command:

```
tar zxvf stacky.<VERSION>.tar.gz
```

Next copy the executable to a place in your `$PATH` file and the lib directory to the corresponding `lib` directory.

Example:

```
cd stacky.<VERSION>/
sudo cp -p bin/stacky /usr/local/bin/
sudo cp -r lib/stacky /usr/local/lib/
```

Now it should be possible to run the Stacky interpreter:

```
$ stacky

Stacky, version: 0.1, build: 2024-07-14.13-10-48
Copyright (c) 2024 Bengt Johansson -- All rights reserved


Loading the Prelude ... DONE

[  <]
> 
```


## Building and installing from source

This describes how to set up the tool-chain and build stacky on a Ubuntu 22.04 machine.

The description is intended for a build server to build releases, but this can be used if you are just intending to build locally as well. In the latter case, the two first sections are often superfluous.

### Create the builder user

**Only for setting up a build server**

```
useradd -m -G sudo -s /bin/bash bob

passwd bob

su bob

cd
```

### Create SSH credentials

**Only for setting up a build server**

As user `bob` do:

```
ssh-keygen -t ed25519
```


### Install GHC and build tool-chain

As a non-root user do:

```
sudo apt update

sudo apt upgrade -y

sudo apt install -y ghc ghc-prof ghc-doc cabal-install git sudo \
                    zlib1g-dev pkg-config build-essential \
                    pandoc texlive curl vim
```

Don't forget to upload this to [Bitbucket](https://bitbucket.org).

### Clone the Stacky repo

As a non-root user do:

```
mkdir src

cd src

git config --global user.email "your.email@example.com"

git config --global user.name "You R. Name"

git clone git@bitbucket.org:bengtj100/stacky.git
```

### Set up the build system

As a non-root user do:

```

cabal update

cabal install cabal-install

~/.cabal/bin/cabal install hasktags
```

### Build a release

**This is only for building on a build server.**

```
cd stacky

make clean release
```

### Install locally on the build machine

```
make clean install
```

This installs to `/usr/local/bin` by default
