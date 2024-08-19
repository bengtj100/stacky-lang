# Installation

## Binary installation

The pre-compiled deliverables are built on a Ubuntu 22.04 machine and should be compatible with most modern Linux distributions. The releases are tested to work on Fedora 40, as well.

### Downloading a TAR-ball

Go to [the releases page](https://github.com/bengtj100/stacky-lang/releases) and fetch the latest build.

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

**NOTE:** The interpreter expects to find its Prelude file(s) at a `lib` location corresponding to the `bin` location, just like in the example above.

```
....+--- bin
    |    |
    |    +--- stacky
    |
    +--- lib
         |
         +--- stacky
              |
              +--- Prelude.sy
              |
              +--- ...
```

If, for some reason, it is not possible to install in this way, the `stacky` binary must be started using the `--prelude` option to point to the  Prelude file:

```
$ stacky --prelude /path/to/Prelude.sy
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

The description is intended for a build server to build releases, but this can be used if you are just intending to build locally as well. In the latter case, the first section is often superfluous.

### Setting up a build server

**Only for setting up a build server**

**NOTE:** This description is tested on Ubuntu 22.04 LTS Server, but should work on most newer Debian based distros, like Debian it self and Linux Mint.

You need to have `curl` installed to run this by pasting. If `curl` is not installed; as a non-root user do:

```
sudo apt install -y curl
```

Either paste the below commands into a shell on your server or run the build-server set-up script directly:

As root on the build server do:
```
curl -O https://raw.githubusercontent.com/bengtj100/stacky-lang/main/tools/make-build-server.sh
chmod +x ./make-build-server
./make-build-server
```

or paste as follows:

```
useradd -m -G sudo -s /bin/bash bob
passwd bob
su bob
cd
ssh-keygen -t ed25519

```

Assuming the username is `bob`. Replace with whatever name that is suitable.

This step requires some human interaction, e.g., when entering the password for the new user.

### Install GHC and build tool-chain using the set-up script.

This will install the Haskell and pandoc tool-chains needed to build the application and documentation.

```
curl -O https://raw.githubusercontent.com/bengtj100/stacky-lang/main/tools/setup-toolchain.sh
chmod +x ./setup-toolchain.sh
./setup-toolchain.sh
```

**NOTE:** This may take some time. On our build server, this step takes about 15 minutes!

### Install GHC and build tool-chain manually from the command-line.

If you want to run the steps manually do the following as a non-root user:

```
sudo apt update

sudo apt upgrade -y

sudo apt install -y ghc ghc-prof ghc-doc cabal-install git sudo \
                    zlib1g-dev pkg-config build-essential \
                    pandoc texlive curl vim

mkdir src

cd src

git clone https://github.com/bengtj100/stacky-lang.git
```

### Set up the build system

As a non-root user do:

```
cabal update

cabal install cabal-install
```

If you plan to use an editor that uses TAGS, then install hasktags:

```
~/.cabal/bin/cabal install hasktags
```

The build system will automatically build the TAGS file, if hasktags is installed and skip that stage if not.

    
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
