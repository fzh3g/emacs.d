# zhengfaxiang's Emacs config #

## Introduction ##

This is zhengfaxiang's Emacs configuration tree. It contains some reasonable 
config for better coding experience, which runs smoothly in my Archlinux. This
config adds improvments for the following languages:

* C/C++
* Python
* Fortran
* Matlab
* LaTeX

## Installation ##

To install, clone this repo to `~/.emacs.d`. Please backup your old `~/.emacs`
and `~/.emacs.d` before you join the journey :-)

```
git clone https://github.com/zhengfaxiang/emacs.d.git ~/.emacs.d
```

Then just fire up your Emacs, further third-party packages will be automatically
downloaded and installed.

## Update ##

Update this config by entering `~/.emacs.d` and typing `git pull`. You also need
to update the third-party packages by:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

## Before you start ##

For fully enjoy the powerfull features of this configuration, there's some
recommended instructions:

### Spelling ###

* `flyspell` is used for automatic spell checking
* Install `aspell` & `aspell-en` or `hunspell` & `hunspell-en` through your
system package manager
* Dictionary was set to `en_US` in `init-spelling.el`. You can modify it.

### Python ###

* [anaconda-mode](https://github.com/proofit404/anaconda-mode) is used for
*IDE-like* experience
* Install `jedi`, `service_factory` through `pip install`
* If you're coding with multiple versions of Python, install
[virtualenv](https://virtualenv.pypa.io/en/latest/)
through `pip install`. After modified your virtualenv, use
<kbd>M-x pyvenv-activate</kbd> to activate it.

### C/C++ ###

* [irony-mode](https://github.com/Sarcasm/irony-mode) is used to improve
the editing experience for the C, C++ and Object-C languages.
* Install [libclang](http://clang.llvm.org/doxygen/group__CINDEX.html) and
[cmake](http://www.cmake.org/) through your system package manager
* <kbd>M-x irony-install-server</kbd> to install `irony-server`

### LaTeX ###

* Make sure TeX is installed. If you are a
  * Ubuntu/Debian/Mint user, install texlive by
  `sudo apt-get install texlive-full`
  * Arch Linux user, install texlive by
  `sudo pacman -S texlive-most texlive-langcjk`
  * Fedora user, install texlive by `sudo yum -S texlive-all`
  * SuSE user, install texlive by `sudo zypper in texlive-xetex`

## References ##

* [purcell's emacs.d](https://github.com/purcell/emacs.d)
* [spacemacs](https://github.com/syl20bnr/spacemacs)
* [Prelude](https://github.com/bbatsov/prelude)
* [oh-my-emacs](https://github.com/xiaohanyu/oh-my-emacs)
* [redguardtoo's emacs.d](https://github.com/redguardtoo/emacs.d)
