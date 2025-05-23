# Swish Lite

Swish Lite is a set of Chez Scheme libraries based on the [Swish Concurrency
Engine](https://github.com/becls/swish).

[Documentation](https://indigobio.github.io/swish-lite/swish-lite.pdf)

# Build System Requirements

## Linux

- Chez Scheme Version 10.2.0
- graphviz, texlive, texlive-latex-recommended, and texlive-latex-extra packages for
  building the documentation

## Mac

- Chez Scheme Version 10.2.0
- dot (can be installed through homebrew using `brew install graphviz`)
- pdflatex (can be installed through homebrew using `brew cask install mactex`)
- Latin Modern fonts from LaTeX (can be installed with Font Book from a location like
  `/usr/local/texlive/2020/texmf-dist/fonts/opentype/public/lm`)

## Windows

- Chez Scheme Version 10.2.0
- Cygwin or MinGW/MSYS with bash, git, graphviz, grep, perl, texlive, GNU make, etc.
- Put scheme in PATH.

# Maintenance

Run `update-pdf` and force-push branch `gh-pages` when documentation changes.
