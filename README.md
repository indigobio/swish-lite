# Swish Lite

Swish Lite is a set of Chez Scheme libraries based on the [Swish
Concurrency Engine](https://github.com/indigobio/swish).

[Documentation](https://indigobio.github.io/swish-lite/swish-lite.pdf)

# Build System Requirements

## Linux

- Chez Scheme 9.5.4 or 9.5.5
- graphviz, texlive, texlive-latex-recommended, and texlive-latex-extra packages for building the documentation

## Mac

- Chez Scheme 9.5.4 or 9.5.5
- dot (can be installed through homebrew using `brew install graphviz`)
- pdflatex (can be installed through homebrew using `brew cask install mactex`)
- Latin Modern fonts from LaTeX (can be installed with Font Book from a location like `/usr/local/texlive/2020/texmf-dist/fonts/opentype/public/lm`)

## Windows

- Chez Scheme 9.5.4 or 9.5.5
- Cygwin or MinGW/MSYS with bash, git, graphviz, grep, perl, texlive,
  GNU make, etc.
- Put scheme in PATH.

# Maintenance

Update the PDF when documentation changes.

```
make -C doc
git checkout gh-pages
cp doc/swish-lite.pdf .
git add -u swish-lite.pdf
git commit --amend --no-edit
git push -f origin
```
