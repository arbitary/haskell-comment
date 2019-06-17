
## Introduction

Haskell Comment Command (HCC) is a command-line program to annotate source code written in Haskell programming language.

## Installation

Clone the repository. Inside the project directory, run `stack install`. This will compile and install file: `hcc` in  directory: `~/.local/bin`

## Usage

```
Usage: hcc COMMAND
  hcc - a command-line program to annotate source code written in Haskell
  programming language.

Available options:
  -h,--help                Show this help text

Available commands:
  add-section              Add a section around the selected text.
  toggle-line              Toggle line comment on/off over the current line.
  toggle-haddock           Toggle Haddock comment on/off over the current line.
  toggle-block             Toggle comment on/off over a range.

```

## Integration

### Vim and iTerm2
[Haskell Comment VIM (HCV)](https://github.com/arbitary/haskell-comment-vim) is a plugin for the VIM editor to interact with the Haskell Comment Command (HCC) line program; this enables you to annotate Haskell programs directly in VIM using features from HCC.
