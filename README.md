# Emacs major mode for GNU MathProg using Tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/mathprog-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/mathprog-ts-mode/actions/workflows/CI.yaml)

This is a major mode for [GNU Emacs](https://www.gnu.org/software/emacs/) 29.1 or later which adds support for the MathProg modeling language used by the [GNU Linear Programming Kit](https://www.gnu.org/software/glpk/) modeling language, which is a subset of the [AMPL](https://en.wikipedia.org/wiki/AMPL) language.

## Features

The mode provides the following features and enhancements to help writing MathProg models.

### Syntax highlighting

Syntax highlighting for the following elements is implemented:

- statements
- comments
- strings
- numbers
- functions
- operators

### Navigation

The keybindings <kbd>C-M-a</kbd> and <kbd>C-M-e</kbd> jump to the preceding or following statement.

## Installation

Emacs 29.1 or above with Tree-sitter support is required.

Also the appropriate [parser](https://github.com/smoeding/tree-sitter-mathprog) for the MathProg language needs to be installed. The following Elisp code should be used to install the MathProg language parser.  This requires some tools -- notably a compiler toolchain -- to be available on your machine.

```elisp
(require 'mathprog-ts-mode)
(mathprog-ts-mode-install-grammar)
```

Using the function provided by the package ensures that a version of the parser matching the package will be installed. These commands should also be used to update the parser to the correct version when the package is updated.

## License

MathProg Tree-sitter Mode is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

MathProg Tree-sitter Mode is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the [GNU General Public License](http://www.gnu.org/licenses/) for more details.
