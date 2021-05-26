# drracket-custom-keybindings

This is a WIP plugin for DrRacket that lets you implement macro-specific keybindings using a domain-specific language for keybinding behavior.

Link to package: https://pkgs.racket-lang.org/package/drracket-custom-keybindings
Link to docs: https://docs.racket-lang.org/kb-base/index.html

Examples: https://github.com/aowens-21/drracket-custom-keybindings/blob/master/kb-base/kb-base/example-macros.rkt

# Future Work

- Improve/Replace kb-base with a `syntax-parse`-like way to describe keybinding code transformations/generation
- Gracefully handle conflicts between keybindings with some GUI window/prompt to the user
