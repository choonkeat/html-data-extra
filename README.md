# HtmlData.Extra

Helper functions to work with [HtmlData](https://package.elm-lang.org/packages/choonkeat/html-data)

- `texthtmlFromHtml` produces a html `String`
- `textplainFromHtml` produces text `String` with reasonable layout to mimic html

## License

BSD

## Why is this a separate package from HtmlData

This package needed several dependencies: `elm/json`, `hecrj/html-parser`, `marcosh/elm-html-to-unicode`. I felt it was unnecessary to impose it on the pure data type package [HtmlData](https://package.elm-lang.org/packages/choonkeat/html-data); anyone else can write their own functions to produce text/html or text/plain `String` with different dependencies than what I've chosen.