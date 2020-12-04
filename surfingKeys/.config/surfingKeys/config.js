/**
 * SurfingKeys
 *
 * Add vim-like keyboard controls to Chrome.
 *
 * API and usage information:
 * https://github.com/brookhong/Surfingkeys/
 */

map("J", "E") // Go one tab left
map("K", "R") // Go one tab right

map("H", "S") // History back
map("L", "D") // History forward

map("F", "/") // Use SurfingKeys search

// Unmap some keys on Gmail
unmap("e", /mail.google.com/)
unmap("u", /mail.google.com/)
unmap("y", /mail.google.com/)
unmap("x", /mail.google.com/)
unmap("l", /mail.google.com/)
unmap("I", /mail.google.com/)
unmap("<Ctrl-l>", /mail.google.com/)
iunmap("<Ctrl-i>", /mail.google.com/)

// Hints
settings.hintAlign = "left"
Hints.characters = "asdfghjkl"
Hints.style(`
    border: none;
    padding: 3px;
    opacity: 0.9;
    margin-bottom: 3px;
    color: #ebdbb2;
    background: #282828;
    background-color: #282828;
`)

// More emoji
settings.startToShowEmoji = 1

// set theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 12pt;
    background: #282828;
    color: #ebdbb2;
}
.sk_theme tbody {
    color: #b8bb26;
}
.sk_theme input {
    color: #d9dce0;
}
.sk_theme .url {
    color: #98971a;
}
.sk_theme .annotation {
    color: #b16286;
}
.sk_theme .omnibar_highlight {
    color: #333;
    background: #ebdbb2;
}
// .sk_theme .omnibar_timestamp {
//     color: #e5c07b;
// }
.sk_theme .omnibar_visitcount {
    color: #98c379;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #282828;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #d3869b;
}
#sk_status, #sk_find {
    right: 50;
    font-size: 16pt;
    padding-bottom:15px;
    background: #282828;
    color: #ebdbb2;
}
`
