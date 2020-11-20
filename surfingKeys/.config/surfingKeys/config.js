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

// Unmap some keys on Gmail
unmap("e", /mail.google.com/)
unmap("u", /mail.google.com/)
unmap("y", /mail.google.com/)
unmap("x", /mail.google.com/)
unmap("l", /mail.google.com/)
unmap("I", /mail.google.com/)
unmap("<Ctrl-l>", /mail.google.com/)
iunmap("<Ctrl-i>", /mail.google.com/)

// Left align hints
settings.hintAlign = "left"
Hints.characters = "asdfghjkl"

// More emoji
settings.startToShowEmoji = 1

// set theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #24272e;
    color: #abb2bf;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d0d0d0;
}
.sk_theme .url {
    color: #61afef;
}
.sk_theme .annotation {
    color: #56b6c2;
}
.sk_theme .omnibar_highlight {
    color: #528bff;
}
.sk_theme .omnibar_timestamp {
    color: #e5c07b;
}
.sk_theme .omnibar_visitcount {
    color: #98c379;
}
.sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #303030;
}
.sk_theme #sk_omnibarSearchResult>ul>li.focused {
    background: #3e4452;
}
#sk_status, #sk_find {
    font-size: 20pt;
}`
