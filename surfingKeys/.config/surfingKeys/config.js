/**
 * SurfingKeys
 *
 * Add vim-like keyboard controls to Chrome.
 *
 * API and usage information:
 * https://github.com/brookhong/Surfingkeys/
 */

const { map, unmap, Hints } = api

map("J", "E") // Go one tab left
map("K", "R") // Go one tab right

map("h", "S") // History back
map("l", "D") // History forward

map("F", "/") // Use SurfingKeys search

map('<Ctrl-i>', '<Alt-s>'); // Disable SurfingKeys on current site

/**
 * Unmap keyboard shortcuts
 */
unmap("i", /dashboard.stripe.com/)

unmap("u", /mail.google.com/)
unmap("y", /mail.google.com/)
unmap("x", /mail.google.com/)
unmap("I", /mail.google.com/)
unmap("<Ctrl-l>", /mail.google.com/)
unmap("<Ctrl-i>", /mail.google.com/)


// Hints
settings.hintAlign = "left"
Hints.setCharacters = "asdfgiuop"
Hints.style(`
    font-family: SauceCodePro Nerd Font, Consolas, Menlo, monospace;
    font-size: 11px;
    padding: 4px;
    border: 0px;
    opacity: 0.8;
    color: #f0edec;
    background: initial;
    background-color: #1A1C20;
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
