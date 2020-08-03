// an example to create a new mapping `ctrl-y`
mapkey("<Ctrl-b>", "Show me the money", function () {
  Front.showPopup(":moneybag:");
});

// an example to replace `T` with `gt`, click `Default mappings` to see how `T` works.
map("gt", "T");

map("J", "E");
map("K", "R");

map("H", "S");
map("L", "D");

// an example to remove mapkey `Ctrl-i`
unmap("<Ctrl-i>");

// Left align hints
settings.hintAlign = "left";
Hints.characters = "hjklasdfg";

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
}`;
// click `Save` button to make above settings to take effect.
