## Eonfusion rule sets
writeEOR <- function(pal) {

t1 <- "<?xml version="1.0" encoding="utf-8" ?>
<rulesetgroupclass label="Color">
<rulesetgroup type="Palette">
<rulesets>
<ruleset name="Color" attribute="Vertices.&quot;Band[0]&quot;" numeric="True">
<rules>"


tmp <- "<rule>
<classifiertype>ClosedRange</classifiertype>
<visible>True</visible>
<value>vvv</value>
<consequence>ccc</consequence>
<comments />
</rule>"

t2 <- "
</rules>
</ruleset></rulesets>
</rulesetgroup>
</rulesetgroupclass>"

txt <- character(length(pal$cols) + 2)
txt[1] <- t1
txt[length(txt)] <- t2

for (i in seq_along(pal$cols)) {
  x <- gsub("vvv", paste(pal$breaks[i], pal$breaks[i+1], sep = ":"), tmp)
  x <- gsub("ccc", pal$cols[i], x)
  txt[i + 1] <- x
}
txt

}
