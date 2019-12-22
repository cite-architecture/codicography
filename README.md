# Codicography

**v.1.1.1**

A Scala Library, using the [CITE Architecture](http://cite-architecture.org) to generate static web views of text-bearing artifacts (like manuscripts, codices, and inscriptions).

The generated pages have been tested on Safari, Chrome, and Firefox.

**Status**: active development (see releases.md)

## Running

A simple demo, creating five pages using data from the [Homer Multitext](http://www.homermultitext.org) is:

- `cd` into the `codicographer` directory, 
-  `sbt run`,
- look at the output in directory `facsimile_output/pages`.

This demo uses settings from `configs/config.txt`. **N.b.** The build process takes a while: on a fast laptop, ~ 5 minutes per folio-side of the Venetus A manuscript.


