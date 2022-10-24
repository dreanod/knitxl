xl_obj$reset()

xl_obj$insert_text(
  "* fruits
  + apples
    - macintosh
    - red delicious
  + pears
  + peaches
* vegetables
  + broccoli
  + chard"
)

xl_obj$insert_text(
  "Because pandoc’s intermediate representation of a document is less expressive than many of the formats it converts between, one should not expect perfect conversions between every format and every other. Pandoc attempts to preserve the structural elements of a document, but not formatting details such as margin size. And some document elements, such as complex tables, may not fit into pandoc’s simple document model. While conversions from pandoc’s Markdown to all formats aspire to be perfect, conversions from formats more expressive than pandoc’s Markdown can be expected to be lossy."
)

xl_obj$view()
