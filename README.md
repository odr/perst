# Perst

This is an attempt to make a persistent functionality on top of type calculation. I.e. I want to guarantee checking of field names, foreign key constraints and so in compile time.

There are three packages:
* grec - Generalised record based on normal Haskell record
* perst - Common persistent functionality
* perst-sqlite - Sqlite interaction

Two other packages aren't used.
