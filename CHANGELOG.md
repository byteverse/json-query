# Revision history for json-query

## 0.2.3.1 -- 2024-01-29

* Update package metadata.

## 0.2.3.0 -- 2024-01-18

* Add `Json.Parser.word32`

## 0.2.2.0 -- 2023-08-09

* Add `Json.Errors.hPut` to make it less verbose to print
  parse errors.
* Add `Json.Parser.foldSmallArray`
* Add variant of `Json.Path.query` that returns Null when path is
  not found.

## 0.2.1.0 -- 2021-07-15

* Allow building with GHC 9.2.3
* Export `unMembers`

## 0.2.0.0 -- 2021-09-07

* Add `Monad` implementation for `MemberParser`.
* Add `Alternative` implementation for `Parser` and `MemberParser`.
* Add `Arrow` interface.
* Add textual descriptions to all errors.
* Track all errors in all choice-like typeclasses.

## 0.1.0.0 -- 2021-03-22

* Initial release.
