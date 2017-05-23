### Software Transactional Memory for Purescript

Note: this lib is still experimental. Example: See the test folder for .

Software Transactional Memory for purescript, mostly written in Purescript itself.
The only FFI call is to generate random identifier for TVar.

Modules defined here has similiar API with Haskell's STM with following limitations:

- Invariant.
- Exception Handling. Since async exception can't be caught.
