# template-haskell-optics-0.4 (2026-??-??)
* Drop support for GHC older than 9.2.
* Add prisms for `template-haskell` constructors that were previously missing:
  `_GetFieldE` and `_ProjectionE`; `_LamCasesE`, `_OpaqueP`, `_PromotedInfixT`
  and `_PromotedUInfixT` (`template-haskell-2.19+`); `_TypeDataD`
  (`template-haskell-2.20+`); `_BndrReq`, `_BndrInvis`, `_TypedBracketE` and
  `_TypedSpliceE` (`template-haskell-2.21+`); `_NoNamespaceSpecifier`,
  `_TypeNamespaceSpecifier`, `_DataNamespaceSpecifier`, `_SCCP`, `_TypeE`,
  `_TypeP` and `_InvisP` (`template-haskell-2.22+`); `_ForallE`, `_ForallVisE`,
  `_ConstrainedE` and `_OrP` (`template-haskell-2.23+`); `_SpecialiseEP`
  (`template-haskell-2.24+`).

# template-haskell-optics-0.3 (2023-11-16)
* Add support for GHC 9.4 to 9.8

# template-haskell-optics-0.2 (2022-03-22)
* Add support for GHC 9.0 and 9.2
* Drop the `DataPrim` type synonym

# template-haskell-optics-0.1 (2020-08-29)
* Initial release
