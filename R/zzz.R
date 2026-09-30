
.onLoad <- function(libname, pkgname) {
  # Build the glyph vocabulary once, by discovering the gs_glyph objects
  # (Glyph*) in this namespace. Cached in .gspace_glyph_cache; see
  # .discover_glyphs()/.glyph_vocab() in gspace-glyph-constructor.R
  .gspace_glyph_cache$vocab <- .discover_glyphs(asNamespace(pkgname))
}
