package perlind_ttf

Glyph_Bezier_Curve :: struct {
    p0: [2]f32,
    p1: [2]f32,
    p2: [2]f32,
}

Glyph_Bezier :: struct {
    curves: []Glyph_Bezier_Curve,
    dim: [2]f32,
    min, max: [2]f32,
}

