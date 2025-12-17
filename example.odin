package perlind_ttf

import "core:fmt"

main :: proc() {
    my_file := #load("./CascadiaCode.ttf")
    ttf_file, ok := ttf_from_data(my_file, context.allocator)
    if ! ok {
        fmt.println("Failed to load ttf file")
        return
    }
    
    id_a := ttf_file.codepoint_to_glyph_index_map['a']
    unhinted_a, unhinted_a_ok := font_get_unhinted_glyph(ttf_file, id_a, context.allocator)
    if ! unhinted_a_ok {
        fmt.println("Failed to load unhinted glyph")
        return
    }

    hinter, hinter_ok := hinter_program_make(ttf_file, 20, 96, context.allocator)
    if ! hinter_ok {
        fmt.println("Failed to make hinter")
        return
    }
    hinted_a, hinted_a_ok := hinter_program_get_hinted_glyph(hinter, id_a, context.allocator)
    if ! hinted_a_ok {
        fmt.println("Failed to load hinted glyph")
        return
    }

    fmt.println("--- Curve points ---")
    assert(len(hinted_a.curves) == len(unhinted_a.curves))
    for i in 0..<len(hinted_a.curves) {
        fmt.printfln("%v hinted: %.3v, unhinted: %.3v", i, hinted_a.curves[i], unhinted_a.curves[i])
    }
    fmt.println("--------------------")
}

