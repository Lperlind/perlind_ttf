# perlind_ttf
An odin ttf parser and hinter. It can parse both ttf and cff fonts with a hinting engine.

The ttf hinting engine is based off of freetype ttf hinter: https://github.com/freetype/freetype/tree/master

The cff virtual machine is based on stb_truetype: https://github.com/nothings/stb/blob/master/stb_truetype.h

This does not do any shaping nor does it do any raserization it simply just extracts the curves.

# Disclaimer

Please note that this has been ripped out of my own personal private project hence relying on some custom arenas. Additionally the curves extracted from the 
hinter api and unhinted code path are all in the form of bezier curves (my gpu solver only does bezier curves). You can keep these as is or replace them
to integrate more with the odin core library.

# Limitations
Currently there are a few fonts that I do not hint correctly, ideally in the future I fix these up but most I have tested work correctly.
Additionally there is no codepath for converting CFF cubic curves into bezier curves (future thing to do) nor do the CFF curves have a hinter.

As previously stated the curves you get out are bezier. You can go digging into the internals if you need something else, I still retain the original
structure of the curves (line segments, bezier, cubics), I just covert them to bezier right at the end.

# Usage

```odin
my_file := #load("./CascadiaCode.ttf")
// your file can be in rodata no problem!
ttf_file, ok := ttf_from_data(my_file, context.allocator)
    
id_a := ttf_file.codepoint_to_glyph_index_map['a'] // typically you would get this from a shaper
unhinted_a, unhinted_a_ok := font_get_unhinted_glyph(ttf_file, id_a, context.allocator) // unhinted bezier curves for 'a'

// create a hinter at 20 ppem with a dpi of 92. You probably want to cache the hinter as it must run a large program on creation.
// you would not realise this if you were using freetype api
hinter, hinter_ok := hinter_program_make(ttf_file, 20, 92, context.allocator)
hinted_a, hinted_a_ok := hinter_program_get_hinted_glyph(hinter, id_a, context.allocator) // hinted bezier curves for 'a'

// cleanup
hinter_program_delete(hinter)
ttf_delete(ttf_file)
```

