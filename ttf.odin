package perlind_ttf

import "core:mem"
import "core:math"
import "core:math/linalg"
import "core:log"
import "core:slice"
import "base:intrinsics"

Ttf_ShortFrac :: i16be
Ttf_Fixed :: i32be
Ttf_Fword :: i16be
Ttf_uFword :: u16be
Ttf_F2Dot14 :: i16be
Ttf_longDateTime :: i64be
Ttf_u32 :: u32be
Ttf_u16 :: u16be
Ttf_i32 :: i32be
Ttf_i16 :: i16be
Ttf_Offset16 :: u16be
Ttf_Offset32 :: u16be
Ttf_Version16Dot16 :: u32be

Cff_Card8 :: u8
Cff_Card16 :: u16be
Cff_Card32 :: u32be
Cff_Offsize:: u8

Ppem :: distinct f32

IDENTITY_TRANSFORM :: matrix[2, 3]f32 {
    1, 0, 0,
    0, 1, 0,
}

Ttf_Transform :: struct {
    a, b, c, d, e, f, m, n: f32
}

Glyph_Coordinate_Type :: enum u8 {
    new_curve,
    point,
    quadratic,
    cubic,
}

Glyph_Curves :: struct {
    coordinates: [][2]f32,
    type: []Glyph_Coordinate_Type,
}

Ttf_Font :: struct {
    codepoint_to_glyph_index_map: map[rune]u16,
    glyphs: []Ttf_Glyph,
    min_glyph_extent, max_glyph_extent: [2]f32,
    units_per_em: f32,
    ascender: f32,
    descender: f32,
    line_gap: f32,

    has_hinting: bool,
    hinting_data_cvt: []Ttf_Fword,
    hinting_data_prep: []byte,
    hinting_stack_size: i32,
    hinting_storage_size: i32,
    hinting_twilight_points_size: i32,
    hinting_op_ptrs: [][]byte,

    arena: ^Arena,
}

Ttf_Read_Context :: struct {
    ok: bool,
    illegal_read_no_message: bool,
}

Ttf_Glyph_Compound :: struct {
    glyph_id: u16,
    transform: matrix[2, 3]f32,
    round_to_grid: bool,
}

Ttf_Glyph_Contour_Point :: struct {
    coord: [2]f32,
    on_curve: bool,
}
Ttf_Glyph :: struct {
    codepoint: rune,
    compound_glyphs: []Ttf_Glyph_Compound,

    ordered_contour_lengths: []u16,
    points: #soa[]Ttf_Glyph_Contour_Point,
    min, max: [2]f32,
    lsb, tsb: f32,
    advance: [2]f32,
    hinting_instructions: []byte,

    gpu_glyph_offset: u32,
    gpu_glyph_len: u32,
}

Ttf_Reader :: struct {
    ctx: ^Ttf_Read_Context,
    data: []byte,
    offset: i64,
}

Ttf_Table_Blob :: struct {
    directory: ^Ttf_Table_Directory,
    data: []byte,
    valid: bool,
}

Ttf_Offset_Subtable :: struct #packed {
    scalar_type: Ttf_u32,
    num_tables: Ttf_u16,
    search_range: Ttf_u16,
    entry_selector: Ttf_u16,
    range_shift: Ttf_u16,
}

Ttf_Table_Directory :: struct #packed {
    tag: Ttf_u32,
    check_sum: Ttf_u32,
    offset: Ttf_u32,
    length: Ttf_u32,
}

Ttf_Character_Map :: struct {
    codepoint: rune,
    glyph_index: u16,
}

TTF_TABLE_HEAD_MAGIC :: 0x5F0F3CF5
Ttf_Table_Head :: struct #packed {
    version: Ttf_Fixed,
    font_revision: Ttf_Fixed,
    check_sum_adjustment: Ttf_u32,
    magic_number: Ttf_u32,
    flags: Ttf_u16,
    units_per_em: Ttf_u16,
    created: Ttf_longDateTime,
    modified: Ttf_longDateTime,
    x_min: Ttf_Fword,
    y_min: Ttf_Fword,
    x_max: Ttf_Fword,
    y_max: Ttf_Fword,
    mac_style: Ttf_u16,
    lowest_rec_ppem: Ttf_u16,
    font_direction_hint: Ttf_i16,
    index_to_loc_format: Ttf_i16,
    glyph_data_format: Ttf_i16,
}

Ttf_Table_GPOS_Header :: struct #packed {
    major_version: Ttf_u16,
    minor_version: Ttf_u16,
    scripts_list_offset: Ttf_u16,
    feature_list_offset: Ttf_u16,
    lookup_list_offset: Ttf_u16,
}

Otf_Table_Lookup_Flag :: enum {
    right_to_left,
    ignore_base_glyphs,
    ignore_ligatures,
    ignore_marks,
    use_mark_filtering_set,
}
Otf_Table_Lookup_Flags :: distinct bit_set[Otf_Table_Lookup_Flag; Ttf_u16]

Otf_Table_Lookup_Header :: struct #packed {
    lookup_count: Ttf_u16,
}

Otf_Subtable_Lookup_Header :: struct #packed {
    lookup_type: Ttf_u16,
    lookup_flag: Otf_Table_Lookup_Flags,
    sub_table_count: Ttf_u16,
}

Otf_Table_Feature_Header :: struct #packed {
    feature_count: Ttf_u16,
}

Otf_Table_Feature_Body :: struct #packed {
    feature_params_offset: Ttf_Offset16,
    lookup_index_count: Ttf_u16,
}

Otf_GPOS_Feature_Tag :: enum Ttf_u32 {
    kern = 0x6B65726E
}

Otf_Feature_Record :: struct #packed {
    feature_tag: Ttf_u32,
    feature_offset: Ttf_Offset16,
}

Otf_Table_Coverage :: struct #packed {
    format: Ttf_u16,
    count: Ttf_u16,
}

Otf_Value_Record :: struct {
    x_placement: Ttf_i16,
    y_placement: Ttf_i16,
    x_advance: Ttf_i16,
    y_advance: Ttf_i16,
    x_pla_device_offset: Ttf_Offset16,
    y_pla_device_offset: Ttf_Offset16,
    x_adv_device_offset: Ttf_Offset16,
    y_adv_device_offset: Ttf_Offset16,
}

Otf_Value_Record_Flag :: enum {
    x_placement = 0,
    y_placement = 1,
    x_advance = 2,
    y_advance = 3,
    x_placement_device = 4,
    y_placement_device = 5,
    x_advance_device = 6,
    y_advance_device = 7,
}
Otf_Value_Record_Format :: bit_set[Otf_Value_Record_Flag; Ttf_u16]

Otf_Table_GPOS_Pair_Pos_Format_1 :: struct #packed {
    format: Ttf_u16,
    coverage_offset: Ttf_Offset16,
    value_format_1: Otf_Value_Record_Format,
    value_format_2: Otf_Value_Record_Format,
    pair_set_count: Ttf_u16,
}

Otf_GPOS_Pair_Pos_Format_1_Pair_Value_Set :: struct {
    pair_value_count: Ttf_u16,
}

Otf_GPOS_Pair_Pos_Format_1_Pair_Value_Record :: struct {
    second_glyph: Ttf_u16,
}

Otf_Table_GPOS_Pair_Pos_Format_2 :: struct #packed {
    format: Ttf_u16,
    coverage_offset: Ttf_Offset16,
    value_format_1: Otf_Value_Record_Format,
    value_format_2: Otf_Value_Record_Format,
    class_def_1_offset: Ttf_Offset16,
    class_def_2_offset: Ttf_Offset16,
    class_1_count: Ttf_u16,
    class_2_count: Ttf_u16,
}

Otf_Coverage_Format_2_Range_Record :: struct #packed {
    start_glyph_id: Ttf_u16,
    end_glyph_id: Ttf_u16,
    start_coverage_index: Ttf_u16,
}

Ttf_Table_Kern :: struct #packed {
    version: Ttf_Fixed,
    n_tables: Ttf_u32,
}

Ttf_Table_OS2 :: struct #packed {
    version: Ttf_u16,
    x_avg_char_width: Ttf_Fword,
    us_weight_class: Ttf_u16,
    us_width_class: Ttf_u16,
    fs_type: Ttf_u16,
    y_subscript_x_size: Ttf_Fword,
    y_subscript_y_size: Ttf_Fword,
    y_subscript_x_offset: Ttf_Fword,
    y_subscript_y_offset: Ttf_Fword,
    y_superscript_x_size: Ttf_Fword,
    y_superscript_y_size: Ttf_Fword,
    y_superscript_x_offset: Ttf_Fword,
    y_superscript_y_offset: Ttf_Fword,
    y_strikeout_size: Ttf_Fword,
    y_strikeout_position: Ttf_Fword,
    s_family_class: Ttf_i16,
    panose: [10]u8,
    ul_unicode_range_1: Ttf_u32,
    ul_unicode_range_2: Ttf_u32,
    ul_unicode_range_3: Ttf_u32,
    ul_unicode_range_4: Ttf_u32,
    ach_vend_id: [4]byte,
    fs_selection: Ttf_u16,
    us_first_char_index: Ttf_u16,
    us_last_char_index: Ttf_u16,
    s_typo_ascender: Ttf_Fword,
    s_typo_descender: Ttf_Fword,
    us_win_ascent: Ttf_uFword,
    us_win_descent: Ttf_uFword,
}

Ttf_Subtable_Kern :: struct #packed {
    length: Ttf_u32,
    coverage: Ttf_u16,
    tuple_index: Ttf_u16,
}

Ttf_Table_Horizontal_Header :: struct #packed {
    major_version: u16be,
    minor_version: u16be,
    ascender: Ttf_Fword,
    descender: Ttf_Fword,
    line_gap: Ttf_Fword,
    advance_width_max: Ttf_uFword,
    min_left_side_bearing: Ttf_Fword,
    min_right_side_bearing: Ttf_Fword,
    x_max_extent: Ttf_Fword,
    caret_slope_rise: i16be,
    caret_slope_run: i16be,
    caret_offset: i16be,
    _: i16be,
    _: i16be,
    _: i16be,
    _: i16be,
    metric_data_format: i16be,
    number_of_h_metrics: i16be,
}

Ttf_Table_Vertical_Header :: struct #packed {
    version: Ttf_Version16Dot16,
    ascent: Ttf_Fword,
    descent: Ttf_Fword,
    line_gap: Ttf_Fword,
    advance_height_max: Ttf_uFword,
    min_top_side_bearing: Ttf_Fword,
    min_bottom_side_bearing: Ttf_Fword,
    y_max_extent: Ttf_Fword,
    caret_slope_rise: i16be,
    caret_slope_run: i16be,
    caret_offset: i16be,
    _: i16be,
    _: i16be,
    _: i16be,
    _: i16be,
    metric_data_format: i16be,
    number_of_v_metrics: i16be,
}

Ttf_Table_Maxp_0_Dot_5 :: struct #packed {
    version: Ttf_Fixed,
    num_glyphs: Ttf_u16,
}

Ttf_Table_Maxp :: struct #packed {
    version: Ttf_Fixed,
    num_glyphs: Ttf_u16,
    max_points: Ttf_u16,
    max_contours: Ttf_u16,
    max_component_points: Ttf_u16,
    max_component_contours: Ttf_u16,
    max_zones: Ttf_u16,
    max_twilight_points: Ttf_u16,
    max_storage: Ttf_u16,
    max_function_defs: Ttf_u16,
    max_instruction_defs: Ttf_u16,
    max_stack_elements: Ttf_u16,
    max_size_of_instructions: Ttf_u16,
    max_component_elements: Ttf_u16,
    max_component_depth: Ttf_u16,
}

Ttf_Platform_Id :: enum Ttf_u16 {
    unicode = 0,
    macintosh = 1,
    microsoft = 3,
}

Ttf_Table_Cmap_Index :: struct #packed {
    version: Ttf_u16,
    number_subtables: Ttf_u16,
}

Ttf_Table_Cmap_Sub :: struct #packed {
    platform_id: Ttf_Platform_Id,
    platform_specific_id: Ttf_u16,
    offset: Ttf_u32,
}

Ttf_Glyph_Loca :: struct {
    offset: u32,
    length: u32,
}

Ttf_Glyf_Table :: struct #packed {
    number_of_contours: Ttf_i16,
    x_min: Ttf_Fword,
    y_min: Ttf_Fword,
    x_max: Ttf_Fword,
    y_max: Ttf_Fword,
}
Ttf_Glyf_Compound_Flag :: enum {
    args_1_and_2_are_words = 0,
    args_are_xy_values = 1,
    round_xy_to_grid = 2,
    we_have_a_scale = 3,
    more_components = 5,
    we_have_an_x_and_y_scale = 6,
    we_have_a_two_by_two = 7,
    we_have_instructions = 8,
    use_my_metrics = 9,
    overlap_compound = 10,
}
Ttf_Glyf_Compound_Flags :: distinct bit_set[Ttf_Glyf_Compound_Flag; Ttf_u16]

Ttf_Glyf_Single_Flag :: enum {
    on_curve = 0,
    x_short_vector = 1,
    y_short_vector = 2,
    repeat = 3,
    x_is_same = 4,
    y_is_same = 5,
}
Ttf_Glyf_Single_Flags :: distinct bit_set[Ttf_Glyf_Single_Flag; u8]

Ttf_Cmap_Format :: enum {
    _4,
    _12,
}
Ttf_Cmap_Formats :: distinct bit_set[Ttf_Cmap_Format]
TTF_CMAP_FORMATS_ALL :: Ttf_Cmap_Formats { ._4, ._12 }

Ttf_Table_Cmap_Format_4 :: struct #packed {
    format: Ttf_u16,
    length: Ttf_u16,
    language: Ttf_u16,
    seg_count_x2: Ttf_u16,
    search_range: Ttf_u16,
    entry_selector: Ttf_u16,
    range_shift: Ttf_u16,
}

Ttf_Table_Cmap_Format_12 :: struct #packed {
    format: Ttf_u16,
    reserved: Ttf_u16,
    length: Ttf_u32,
    language: Ttf_u32,
    n_groups: Ttf_u32,
}

Ttf_Table_Cmap_Format_12_Group :: struct #packed {
    start_char_code: Ttf_u32,
    end_char_code: Ttf_u32,
    start_glyph_code: Ttf_u32,
}

Ttf_Long_Metric_Record :: struct #packed {
    advance: Ttf_u16,
    side_bearing: Ttf_u16,
}

Cff_Header_Table :: struct #packed {
    major: Cff_Card8,
    minor: Cff_Card8,
    hdr_size: Cff_Card8,
    offsize: Cff_Offsize
}

Ttf_Tag :: enum {
    unknown, // NOTE(lucas): not an actual table 
    cmap,
    glyf,
    head,
    hhea,
    hmtx,
    vhea,
    os2,
    vmtx,
    loca,
    maxp,
    name,
    post,
    kern,
    cvt,
    fpgm,
    prep,
    GPOS,
    CFF,
}
Ttf_Tags :: distinct bit_set[Ttf_Tag]
TTF_REQUIRED_TABLES :: Ttf_Tags {
    .cmap,
    .glyf,
    .head,
    .hhea,
    .hmtx,
    .loca,
    .maxp,
    .name,
    .post,
}

TTF_CFF_REQUIRED_TABLES :: Ttf_Tags {
    .cmap,
    .CFF,
    .head,
    .hhea,
    .hmtx,
    .maxp,
    .name,
    .post,
}

ttf_u32_to_tag :: proc(tag: Ttf_u32) -> Ttf_Tag {
    switch tag {
    case 0x636D6170: return .cmap
    case 0x676C7966: return .glyf
    case 0x68656164: return .head
    case 0x68686561: return .hhea
    case 0x686D7478: return .hmtx
    case 0x76686561: return .vhea
    case 0x766D7478: return .vmtx
    case 0x6C6F6361: return .loca
    case 0x6D617870: return .maxp
    case 0x6E616D65: return .name
    case 0x706F7374: return .post
    case 0x6B65726E: return .kern
    case 0x47504F53: return .GPOS
    case 0x43464620: return .CFF
    case 0x63767420: return .cvt
    case 0x70726570: return .prep
    case 0x6670676D: return .fpgm
    case 0x4F532F32: return .os2
    }
    return .unknown
}

@(cold)
_read_fail :: proc(r: ^Ttf_Reader, loc := #caller_location) {
    if r.ctx.ok && ! r.ctx.illegal_read_no_message {
        log.error("[Ttf parser] Illegal read", location = loc)
    }
    r.ctx.ok = false
}

ttf_read_bytes_copy :: proc(r: ^Ttf_Reader, size: i64, ptr: rawptr, loc := #caller_location) -> (bool) #no_bounds_check {
    head, did_overflow := intrinsics.overflow_add(r.offset, size)
    if ! r.ctx.ok || did_overflow || size > i64(max(int)) || head > i64(len(r.data)) || size < 0 {
        _read_fail(r, loc)
        return false
    }
    if ptr != nil && size > 0 {
        mem.copy_non_overlapping(ptr, &r.data[r.offset], int(size))
    }
    r.offset = head
    return true
}

ttf_read_bytes_ptr :: proc(r: ^Ttf_Reader, size: i64, ptr: ^rawptr, loc := #caller_location) -> (bool) #no_bounds_check {
    head, did_overflow := intrinsics.overflow_add(r.offset, size)
    if ! r.ctx.ok || did_overflow || size > i64(max(int)) || head > i64(len(r.data)) || size < 0 {
        _read_fail(r, loc)
        return false
    }
    if ptr != nil && size > 0 {
        ptr^ = &r.data[r.offset]
    }
    r.offset = head
    return true
}

ttf_read_t_copy :: proc($T: typeid, r: ^Ttf_Reader, loc := #caller_location) -> (T, bool) #optional_ok {
    t: T
    ok := ttf_read_bytes_copy(r, size_of(T), &t, loc)
    return t, ok
}

Cff_Index :: struct {
    data: []byte,
}
ttf_read_cff_index :: proc(r: ^Ttf_Reader) -> (Cff_Index, bool) #optional_ok {
    result: Cff_Index
    base := r.offset
    index_count := ttf_read_t_copy(Cff_Card16, r)
    if index_count > 0 {
        off_size := ttf_read_t_copy(Cff_Offsize, r)
        read_count := i64(index_count) + 1
        if off_size < 1 || off_size > 4 {
            r.ctx.ok = false
            return {}, r.ctx.ok
        }
        offsize_data := ttf_read_t_slice(Cff_Card8, r, i64(off_size) * read_count)
        skip_amount := i64(0)
        if r.ctx.ok {
            sub_data := offsize_data[len(offsize_data) - int(off_size):]
            for j in sub_data {
                skip_amount = skip_amount << 8 | i64(j)
            }
        }
        ttf_read_t_slice(Cff_Card8, r, skip_amount - 1)
        end := r.offset
        if r.ctx.ok {
            result = { r.data[base:end] }
        }
    }

    return result, r.ctx.ok
}

cff_index_len :: proc(index: Cff_Index) -> (i64) {
    local_context := Ttf_Read_Context { ok = true, illegal_read_no_message = true }
    reader := Ttf_Reader { &local_context, index.data, 0 }
    return i64(ttf_read_t_copy(Cff_Card16, &reader))
}

cff_index_get :: proc(index: Cff_Index, offset: i64) -> ([]byte, bool) #optional_ok {
    result: []byte
    local_context := Ttf_Read_Context { ok = true, illegal_read_no_message = true }
    reader := Ttf_Reader { &local_context, index.data, 0 }
    index_count := ttf_read_t_copy(Cff_Card16, &reader)
    if index_count > 0 {
        off_size := ttf_read_t_copy(Cff_Offsize, &reader)
        if off_size < 1 || off_size > 4 {
            reader.ctx.ok = false
            return {}, reader.ctx.ok
        }
        index_reader := reader
        reader.offset += offset * i64(off_size)

        base := i64(0)
        base_next := i64(0)
        for _ in 0..<off_size {
            base = base << 8 | i64(ttf_read_t_copy(Cff_Card8, &reader))
        }
        for _ in 0..<off_size {
            base_next = base_next << 8 | i64(ttf_read_t_copy(Cff_Card8, &reader))
        }

        index_reader.offset = 2 + (i64(index_count) + 1)* i64(off_size) + base
        result = ttf_read_t_slice(byte, &index_reader, base_next - base)
    }

    return result, reader.ctx.ok
}

cff_dict_parse_i32_reader :: proc(reader: ^Ttf_Reader, b0: u8 = 0) -> (i32, bool) #optional_ok {
    b0 := b0
    if b0 == 0 {
        b0 = ttf_read_t_copy(u8, reader)
    }
    switch {
    case b0 == 28:
        v, ok := ttf_read_t_copy(Cff_Card16, reader)
        return i32(v), ok
    case b0 == 29:
        v, ok := ttf_read_t_copy(Cff_Card32, reader)
        return i32(v), ok
    case b0 >= 32 && b0 <= 246:
        return i32(b0) - 139, true
    case b0 >= 247 && b0 <= 250:
        b1, ok := ttf_read_t_copy(u8, reader)
        return i32(b0 - 247) * 256 + i32(b1) + 108, ok
    case b0 >= 251 && b0 <= 254:
        b1, ok := ttf_read_t_copy(u8, reader)
        return -i32(b0 - 251) * 256 - i32(b1) - 108, ok
    }
    return 0, false
}

cff_dict_parse_i32_operand :: proc(operands: []byte) -> (i32, bool) #optional_ok {
    ctx := Ttf_Read_Context { ok = true, illegal_read_no_message = true }
    reader := Ttf_Reader { &ctx, operands, 0 } 
    return cff_dict_parse_i32_reader(&reader)
}

cff_dict_parse_i32_operands :: proc(operands: []byte, write_to: []i32) -> bool {
    ctx := Ttf_Read_Context { ok = true, illegal_read_no_message = true }
    reader := Ttf_Reader { &ctx, operands, 0 } 
    ok: bool = true
    for i in 0..<len(write_to) {
        write_to[i], ok = cff_dict_parse_i32_reader(&reader)
        if ! ok {
            break
        }
    }
    return ok
}

cff_subr :: proc(index: Cff_Index, n: i32) -> []byte {
    n := n
    bias := i32(107)
    index_len := cff_index_len(index)
    if index_len >= 33900 {
        bias = 32768
    } else if index_len >= 1240 {
        bias = 1131
    }
    n += bias
    if n < 0 || i64(n) >= index_len {
        return {}
    }
    return cff_index_get(index, i64(n))
}

cff_subrs :: proc(cff: []byte, dict: []byte, allocator: mem.Allocator) -> (Cff_Index, bool) #optional_ok {
    private_loc: [2]i32
    cff_dict_parse_i32_operands(cff_dict_get(dict, 18), private_loc[:])
    if private_loc[0] == 0 || private_loc[1] == 0 {
        return {}, false
    }
    start := private_loc[1]
    end := start + private_loc[0]
    priv_dict := cff[start:end]
    sub_offset := cff_dict_parse_i32_operand(cff_dict_get(priv_dict, 19))
    if sub_offset == 0 {
        return {}, false
    }

    ctx := Ttf_Read_Context { ok = true, illegal_read_no_message = true }
    reader := Ttf_Reader { &ctx, cff, i64(private_loc[1] + sub_offset) } 
    return ttf_read_cff_index(&reader)
}

cff_dict_parse_i32 :: proc {
    cff_dict_parse_i32_reader,
    cff_dict_parse_i32_operand,
    cff_dict_parse_i32_operands,
}

cff_dict_get :: proc(dict: []byte, key: i32) -> ([]byte, bool) #optional_ok {
    ctx := Ttf_Read_Context { ok = true, illegal_read_no_message = true  }
    reader := Ttf_Reader { &ctx, dict, 0 } 
    for ctx.ok {
        start := reader.offset
        op := ttf_read_t_copy(u8, &reader)
        for op >= 28 && ctx.ok {
            if op == 30 {
                consume := ttf_read_t_copy(u8, &reader)
                for ctx.ok && ! ((consume & 0xF) == 0xF || (consume >> 4) == 0xF) {
                    consume = ttf_read_t_copy(u8, &reader)
                }
            } else {
                cff_dict_parse_i32(&reader, op)
            }
            op = ttf_read_t_copy(u8, &reader)
        }
        end := reader.offset - 1
        op_32 := i32(op)
        if op_32 == 12 {
            op_32 = i32(ttf_read_t_copy(u8, &reader)) | 0x100
        }
        if op_32 == key && ctx.ok {
            return dict[start:end], true
        }
    }
    return {}, false
}

ttf_read_t_ptr :: proc($T: typeid, r: ^Ttf_Reader, loc := #caller_location) -> (^T, bool) #optional_ok {
    @static _dummy: T
    t: ^T = &_dummy
    ok := ttf_read_bytes_ptr(r, size_of(T), auto_cast &t, loc)
    return t, ok
}

ttf_read_t_slice :: proc($T: typeid, r: ^Ttf_Reader, len: i64, loc := #caller_location) -> ([]T, bool) #optional_ok {
    t: ^T
    ok := ttf_read_bytes_ptr(r, size_of(T) * len, auto_cast &t, loc)
    if ok {
        return mem.slice_ptr(t, int(len)), true
    } else {
        return {}, false
    }
}

ttf_read_value_record :: proc(r: ^Ttf_Reader, format: Otf_Value_Record_Format, loc := #caller_location) -> (Otf_Value_Record, bool) #optional_ok {
    result: Otf_Value_Record
    if .x_placement in format {
        result.x_placement = ttf_read_t_copy(Ttf_i16, r, loc)
    }
    if .y_placement in format {
        result.y_placement = ttf_read_t_copy(Ttf_i16, r, loc)
    }
    if .x_advance in format {
        result.x_advance = ttf_read_t_copy(Ttf_i16, r, loc)
    }
    if .y_advance in format {
        result.y_advance = ttf_read_t_copy(Ttf_i16, r, loc)
    }
    if .x_placement_device in format {
        result.x_pla_device_offset = ttf_read_t_copy(Ttf_Offset16, r, loc)
    }
    if .y_placement_device in format {
        result.y_pla_device_offset = ttf_read_t_copy(Ttf_Offset16, r, loc)
    }
    if .x_advance_device in format {
        result.x_adv_device_offset = ttf_read_t_copy(Ttf_Offset16, r, loc)
    }
    if .y_advance_device in format {
        result.y_adv_device_offset = ttf_read_t_copy(Ttf_Offset16, r, loc)
    }
    return result, r.ctx.ok
}
ttf_value_record_size :: proc(format: Otf_Value_Record_Format) -> i64 {
    result: i64
    for f in Otf_Value_Record_Flag {
        if f in format {
            result += 2
        }
    }
    return result
}

ttf_get_table_from_directory :: proc(ctx: ^Ttf_Read_Context, offset: i64, length: i64, data: []byte) -> ([]byte, bool) {
    i64_len := i64(len(data))
    table_start := offset
    table_end, did_overflow := intrinsics.overflow_add(offset, length)
    if offset < 0 || length < 0 || table_start > i64_len || table_end > i64_len || did_overflow {
        ctx.ok = false
        return {}, false
    }
    return data[table_start:table_end], true
}

ttf_table_check_sum :: proc(data: []byte) -> Ttf_u32 {
    sum: Ttf_u32
    data_len := len(data)
    for i := 0; i < data_len; i += 4 {
        sum += (cast(^Ttf_u32)(&data[i]))^
    }
    return sum
}

ttf_parse_head_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob) -> (^Ttf_Table_Head, bool) {
    @(static) _dummy: Ttf_Table_Head
    result: ^Ttf_Table_Head = &_dummy
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        head, _ := ttf_read_t_ptr(Ttf_Table_Head, &reader)
        if head.magic_number != TTF_TABLE_HEAD_MAGIC {
            log.error("[Ttf parser] Bad head table magic number")
            ctx.ok = false
        }
        result = head
    } else {
        log.error("[Ttf parser] Bad head table")
        ctx.ok = false
    }
    return result, ctx.ok
}

ttf_parse_maxp_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, scratch: mem.Allocator) -> (^Ttf_Table_Maxp, bool) {
    @(static) _dummy: Ttf_Table_Maxp
    result: ^Ttf_Table_Maxp = &_dummy
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        reader_copy := reader
        version := ttf_read_t_copy(Ttf_Version16Dot16, &reader)
        reader = reader_copy
        switch version {
        case 0x00005000:
            dummy_table := new(Ttf_Table_Maxp, scratch)
            v_0dot5_header := ttf_read_t_ptr(Ttf_Table_Maxp_0_Dot_5, &reader)
            dummy_table.version = v_0dot5_header.version
            dummy_table.num_glyphs = v_0dot5_header.num_glyphs
            result = dummy_table
        case 0x00010000:
            result = ttf_read_t_ptr(Ttf_Table_Maxp, &reader)
        }
    } else {
        log.error("[Ttf parser] Bad maxp table")
        ctx.ok = false
    }
    return result, ctx.ok
}

ttf_parse_hmtx_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, hhea: ^Ttf_Table_Horizontal_Header, glyphs: []Ttf_Glyph) -> (bool) {
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        metrics, _ := ttf_read_t_slice(Ttf_Long_Metric_Record, &reader, i64(hhea.number_of_h_metrics))
        lsb, metrics_ok := ttf_read_t_slice(Ttf_Fword, &reader, i64(len(glyphs)) - i64(hhea.number_of_h_metrics))
        if metrics_ok {
            for m, i in metrics {
                glyphs[i].lsb = f32(i16(m.side_bearing))
                glyphs[i].advance.x = f32(u16(m.advance))
            }
            lsb_i := 0
            advance := f32(u16(hhea.advance_width_max))
            for i in len(metrics)..<len(glyphs) {
                glyphs[i].lsb = f32(i16(lsb[lsb_i]))
                glyphs[i].advance.x = advance

                lsb_i += 1
            }
        }
    } else {
        log.error("[Ttf parser] Bad hmtx table")
        ctx.ok = false
    }
    return ctx.ok
}

ttf_parse_vmtx_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, vhea: ^Ttf_Table_Vertical_Header, glyphs: []Ttf_Glyph) -> (bool) {
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        metrics, _ := ttf_read_t_slice(Ttf_Long_Metric_Record, &reader, i64(vhea.number_of_v_metrics))
        tsb, metrics_ok := ttf_read_t_slice(Ttf_Fword, &reader, i64(len(glyphs)) - i64(vhea.number_of_v_metrics))
        if metrics_ok {
            for m, i in metrics {
                glyphs[i].tsb = f32(i16(m.side_bearing))
                glyphs[i].advance.y = f32(u16(m.advance))
            }
            tsb_i := 0
            advance := f32(u16(vhea.advance_height_max))
            for i in len(metrics)..<len(glyphs) {
                glyphs[i].tsb = f32(i16(tsb[tsb_i]))
                glyphs[i].advance.y = advance

                tsb_i += 1
            }
        }
    } else {
        log.error("[Ttf parser] Bad hmtx table")
        ctx.ok = false
    }
    return ctx.ok
}

ttf_parse_hhea_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob) -> (^Ttf_Table_Horizontal_Header, bool) {
    @(static) _dummy: Ttf_Table_Horizontal_Header
    result: ^Ttf_Table_Horizontal_Header = &_dummy
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        result, _ = ttf_read_t_ptr(Ttf_Table_Horizontal_Header, &reader)
    } else {
        log.error("[Ttf parser] Bad hhea table")
        ctx.ok = false
    }
    return result, ctx.ok
}

ttf_parse_cmap_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, allowed_formats: Ttf_Cmap_Formats, allocator: mem.Allocator) -> ([]Ttf_Character_Map, bool) {
    mapping: []Ttf_Character_Map
    scratch := arena_scratch({ allocator })
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        loca, _ := ttf_read_t_ptr(Ttf_Table_Cmap_Index, &reader)
        subtables, _ := ttf_read_t_slice(Ttf_Table_Cmap_Sub, &reader, i64(loca.number_subtables))

        subtable_reader: Ttf_Reader
        best_format: int = -1
        got_platform_ids := make([]struct { platform_id: Ttf_Platform_Id, platform_specific: int, format: int }, len(subtables), scratch.arena)

        // NOTE(lucas): search for the best subtable to use
        for s, i in subtables {
            supported_platform := false
            got_platform_ids[i] = { s.platform_id, int(s.platform_specific_id), -1 }
            switch s.platform_id {
            case .unicode:
                supported_platform = true
            case .microsoft:
                // NOTE(lucas): 1 and 10 map to unicode for microsoft platform, if it ain't unicode we
                // don't support it
                supported_platform = s.platform_specific_id == 1 || s.platform_specific_id == 10
            case .macintosh:
            }

            offset_i64 := i64(s.offset)
            subtable_data, ok := ttf_get_table_from_directory(ctx, offset_i64, i64(len(table.data)) - offset_i64, table.data)
            if ok {
                maybe_subtable_reader := Ttf_Reader { ctx, subtable_data, 0 }
                format, _ := ttf_read_t_copy(Ttf_u16, &maybe_subtable_reader)
                got_platform_ids[i].format = int(format)
                maybe_subtable_reader.offset = 0
                if supported_platform {
                    switch {
                    case format == 4 && best_format != 12 && ._4 in allowed_formats:
                        best_format = 4
                        subtable_reader = maybe_subtable_reader
                    case format == 12 && ._12 in allowed_formats:
                        best_format = 12
                        subtable_reader = maybe_subtable_reader
                    }
                }
            }
        }

        mapping_i := u32(0)
        // Parse the best subtable if we have one
        switch best_format {
        case 4:
            format_4_header, _ := ttf_read_t_ptr(Ttf_Table_Cmap_Format_4, &subtable_reader)
            seg_count := i64(format_4_header.seg_count_x2 / 2)
            end_codes, _ := ttf_read_t_slice(Ttf_u16, &subtable_reader, seg_count)
            ttf_read_t_copy(Ttf_u16, &subtable_reader)
            start_codes, _ := ttf_read_t_slice(Ttf_u16, &subtable_reader, seg_count)
            id_deltas, _ := ttf_read_t_slice(Ttf_u16, &subtable_reader, seg_count)
            id_range_offset, _ := ttf_read_t_slice(Ttf_u16, &subtable_reader, seg_count)
            if ctx.ok {
                end_of_table := uintptr(&subtable_reader.data[len(subtable_reader.data) - 1])

                sparse_lookup := soa_zip(end = end_codes, start = start_codes, delta = id_deltas, offset = id_range_offset)

                n_maps: u32
                for lookup in sparse_lookup {
                    n_maps += u32(lookup.end) - u32(lookup.start) + 1
                }

                mapping = make([]Ttf_Character_Map, n_maps, allocator)

                for lookup, i in sparse_lookup {
                    base_offset := lookup.offset
                    for c in lookup.start..=lookup.end {
                        if lookup.offset == 0 {
                            mapping[mapping_i] = { rune(c), u16(c + lookup.delta) }
                            mapping_i += 1
                        } else {
                            index_of_mapping_byte := base_offset + 2 * (c - lookup.start)
                            delta_location := uintptr(&id_range_offset[i]) + uintptr(index_of_mapping_byte)
                            if delta_location + 1 <= end_of_table {
                                glyph_index := (cast(^u16be)delta_location)^
                                if glyph_index != 0 {
                                    glyph_index += lookup.delta
                                }
                                mapping[mapping_i] = { rune(c), u16(glyph_index) }
                                mapping_i += 1
                            } else {
                                log.error("[Ttf parser] Bad cmap table")
                                ctx.ok = false
                            }
                        }
                    }
                }
            }
        case 12:
            format_12_header, _ := ttf_read_t_ptr(Ttf_Table_Cmap_Format_12, &subtable_reader)

            subtable_reader_copy := subtable_reader

            n_maps: u32
            for _ in 0..<format_12_header.n_groups {
                group, _ := ttf_read_t_ptr(Ttf_Table_Cmap_Format_12_Group, &subtable_reader)
                n_maps += u32(group.end_char_code - group.start_char_code) + 1
            }

            subtable_reader = subtable_reader_copy

            mapping = make([]Ttf_Character_Map, n_maps, allocator)
            for _ in 0..<format_12_header.n_groups {
                group, _ := ttf_read_t_ptr(Ttf_Table_Cmap_Format_12_Group, &subtable_reader)
                for g in group.start_char_code..=group.end_char_code {
                    mapping[mapping_i] = { rune(g), u16(group.start_glyph_code + g - group.start_char_code) }
                    mapping_i += 1
                }
            }
        case:
            log.errorf("[Ttf parser] Unsupported cmap table, got: %v", got_platform_ids)
            ctx.ok = false
        }
    } else {
        log.error("[Ttf parser] Bad cmap table")
        ctx.ok = false
    }
    return mapping, ctx.ok
}

ttf_parse_loca_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, head: ^Ttf_Table_Head, maxp: ^Ttf_Table_Maxp, allocator: mem.Allocator) -> ([]Ttf_Glyph_Loca, bool) {
    result: []Ttf_Glyph_Loca
    if table.valid {
        result = make([]Ttf_Glyph_Loca, maxp.num_glyphs, allocator)
        reader := Ttf_Reader { ctx, table.data, 0 }
        if head.index_to_loc_format == 0 {
            shorts, shorts_ok := ttf_read_t_slice(Ttf_u16, &reader, i64(len(result) + 1))
            if shorts_ok {
                for _, i in result {
                    result[i] = {
                        u32(shorts[i]) * 2,
                        u32(shorts[i + 1] - shorts[i]) * 2
                    }
                }
            }
        } else {
            longs, longs_ok := ttf_read_t_slice(Ttf_u32, &reader, i64(len(result) + 1))
            if longs_ok {
                for _, i in result {
                     result[i] = {
                        u32(longs[i]),
                        u32((longs[i + 1] - longs[i]))
                    }
                }
            }
        }
    } else {
        log.error("[Ttf parser] Bad loca table")
        ctx.ok = false
    }
    return result, ctx.ok
}

Ttf_Parse_Glyf_Table_Result :: struct {
    glyphs: []Ttf_Glyph,
    global_min: [2]f32,
    global_max: [2]f32,
}

ttf_parse_glyf_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, locas: []Ttf_Glyph_Loca, maxp: ^Ttf_Table_Maxp, allocator: mem.Allocator, scratch: ^Arena) -> (Ttf_Parse_Glyf_Table_Result, bool) {
    glyphs := make([]Ttf_Glyph, len(locas), allocator)
    global_min: [2]f32 = math.INF_F32
    global_max: [2]f32 = math.NEG_INF_F32
    glyphs_i := 0
    // NOTE(lucas): we use u64 here because loca contains u32's
    table_len := u64(len(table.data))
    for loca in locas[:] {
        defer glyphs_i += 1
        // This is equivalent to an empty single glyph
        if loca.length == 0 {
            continue
        }
        start_offset := u64(loca.offset)
        end_offset := u64(loca.offset) + u64(loca.length)
        if start_offset > table_len || end_offset > table_len {
            log.error("[Ttf parser] Bad glyf table")
            ctx.ok = false
            continue
        }
        reader_data := table.data[start_offset:end_offset]
        reader := Ttf_Reader { ctx, reader_data, 0 }
        head, _ := ttf_read_t_copy(Ttf_Glyf_Table, &reader)
        if head.number_of_contours < 0 { // Compound glpyh
            arena_temp_scope(scratch)
            compound_glyphs := make([dynamic]Ttf_Glyph_Compound, scratch)
            has_instructions := false
            round_transform_matrix := false
            flags := Ttf_Glyf_Compound_Flags { .more_components }
            for ctx.ok && (.more_components in flags) {
                flags = ttf_read_t_copy(Ttf_Glyf_Compound_Flags, &reader)
                glyph_index := ttf_read_t_copy(Ttf_u16, &reader)

                transform: matrix[2, 3]f32 = {
                    1, 0, 0,
                    0, 1, 0
                }

                arg_1, arg_2: f32
                if .args_1_and_2_are_words in flags {
                    if .args_are_xy_values in flags {
                        arg_1 = f32(i16(ttf_read_t_copy(Ttf_i16, &reader)))
                        arg_2 = f32(i16(ttf_read_t_copy(Ttf_i16, &reader)))
                    } else {
                        arg_1 = f32(u16(ttf_read_t_copy(Ttf_u16, &reader)))
                        arg_2 = f32(u16(ttf_read_t_copy(Ttf_u16, &reader)))
                    }
                } else {
                    if .args_are_xy_values in flags {
                        arg_1 = f32(ttf_read_t_copy(i8, &reader))
                        arg_2 = f32(ttf_read_t_copy(i8, &reader))
                    } else {
                        arg_1 = f32(ttf_read_t_copy(u8, &reader))
                        arg_2 = f32(ttf_read_t_copy(u8, &reader))
                    }
                }
                transform[0, 2] = arg_1
                transform[1, 2] = arg_2

                F2DOT_DIV :: f32(16384)
                if .we_have_a_scale in flags {
                    scale := f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                    transform[0, 0] = scale
                    transform[1, 1] = scale
                } else if .we_have_an_x_and_y_scale in flags {
                    transform[0, 0] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                    transform[1, 1] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                } else if .we_have_a_two_by_two in flags {
                    transform[0, 0] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                    transform[0, 1] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                    transform[1, 0] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                    transform[1, 1] = f32(i16(ttf_read_t_copy(Ttf_F2Dot14, &reader))) / F2DOT_DIV
                }
                has_instructions |= .we_have_instructions in flags
                round_transform_matrix |= .round_xy_to_grid in flags

                append(&compound_glyphs, Ttf_Glyph_Compound { u16(glyph_index), transform, round_transform_matrix })
            }
            instructions: []byte
            if has_instructions {
                num_instructions := ttf_read_t_copy(Ttf_u16, &reader)
                instructions = ttf_read_t_slice(byte, &reader, i64(num_instructions))
            }

            glyphs[glyphs_i] = {
                compound_glyphs = slice.clone(compound_glyphs[:], allocator),
                hinting_instructions = instructions,
                // NOTE(lucas): set later when checking for cycles
                min = math.INF_F32,
                max = math.NEG_INF_F32,
            }
        } else {
            end_pt_of_contours := ttf_read_t_slice(Ttf_u16, &reader, i64(head.number_of_contours))
            number_of_points := len(end_pt_of_contours) == 0 ? 0 : int(end_pt_of_contours[len(end_pt_of_contours) - 1]) + 1
            instruction_length := ttf_read_t_copy(Ttf_u16, &reader)
            instructions := ttf_read_t_slice(u8, &reader, i64(instruction_length))
            flags: []Ttf_Glyf_Single_Flags
            // NOTE(lucas): parse flags
            {
                reader_copy := reader
                flags_parsed := 0
                flags_bytes_parsed := 0
                for flags_parsed < number_of_points {
                    flag := ttf_read_t_copy(Ttf_Glyf_Single_Flags, &reader)
                    flags_bytes_parsed += 1
                    flags_parsed += 1
                    if .repeat in flag {
                        flags_parsed += int(ttf_read_t_copy(u8, &reader))
                        flags_bytes_parsed += 1
                    }
                }
                reader = reader_copy
                flags = ttf_read_t_slice(Ttf_Glyf_Single_Flags, &reader, i64(flags_bytes_parsed))
            }
            // NOTE(lucas): parse coordinates
            _parse_coordinate :: proc(short_vec_flag: Ttf_Glyf_Single_Flag, is_same_flag: Ttf_Glyf_Single_Flag, number_of_points: int, flags: []Ttf_Glyf_Single_Flags, reader: ^Ttf_Reader, write_coordinate: [][2]f32, coord_index: int, write_on_curve_points: []bool) {
                flags_parsed := 0
                coordinates_added := 0
                prev_coordinate: i16
                for flags_parsed < len(flags) {
                    flag := flags[flags_parsed]
                    flags_parsed += 1
                    repeat_count := 1
                    if .repeat in flag {
                        repeat_count = int((transmute(u8)flags[flags_parsed])) + 1
                        flags_parsed += 1
                    }
                    is_short := short_vec_flag in flag
                    is_same := is_same_flag in flag
                    for _ in 0..<repeat_count {
                        coordinate: i16
                        switch {
                        case is_short && is_same:
                            coordinate = i16(ttf_read_t_copy(u8, reader))
                        case is_short && ! is_same:
                            coordinate = -i16(ttf_read_t_copy(u8, reader))
                        case ! is_short && ! is_same:
                            coordinate = i16(ttf_read_t_copy(Ttf_i16, reader))
                        }
                        coordinate += prev_coordinate
                        prev_coordinate = coordinate
                        write_coordinate[coordinates_added][coord_index] = f32(coordinate)
                        write_on_curve_points[coordinates_added] = .on_curve in flag
                        coordinates_added += 1
                    }
                }
            }
            glyph: Ttf_Glyph
            glyph.points = make(#soa[]Ttf_Glyph_Contour_Point, number_of_points, allocator)
            glyph.ordered_contour_lengths = make([]u16, len(end_pt_of_contours), allocator)
            max_potential_length := i64(0)
            for contour, i in end_pt_of_contours {
                glyph.ordered_contour_lengths[i] = u16(contour)
                // NOTE(lucas): we add 1 again to account for the curves having the last
                // point set to the first point
                max_potential_length += i64(glyph.ordered_contour_lengths[i]) + 1
                if (i > 0 && glyph.ordered_contour_lengths[i - 1] >= glyph.ordered_contour_lengths[i]) {
                    log.error("[Ttf parser] Bad glyf table")
                    ctx.ok = false
                }
            }
            coord, on_curve := soa_unzip(glyph.points)
            _parse_coordinate(.x_short_vector, .x_is_same, number_of_points, flags, &reader, coord, 0, on_curve)
            _parse_coordinate(.y_short_vector, .y_is_same, number_of_points, flags, &reader, coord, 1, on_curve)
            glyph.min = { f32(i16(head.x_min)), f32(i16(head.y_min)) }
            glyph.max = { f32(i16(head.x_max)), f32(i16(head.y_max)) }
            global_min = linalg.min(glyph.min, global_min)
            global_max = linalg.max(glyph.max, global_max)
            glyph.hinting_instructions = slice.clone(instructions, allocator)
            glyphs[glyphs_i] = glyph
        }
    }

    // NOTE(lucas): patch up compound glyph sizes and ensure no cycles in the glyphs
    _verify_compound :: proc(index: u16, glyphs: []Ttf_Glyph, touched: []bool) -> (bool) {
        if touched[index] {
            return false
        }
        touched[index] = true
        for c in glyphs[index].compound_glyphs {
            if ! _verify_compound(c.glyph_id, glyphs, touched) {
                return false
            }
        }
        touched[index] = false
        return true
    }

    _set_compound_glyph_size :: proc(index: u16, glyphs: []Ttf_Glyph) -> ([2]f32, [2]f32) {
        g := &glyphs[index]
        if len(g.compound_glyphs) > 0 {
            for c in g.compound_glyphs {
                min, max := _set_compound_glyph_size(c.glyph_id, glyphs)
                g.min = linalg.min(min, g.min)
                g.max = linalg.max(max, g.max)
            }
            return g.min, g.max
        } else {
            return g.min, g.max
        }
    }

    touched := make([]bool, len(glyphs), scratch)
    for i in 0..<len(glyphs) {
        if ! _verify_compound(u16(i), glyphs, touched) {
            log.error("[Ttf parser] Cyclic compound glyph in glyf table")
            ctx.ok = false
            break
        } else {
            _set_compound_glyph_size(u16(i), glyphs)
        }
    }

    result := Ttf_Parse_Glyf_Table_Result {
        glyphs,
        global_min,
        global_max,
    }

    return result, ctx.ok
}


ttf_parse_cff_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, maxp: ^Ttf_Table_Maxp, allocator: mem.Allocator) -> (Ttf_Parse_Glyf_Table_Result, bool) {
    glyphs: []Ttf_Glyph
    global_min: [2]f32 = math.INF_F32
    global_max: [2]f32 = math.NEG_INF_F32
    scratch := arena_scratch({ allocator })
    if table.valid {
        glyphs = make([]Ttf_Glyph, int(maxp.num_glyphs), allocator)

        reader := Ttf_Reader { ctx, table.data, 0 }
        is_cff2 := table.directory.tag == 0x43464632
        _ = is_cff2

        cff_header := ttf_read_t_ptr(Cff_Header_Table, &reader)
        if cff_header.major != 1 && cff_header.minor != 0 {
            log.error("[Ttf parser] Unsupported CFF(2) table")
            ctx.ok = false
            return {}, false
        }

        name_index := ttf_read_cff_index(&reader)
        _ = name_index
        top_dict_index := ttf_read_cff_index(&reader)
        if cff_index_len(top_dict_index) == 0 {
            log.error("[Ttf parser] Bad CFF(2) table, at least 1 top dict is required")
            ctx.ok = false
            return {}, false
        }
        top_dict := cff_index_get(top_dict_index, 0)
        string_index := ttf_read_cff_index(&reader)
        _ = string_index
        global_subr_index := ttf_read_cff_index(&reader)

        char_strings_off := cff_dict_parse_i32(cff_dict_get(top_dict, 17))
        cstype := cff_dict_parse_i32(cff_dict_get(top_dict, 0x100 | 6)) or_else 2
        fd_array_off := cff_dict_parse_i32(cff_dict_get(top_dict, 0x100 | 36))
        fd_select_off := cff_dict_parse_i32(cff_dict_get(top_dict, 0x100 | 37))
        subrs := cff_subrs(table.data, top_dict, scratch.arena)


          // we only support Type 2 charstrings
        if cstype != 2 {
            log.error("[Ttf parser] Unsupported CFF(2) table, only Type 2 charstrings are allowed")
            ctx.ok = false
            return {}, false
          }
        if char_strings_off == 0 {
            log.error("[Ttf parser] Bad CFF(2) table, missing charstrings")
        }

        char_strings_index_reader := Ttf_Reader { ctx, table.data, i64(char_strings_off) }
        char_strings_index := ttf_read_cff_index(&char_strings_index_reader)
        if cff_index_len(char_strings_index) < i64(maxp.num_glyphs) {
            log.error("[Ttf parser] Bad CFF(2) table")
            ctx.ok = false
            return {}, false
        }

        font_dict_index: Cff_Index
        font_dict_select_data: []byte
        if fd_array_off != 0 {
            font_dict_index_reader := Ttf_Reader { ctx, table.data, i64(fd_array_off) }
            font_dict_index = ttf_read_cff_index(&font_dict_index_reader)
            if int(fd_select_off) > len(table.data) {
                log.error("[Ttf parser] Unsupported CFF(2) table, font select is out of bounds")
                ctx.ok = false
                return {}, false
            } else {
                font_dict_select_data = table.data[fd_select_off:]
            }
        }

        for glyph_idx in 0..<maxp.num_glyphs {
            arena_temp_scope(scratch.arena)
            glyph_data := cff_index_get(char_strings_index, i64(glyph_idx))
            _ = glyph_data

            types := make([dynamic]Glyph_Coordinate_Type, 0, 128, scratch.arena)
            points := make([dynamic][2]f32, 0, 256, scratch.arena)
            Cff_Builder_Context :: struct {
                types: ^[dynamic]Glyph_Coordinate_Type,
                points: ^[dynamic][2]f32,
                start: [2]f32,
                current: [2]f32,
                min: [2]f32,
                max: [2]f32,
                debug: bool,
            }

            // NOTE(lucas): this is a port from stb truetype
            in_header: bool = true
            clear_stack: bool
            has_local_subrs: bool
            maskbits, subr_stack_height: i32
            local_subrs: Cff_Index = subrs
            sp: i32 // NOTE(lucas): stack pointer
            // NOTE(lucas): we up the stack size to 513 from 48 for CFF2 fonts
            stack: [513]f32 = ---
            Stack_Frame :: struct {
                reader: Ttf_Reader
            }
            subr_stack: [10]Stack_Frame
            builder_ctx: Cff_Builder_Context = {
                &types,
                &points,
                0, 0,
                math.INF_F32, math.NEG_INF_F32,
                false
            }

            _builder_ctx_push :: proc(ctx: ^Cff_Builder_Context, coord: [][2]f32, type: Glyph_Coordinate_Type) {
                append(ctx.types, type)
                append(ctx.points, ..coord)
                if len(coord) > 0 {
                    ctx.current = coord[len(coord) - 1]
                    for c in coord {
                        ctx.min = linalg.min(c, ctx.min)
                        ctx.max = linalg.max(c, ctx.max)
                    }
                }
            }

            _builder_ctx_close :: proc(ctx: ^Cff_Builder_Context) {
                if ctx.start != ctx.current {
                    current := ctx.current
                    _builder_ctx_push(ctx, { ctx.start }, .point)
                    ctx.current = current
                }
            }

            _builder_ctx_move_to :: proc(ctx: ^Cff_Builder_Context, delta: [2]f32) {
                _builder_ctx_close(ctx)
                _builder_ctx_push(ctx, {}, .new_curve)
                coord := ctx.current + delta
                ctx.start = coord
                _builder_ctx_push(ctx, { coord }, .point)
            }

            _builder_ctx_rline_to :: proc(ctx: ^Cff_Builder_Context, delta: [2]f32) {
                coord := ctx.current + delta
                _builder_ctx_push(ctx, { coord }, .point)
            }

            _builder_ctx_rcurve_to :: proc(ctx: ^Cff_Builder_Context, delta_0: [2]f32, delta_1: [2]f32, delta_2: [2]f32) {
                coord_1 := ctx.current + delta_0
                coord_2 := coord_1 + delta_1
                coord_3 := coord_2 + delta_2
                _builder_ctx_push(ctx, { coord_1, coord_2, coord_3 }, .cubic)
            }

            current_frame := Stack_Frame { Ttf_Reader { ctx, glyph_data, 0 } }
            instructions_finished := false
            for current_frame.reader.offset < i64(len(current_frame.reader.data)) && ctx.ok && ! instructions_finished {
                i := i32(0)
                clear_stack = true
                b0 := ttf_read_t_copy(Cff_Card8, &current_frame.reader)
                switch b0 {
                case 0x13: fallthrough // hintmask
                case 0x14: // cntrmask
                    if in_header {
                        maskbits += sp / 2
                    }
                    in_header = false
                    current_frame.reader.offset += i64(maskbits + 7) / 8

                case 0x01: fallthrough // hstem
                case 0x03: fallthrough // vstem
                case 0x12: fallthrough // hstemhm
                case 0x17: // vstemhm
                    maskbits += sp / 2

                case 0x15: // rmoveto
                    in_header = false
                    if sp < 2 {
                        log.error("[Ttf parser] CFF interpreter, rmoveto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    _builder_ctx_move_to(&builder_ctx, { stack[sp - 2], stack[sp - 1] })
                case 0x04: // vmoveto
                    in_header = false
                    if sp < 1 {
                        log.error("[Ttf parser] CFF interpreter, vmoveto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    _builder_ctx_move_to(&builder_ctx, { 0, stack[sp - 1] })
                case 0x16: // hmoveto
                    in_header = false
                    if sp < 1 {
                        log.error("[Ttf parser] CFF interpreter, hmoveto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    _builder_ctx_move_to(&builder_ctx, { stack[sp - 1], 0 })
                case 0x05: // rlineto
                    in_header = false
                    if sp < 2 {
                        log.error("[Ttf parser] CFF interpreter, rlineto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    for ; i + 1 < sp; i += 2 {
                        _builder_ctx_rline_to(&builder_ctx, { stack[i], stack[i + 1] })
                    }
                case 0x07: // vlineto
                    if sp < 1 {
                        log.error("[Ttf parser] CFF interpreter, vlineto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    for {
                        if i >= sp {
                            break
                        }
                        _builder_ctx_rline_to(&builder_ctx, { 0, stack[i] })
                        i += 1
                        if i >= sp {
                            break
                        }
                        _builder_ctx_rline_to(&builder_ctx, { stack[i], 0 })
                        i += 1
                    }
                case 0x06: // hlineto
                    if sp < 1 {
                        log.error("[Ttf parser] CFF interpreter, hlineto")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    for {
                        if i >= sp {
                            break
                        }
                        _builder_ctx_rline_to(&builder_ctx, { stack[i], 0 })
                        i += 1
                        if i >= sp {
                            break
                        }
                        _builder_ctx_rline_to(&builder_ctx, { 0, stack[i] })
                        i += 1
                    }
                case 0x1F: // hvcurveto
                    if sp < 4 {
                        log.error("[Ttf parser] CFF interpreter, hvcurveto")
                        ctx.ok = false
                        return {}, false
                    }
                    for {
                        if i + 3 >= sp {
                            break
                        }
                        _builder_ctx_rcurve_to(&builder_ctx, { stack[i], 0 }, { stack[i+1], stack[i+2] }, { (sp - i == 5) ? stack[i + 4] : 0, stack[i+3] })
                        i += 4
                        if i + 3 >= sp {
                            break
                        }
                        _builder_ctx_rcurve_to(&builder_ctx, { 0, stack[i] }, { stack[i+1], stack[i+2] }, { stack[i+3], (sp - i == 5) ? stack[i + 4] : 0 })
                        i += 4
                    }
                case 0x1E: // vhcurveto
                    if sp < 4 {
                        log.error("[Ttf parser] CFF interpreter, vhcurveto")
                        ctx.ok = false
                        return {}, false
                    }
                    for {
                        if i + 3 >= sp {
                            break
                        }
                        _builder_ctx_rcurve_to(&builder_ctx, { 0, stack[i] }, { stack[i+1], stack[i+2] }, { stack[i+3], (sp - i == 5) ? stack[i + 4] : 0 })
                        i += 4
                        if i + 3 >= sp {
                            break
                        }
                        _builder_ctx_rcurve_to(&builder_ctx, { stack[i], 0 }, { stack[i+1], stack[i+2] }, { (sp - i == 5) ? stack[i + 4] : 0, stack[i+3] })
                        i += 4
                    }
                case 0x08: // rrcurveto
                    if sp < 6 {
                        log.error("[Ttf parser] CFF interpreter, rrcurveto")
                        ctx.ok = false
                        return {}, false
                    }
                    for ; i + 5 < sp; i += 6 {
                        _builder_ctx_rcurve_to(&builder_ctx, { stack[i], stack[i + 1] }, { stack[i+2], stack[i+3] }, { stack[i + 4], stack[i+5] })
                    }
                case 0x18: // rcurveline
                    if sp < 8 {
                        log.error("[Ttf parser] CFF interpreter, rcurveline")
                        ctx.ok = false
                        return {}, false
                    }
                    for ; i + 5 < sp - 2; i += 6 {
                        _builder_ctx_rcurve_to(&builder_ctx, { stack[i], stack[i + 1] }, { stack[i+2], stack[i+3] }, { stack[i + 4], stack[i+5] })
                    }
                    if i + 1 >= sp {
                        log.error("[Ttf parser] CFF interpreter, rcurveline")
                        ctx.ok = false
                        return {}, false
                    }
                    _builder_ctx_rline_to(&builder_ctx, { stack[i], stack[i + 1] })
                case 0x19: // rlinecurve
                    if sp < 8 {
                        log.error("[Ttf parser] CFF interpreter, rlinecurve")
                        ctx.ok = false
                        return {}, false
                    }
                    for ; i + 1 < sp - 6; i += 2 {
                        _builder_ctx_rline_to(&builder_ctx, { stack[i], stack[i + 1] })
                    }
                    if i + 5 >= sp {
                        log.error("[Ttf parser] CFF interpreter, rlinecurve")
                        ctx.ok = false
                        return {}, false
                    }
                    _builder_ctx_rcurve_to(&builder_ctx, { stack[i], stack[i + 1] }, { stack[i+2], stack[i+3] }, { stack[i + 4], stack[i+5] })
                case 0x1A, 0x1B: // vvcurveto, hhcurveto
                    if sp < 4 {
                        log.error("[Ttf parser] CFF interpreter, vvcurveto, hhcurveto")
                        ctx.ok = false
                        return {}, false
                    }
                    f := f32(0)
                    if sp & 1 != 0 {
                        f = stack[i]
                        i += 1
                    }
                    if b0 == 0x1B {
                        for ; i + 3 < sp; i += 4 {
                            _builder_ctx_rcurve_to(&builder_ctx, { stack[i], f }, { stack[i + 1], stack[i + 2] }, { stack[i + 3], 0 } )
                            f = 0
                        }
                    } else {
                        for ; i + 3 < sp; i += 4 {
                            _builder_ctx_rcurve_to(&builder_ctx, { f, stack[i] }, { stack[i + 1], stack[i + 2] }, { 0, stack[i + 3] } )
                            f = 0
                        }
                    }
                case 0x0A: // callsubr
                    if ! has_local_subrs {
                        if len(font_dict_select_data) > 0 {
                            fd_reader := Ttf_Reader{ ctx, font_dict_select_data, 0 }
                            format := ttf_read_t_copy(Cff_Card8, &fd_reader)
                            fd_select := i64(-1)
                            switch format {
                            case 0:
                                fd_selectors := ttf_read_t_slice(Cff_Card8, &fd_reader, i64(len(glyphs)))
                                if len(fd_selectors) <= int(glyph_idx) {
                                    ctx.ok = false
                                } else {
                                    fd_select = i64(fd_selectors[glyph_idx])
                                }
                            case 3:
                                n_ranges := i64(ttf_read_t_copy(Cff_Card16, &fd_reader))
                                start := i64(ttf_read_t_copy(Cff_Card16, &fd_reader))
                                for i := i64(0); i < n_ranges; i += 1 {
                                    v := i64(ttf_read_t_copy(Cff_Card8, &fd_reader))
                                    end := i64(ttf_read_t_copy(Cff_Card16, &fd_reader))
                                    if i64(glyph_idx) >= start && i64(glyph_idx) < end {
                                        fd_select = v
                                        break
                                    }
                                    start = end
                                }
                            case: ctx.ok = false

                            }

                            if fd_select >= 0 {
                                if fd_select >= cff_index_len(font_dict_index) {
                                    ctx.ok = false
                                } else {
                                    sub_dict := cff_index_get(font_dict_index, fd_select)
                                    local_subrs = cff_subrs(table.data, sub_dict, scratch.arena)
                                }
                            }

                            if ! ctx.ok {
                                log.error("[Ttf parser] CFF interpreter, callsubr")
                                return {}, ctx.ok
                            }
                        }
                        has_local_subrs = true
                    }
                    fallthrough
                case 0x1D: // callgsubr
                    if sp < 1 {
                        log.error("[Ttf parser] CFF interpreter, callsubr, callgsubr")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    sp -= 1
                    v := i32(stack[sp])
                    if subr_stack_height >= 10 {
                        log.error("[Ttf parser] CFF interpreter, callsubr, callgsubr")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    subr_stack[subr_stack_height] = current_frame
                    subr_stack_height += 1
                    subroutine := b0 == 0x0A ? local_subrs : global_subr_index
                    current_frame = { { ctx, cff_subr(subroutine, v), 0 }}
                    clear_stack = false
                    
                case 0x0B: // return
                    if subr_stack_height <= 0 {
                        log.error("[Ttf parser] CFF interpreter, return")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    subr_stack_height -= 1
                    current_frame = subr_stack[subr_stack_height]
                    clear_stack = false

                case 0x0E: // endchar
                    _builder_ctx_close(&builder_ctx)
                    instructions_finished = true

                case 0x0C: // two-byte escape
                    b1 := ttf_read_t_copy(Cff_Card8, &current_frame.reader)
                    switch b1 {
                    case 0x22: // hflex
                        if sp < 7 {
                            log.error("[Ttf parser] CFF interpreter, hflex")
                            ctx.ok = false
                            return {}, ctx.ok
                        }
                        dx1 := stack[0]
                        dx2 := stack[1]
                        dy2 := stack[2]
                        dx3 := stack[3]
                        dx4 := stack[4]
                        dx5 := stack[5]
                        dx6 := stack[6]
                        _builder_ctx_rcurve_to(&builder_ctx, { dx1, 0 }, { dx2, dy2 }, { dx3, 0 })
                        _builder_ctx_rcurve_to(&builder_ctx, { dx4, 0 }, { dx5, -dy2 }, { dx6, 0 })
                    case 0x23: // flex
                        if sp < 13 {
                            log.error("[Ttf parser] CFF interpreter, flex")
                            ctx.ok = false
                            return {}, ctx.ok
                        }
                        dx1 := stack[0]
                        dy1 := stack[1]
                        dx2 := stack[2]
                        dy2 := stack[3]
                        dx3 := stack[4]
                        dy3 := stack[5]
                        dx4 := stack[6]
                        dy4 := stack[7]
                        dx5 := stack[8]
                        dy5 := stack[9]
                        dx6 := stack[10]
                        dy6 := stack[11]
                        _builder_ctx_rcurve_to(&builder_ctx, { dx1, dy1 }, { dx2, dy2 }, { dx3, dy3 })
                        _builder_ctx_rcurve_to(&builder_ctx, { dx4, dy4 }, { dx5, dy5 }, { dx6, dy6 })
                    case 0x24: // hflex1
                        if sp < 9 {
                            log.error("[Ttf parser] CFF interpreter, hflex1")
                            ctx.ok = false
                            return {}, ctx.ok
                        }
                        dx1 := stack[0]
                        dy1 := stack[1]
                        dx2 := stack[2]
                        dy2 := stack[3]
                        dx3 := stack[4]
                        dx4 := stack[5]
                        dx5 := stack[6]
                        dy5 := stack[7]
                        dx6 := stack[8]
                        _builder_ctx_rcurve_to(&builder_ctx, { dx1, dy1 }, { dx2, dy2 }, { dx3, 0 })
                        _builder_ctx_rcurve_to(&builder_ctx, { dx4, 0 }, { dx5, dy5 }, { dx6, -(dy1+dy2+dy5) })
                    case 0x25: // flex1
                        if sp < 11 {
                            log.error("[Ttf parser] CFF interpreter, flex1")
                            ctx.ok = false
                            return {}, ctx.ok
                        }
                        dx1 := stack[0];
                        dy1 := stack[1];
                        dx2 := stack[2];
                        dy2 := stack[3];
                        dx3 := stack[4];
                        dy3 := stack[5];
                        dx4 := stack[6];
                        dy4 := stack[7];
                        dx5 := stack[8];
                        dy5 := stack[9];
                        dx6 := stack[10];
                        dy6 := dx6
                        dx := dx1+dx2+dx3+dx4+dx5;
                        dy := dy1+dy2+dy3+dy4+dy5;
                        if abs(dx) > abs(dy) {
                            dy6 = -dy
                        } else {
                            dx6 = -dx
                        }
                        _builder_ctx_rcurve_to(&builder_ctx, { dx1, dy1 }, { dx2, dy2 }, { dx3, 0 })
                        _builder_ctx_rcurve_to(&builder_ctx, { dx4, dy4 }, { dx5, dy5 }, { dx6, dy6 })
                    case:
                        log.error("[Ttf parser] CFF interpreter, two-byte escape")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                case:
                    if b0 != 255 && b0 != 28 && b0 < 32 {
                        log.error("[Ttf parser] CFF interpreter, push immediate")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    f := f32(0)
                    if b0 == 255 {
                        value := ttf_read_t_copy(Cff_Card32, &current_frame.reader)
                        f = f32(i32(value) / 0x10000) 
                    } else {
                        value := cff_dict_parse_i32(&current_frame.reader, b0)
                        f = f32(i16(value))
                    }
                    if sp >= len(stack) {
                        log.error("[Ttf parser] CFF interpreter, stack overflow")
                        ctx.ok = false
                        return {}, ctx.ok
                    }
                    stack[sp] = f
                    sp += 1
                    clear_stack = false
                }

                if clear_stack {
                    sp = 0
                }
            }

            if instructions_finished {
                glyph := &glyphs[glyph_idx]
                glyph.min = builder_ctx.min
                glyph.max = builder_ctx.max
                global_min = linalg.min(global_min, glyph.min)
                global_max = linalg.max(global_max, glyph.max)
            } else {
                log.errorf("[Ttf parser] CFF interpreter, did not get endchar: %v", glyph_idx)
                ctx.ok = false
                break
            }
        }
    } else {
        log.error("[Ttf parser] Bad CFF(2) table")
        ctx.ok = false
    }

    result := Ttf_Parse_Glyf_Table_Result {
        glyphs,
        global_min,
        global_max,
    }
    return result, ctx.ok
}

ttf_from_data :: proc(data: []byte, allocator: mem.Allocator) -> (_result: ^Ttf_Font, _ok: bool) {
    allocator := allocator
    arena, arena_err := arena_make(size = i64(len(data)) * 4, backing_allocator = allocator)
    if arena_err != nil {
        return {}, false
    }
    allocator = arena

    ctx: Ttf_Read_Context = { ok = true }
    defer if ! ctx.ok {
        arena_delete(arena)
    }

    reader := Ttf_Reader { &ctx, data, 0 }

    ttf_offset_subtable, _ := ttf_read_t_ptr(Ttf_Offset_Subtable, &reader)
    required_tables: Ttf_Tags
    use_glyf := false
    use_cff := false
    switch ttf_offset_subtable.scalar_type {
    case 0x74727565, 0x00010000:
        use_glyf = true
        required_tables = TTF_REQUIRED_TABLES
    case 0x4F54544F: 
        use_cff = true
        required_tables = TTF_CFF_REQUIRED_TABLES
    case:
        log.error("[Ttf parser] Unsupported table font type")
        ctx.ok = false
        return {}, false
    }
    ttf_tables, _ := ttf_read_t_slice(Ttf_Table_Directory, &reader, i64(ttf_offset_subtable.num_tables))

    parsed_table_tags: Ttf_Tags
    parsed_table_data: [Ttf_Tag]Ttf_Table_Blob

    // NOTE(lucas): gather tables
    for &table in ttf_tables {
        tag := ttf_u32_to_tag(table.tag)
        table_data, table_ok := ttf_get_table_from_directory(&ctx, i64(table.offset), i64(table.length), data); if table_ok {
            parsed_table_tags += { tag }
            parsed_table_data[tag] = { &table, table_data, true }
        }
    }

    if required_tables - parsed_table_tags != {} {
        ctx.ok = false
        log.errorf("[Ttf parser] Some required tables are missing: %v", required_tables - parsed_table_tags)
    }

    // NOTE(lucas): validate checksums
    for tag in parsed_table_tags {
        if tag == .unknown {
            continue
        }

        parsed_info := parsed_table_data[tag]
        if ! parsed_info.valid {
            ctx.ok = false
        }
        if tag == .head {
            checksum := ttf_table_check_sum(data)
            if 0xB1B0AFBA - checksum != 0 {
                log.errorf("[Ttf parser] table %v has a bad checksum", tag)
                ctx.ok = false
            }
        } else {
            if ttf_table_check_sum(parsed_info.data) != parsed_info.directory.check_sum {
                log.errorf("[Ttf parser] table %v has a bad checksum", tag)
                ctx.ok = false
            }
        }
    }

    scratch := arena_scratch({ allocator })

    head := ttf_parse_head_table(&ctx, parsed_table_data[.head]) or_return
    hhea := ttf_parse_hhea_table(&ctx, parsed_table_data[.hhea]) or_return
    maxp := ttf_parse_maxp_table(&ctx, parsed_table_data[.maxp], scratch.arena) or_return
    mapping := ttf_parse_cmap_table(&ctx, parsed_table_data[.cmap], TTF_CMAP_FORMATS_ALL, scratch.arena) or_return

    global_min, global_max: [2]f32
    glyphs: []Ttf_Glyph
    if use_glyf {
        locas := ttf_parse_loca_table(&ctx, parsed_table_data[.loca], head, maxp, scratch.arena) or_return
        glyf_result := ttf_parse_glyf_table(&ctx, parsed_table_data[.glyf], locas, maxp, allocator, scratch.arena) or_return
        glyphs = glyf_result.glyphs
        global_min = glyf_result.global_min
        global_max = glyf_result.global_max
    }
    if use_cff {
        glyf_result := ttf_parse_cff_table(&ctx, parsed_table_data[.CFF], maxp, allocator) or_return
        glyphs = glyf_result.glyphs
        global_min = glyf_result.global_min
        global_max = glyf_result.global_max
    }
    codepoint_to_glyph_index_map := make(map[rune]u16, len(mapping) * 2, allocator)
    for m in mapping {
        if m.glyph_index != 0 && int(m.glyph_index) < len(glyphs) {
            codepoint_to_glyph_index_map[m.codepoint] = m.glyph_index
            glyphs[m.glyph_index].codepoint = m.codepoint
        }
    }

    ttf_parse_hmtx_table(&ctx, parsed_table_data[.hmtx], hhea, glyphs) or_return
    VERTICAL_TABLES :: Ttf_Tags { .vhea, .vmtx }
    if VERTICAL_TABLES & parsed_table_tags == VERTICAL_TABLES {
        vhea, vhea_ok := ttf_parse_vhea_table(&ctx, parsed_table_data[.vhea])
        if vhea_ok {
            ttf_parse_vmtx_table(&ctx, parsed_table_data[.vmtx], vhea, glyphs) or_return
        }
    } else if .os2 in parsed_table_tags {
        ttf_parse_os2_table(&ctx, parsed_table_data[.os2], glyphs) or_return
    } else {
        // NOTE(lucas): no vertical information, use hhea to gather bearings
        ascender := f32(i16(hhea.ascender))
        descender := f32(i16(hhea.descender))
        for &glyph in glyphs {
            glyph.tsb = ascender - glyph.max.y
            glyph.advance.y = ascender - descender
        }
    }

    HINTING_TABLES :: Ttf_Tags { .fpgm, .cvt, .prep }
    has_hinting := HINTING_TABLES & parsed_table_tags == HINTING_TABLES

    hinting_data_cvt: []Ttf_Fword
    hinting_data_prep: []byte
    hinting_stack_size: i32
    hinting_storage_size: i32
    hinting_twilight_points_size: i32
    hinting_op_ptrs: [][]byte

    if has_hinting {
        hinting_stack_size = i32(maxp.max_stack_elements)
        hinting_storage_size = i32(maxp.max_storage)
        hinting_twilight_points_size = i32(maxp.max_twilight_points)
        hinting_op_ptrs = make([][]byte, maxp.max_function_defs, allocator)
        hinting_data_cvt = slice.reinterpret([]Ttf_Fword, parsed_table_data[.cvt].data)
        hinting_data_prep = parsed_table_data[.prep].data

        program: Hinter_Program = {
            shared_intructions = hinting_op_ptrs,
            stack_data = make([]i32, hinting_stack_size, scratch.arena), 
        }
        program_ctx := hinter_program_context_make(&program, .font, nil, true, parsed_table_data[.fpgm].data, {})
    context.logger.options = {}
        hinter_program_ttf_execute(&program_ctx)
    }

    result := new_clone(Ttf_Font {
        codepoint_to_glyph_index_map,
        glyphs,
        global_min, global_max,
        f32(u16(head.units_per_em)),
        f32(i16(hhea.ascender)),
        f32(i16(hhea.descender)),
        f32(i16(hhea.line_gap)),

        has_hinting,
        hinting_data_cvt,
        hinting_data_prep,
        hinting_stack_size,
        hinting_storage_size,
        hinting_twilight_points_size,
        hinting_op_ptrs,

        arena,
    }, arena)
    return result, ctx.ok
}

Otf_Coverage_Pair :: struct {
    glyph_id: Ttf_u16,
    coverage_index: Ttf_u16,
}
Otf_Coverage_Pair_List :: #soa[]Otf_Coverage_Pair
ttf_parse_coverage_table :: proc(ctx: ^Ttf_Read_Context, data: []byte, offset: i64, allocator: mem.Allocator) -> (Otf_Coverage_Pair_List, bool) {
    glyph_ids: []Ttf_u16
    coverage_pairs: []Ttf_u16
    coverage_reader := Ttf_Reader { ctx, data, offset }
    coverage_table := ttf_read_t_ptr(Otf_Table_Coverage, &coverage_reader)
    switch coverage_table.format {
    case 1:
        glyph_ids = ttf_read_t_slice(Ttf_u16, &coverage_reader, i64(coverage_table.count))
        coverage_pairs = make([]Ttf_u16, len(glyph_ids), allocator)
        for &c, i in coverage_pairs {
            c = Ttf_u16(i)
        }
    case 2:
        ranges := ttf_read_t_slice(Otf_Coverage_Format_2_Range_Record, &coverage_reader, i64(coverage_table.count))
        total_glyphs := 0
        for range in ranges {
            total_glyphs += 1 + int(range.end_glyph_id - range.start_glyph_id)
        }
        glyph_ids = make([]Ttf_u16, total_glyphs, allocator)
        coverage_pairs = make([]Ttf_u16, total_glyphs, allocator)
        i := 0
        for range in ranges {
            for c in range.start_glyph_id..=range.end_glyph_id {
                glyph_ids[i] = c 
                coverage_pairs[i] = range.start_coverage_index + c - range.start_glyph_id
                i += 1
            }
        }
    case:
        log.error("[Ttf parser] Bad coverage table")
        ctx.ok = false
    }
    return soa_zip(glyph_ids, coverage_pairs), ctx.ok
}

Otf_Class_Range :: struct #packed {
    start_glyph_id: Ttf_u16,
    end_glyph_id: Ttf_u16,
    class: Ttf_u16,
}

ttf_get_glyph_class :: proc(ctx: ^Ttf_Read_Context, data: []byte, offset: i64, glyph: Ttf_u16) -> (Ttf_u16, bool) {
    class_reader := Ttf_Reader { ctx, data, offset }
    format := ttf_read_t_copy(Ttf_u16, &class_reader)
    result: Ttf_u16
    has_class := false
    switch format {
    case 1:
        start_glyph_id := ttf_read_t_copy(Ttf_u16, &class_reader)
        glyph_count := ttf_read_t_copy(Ttf_u16, &class_reader)
        class_values := ttf_read_t_slice(Ttf_u16, &class_reader, i64(glyph_count))
        if glyph >= start_glyph_id && glyph < start_glyph_id + glyph_count {
            result = class_values[glyph - start_glyph_id]
            has_class = true
        }
    case 2:
        class_range_count := ttf_read_t_copy(Ttf_u16, &class_reader)
        class_ranges := ttf_read_t_slice(Otf_Class_Range, &class_reader, i64(class_range_count))
        // TODO(lucas): binary search
        for range in class_ranges {
            if glyph >= range.start_glyph_id && glyph <= range.end_glyph_id {
                result = range.class
                has_class = true
            }
            if has_class {
                break
            }
        }
    case: ctx.ok = false
    }
    return result, ctx.ok && has_class
}


// TODO(lucas): do a better lookup structure than this
ttf_class_to_glyph_id_list :: proc(ctx: ^Ttf_Read_Context, data: []byte, offset: i64, allocator: mem.Allocator) -> (map[u16][dynamic]Ttf_u16, bool) {
    class_reader := Ttf_Reader { ctx, data, offset }
    format := ttf_read_t_copy(Ttf_u16, &class_reader)
    result := make(map[u16][dynamic]Ttf_u16, allocator)
    switch format {
    case 1:
        start_glyph_id := ttf_read_t_copy(Ttf_u16, &class_reader)
        glyph_count := ttf_read_t_copy(Ttf_u16, &class_reader)
        class_values := ttf_read_t_slice(Ttf_u16, &class_reader, i64(glyph_count))
        for be_class_id, i in class_values {
            glyph_id := start_glyph_id + Ttf_u16(i)
            class := u16(be_class_id)
            if class not_in result {
                result[class] = make([dynamic]Ttf_u16, allocator)
            }
            append(&result[class], glyph_id)
        }
    case 2:
        class_range_count := ttf_read_t_copy(Ttf_u16, &class_reader)
        class_ranges := ttf_read_t_slice(Otf_Class_Range, &class_reader, i64(class_range_count))
        // TODO(lucas): binary search
        for range in class_ranges {
            class := u16(range.class)
            if class not_in result {
                result[u16(range.class)] = make([dynamic]Ttf_u16, allocator)
            }
            dyn_array := &result[class]
            for glyph_id in range.start_glyph_id..=range.end_glyph_id {
                append(dyn_array, glyph_id)
            }
        }
    case: ctx.ok = false
    }

    return result, ctx.ok
}

ttf_parse_os2_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob, glyphs: []Ttf_Glyph) -> bool {
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        os2 := ttf_read_t_ptr(Ttf_Table_OS2, &reader)
        // NOTE(lucas): no vertical information, use hhea to gather bearings
        ascender := f32(i16(os2.s_typo_ascender))
        descender := f32(i16(os2.s_typo_descender))
        for &glyph in glyphs {
            glyph.tsb = ascender - glyph.max.y
            glyph.advance.y = ascender - descender
        }
    } else {
        ctx.ok = false
    }
    return ctx.ok
}

ttf_parse_vhea_table :: proc(ctx: ^Ttf_Read_Context, table: Ttf_Table_Blob) -> (^Ttf_Table_Vertical_Header, bool) {
    @(static) _dummy: Ttf_Table_Vertical_Header
    result: ^Ttf_Table_Vertical_Header = &_dummy
    if table.valid {
        reader := Ttf_Reader { ctx, table.data, 0 }
        result, _ = ttf_read_t_ptr(Ttf_Table_Vertical_Header, &reader)
    } else {
        log.error("[Ttf parser] Bad vhea table")
        ctx.ok = false
    }
    return result, ctx.ok
}

ttf_delete :: proc(f: ^Ttf_Font) {
    if f != nil {
        arena_delete(f.arena)
    }
}

ppem_get :: proc(pt: f32, dpi: f32) -> Ppem {
    return max(Ppem(math.round((pt * dpi) / 72)), 0)
}

funit_scalar :: proc(ppem: Ppem, font: Ttf_Font) -> f32 {
    return f32(ppem) / font.units_per_em
}

generate_curves_from_ttf_contours :: proc(max_potential_length: i64, coords: [][2]f32, on_curve: []bool, ordered_contour_lengths: []$E, allocator: mem.Allocator, normalize := true) -> Glyph_Bezier {

    start := 0
    points := soa_zip(coord = coords, on_curve = on_curve)
    if len(ordered_contour_lengths) == 0 {
        return {}
    }

    result := make([dynamic]Glyph_Bezier_Curve, 0, max_potential_length, allocator)
    for contour_end_index in ordered_contour_lengths {
        actual_length := int(contour_end_index) - start + 1
        for i := 0; i < actual_length; i += 1 {
            p0 := points[i + start]
            p1 := points[((i + 1) %% actual_length) + start]
            c0 := [2]f32 { f32(p0.coord.x), f32(p0.coord.y) }
            c1 := [2]f32 { f32(p1.coord.x), f32(p1.coord.y) }
            if p0.on_curve {
                if p1.on_curve {
                    // NOTE(lucas): line segment
                    cm := (c0 + c1) * 0.5
                    append(&result, Glyph_Bezier_Curve { c0, cm, c1 })
                } else {
                    // NOTE(lucas): quadratic bezier
                    p2 := points[((i + 2) %% actual_length) + start]
                    c2: [2]f32 = { f32(p2.coord.x), f32(p2.coord.y) }
                    if ! p2.on_curve {
                        c2 = (c2 + c1) * 0.5
                    }
                    append(&result, Glyph_Bezier_Curve { c0, c1, c2 })
                    // NOTE(lucas): consume the off curve
                    i += 1
                }
            } else {
                // NOTE(lucas): quadratic bezier
                pb := points[((i - 1) %% actual_length) + start]
                cb := [2]f32 { f32(pb.coord.x), f32(pb.coord.y) }
                if ! pb.on_curve {
                    cb = (c0 + cb) * 0.5
                }
                if ! p1.on_curve {
                    c1 = (c0 + c1) * 0.5
                }
                append(&result, Glyph_Bezier_Curve { cb, c0, c1 })
            }
        }
        start = int(contour_end_index + 1)
    }

    min: [2]f32 = math.INF_F32
    max: [2]f32 = math.NEG_INF_F32
    for c in result {
        min = linalg.min(min, c.p0)
        min = linalg.min(min, c.p1)
        min = linalg.min(min, c.p2)

        max = linalg.max(max, c.p0)
        max = linalg.max(max, c.p1)
        max = linalg.max(max, c.p2)
    }
    dim := max - min
    if normalize {
        for &c in result {
            c.p0 = (c.p0 - min) / dim
            c.p1 = (c.p1 - min) / dim
            c.p2 = (c.p2 - min) / dim
        }
    }

    return { result[:], dim, min, max }
}

Glyph_Job :: struct {
    glyph: ^Ttf_Glyph,
    transform: matrix[2, 3]f32,
    child_contour_length: int,
    child_length: int,
    round_transform: bool,
}

gather_glyphs_jobs :: proc(glyphs_to_hint: ^[dynamic]Glyph_Job, font: ^Ttf_Font, glyph_id: u16, transform: matrix[2, 3]f32, round_transform: bool) -> (int, int) {
    g := &font.glyphs[glyph_id]
    child_contour_length := 0
    child_length := 1
    for c in g.compound_glyphs {
        contour_len, child_len := gather_glyphs_jobs(glyphs_to_hint, font, c.glyph_id, c.transform, c.round_to_grid)
        child_contour_length += contour_len
        child_length += child_len
    }
    if len(g.compound_glyphs) > 0 || len(g.points) > 0 {
        append(glyphs_to_hint, Glyph_Job { g, transform, child_contour_length, child_length, round_transform })
        if len(g.compound_glyphs) == 0 {
            child_contour_length += len(g.ordered_contour_lengths)
        }
    }
    return child_contour_length, child_length
}

transform_coordinate :: proc(transform: matrix[2, 3]f32, coordinate: [2]f32) -> [2]f32 {
    return transform * [3]f32 { coordinate.x, coordinate.y, 1 }
}

font_get_unhinted_glyph :: proc(font: ^Ttf_Font, glyph_id: u16, allocator: mem.Allocator) -> (Glyph_Bezier, bool) {
    scratch := arena_scratch({ allocator })

    glyphs_to_combine := make([dynamic]Glyph_Job, scratch.arena)
    gather_glyphs_jobs(&glyphs_to_combine, font, glyph_id, IDENTITY_TRANSFORM, false)

    glyph_curves := make([]Glyph_Bezier, len(glyphs_to_combine), scratch.arena)

    // NOTE(lucas): copy coordinates to be transformed
    for glyph, i in glyphs_to_combine {
        contour_length := len(glyph.glyph.ordered_contour_lengths)
        if contour_length == 0 {
            continue
        }
        max_potential_guess := (i64(glyph.glyph.ordered_contour_lengths[contour_length - 1]) + 1) * 2
        glyph_curves[i] = generate_curves_from_ttf_contours(max_potential_guess, soa_unzip(glyph.glyph.points), glyph.glyph.ordered_contour_lengths, scratch.arena, false)
    }

    // NOTE(lucas): transform all coordinates
    for glyph, i in glyphs_to_combine {
        if glyph.transform != IDENTITY_TRANSFORM {
            group_to_transform := glyph_curves[i - glyph.child_length + 1:i + 1]
            for &curve in group_to_transform {
                for &coord in curve.curves {
                    coord.p0 = transform_coordinate(glyph.transform, coord.p0)
                    coord.p1 = transform_coordinate(glyph.transform, coord.p1)
                    coord.p2 = transform_coordinate(glyph.transform, coord.p2)
                }
                new_min := transform_coordinate(glyph.transform, curve.min)
                new_max := transform_coordinate(glyph.transform, curve.max)

                curve.min = linalg.min(new_min, new_max)
                curve.max = linalg.max(new_min, new_max)
            }
        }
    }

    min: [2]f32 = math.INF_F32
    max: [2]f32 = math.NEG_INF_F32

    total_curves := 0
    for curve, i in glyph_curves {
        total_curves += len(curve.curves)
        if len(glyphs_to_combine[i].glyph.ordered_contour_lengths) != 0 {
            min = linalg.min(curve.min, min)
            max = linalg.max(curve.max, max)
        }
    }

    all_curves := make([]Glyph_Bezier_Curve, total_curves, allocator)
    if len(all_curves) != total_curves {
        return {}, false
    }
    offset := 0
    for c in glyph_curves {
        copy(all_curves[offset:], c.curves)
        offset += len(c.curves)
    }
    dim := max - min
    for &c in all_curves {
        c.p0 = (c.p0 - min) / dim
        c.p1 = (c.p1 - min) / dim
        c.p2 = (c.p2 - min) / dim
    }

    return { all_curves, dim, min, max }, true
}

