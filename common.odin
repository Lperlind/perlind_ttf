#+vet explicit-allocators
package perlind_ttf

import "base:intrinsics"
import "base:runtime"
import "core:mem"
import "core:fmt"
import "core:os"

Allocator :: runtime.Allocator
Allocator_Error :: runtime.Allocator_Error
Allocator_Mode :: runtime.Allocator_Mode
Allocator_Mode_Set :: runtime.Allocator_Mode_Set

mem_alloc_non_zeroed :: runtime.mem_alloc_non_zeroed
@(require_results)
align_forward :: proc(val, alignment: $T) -> T
    where intrinsics.type_is_numeric(T){
    return (val + alignment - 1) & ~(alignment - 1);
}
overflow_add :: intrinsics.overflow_add
make_aligned :: runtime.make_aligned
/*

show_os_dialogue_box :: proc(caption: string, msg: string) {
	_show_dialogue_box(caption, msg)
}

// NOTE(lucas): we have a separate error path for the scratch arena as the fatal error itself uses
// the scratch arena. Therefore we would recursively fail if the scratch needed to itself print out
// a fatal error.
fatal_scratch_error :: proc() -> ! {
	_show_scratch_died()
	os.exit(1)
}

fatal_error :: proc(msg: string) -> ! {
	scratch := arena_scratch({})
	prepend := "A fatal error has occured and the application will be shutdown"
	msg := msg
	if msg == "" {
		msg = fmt.aprintf("%v.", prepend, allocator = scratch.arena)
	} else {
		msg = fmt.aprintf("%v.\n\n%v.", prepend, msg, allocator = scratch.arena)
	}
	{ panic(msg) }
	show_os_dialogue_box("Fatal Error", msg)
	{os.exit(1)}
}

get_last_os_error_string :: proc() -> string {
	return _get_last_os_error_string()
}

Generational_Pointer :: struct($T: typeid) where intrinsics.type_is_pointer(T) {
	gen, ptr: u64,
}
generational_pointer_to_t :: proc(handle: Generational_Pointer($T)) -> (T, bool) {
	if handle.gen == 0 || handle.ptr == 0 {
		return nil, false
	}
	as_real_ptr := cast(T)(uintptr(handle.ptr))
	if as_real_ptr.generation != handle.gen {
		return nil, false
	}
	return as_real_ptr, true
}
t_to_generational_pointer :: proc(
	t: $T,
) -> Generational_Pointer(T) where intrinsics.type_is_pointer(T) {
	if t == nil {
		return {}
	}
	return {gen = t.generation, ptr = u64(uintptr(t))}
}

@(require_results)
pack_2_i32_to_u64 :: proc(data: [2]i32) -> u64 {
	return transmute(u64)(data)
}

@(require_results)
unpack_u64_to_2_i32 :: proc(data: u64) -> [2]i32 {
	return transmute([2]i32)(data)
}

@(require_results)
array_cast :: proc($T: typeid, v: [$N]$U) -> [N]T #no_bounds_check {
	new_v: [N]T = ---
	#unroll for i in 0 ..< N {
		new_v[i] = cast(T)(v[i])
	}
	return new_v
}

map_freeze :: proc(m: ^map[$T]$N) {
    raw_map := cast(^runtime.Raw_Map)m
    raw_map.allocator = mem.panic_allocator()
}

Bad_Handle :: struct {}



safe_add :: proc(a, b: $T) -> (T, bool) {
	result, did_overflow := overflow_add(a, b)
	return result, ! did_overflow
}
*/
