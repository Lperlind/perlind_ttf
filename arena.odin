#+vet explicit-allocators
package perlind_ttf

import "base:intrinsics"
import "core:testing"
import "core:math/rand"
import "core:mem"
import "core:slice"
import "core:fmt"
import "core:os"

TOPDOG_ARENA_POOL_COUNT :: 8
TOPDOG_ARENA_POOL_DEFAULT_SIZE :: 1024 * 1024
TOPDOG_ARENA_DEFAULT_BLOCK_SIZE :: 1024 * 1024
TOPDOG_ARENA_DEFAULT_VIRTUAL_SIZE :: 1024 * 1024 * 1024
TOPDOG_ARENA_DEFAULT_COMMIT_SIZE :: 1024 * 1024 * 8
/*
    NOTE(lucas): An arena construct which provides bump like allocations from a growing
    pool. Freeing is done by releasing the entire arena or by using temporary
    arenas which release portions of an arena.
*/
Arena :: struct #align(64) {
    backing_data: []byte,
    backing_allocator: Allocator,

    head: i64,
    block_size: i64,
    reserve_size: i64,

    next: ^Arena,
    prev: ^Arena,
    current: ^Arena,

    last_ptr: rawptr,

    policy: Arena_Policy,

    total_bytes_allocated: i64,

    using self_allocator: Allocator,
}

Arena_Temp :: struct {
    arena: ^Arena,
    _current: ^Arena,
    _current_head: i64,
    _current_last_ptr: rawptr,
}

Arena_Policy :: enum {
    Non_Growable,
    Growable,
}

@(private)
Arena_Pool :: struct #align(64) {
    pool: [TOPDOG_ARENA_POOL_COUNT]^Arena,
}
@(thread_local, private)
tls_scratch_pool: Arena_Pool

arena_make :: proc(size: i64 = 0, growable := true, backing_allocator: mem.Allocator = {}) -> (^Arena, Allocator_Error) {
    return _arena_make(size, 0, growable ? .Growable : .Non_Growable, backing_allocator)
}

arena_calc_total_allocations :: proc(arena: ^Arena) -> i64 {
    result: i64
    for it := arena; it != nil; it = it.next {
        result += it.head
    }
    return result
}

@(private)
_arena_make :: proc(reserve_size: i64, commit_size: i64, policy: Arena_Policy, backing_allocator: mem.Allocator) -> (result: ^Arena, mem_error: Allocator_Error){
    assert(reserve_size >= 0)
    assert(commit_size >= 0)

    reserve_size := reserve_size
    commit_size := commit_size
    backing_allocator := backing_allocator

    if reserve_size == 0 {
        reserve_size = TOPDOG_ARENA_DEFAULT_BLOCK_SIZE
    }
    block_size := reserve_size
    if commit_size == 0 {
        commit_size = TOPDOG_ARENA_DEFAULT_COMMIT_SIZE
    }
    commit_size = min(commit_size, reserve_size)

    reserve_size += size_of(Arena)

    arena: ^Arena
    switch policy {
    case .Growable, .Non_Growable:
        block := mem_alloc_non_zeroed(int(reserve_size), 64, backing_allocator) or_return
        arena = cast(^Arena)raw_data(block)
        block = block[size_of(Arena):]

        arena^ = {
            backing_data = block,
            backing_allocator = backing_allocator,
            current = arena,
            policy = policy,
            block_size = block_size,
        }
    }
    arena.self_allocator = arena_allocator(arena)
    return arena, .None
}

arena_allocator :: proc(arena: ^Arena) -> Allocator {
    return { arena_allocator_proc, rawptr(arena) }
}

arena_alloc :: proc(arena: ^Arena, size: i64, aligment: i64, zero: bool) -> ([]byte, Allocator_Error) {
    return _arena_alloc(arena, size, aligment, zero, true)
}

@(private)
_arena_alloc :: proc(arena: ^Arena, size: i64, aligment: i64, zero: bool, can_do_regrow_path: bool) -> (_result: []byte, _error: Allocator_Error) {
    assert(arena.current != nil)

    if size < 0 || aligment < 0 {
        return {}, .Invalid_Argument
    }
    if size == 0 {
        return {}, .None
    }

    current := arena.current
    backing_offset := uintptr(raw_data(current.backing_data)) + uintptr(current.head)
    aligned_backing := align_forward(backing_offset, uintptr(aligment))
    alignment_bytes_required := i64(aligned_backing - backing_offset)
    bytes_required, bytes_required_bad := overflow_add(size, alignment_bytes_required)

    try_to_regrow := true
    new_head: i64
    if ! bytes_required_bad {
        new_head_bad: bool
        new_head, new_head_bad = overflow_add(current.head, bytes_required)
        if ! new_head_bad && new_head <= i64(len(current.backing_data)) {
            try_to_regrow = false
        }
    }

    result: []byte
    if try_to_regrow {
        // NOTE(lucas): we assert here that when we do a regrow and try to allocate again
        // we never this this path, i.e. a regrow should not cause another regrow
        assert(can_do_regrow_path)

        switch current.policy {
        case .Non_Growable: return {}, .Out_Of_Memory
        case .Growable:
            // NOTE(lucas): we keep the current arena to not internally fragment if at least 1/4 of it
            // can still be used
            keep_this_arena := i64(len(current.backing_data)) - new_head > (i64(len(current.backing_data)) / 4)
            // NOTE(lucas): we add both size + alignment so that we guaruntee we can fulfill the allocation
            // given arena_make doesn't provide the alignment we need
            new_arena_size := max(current.block_size, size + aligment)
            new_arena := _arena_make(new_arena_size, 0, current.policy, current.backing_allocator) or_return

            new_arena.prev = current
            new_arena.next = current.next
            if new_arena.next != nil {
                new_arena.next.prev = new_arena
            }
            current.next = new_arena

            if ! keep_this_arena {
                arena.current = new_arena
            }
            return _arena_alloc(new_arena, size, aligment, zero, false)
        case: panic("bad arena enum")
        }
    }
    result = current.backing_data[current.head + alignment_bytes_required:new_head]
    assert(uintptr(raw_data(result)) % uintptr(aligment) == 0)
    current.head = new_head
    current.last_ptr = raw_data(result)

    arena.total_bytes_allocated += bytes_required
    if zero {
        mem.zero_slice(result)
    }

    return result, .None
}

arena_realloc :: proc(arena: ^Arena, size: i64, aligment: i64, old_memory: rawptr, old_size: i64, zero: bool) -> (_result: []byte, _error: Allocator_Error) {
    assert(arena.current != nil)
    current := arena.current
    delta := size - old_size
    just_alloc := old_memory == nil ||
                  uintptr(old_memory) % uintptr(aligment) != 0 ||
                  old_memory != current.last_ptr ||
                  current.head + delta > i64(len(current.backing_data))

    if just_alloc {
        new_data := arena_alloc(arena, size, aligment, zero) or_return
        mem.copy_non_overlapping(raw_data(new_data), old_memory, int(old_size))
        return new_data, .None
    } else {
        if delta > 0 {
            mem.zero(rawptr(uintptr(old_memory) + uintptr(old_size)), int(delta))
        }
        current.head += delta
        return mem.slice_ptr((^byte)(old_memory), int(size)), .None
    }
}

@(deferred_out=arena_temp_delete)
arena_scratch :: proc(collisions: []Allocator) -> Arena_Temp {
    assert(len(collisions) <= TOPDOG_ARENA_POOL_COUNT)
    arena: ^Arena
    for a in tls_scratch_pool.pool {
        arena = a
        for c in collisions {
            if c.data == rawptr(a) {
                arena = nil
                break
            }
        }
        if arena != nil {
            break
        }
    }

    if arena == nil {
        @(cold)
        _allocate_scratch :: #force_no_inline proc() -> ^Arena {
            for &a in tls_scratch_pool.pool {
                if a != nil {
                    continue
                }
                error: Allocator_Error
                a, error = arena_make(TOPDOG_ARENA_POOL_DEFAULT_SIZE, true, os.heap_allocator())
                if error != nil {
                    panic("Out of scratch space!")
                }
                return a
            }
            panic("Out of scratch space!")
        }
        arena = _allocate_scratch()
    }

    return arena_temp_make(arena)
}

arena_delete :: proc(arena: ^Arena) {
    if arena == nil {
        return
    }
    for it := arena.next; it != nil; {
        next := it.next
        free(it, it.backing_allocator)
        it = next
    }
    if arena.prev != nil {
        arena.prev.next = nil
    }
    free(arena, arena.backing_allocator)
}

arena_free_all :: proc(arena: ^Arena) {
    arena_delete(arena.next)
    arena.current = arena
    arena.head = 0
    arena.last_ptr = nil
    arena.next = nil
    arena.prev = nil
}

arena_allocator_proc :: proc(allocator_data: rawptr, mode: Allocator_Mode,
                             size, alignment: int,
                             old_memory: rawptr, old_size: int,
                             location := #caller_location) -> (data: []byte, err: Allocator_Error) {

    arena := cast(^Arena)(allocator_data)
	switch mode {
	case .Alloc, .Alloc_Non_Zeroed: return arena_alloc(arena, i64(size), i64(alignment), mode == .Alloc)
	case .Free: err = .Mode_Not_Implemented
	case .Free_All: arena_free_all(arena)
	case .Resize, .Resize_Non_Zeroed: return arena_realloc(arena, i64(size), i64(alignment), old_memory, i64(old_size), mode == .Resize)
	case .Query_Features:
		set := (^Allocator_Mode_Set)(old_memory)
		if set != nil {
			set^ = {.Alloc, .Alloc_Non_Zeroed, .Free_All, .Alloc_Non_Zeroed}
		}
	case .Query_Info:
		err = .Mode_Not_Implemented
	}

	return
}

@(deferred_out=arena_temp_delete)
arena_temp_scope :: proc(arena: ^Arena) -> Arena_Temp {
    return arena_temp_make(arena)
}

@(require_results)
arena_temp_make :: proc(arena: ^Arena) -> Arena_Temp {
    return {
        arena,
        arena.current,
        arena.current.head,
        arena.current.last_ptr,
    }
}

arena_temp_delete :: proc(temp: Arena_Temp) {
    temp.arena.current = temp._current
    temp._current.head = temp._current_head
    temp._current.last_ptr = temp._current_last_ptr
    arena_delete(temp._current.next)
}

// NOTE(lucas): tests

@(test)
test_arena_basic_alloc_and_free :: proc(t: ^testing.T) {
    // NOTE(lucas): test basic create/destroy
    arena, arena_err := arena_make(0, true, context.allocator)
    allocator := arena_allocator(arena)
    testing.expect(t, arena_err == nil)
    defer arena_delete(arena)

    // NOTE(lucas): test a few allocations and deletes
    for i in 0..<u64(128) {
        _, error := make_aligned([]byte, 139 * (i + 1), 64, allocator)
        testing.expect(t, error == nil)
    }
}

@(test)
test_arena_random_alloc_and_free :: proc(t: ^testing.T) {
    alignments := []i64 { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096 }

    for inital_size in ([]i64 { 32, 64, 128, 256, 512, 1024, 4096, 8196 }) {
        // NOTE(lucas): test a few allocations and deletes
        arena, arena_err := arena_make(inital_size, true, context.allocator)
        allocator := arena_allocator(arena)
        testing.expect(t, arena_err == nil)
        defer arena_delete(arena)

        N_BINS :: 16
        bins: [N_BINS][]i64
        // NOTE(lucas): randomly allocate bins and fill them up with numbers
        for i in 0..<i64(N_BINS) {
            size := rand.int63_max(inital_size * 2) + 1
            alignment := rand.choice(alignments)

            new_bin, error := make_aligned([]i64, size, int(alignment), allocator)
            testing.expect(t, error == nil)
            testing.expect(t, i64(len(new_bin)) == size)
            testing.expect(t, uintptr(raw_data(new_bin)) % uintptr(alignment) == 0)
            slice.fill(new_bin, i + 1)

            bins[i] = new_bin
        }

        // NOTE(lucas): asserts all bins are okay
        for i in 0..<i64(N_BINS) {
            expect := i + 1
            testing.expect(t, slice.all_of(bins[i], expect), fmt.tprintf("Expected: %v\nArray was: %v", expect, bins[i]))
        }
    }
}

@(test)
test_arena_realloc_in_place :: proc(t: ^testing.T) {
    PUSH_COUNT :: 128
    ARENA_SIZE :: PUSH_COUNT * size_of(i64) * 16
    arena, arena_err := arena_make(ARENA_SIZE, false, context.allocator)
    allocator := arena_allocator(arena)
    testing.expect(t, arena_err == nil)
    defer arena_delete(arena)

    // NOTE(lucas): test that we realloc in place
    {
        defer arena_free_all(arena)
        dyn_array := make([dynamic]i64, allocator)
        append(&dyn_array, 1)
        starting_ptr := raw_data(dyn_array)

        for _ in 0..<PUSH_COUNT {
            n := append(&dyn_array, 1)
            testing.expect(t, n > 0)
            testing.expect(t, starting_ptr == raw_data(dyn_array))
        }
        testing.expect(t, slice.all_of(dyn_array[:], 1))
    }
    // NOTE(lucas): test that realloc does not go in place if we allocate something new
    {
        dyn_array := make([dynamic]i64, allocator = allocator)
        append(&dyn_array, 1)

        for _ in 0..<PUSH_COUNT {
            starting_ptr := raw_data(dyn_array)
            starting_cap := cap(dyn_array)
            _ = new(i64, allocator)
            n := append(&dyn_array, 1)
            testing.expect(t, n > 0)
            testing.expect(t, starting_ptr != raw_data(dyn_array) || starting_cap == cap(dyn_array))
        }
        testing.expect(t, slice.all_of(dyn_array[:], 1))
    }
}

@(test)
test_temp_arena :: proc(t: ^testing.T) {
    arena, arena_err := arena_make(mem.Megabyte, false, context.allocator)
    testing.expect(t, arena_err == nil)
    defer arena_delete(arena)

    allocator := arena_allocator(arena)
    for _ in 0..<16 {
        arena_temp_scope(arena)
        s, error := make([]byte, mem.Megabyte, allocator)
        testing.expect(t, error == nil)
        slice.fill(s, 1)
    }
}

