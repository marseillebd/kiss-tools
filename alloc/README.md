# Kiss Alloc

The Kiss allocator is part of the kisstools project.
This project aims to be a development toolchain able to bootstrap itself,
while also being simple, hackable, and instructive.

This component provides a simple memory allocator.
Its interface is just `kissAlloc` and `kissFree`, calls to which must be paired.
It relies on linking to an implementation of `kissAllocPages`,
which must be tailored to each platform.

## Overview

The main idea is to turn each page into a bump allocator.
Small objects are allocated into a page.
Each page counts the number of live objects it contains.
Releasing an object merely decrements the page's count; that memory is not yet available for reuse.
Instead, it is when a page has no more live objects that its memory may be reused for additional allocations.

Large objects are allocated across multiple contiguous pages.
The pointer users receive is page-aligned.
Bookkeeping information is then stored in the page just before the object itself.
(TODO: Allocation of larger objects in not yet implemented.)

The question arises: how do we find the page that an object belongs to?
A page header occupies the start of a page.
All small objects will be at some non-zero offset from a page boundary.
In contrast, large objects will be aligned to page boundaries.
So, an object is large if and only if its pointer's low bits are cleared.
For a small object, its header is just at the start of its page: obtainable by a mask operation.
For large objects, multiple contiguous pages must be allocated, but only the first has a header.
Then, that first page is left empty, and the object starts on the next page.
Thus, the header for a large object is obtained by subtracting off a page from its pointer.

## Extension

The `kiss-alloc.c` unit relies on a function that can provide pages, which must be implemented per-platform.
That's all you have to write to bring kiss allocator to a new platform.

Currently, I've only implemented kiss allocator on top of `malloc`,
which is quite the cheat if I intend to bootstrap the whole thing.
I suspect it'll be relatively easy for me to target `mmap` or linux (and other \*nixes).
Likewise, it shuold be quite easy to allocate pages out of a designated heap space for standalone targets like BIOS.

## Roadmap

- [ ] the freelist should probably be a queue rather than a stack
- [ ] implement large objects
- [ ] look for large object space in the freelist
- [ ] I'm not sure I actually need to track a circular list of pages; a singley-linked list is fine, right?

- [ ] hide a bunch of implementation structs
- [ ] extra functions to help cleanup (in case the allocator needs to be torn down, or we want to pass memory back to the platform)

- [ ] build system
- [ ] testing, testing, testing

- [ ] port to other platforms
  - [ ] mmap
  - [ ] standalone (configure a region in memory for use as a heap)


I'm not supporting reallocation.
Odds are, something else will be allocated between allocation and reallocation.
If so, a bump allocator would not be able to grow the allocation in-place.
Thus, `alloc new -> copy -> free old` is usually the best performance possible.
By abandoning realloc, there's no need to track the sizes of individual object allocations,
which saves a lot of memory for small allocations.

I'm torn on allowing more than just the global allocator.
The benefit would be multi-threading your allocator.
However, you don't need multiple threads to bootstrap a development environment.
The cost is adding more functions to the interface, which is opposite of the kiss minimalism.
Perhaps it's more principled to not have globals (even if they are static), but right now I don't think it's worth it.

