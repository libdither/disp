# Reorganization Notes

This file used to propose splitting the old monolithic `lib/kernel.disp`.
That migration has mostly happened.

## Current Layout

```text
lib/
├── prelude.disp
├── kernel/
│   ├── utils.disp
│   ├── handlers.disp
│   ├── walker.disp
│   └── prelude.disp
├── types/
│   ├── bool.disp
│   ├── nat.disp
│   ├── pi.disp
│   ├── type.disp
│   ├── eq.disp
│   ├── ord.disp
│   └── conv.disp
├── std/
└── tests/
```

`kernel/prelude.disp` is the compatibility shim: it opens the kernel
substrate and the library type files so most user code can write
`open use "../kernel/prelude.disp"`.

## Remaining Cleanup Ideas

- Split `kernel/handlers.disp` further if the primitive handlers grow
  again. Today it is acceptable as the one file that assembles the
  recursive kernel record.
- Add an explicit export-all or re-export syntax, then migrate
  `kernel/prelude.disp` away from the legacy fieldless-file export
  mode.
- Consider carving tests into subdirectories by layer (`kernel`,
  `types`, `std`) if the suite grows much larger.
- Continue moving standard-library additions under `lib/std/`.

The old `dither-spec` submodule was unlinked; it described a much
older language version and should not drive current design decisions.
