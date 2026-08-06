# warren

A macOS Finder–style **Miller column** file browser, written from scratch in pure
Common Lisp. Directories cascade left-to-right as columns; a single click on a
folder opens its contents in a new column to the right. The UI is drawn with
[gesso](https://github.com/modus-lisp/gesso) (antialiased 2D vector graphics),
packed into a [glass](https://github.com/modus-lisp/glass) framebuffer, and
served over VNC — no FFI, no X, no toolkit.

## What it is

- **Miller / cascading columns** — each column lists one directory as a vertical
  list of rows: `[icon] name … [›]`. Columns are separated by 1px hairlines.
- **Single-click navigation** — clicking a row selects it, discards every column
  to the right, and (if it is a folder) appends a new column with that folder's
  contents. Clicking a file selects it and shows a trailing info panel
  (name / kind / size). After a click the view auto-scrolls horizontally so the
  newest column is visible, exactly like Finder.
- **Vector icons** — the folder, document, and row chevron are drawn as gesso
  paths (no image assets).
- **Path bar** — a bottom strip shows the full path of the current selection.
- **Scrolling** — the mouse wheel scrolls the column under the pointer.
- **Keyboard** — Up/Down move the selection in the active column, Left focuses the
  parent column, Right/Enter descend into a selected folder.

## Usage

```lisp
(ql:quickload :warren)          ; or (asdf:load-system :warren)

;; Serve a browser over VNC on :5900, rooted at your home directory.
(warren:run)

;; Options:
(warren:run :port 5901 :root #p"/" :width 1200 :height 800)
```

Point any VNC client at `localhost:5900`. `warren:run` runs the paint pump on the
calling thread (the RFB server runs on its own); pass `:background t` to run it on
a separate thread and get the app object back immediately.

Dotfiles are hidden by default; bind `warren:*show-hidden*` to `t` before building
a column to include them.

### Headless rendering

```lisp
(let ((app (warren::make-browser :root #p"/home/me/")))
  (warren::render-to-png app "/tmp/view.png"))
```

`tests.lisp` runs the model assertions and writes three sample screenshots to
`shots/`.

`desktop-5903.lisp` is not part of the library — it is the author's own launcher,
kept here because it is the only worked example of warren living inside a full
[glass](https://github.com/modus-lisp/glass) desktop alongside other apps. Its
paths are machine-specific; read it, don't run it.

## Dependencies

- [gesso](https://github.com/modus-lisp/gesso) — 2D vector drawing (paths, fills,
  text via scribe)
- [glass](https://github.com/modus-lisp/glass) — in-memory framebuffer +
  from-scratch VNC/RFB server
- [scribe](https://github.com/modus-lisp/scribe) — font shaping / rasterization
  (pulled in by gesso)

## Architecture

- `src/fs.lisp` — directory listing (`uiop:subdirectories` / `directory-files`),
  sorted directories-first then case-insensitive; the column model.
- `src/icons.lisp` — folder / document / chevron glyphs as gesso paths.
- `src/view.lisp` — column layout, drawing, and point → (column, row) hit-testing.
- `src/app.lisp` — the glass app: a gesso canvas repainted on a dirty flag and
  blitted into the framebuffer, RFB pointer/key callbacks, the pump loop, and the
  single-click select / truncate / append logic. `run` mirrors a standard glass
  driver (make framebuffer → `glass:serve` on one thread → pump loop).

## Status & limitations

Works: column navigation, single-click open, right-truncate, file info panel,
horizontal auto-scroll, wheel scrolling, keyboard navigation, the path bar,
unreadable-directory handling.

Not yet: double-click / open-with, file previews (image thumbnails, text peek),
drag & move, rename, a hidden-files toggle in the UI, a breadcrumb bar, and
keeping a selected row vertically scrolled into view in inactive columns.

## License

MIT.
