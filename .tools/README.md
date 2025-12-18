## Dotfiles Tools

Collection of scripts for screen capture and recording workflows.

## Geometry Discovery

### `discover_geometry`
Interactive tool to select a screen region and get its geometry.

**Usage:**
```bash
discover_geometry
```

Outputs geometry in format: `X,Y WIDTHxHEIGHT` (Wayland/grim format)

---

## Basic Capture Tools

### `capture_thumbnail`
Capture a single screenshot of a hard-coded region.

**Usage:**
```bash
capture_thumbnail NAME IDENTIFIER
```

**Example:**
```bash
capture_thumbnail random-matching ch-1
```

**Output:** `~/development/Lando/lando-video/random-matching/random-matching-ch-1.png`

**Configuration:** Edit `GEOM` variable in script to set capture region.

### `capture_pair`
Capture two screenshots (scramble and complete) of hard-coded regions.

**Usage:**
```bash
capture_pair NAME IDENTIFIER
```

**Example:**
```bash
capture_pair random-matching hero
```

**Output:**
- `~/development/Lando/lando-video/random-matching/scramble-random-matching-hero.png`
- `~/development/Lando/lando-video/random-matching/complete-random-matching-hero.png`

**Configuration:** Edit `SCRAMBLE_GEOM` and `COMPLETE_GEOM` variables in script.

---

## Lando Recording System

High-throughput recording system for Lando puzzle videos with structured identifiers (challenges → hero → extensions).

### Key Features

**Independent Counters:** Each capture type (video, thumbnail, screenshot) has its own independent counter, allowing flexible workflows:
- Do all videos first, then all thumbnails, then all screenshots
- OR interleave them in any order
- Counters track separately and display together

**Multiple Geometries:** Supports three different capture regions:
- **GEOM**: Used by video recording and thumbnails (single app view)
- **SCRAMBLE_GEOM**: Used by screenshot pairs for scrambled state
- **COMPLETE_GEOM**: Used by screenshot pairs for completed state

**Status Display:** After every operation, see all 3 counter states:
```
Video: ch-3  Thumbnail: ch-1  Screenshot: ch-5
```

### Setup

#### `record-lando-setup`
Initialize a new Lando recording project with geometry configuration.

**Usage:**
```bash
record-lando-setup NAME NUM_CHALLENGES NUM_EXTENSIONS
record-lando-setup GEOM SCRAMBLE_GEOM COMPLETE_GEOM NAME NUM_CHALLENGES NUM_EXTENSIONS
```

**Examples:**
```bash
# Use default geometries
record-lando-setup "random-matching" 5 3

# Use custom geometries (all three must be specified)
record-lando-setup "1507,345 720x597" "2650,889 646x712" "3294,897 624x704" "random-matching" 5 3
```

**Default Geometries:**
- Video/Thumbnail: `1507,345 720x597`
- Scramble: `2650,889 646x712`
- Complete: `3294,897 624x704`

**Output:**
```
Setup complete!
Project: random-matching
Geometry (video/thumbnail): 1507,345 720x597
Geometry (scramble): 2650,889 646x712
Geometry (complete): 3294,897 624x704
Sequence: 5 challenges → hero → 3 extensions

Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1
```

**Creates:**
- State file at `~/.config/record-lando/state`
- Sequence: ch-1, ch-2, ..., ch-N, hero, ext-1, ext-2, ..., ext-M
- 3 independent counters (VIDEO_INDEX, THUMBNAIL_INDEX, SCREENSHOT_INDEX)
- 3 geometries (GEOM, SCRAMBLE_GEOM, COMPLETE_GEOM)

### Main Capture Tools

Each tool has its own independent counter and can be used in any order.

#### `record-lando-video` - Video Recording
Toggle video recording on/off. Uses VIDEO_INDEX counter and GEOM.

**Keybinding:** `Mod+Ctrl+R`

**Usage:**
1. Press `Mod+Ctrl+R` to start recording
2. Demo the content
3. Press `Mod+Ctrl+R` to stop and save

**Output:** `~/development/Lando/lando-video/{NAME}/{NAME}-{IDENTIFIER}.mp4`

**Example:** `random-matching-ch-1.mp4`, `random-matching-hero.mp4`

**Status Display:**
```
Saved random-matching-ch-1.mp4
Video: ch-2  Thumbnail: ch-1  Screenshot: ch-1
```

#### `get-lando-thumbnail` - Single Screenshot
Capture a thumbnail and auto-increment. Uses THUMBNAIL_INDEX counter and GEOM.

**Keybinding:** `Mod+Ctrl+T`

**Output:** `~/development/Lando/lando-video/{NAME}/{NAME}-{IDENTIFIER}.png`

**Example:** `random-matching-ext-1.png`

**Status Display:**
```
Captured random-matching-ch-1.png
Video: ch-2  Thumbnail: ch-2  Screenshot: ch-1
```

#### `get-lando-screenshot` - Screenshot Pair
Capture scramble/complete pair and auto-increment. Uses SCREENSHOT_INDEX counter with SCRAMBLE_GEOM and COMPLETE_GEOM.

**Keybinding:** `Mod+Ctrl+S`

**Output:**
- `~/development/Lando/lando-video/{NAME}/scramble-{NAME}-{IDENTIFIER}.png`
- `~/development/Lando/lando-video/{NAME}/complete-{NAME}-{IDENTIFIER}.png`

**Example:** `scramble-random-matching-ch-2.png`, `complete-random-matching-ch-2.png`

**Status Display:**
```
Captured scramble/complete-random-matching-ch-1.png
Video: ch-2  Thumbnail: ch-2  Screenshot: ch-2
```

**Note:** This tool captures two separate regions (scramble and complete) in a single operation, perfect for side-by-side puzzle states.

### Navigation Tools

#### `record-lando-prev`
**Smart decrement** - Goes back on whichever counter was most recently used.

**Keybinding:** `Mod+Ctrl+[`

**Example:**
```bash
Mod+Ctrl+T              # Use thumbnail
Mod+Ctrl+[              # Goes back on thumbnail counter
# Output: Decremented Thumbnail counter.
#         Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1

Mod+Ctrl+R              # Use video
Mod+Ctrl+[              # Goes back on video counter
# Output: Decremented Video counter.
#         Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1
```

#### `record-lando-next`
**Smart increment** - Skips forward on whichever counter was most recently used.

**Keybinding:** `Mod+Ctrl+]`

**Example:**
```bash
record-lando-next
# Output: Skipped ch-3 on Video.
#         Video: ch-4  Thumbnail: ch-2  Screenshot: ch-1
```

#### `record-lando-status`
Show current project status and progress for all capture types.

**Example output:**
```
═══════════════════════════════════════
Project: random-matching
Sequence: 5 challenges → hero → 3 extensions
───────────────────────────────────────
Video:      ch-3 (3/9)
Thumbnail:  ch-1 (1/9)
Screenshot: ch-5 (5/9)
───────────────────────────────────────
Last used:  VIDEO
Recording:  No
═══════════════════════════════════════
```

---

## Monitor Tools

### `toggle-monitor`
Toggle external monitor on/off.

**Keybinding:** `Mod+Z`

---

## Workflow Examples

### Sequential Capture (All of One Type, Then Another)

```bash
# Setup
record-lando-setup "sliding-puzzle" 6 4
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1

# ===== Record ALL videos first =====
Mod+Ctrl+R → demo → Mod+Ctrl+R  # ch-1
# Output: Video: ch-2  Thumbnail: ch-1  Screenshot: ch-1

Mod+Ctrl+R → demo → Mod+Ctrl+R  # ch-2
# Output: Video: ch-3  Thumbnail: ch-1  Screenshot: ch-1

# ... continue for ch-3, ch-4, ch-5, ch-6, hero, ext-1, ext-2, ext-3, ext-4
# Output: Video: DONE  Thumbnail: ch-1  Screenshot: ch-1

# ===== Then capture ALL thumbnails =====
Mod+Ctrl+T  # ch-1
# Output: Video: DONE  Thumbnail: ch-2  Screenshot: ch-1

Mod+Ctrl+T  # ch-2
# Output: Video: DONE  Thumbnail: ch-3  Screenshot: ch-1

# ... continue through all thumbnails
# Output: Video: DONE  Thumbnail: DONE  Screenshot: ch-1

# ===== Then capture ALL screenshots =====
Mod+Ctrl+S  # ch-1
# Output: Video: DONE  Thumbnail: DONE  Screenshot: ch-2

# ... continue through all screenshots
# Output: Video: DONE  Thumbnail: DONE  Screenshot: DONE
```

### Interleaved Capture (Mix and Match)

```bash
# Setup
record-lando-setup "puzzle" 3 2
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1

# Do different capture types for each puzzle
Mod+Ctrl+T  # Thumbnail of ch-1
# Output: Video: ch-1  Thumbnail: ch-2  Screenshot: ch-1

Mod+Ctrl+S  # Screenshot of ch-1
# Output: Video: ch-1  Thumbnail: ch-2  Screenshot: ch-2

Mod+Ctrl+R → demo → Mod+Ctrl+R  # Video of ch-1
# Output: Video: ch-2  Thumbnail: ch-2  Screenshot: ch-2

Mod+Ctrl+T  # Thumbnail of ch-2
# Output: Video: ch-2  Thumbnail: ch-3  Screenshot: ch-2

# Counters are independent - use any tool in any order!
```

### Error Recovery

```bash
# Oops, messed up video ch-3
Mod+Ctrl+[  # Goes back on video counter (last used)
# Output: Video: ch-2  Thumbnail: ch-5  Screenshot: ch-1

Mod+Ctrl+]  # Skip ch-2
# Output: Video: ch-3  Thumbnail: ch-5  Screenshot: ch-1

Mod+Ctrl+R → demo → Mod+Ctrl+R  # Re-record ch-3
# Output: Video: ch-4  Thumbnail: ch-5  Screenshot: ch-1
```

### Check Progress Anytime

```bash
record-lando-status
# Shows all 3 counters, which was last used, recording status
```

---

## File Outputs

All Lando captures go to: `~/development/Lando/lando-video/{NAME}/`

**File naming:**
- Videos: `{NAME}-{IDENTIFIER}.mp4`
- Thumbnails: `{NAME}-{IDENTIFIER}.png`
- Screenshots: `scramble-{NAME}-{IDENTIFIER}.png`, `complete-{NAME}-{IDENTIFIER}.png`

**Identifiers:**
- Challenges: `ch-1`, `ch-2`, `ch-3`, ...
- Hero: `hero`
- Extensions: `ext-1`, `ext-2`, `ext-3`, ...

---

## Dependencies

### Wayland Screen Capture
- **slurp** - Region selection
- **grim** - Screenshots
- **wf-recorder** - Video recording

### Notifications
- **libnotify** - Desktop notifications

All dependencies are managed via NixOS configuration.

---

## Keybinding Summary

| Keybinding | Action | Tool |
|------------|--------|------|
| `Mod+Ctrl+R` | Toggle recording | record-lando-video |
| `Mod+Ctrl+T` | Capture thumbnail | get-lando-thumbnail |
| `Mod+Ctrl+S` | Capture screenshot pair | get-lando-screenshot |
| `Mod+Ctrl+[` | Go to previous (smart) | record-lando-prev |
| `Mod+Ctrl+]` | Go to next (smart) | record-lando-next |
| `Mod+Z` | Toggle monitor | toggle-monitor |
| `Print` | Screenshot | screenshot --copy |

**Note:** `Mod` = Super/Windows key

---

## Tips

- **Flexible workflow:** Do all videos at once, then all thumbnails, or mix them freely
- **Always visible:** Every operation shows all 3 counter states
- **Smart navigation:** Prev/next automatically work on whichever tool you last used
- **Check status:** Run `record-lando-status` anytime to see your progress
- **Three geometries:** Video/thumbnail share one geometry, screenshot pairs use two separate geometries for scrambled and completed states
- **Default geometries:** All three can be overridden during setup if needed
