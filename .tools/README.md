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
record-lando-setup NAME NUM_CHALLENGES NUM_EXTENSIONS [SCREENSHOTS_CONFIG]
record-lando-setup GEOM SCRAMBLE_GEOM COMPLETE_GEOM NAME NUM_CHALLENGES NUM_EXTENSIONS [SCREENSHOTS_CONFIG]
```

**Examples:**
```bash
# Use default geometries, all challenges have 1 screenshot
record-lando-setup "random-matching" 5 3

# Specify multiple screenshots for some challenges (only videos/thumbnails unaffected)
record-lando-setup "random-matching" 5 3 "4-2,5-3,hero-2,ex1-3"

# Use custom geometries with screenshot config
record-lando-setup "1507,345 720x597" "2650,889 646x712" "3294,897 624x704" "random-matching" 5 3 "4-2,hero-2"
```

**Screenshots Config Format:**
- Optional parameter specifying challenges with multiple screenshots
- Format: `"CHALLENGE-COUNT,CHALLENGE-COUNT,..."`
- Only list challenges with **more than 1** screenshot (others default to 1)
- Challenge formats:
  - `4-2` = Challenge 4 has 2 screenshots
  - `hero-3` = Hero has 3 screenshots
  - `ex1-2` = Extension 1 has 2 screenshots

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
Screenshots config: 4-2,5-3,hero-2,ex1-3

Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1
```

**Note:** The "Screenshots config" line only appears if you specify the optional parameter.

**Creates:**
- State file at `~/.config/record-lando/state`
- Sequence: ch-1, ch-2, ..., ch-N, hero, ext-1, ext-2, ..., ext-M
- 3 independent counters (VIDEO_INDEX, THUMBNAIL_INDEX, SCREENSHOT_INDEX)
- Screenshot sub-index counter (SCREENSHOT_SUB_INDEX)
- Screenshots configuration (SCREENSHOTS_CONFIG)
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

**Multiple Screenshots Per Challenge:**
When a challenge has multiple screenshots configured (e.g., `"4-2"`), files use sub-index notation:
- `scramble-random-matching-ch-4_1.png` (first screenshot of challenge 4)
- `scramble-random-matching-ch-4_2.png` (second screenshot of challenge 4)
- `scramble-random-matching-hero_1.png`, `scramble-random-matching-hero_2.png` (hero with 2 screenshots)

**Navigation:**
- Automatically increments sub-index first (ch-4_1 → ch-4_2)
- Moves to next challenge when all sub-screenshots complete (ch-4_2 → ch-5_1)
- `record-lando-prev` and `record-lando-next` work with sub-indexes

**Status Display:**
```
Captured scramble/complete-random-matching-ch-4_1.png
Video: ch-2  Thumbnail: ch-2  Screenshot: ch-4_2
```

**Note:** This tool captures two separate regions (scramble and complete) in a single operation, perfect for side-by-side puzzle states. Videos and thumbnails are unaffected by screenshot configuration.

### Navigation Tools

#### `record-lando-prev`
**Smart decrement** - Goes back on whichever counter was most recently used.

**Keybinding:** `Mod+Ctrl+[`

**Example:**
```bash
Mod+Ctrl+T              # Use thumbnail
Mod+Ctrl+[              # Goes back on thumbnail counter
# Output: Decremented Thumbnail counter.
#         Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1

Mod+Ctrl+R              # Use video
Mod+Ctrl+[              # Goes back on video counter
# Output: Decremented Video counter.
#         Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1

Mod+Ctrl+S              # Use screenshot (on ch-4 with 2 screenshots, currently at ch-4_2)
Mod+Ctrl+[              # Goes back on screenshot counter
# Output: Decremented Screenshot counter.
#         Video: ch-1  Thumbnail: ch-1  Screenshot: ch-4_1
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
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1

# ===== Record ALL videos first =====
Mod+Ctrl+R → demo → Mod+Ctrl+R  # ch-1
# Output: Video: ch-2  Thumbnail: ch-1  Screenshot: ch-1_1

Mod+Ctrl+R → demo → Mod+Ctrl+R  # ch-2
# Output: Video: ch-3  Thumbnail: ch-1  Screenshot: ch-1_1

# ... continue for ch-3, ch-4, ch-5, ch-6, hero, ext-1, ext-2, ext-3, ext-4
# Output: Video: DONE  Thumbnail: ch-1  Screenshot: ch-1_1

# ===== Then capture ALL thumbnails =====
Mod+Ctrl+T  # ch-1
# Output: Video: DONE  Thumbnail: ch-2  Screenshot: ch-1_1

Mod+Ctrl+T  # ch-2
# Output: Video: DONE  Thumbnail: ch-3  Screenshot: ch-1_1

# ... continue through all thumbnails
# Output: Video: DONE  Thumbnail: DONE  Screenshot: ch-1_1

# ===== Then capture ALL screenshots =====
Mod+Ctrl+S  # ch-1
# Output: Video: DONE  Thumbnail: DONE  Screenshot: ch-2_1

# ... continue through all screenshots
# Output: Video: DONE  Thumbnail: DONE  Screenshot: DONE
```

### Interleaved Capture (Mix and Match)

```bash
# Setup
record-lando-setup "puzzle" 3 2
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1

# Do different capture types for each puzzle
Mod+Ctrl+T  # Thumbnail of ch-1
# Output: Video: ch-1  Thumbnail: ch-2  Screenshot: ch-1_1

Mod+Ctrl+S  # Screenshot of ch-1
# Output: Video: ch-1  Thumbnail: ch-2  Screenshot: ch-2_1

Mod+Ctrl+R → demo → Mod+Ctrl+R  # Video of ch-1
# Output: Video: ch-2  Thumbnail: ch-2  Screenshot: ch-2_1

Mod+Ctrl+T  # Thumbnail of ch-2
# Output: Video: ch-2  Thumbnail: ch-3  Screenshot: ch-2_1

# Counters are independent - use any tool in any order!
```

### Error Recovery

```bash
# Oops, messed up video ch-3
Mod+Ctrl+[  # Goes back on video counter (last used)
# Output: Video: ch-2  Thumbnail: ch-5  Screenshot: ch-1_1

Mod+Ctrl+]  # Skip ch-2
# Output: Video: ch-3  Thumbnail: ch-5  Screenshot: ch-1_1

Mod+Ctrl+R → demo → Mod+Ctrl+R  # Re-record ch-3
# Output: Video: ch-4  Thumbnail: ch-5  Screenshot: ch-1_1
```

### Check Progress Anytime

```bash
record-lando-status
# Shows all 3 counters, which was last used, recording status
```

### Multiple Screenshots Per Challenge

```bash
# Setup with challenges 2 and 3 having multiple screenshots
record-lando-setup "multi-screenshot-puzzle" 4 2 "2-3,3-2,hero-2"
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-1_1

# Capture screenshots for challenge 1 (default: 1 screenshot)
Mod+Ctrl+S  # Captures ch-1_1
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-2_1

# Capture screenshots for challenge 2 (has 3 screenshots)
Mod+Ctrl+S  # Captures ch-2_1
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-2_2

Mod+Ctrl+S  # Captures ch-2_2
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-2_3

Mod+Ctrl+S  # Captures ch-2_3
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-3_1
# (automatically moves to next challenge after completing all sub-screenshots)

# Capture screenshots for challenge 3 (has 2 screenshots)
Mod+Ctrl+S  # Captures ch-3_1
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-3_2

Mod+Ctrl+S  # Captures ch-3_2
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-4_1

# Navigation works with sub-indexes
Mod+Ctrl+[  # Goes back (last used was screenshot)
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-3_2

Mod+Ctrl+[  # Goes back again
# Output: Video: ch-1  Thumbnail: ch-1  Screenshot: ch-3_1

# Note: Videos and thumbnails still follow the old pattern (no sub-indexes)
```

---

## File Outputs

All Lando captures go to: `~/development/Lando/lando-video/{NAME}/`

**File naming:**
- Videos: `{NAME}-{IDENTIFIER}.mp4`
- Thumbnails: `{NAME}-{IDENTIFIER}.png`
- Screenshots (single): `scramble-{NAME}-{IDENTIFIER}.png`, `complete-{NAME}-{IDENTIFIER}.png`
- Screenshots (multiple): `scramble-{NAME}-{IDENTIFIER}_{SUB}.png`, `complete-{NAME}-{IDENTIFIER}_{SUB}.png`

**Identifiers:**
- Challenges: `ch-1`, `ch-2`, `ch-3`, ...
- Hero: `hero`
- Extensions: `ext-1`, `ext-2`, `ext-3`, ...

**Screenshot Sub-indexes:**
When a challenge has multiple screenshots, files include a sub-index:
- `scramble-random-matching-ch-1_1.png` (first screenshot)
- `scramble-random-matching-ch-1_2.png` (second screenshot)
- `complete-random-matching-ch-1_1.png`, `complete-random-matching-ch-1_2.png`

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
- **Multiple screenshots:** Only specify challenges with more than 1 screenshot in config; others default to 1
- **Sub-index navigation:** Screenshot navigation handles two dimensions automatically (challenge and sub-screenshot)
- **Videos unaffected:** Multiple screenshots configuration only affects screenshot pairs; videos and thumbnails remain one per challenge
