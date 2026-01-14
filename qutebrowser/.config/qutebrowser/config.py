import os

# ==============================================================================
# General Configuration
# ==============================================================================

# Load existing settings made via :set
# set to False to strictly use this file (declarative)
config.load_autoconfig(False)

# Minimalist UI: Hide statusbar unless in insert/command mode
c.statusbar.show = "in-mode"

# Minimalist UI: Only show tabs if there is more than one
c.tabs.show = "multiple"

# Clean up the window (hide scrollbars)
c.scrolling.bar = "never"

# Enable smooth scrolling for a better feel
c.scrolling.smooth = True

# Set default start page and search engine
c.url.start_pages = ["http://glance.homelab.local/home"]
c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "aw": "https://wiki.archlinux.org/?search={}",  # Arch Wiki shortcut
    "g": "https://google.com/search?q={}",
    "yt": "https://youtube.com/results?search_query={}",
}

# ==============================================================================
# Keybindings (Spacemacs/Vim style)
# ==============================================================================

# Toggle statusbar and tabs visibility with ",u" (UI toggle)
# Unbind the default <Space> (which usually scrolls down)
# This isn't strictly necessary as binding a sequence overwrites it,
# but it keeps things clean and intentional.
config.unbind("<Space>", mode="normal")

# General / Meta
# SPC SPC -> Open URL (Like M-x or helm-find-files)
config.bind("<Space><Space>", "cmd-set-text -s :open", mode="normal")
# SPC : -> M-x (Run Command)
config.bind("<Space>:", "cmd-set-text :", mode="normal")
# SPC / -> Search in page (Like swiper/search)
config.bind("<Space>/", "cmd-set-text /", mode="normal")
# SPC TAB -> Switch to previous tab (Like mode-line-other-buffer)
config.bind("<Space><Tab>", "tab-focus last", mode="normal")

# Bookmarks
config.bind("<Space>bl", "bookmark-list", mode="normal")
config.bind("<Space>bk", ":open karakeep.homelab.local", mode="normal")

# Buffers (Tabs) - 'b' prefix
# SPC b b -> List/Switch buffers
config.bind("<Space>bb", "cmd-set-text -s :buffer", mode="normal")
# SPC b d -> Kill buffer (close tab)
config.bind("<Space>bd", "tab-close", mode="normal")
# SPC b n -> Next buffer
config.bind("<Space>bn", "tab-next", mode="normal")
# SPC b p -> Previous buffer
config.bind("<Space>bp", "tab-prev", mode="normal")
# SPC b D -> Close all other tabs (cleanup)
config.bind("<Space>bD", "tab-close --other", mode="normal")
# SPC b u -> Reopen closed tab (Undo close)
config.bind("<Space>bu", "undo", mode="normal")

# Files / Open - 'f' prefix
# SPC f f -> Open URL (same as SPC SPC, purely for mnemonic)
config.bind("<Space>ff", "cmd-set-text -s :open", mode="normal")
# SPC f t -> Open URL in new tab
config.bind("<Space>ft", "cmd-set-text -s :open -t", mode="normal")
# SPC f y -> Copy URL (Yank)
config.bind("<Space>fy", "hint links yank", mode="normal")

# Windows / Navigation - 'w' prefix
# SPC w / -> Split view (Qutebrowser doesn't do internal panes well,
# so we map this to opening a new window)
config.bind("<Space>w/", "open -w", mode="normal")
# SPC w d -> Close window
config.bind("<Space>wd", "close", mode="normal")

# Quit / Session - 'q' prefix
# SPC q q -> Quit application
config.bind("<Space>qq", "quit", mode="normal")
# SPC q r -> Restart/Reload Config
config.bind("<Space>qr", "config-source", mode="normal")

# Zooming - 'z' prefix (Spacemacs uses z for frame/text scaling)
config.bind("<Space>z+", "zoom-in", mode="normal")
config.bind("<Space>z-", "zoom-out", mode="normal")
config.bind("<Space>z0", "zoom", mode="normal")

# ==============================================================================
# Password Management (Bitwarden)
# ==============================================================================

# Make sure the userscript is executable.
# On Arch, these scripts are usually located at /usr/share/qutebrowser/userscripts/
# You can refer to them by name if they are in the path, or by full path.

# SPC p l -> Login (Fill credentials)
# This spawns the qute-bitwarden script which uses 'rofi' to show your accounts.
config.bind("<Space>pl", "spawn --userscript qute-bitwarden", mode="normal")

# SPC p u -> Username only
config.bind(
    "<Space>pu", "spawn --userscript qute-bitwarden --username-only", mode="normal"
)

# SPC p p -> Password only
config.bind(
    "<Space>pp", "spawn --userscript qute-bitwarden --password-only", mode="normal"
)

# ==============================================================================
# External Editor (Spacemacs)
# ==============================================================================

# Use emacsclient to open the editor in a new frame (-c).
# This creates a seamless popup experience when editing text fields.
# '-a ""' attempts to start emacs daemon if it's not currently running.
# Write your text, save the buffer (SPC f s), and close the frame
# (SPC q f or C-x 5 0). The text will automatically populate in Qutebrowser.
c.editor.command = ["emacsclient", "-c", "+{line}:{column}", "{file}"]

# ==============================================================================
# Gruvbox Dark (Hard) Theme
# ==============================================================================

# Define Gruvbox colors
gruvbox = {
    "bg0_hard": "#1d2021",
    "bg0": "#282828",
    "bg0_soft": "#32302f",
    "bg1": "#3c3836",
    "bg2": "#504945",
    "bg3": "#665c54",
    "bg4": "#7c6f64",
    "fg0": "#fbf1c7",
    "fg1": "#ebdbb2",
    "fg2": "#d5c4a1",
    "fg3": "#bdae93",
    "fg4": "#a89984",
    "red": "#fb4934",
    "green": "#b8bb26",
    "yellow": "#fabd2f",
    "blue": "#83a598",
    "purple": "#d3869b",
    "aqua": "#8ec07c",
    "orange": "#fe8019",
}

# Apply Colors

# Completion widget
c.colors.completion.fg = gruvbox["fg1"]
c.colors.completion.odd.bg = gruvbox["bg1"]
c.colors.completion.even.bg = gruvbox["bg0"]
c.colors.completion.category.fg = gruvbox["yellow"]
c.colors.completion.category.bg = gruvbox["bg2"]
c.colors.completion.category.border.top = gruvbox["bg2"]
c.colors.completion.category.border.bottom = gruvbox["bg2"]
c.colors.completion.item.selected.fg = gruvbox["fg0"]
c.colors.completion.item.selected.bg = gruvbox["bg4"]
c.colors.completion.item.selected.border.top = gruvbox["bg4"]
c.colors.completion.item.selected.border.bottom = gruvbox["bg4"]
c.colors.completion.match.fg = gruvbox["orange"]

# Status bar
c.colors.statusbar.normal.fg = gruvbox["fg1"]
c.colors.statusbar.normal.bg = gruvbox["bg0_hard"]
c.colors.statusbar.insert.fg = gruvbox["bg0"]
c.colors.statusbar.insert.bg = gruvbox["blue"]
c.colors.statusbar.passthrough.fg = gruvbox["bg0"]
c.colors.statusbar.passthrough.bg = gruvbox["purple"]
c.colors.statusbar.command.fg = gruvbox["fg1"]
c.colors.statusbar.command.bg = gruvbox["bg2"]
c.colors.statusbar.url.fg = gruvbox["fg1"]
c.colors.statusbar.url.error.fg = gruvbox["red"]
c.colors.statusbar.url.warn.fg = gruvbox["orange"]
c.colors.statusbar.url.hover.fg = gruvbox["aqua"]
c.colors.statusbar.url.success.http.fg = gruvbox["green"]
c.colors.statusbar.url.success.https.fg = gruvbox["green"]

# Tabs
c.colors.tabs.bar.bg = gruvbox["bg0_hard"]
c.colors.tabs.odd.fg = gruvbox["fg1"]
c.colors.tabs.odd.bg = gruvbox["bg0_hard"]
c.colors.tabs.even.fg = gruvbox["fg1"]
c.colors.tabs.even.bg = gruvbox["bg0_hard"]
c.colors.tabs.selected.odd.fg = gruvbox["fg0"]
c.colors.tabs.selected.odd.bg = gruvbox["bg2"]
c.colors.tabs.selected.even.fg = gruvbox["fg0"]
c.colors.tabs.selected.even.bg = gruvbox["bg2"]
c.colors.tabs.pinned.odd.fg = gruvbox["fg0"]
c.colors.tabs.pinned.odd.bg = gruvbox["bg2"]
c.colors.tabs.pinned.even.fg = gruvbox["fg0"]
c.colors.tabs.pinned.even.bg = gruvbox["bg2"]

# Webpage (Dark Mode)
# This forces the browser to render pages in dark mode (uses Chromium's blink settings)
c.colors.webpage.preferred_color_scheme = "dark"
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = "smart"

# ==============================================================================
# Fonts & UI Polishing
# ==============================================================================

# Fonts
# Use a good monospace font for the UI to match Spacemacs.
# Note: 'Monospace' usually resolves to your system default (e.g., Source Code Pro, JetBrains Mono).
c.fonts.default_family = "Monospace"
c.fonts.default_size = "10pt"

# Specific UI elements
c.fonts.tabs.selected = "bold 10pt default_family"
c.fonts.tabs.unselected = "10pt default_family"
c.fonts.statusbar = "10pt default_family"
c.fonts.downloads = "10pt default_family"

# Hints (The yellow boxes)
# Make them smaller, bold, and high contrast
c.fonts.hints = "bold 10pt default_family"

# Border radius for a slight polish (optional, set to 0 for strict rectangle)
c.hints.radius = 2

# Border styling - minimal 1px
c.hints.border = "1px solid " + gruvbox["bg0"]

# Hints Colors (Gruvbox High Contrast)
# Background: Yellow (easy to spot)
# Foreground: Dark Hard (readable text)
c.colors.hints.bg = gruvbox["yellow"]
c.colors.hints.fg = gruvbox["bg0_hard"]

# Match Color (when you type the first letter of a hint)
c.colors.hints.match.fg = gruvbox["fg1"]  # Text color for match
c.colors.hints.fg = gruvbox["bg0"]  # Keep text dark

# Hint Behavior
# Use home row keys for hints (standard vim/spacemacs feel)
c.hints.chars = "asdfghjkl"

# Uppercase hints? No, keep it lowercase for speed
c.hints.uppercase = False
