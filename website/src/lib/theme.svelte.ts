/// Theme preference: 'system' follows the OS, 'light'/'dark' pin it, and a
/// number strictly between 0 and 1 is an intermediate the page renders
/// continuously (0 is light, 1 is dark). `mix` is that number for any
/// preference; it is mirrored onto <html style="--mix"> (app.css derives every
/// colour from it) and the side of the midpoint onto <html data-theme> for the
/// few things that can only flip.
export type ThemePref = 'system' | 'light' | 'dark' | number
export type Theme = 'light' | 'dark'
export type SiteStyle = 'original' | 'simple'

export const THEME_KEY = 'disp-theme'
export const STYLE_KEY = 'disp-style'
export const FUNNY_KEY = 'disp-funny'

const DARK_QUERY = '(prefers-color-scheme: dark)'

/// A stored preference: one of the three words, or a number between the two
/// ends (the ends themselves read back as the words). Keep in step with the
/// app.html head script.
export function parsePref(v: unknown): ThemePref {
  if (v === 'system' || v === 'light' || v === 'dark') return v
  if (typeof v === 'number' || (typeof v === 'string' && v !== '')) {
    const n = Number(v)
    if (Number.isFinite(n)) return n <= 0 ? 'light' : n >= 1 ? 'dark' : n
  }
  return 'system'
}

/// Reads the stored preference. Private-mode Safari throws on localStorage.
function storedPref(): ThemePref {
  try {
    return parsePref(localStorage.getItem(THEME_KEY))
  } catch {
    return 'system'
  }
}

function storedStyle(): SiteStyle {
  try {
    return localStorage.getItem(STYLE_KEY) === 'simple' ? 'simple' : 'original'
  } catch {
    return 'original'
  }
}

/// The field guide's jokes instead of its definitions; off unless asked for.
function storedFunny(): boolean {
  try {
    return localStorage.getItem(FUNNY_KEY) === '1'
  } catch {
    return false
  }
}

function systemTheme(): Theme {
  return typeof matchMedia === 'function' && matchMedia(DARK_QUERY).matches
    ? 'dark'
    : 'light'
}

class ThemeStore {
  /// SSR prerenders the light palette; the app.html head script fixes the DOM
  /// before first paint, and `sync()` re-reads storage once mounted.
  pref = $state<ThemePref>('system')
  system = $state<Theme>('light')
  style = $state<SiteStyle>('original')
  funny = $state(false)

  /// 0 light … 1 dark, whatever the preference
  get mix(): number {
    if (this.pref === 'system') return this.system === 'dark' ? 1 : 0
    if (this.pref === 'light') return 0
    if (this.pref === 'dark') return 1
    return this.pref
  }

  /// the side of the midpoint, for what can only be one or the other
  get resolved(): Theme {
    return this.mix < 0.5 ? 'light' : 'dark'
  }

  /// Adopt the real preference after hydration and follow later OS changes.
  sync() {
    this.pref = storedPref()
    this.system = systemTheme()
    this.style = storedStyle()
    this.funny = storedFunny()
    this.apply()

    if (typeof matchMedia !== 'function') return
    const mq = matchMedia(DARK_QUERY)
    const onChange = (e: MediaQueryListEvent) => {
      this.system = e.matches ? 'dark' : 'light'
      this.apply()
    }
    mq.addEventListener('change', onChange)
    return () => mq.removeEventListener('change', onChange)
  }

  set(pref: ThemePref) {
    this.pref = parsePref(pref) // the ends snap to their words
    try {
      if (this.pref === 'system') localStorage.removeItem(THEME_KEY)
      else localStorage.setItem(THEME_KEY, String(this.pref))
    } catch {
      // no persistence available; the in-memory preference still applies
    }
    this.apply()
  }

  /// system → light → dark → system; an intermediate steps on to dark
  cycle() {
    const p = this.pref
    this.set(p === 'system' ? 'light' : p === 'light' ? 'dark' : p === 'dark' ? 'system' : 'dark')
  }

  /// While a scrub drags the mix, the page follows the pointer with no easing.
  setScrubbing(on: boolean) {
    if (typeof document === 'undefined') return
    document.documentElement.classList.toggle('scrubbing', on)
  }

  setStyle(style: SiteStyle) {
    this.style = style
    try {
      if (style === 'original') localStorage.removeItem(STYLE_KEY)
      else localStorage.setItem(STYLE_KEY, style)
    } catch {
      // The switch still works when storage is unavailable.
    }
    this.apply()
  }

  toggleStyle() {
    this.setStyle(this.style === 'simple' ? 'original' : 'simple')
  }

  setFunny(funny: boolean) {
    this.funny = funny
    try {
      if (funny) localStorage.setItem(FUNNY_KEY, '1')
      else localStorage.removeItem(FUNNY_KEY)
    } catch {
      // as above
    }
    this.apply()
  }

  private apply() {
    if (typeof document === 'undefined') return
    const el = document.documentElement
    el.dataset.theme = this.resolved
    el.style.setProperty('--mix', String(this.mix))
    el.dataset.style = this.style
    el.dataset.funny = this.funny ? '1' : '0'
  }
}

export const theme = new ThemeStore()
