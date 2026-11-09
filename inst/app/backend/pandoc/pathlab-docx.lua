-- PathLab: keep editor formatting that pandoc's HTML reader does not map to Word by itself.
-- * <span style="font-family: …; font-size: …; color: …; background-color: …"> -> Word runs with that font, size,
--   text colour and shading. Bold, italic, underline, strikethrough, sub- and superscript inside the span are kept;
--   nested spans combine (the inner one wins). A span holding anything else (a link, a formula) is left to pandoc.
-- * <div data-align="center|right|justify"> -> paragraphs in the Centered / Right / Justified styles.
local function esc(s) return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")) end

local function hex(v)
  if not v then return nil end
  local h = v:match("#(%x%x%x%x%x%x)")
  if h then return h:upper() end
  local r, g, b = v:match("rgba?%((%d+),%s*(%d+),%s*(%d+)")
  if r then return string.format("%02X%02X%02X", tonumber(r), tonumber(g), tonumber(b)) end
  return nil
end

-- Font size in half-points (Word's unit) from "14pt", "18.6667px", "1.2em" is ignored.
local function half_points(v)
  if not v then return nil end
  local n, unit = v:match("([%d%.]+)%s*(%a*)")
  n = tonumber(n)
  if not n then return nil end
  if unit == "px" then n = n * 0.75 elseif unit ~= "pt" then return nil end
  return tostring(math.floor(n * 2 + 0.5))
end

local function props_of(style)
  local p = {}
  p.fg = hex(style:match("[^-]color:%s*([^;]+)") or style:match("^color:%s*([^;]+)"))
  p.bg = hex(style:match("background%-color:%s*([^;]+)"))
  local fam = style:match("font%-family:%s*([^;]+)")
  if fam then p.font = fam:match("^%s*[\"']?([^,\"']+)") end
  if p.font then p.font = p.font:gsub("%s+$", "") end
  p.size = half_points(style:match("font%-size:%s*([^;]+)"))
  return p
end

-- A Word run of `text` with the span properties `p` and the character flags `f`, in the order Word's schema wants.
local function run(text, p, f)
  local r = ""
  if p.font then local q = esc(p.font):gsub('"', "&quot;"); r = r .. '<w:rFonts w:ascii="' .. q .. '" w:hAnsi="' .. q .. '" w:cs="' .. q .. '"/>' end
  if f.b then r = r .. "<w:b/>" end
  if f.i then r = r .. "<w:i/>" end
  if f.strike then r = r .. "<w:strike/>" end
  if p.fg then r = r .. '<w:color w:val="' .. p.fg .. '"/>' end
  if p.size then r = r .. '<w:sz w:val="' .. p.size .. '"/><w:szCs w:val="' .. p.size .. '"/>' end
  if f.u then r = r .. '<w:u w:val="single"/>' end
  if p.bg then r = r .. '<w:shd w:val="clear" w:color="auto" w:fill="' .. p.bg .. '"/>' end
  if f.va then r = r .. '<w:vertAlign w:val="' .. f.va .. '"/>' end
  return '<w:r><w:rPr>' .. r .. '</w:rPr><w:t xml:space="preserve">' .. esc(text) .. '</w:t></w:r>'
end

local function with(f, k, v) local g = {}; for a, b in pairs(f) do g[a] = b end; g[k] = v; return g end
local function merged(outer, inner) local m = {}; for k, v in pairs(outer) do m[k] = v end; for k, v in pairs(inner) do m[k] = v end; return m end

-- The runs of a span's content, or nil when it holds something this filter does not rebuild.
local function runs(inlines, p, f)
  local out = {}
  for _, el in ipairs(inlines) do
    local t = el.t
    local part
    if t == "Str" then part = run(el.text, p, f)
    elseif t == "Space" or t == "SoftBreak" then part = run(" ", p, f)
    elseif t == "LineBreak" then part = "<w:r><w:br/></w:r>"
    elseif t == "Strong" then part = runs(el.content, p, with(f, "b", true))
    elseif t == "Emph" then part = runs(el.content, p, with(f, "i", true))
    elseif t == "Underline" then part = runs(el.content, p, with(f, "u", true))
    elseif t == "Strikeout" then part = runs(el.content, p, with(f, "strike", true))
    elseif t == "Subscript" then part = runs(el.content, p, with(f, "va", "subscript"))
    elseif t == "Superscript" then part = runs(el.content, p, with(f, "va", "superscript"))
    elseif t == "Span" then part = runs(el.content, merged(p, props_of(el.attributes["style"] or "")), f)
    else return nil end
    if not part then return nil end
    out[#out + 1] = part
  end
  return table.concat(out)
end

-- Top-down, so an outer span sees its nested spans (and combines their styles) before they are converted.
function Span(el)
  local p = props_of(el.attributes["style"] or "")
  if not (p.fg or p.bg or p.font or p.size) then return nil end
  local xml = runs(el.content, p, {})
  if not xml then return nil end
  return pandoc.RawInline("openxml", xml), false
end

local styles = { center = "Centered", right = "Right", justify = "Justified" }
function Div(el)
  local a = el.attributes["align"] or el.attributes["data-align"]
  if a and styles[a] then el.attributes["custom-style"] = styles[a]; return el end
  return nil
end

traverse = "topdown"
