-- PathLab: keep editor formatting that pandoc's HTML reader does not map to Word by itself.
-- * <span style="color: …; background-color: …"> -> a Word run with that text colour and shading
--   (the span's text; nested formatting inside a coloured span is flattened).
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

function Span(el)
  local style = el.attributes["style"] or ""
  local fg = hex(style:match("[^-]color:%s*([^;]+)") or style:match("^color:%s*([^;]+)"))
  local bg = hex(style:match("background%-color:%s*([^;]+)"))
  if not fg and not bg then return nil end
  local rpr = ""
  if fg then rpr = rpr .. '<w:color w:val="' .. fg .. '"/>' end
  if bg then rpr = rpr .. '<w:shd w:val="clear" w:color="auto" w:fill="' .. bg .. '"/>' end
  local text = pandoc.utils.stringify(el.content)
  return pandoc.RawInline("openxml", '<w:r><w:rPr>' .. rpr .. '</w:rPr><w:t xml:space="preserve">' .. esc(text) .. '</w:t></w:r>')
end

local styles = { center = "Centered", right = "Right", justify = "Justified" }
function Div(el)
  local a = el.attributes["align"] or el.attributes["data-align"]
  if a and styles[a] then el.attributes["custom-style"] = styles[a]; return el end
  return nil
end
