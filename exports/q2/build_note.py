"""build_note.py: the Downloads note as a Word file for the dashboard.

Every piece of text that depends on the data (body text, captions, appendix
lines, tables) is read from a JSON spec written by exports/R/note_q2_spec.R.
No AI is used.
Usage:  python3 build_note.py spec.json
"""
import csv, json, re, sys, os
sys.path.insert(0, os.path.dirname(__file__))
from docx.shared import Pt, Inches
from docx.oxml.ns import qn
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx_helpers import (new_document, footer, rich, rule, add_hyperlink, table, GREY)

S = json.load(open(sys.argv[1], encoding="utf-8"))
FIGDIR = S["figures_dir"]
facts = {r["id"]: r for r in csv.DictReader(open(S["facts_csv"], encoding="utf-8"))}
refs = list(csv.DictReader(open(S["refs_csv"], encoding="utf-8")))
used = []


def F(text):
    """Replace {fact_id} with the fact's display text and record the id."""
    def sub(m):
        k = m.group(1)
        if k not in facts:
            raise KeyError("Unknown fact id: " + k)
        if k not in used:
            used.append(k)
        return facts[k]["display"]
    return re.sub(r"\{([a-z0-9_]+)\}", sub, text)


doc = new_document(margins=(0.6, 0.7, 0.6, 0.7), body_pt=10.5)
footer(doc, S["footer"])
doc.styles["Heading 1"].paragraph_format.space_before = Pt(4)
doc.styles["Heading 2"].paragraph_format.space_before = Pt(3)
doc.styles["Reference"].paragraph_format.space_after = Pt(0)


def P(text, style=None, after=None):
    p = doc.add_paragraph(style=style)
    rich(p, F(text))
    if after is not None:
        p.paragraph_format.space_after = Pt(after)
    return p


def bullet(text):
    p = doc.add_paragraph(style="List Bullet")
    rich(p, F(text))
    return p


def small_grey(par, text):
    r = par.add_run(F(text)); r.bold = False; r.font.size = Pt(7.5); r.font.color.rgb = GREY
    return r


def picture(path, label, text, width=6.6, note=None):
    p = doc.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_before = Pt(2); p.paragraph_format.space_after = Pt(0)
    p.paragraph_format.keep_with_next = True
    p.add_run().add_picture(os.path.join(FIGDIR, path), width=Inches(width))
    c = doc.add_paragraph(style="Caption")
    r = c.add_run(label + ". "); r.bold = True
    rich(c, F(text))
    if note:
        small_grey(c, " " + note)
    c.paragraph_format.space_before = Pt(1); c.paragraph_format.space_after = Pt(3)


# ---- Body --------------------------------------------------------------------
paras = [l for l in S["paras"] if l.strip()]
H1 = {"Summary", "Background", "Findings", "Implications and next steps for B.C.", "Data notes", "References"}
FIG = {k: (v[0], float(v[1]), v[2]) for k, v in S["fig"].items()}
SHORT_LINK = {r["n"]: r["url"] for r in refs}


def styled(t):
    t = t.replace("B.C. Labour Market Outlook", "*B.C. Labour Market Outlook*")
    t = t.replace("The Outlook expects", "The *Outlook* expects")
    return t


title, byline = paras[0], paras[1]
doc.add_paragraph(title, style="Title")
by = doc.add_paragraph(byline, style="Byline"); rule(by)
section = None
i = 2
while i < len(paras):
    t = paras[i]
    if t in H1:
        section = t
        h = doc.add_paragraph(style="Heading 1"); h.add_run(t)
    elif t.startswith("[FIGURE"):
        num = re.match(r"\[FIGURE (\d+)", t).group(1)
        cap = paras[i + 1]; assert cap.startswith(f"Figure {num}. "), cap
        path, width, dash = FIG[num]
        picture(path, f"Figure {num}", cap[len(f"Figure {num}. "):], width, note=dash)
        i += 1
    elif t.startswith("[TABLE"):
        num = re.match(r"\[TABLE (\d+)", t).group(1)
        cap = paras[i + 1]; assert cap.startswith(f"Table {num}. "), cap
        c = doc.add_paragraph(style="Caption"); c.paragraph_format.space_before = Pt(3); c.paragraph_format.space_after = Pt(1)
        r = c.add_run(f"Table {num}. "); r.bold = True
        rich(c, cap[len(f"Table {num}. "):])
        j = i + 2; rows_ = []
        while j < len(paras) and paras[j].startswith("|"):
            rows_.append([x.strip() for x in paras[j].strip("|").split("|")]); j += 1
        table(doc, rows_[0], rows_[1:], [2.15, 0.7, 0.7, 0.8, 0.9, 0.9, 0.95], font_pt=8.5)
        i = j - 1
    elif section == "References":
        m = re.match(r"\[(\d+)\] ([^,]+), (.+?)(, .*|\.)$", t)
        n_, author, ttl, rest = m.groups()
        p = doc.add_paragraph(style="Reference")
        p.add_run(f"[{n_}] {author}, "); add_hyperlink(p, SHORT_LINK[n_], ttl, size=8); p.add_run(rest)
    elif t.startswith("- "):
        b = t[2:]
        lead, _, restt = b.partition(". ")
        txt = f"**{lead}.** " + styled(restt)
        p = doc.add_paragraph(style="List Bullet"); rich(p, txt)      # one bullet style throughout
        if section == "Summary":
            p.paragraph_format.space_after = Pt(1.5)
    elif section == "Findings" and not t.endswith((".", "]", ")")):
        doc.add_paragraph(t, style="Heading 2")
    else:
        p = doc.add_paragraph(); rich(p, styled(t))
    i += 1

nums = list(csv.DictReader(open(S["nums_csv"], encoding="utf-8")))
flat_text = " ".join(" ".join(x.strip() for x in l.strip("|").split("|")) if l.startswith("|") else l for l in paras).replace("\u2212", "-")
for r in nums:
    assert r["phrase"].replace("\u2212", "-") in flat_text, "phrase not in the text: " + r["phrase"]
    if r["fact_id"] in facts and r["fact_id"] not in used:
        used.append(r["fact_id"])

# ---- Appendices --------------------------------------------------------------
body_ids = list(used)
p = doc.add_paragraph(); p.paragraph_format.page_break_before = True
r = p.add_run("APPENDICES"); r.bold = True
p.add_run(" (supporting detail; not part of the two-page note)")

doc.add_paragraph("Appendix A. Method and assumptions", style="Heading 2")
for t in S["appendix_a"]:
    bullet(t)

doc.add_paragraph("Appendix B. Policy context timeline (context, not cause)", style="Heading 2")
table(doc, ["Date", "Event", "Source"], S["appendix_b"], [1.0, 5.3, 0.7], font_pt=8.5)

doc.add_paragraph("Appendix C. Additional dashboard views and tables", style="Heading 2").paragraph_format.page_break_before = True
for f, label, cap, width in S["appendix_c"]:
    picture(f, label, cap, float(width))


def pct(x, sign=True):
    v = float(x) * 100
    s = f"{abs(v):.1f}%"
    return (("+" if v >= 0 else "\u2212") + s) if sign else s


def n(x):
    return f"{int(round(float(x))):,}"


geo = list(csv.DictReader(open(S["geo_csv"], encoding="utf-8")))
P(S["table_c1_caption"], "Small Note")
table(doc, S["table_c1_headers"],
      [[g["geography"], n(g["p_a"]), n(g["p_b"]), n(g["p_c"]), pct(g["share_c"], False), pct(g["growth_a_b"]),
        pct(g["change_b_c"]), pct(g["ytd_change"]), pct(g["contrib_up"], False), pct(g["contrib_ytd"], False)] for g in geo],
      [1.45, 0.6, 0.6, 0.6, 0.55, 0.62, 0.62, 0.65, 0.65, 0.66], font_pt=7.5)

mix = list(csv.DictReader(open(S["mix_csv"], encoding="utf-8")))
geos = ["Canada", "British Columbia", "Ontario", "Alberta"]
tot = {g: sum(float(m["value"]) for m in mix if m["geography"] == g) for g in geos}
tf = {g: sum(float(m["value"]) for m in mix if m["geography"] == g and m["program"] == "TFWP") / tot[g] for g in geos}
assert pct(tf["British Columbia"], False) == facts[S["bc_tfwp_share_id"]]["display"]
assert pct(tf["Canada"], False) == facts[S["ca_tfwp_share_id"]]["display"]
cats = sorted({(m["program"], m["category"]) for m in mix}, key=lambda x: (x[0] != "TFWP", x[1]))
rows = [["TFWP", "**All TFWP streams**"] + [pct(tf[g], False) for g in geos]]
for prog, cat in cats:
    rows.append([prog, cat] + [pct(next((m["share"] for m in mix if m["geography"] == g and m["program"] == prog and m["category"] == cat), 0), False) for g in geos])
P(S["table_c2_caption"], "Small Note")
table(doc, ["Program", "Stream", "Canada", "B.C.", "Ontario", "Alberta"], rows, [0.7, 3.5, 0.7, 0.7, 0.7, 0.7], font_pt=8)

doc.add_paragraph("Appendix D. Exact values behind the rounded numbers", style="Heading 2").paragraph_format.page_break_before = True
P(S["appendix_d_intro"], "Small Note")
MINUS = "\u2212"


def exact(r):
    k = r["fact_id"]
    if k == "context_statcan_npr":
        return "\u2212154,614 (Statistics Canada)"
    f = facts[k]; v = float(f["value"])
    if f["type"] == "pct":
        s = ("+" if v >= 0 else MINUS) + f"{abs(v) * 100:.1f}%"
        return s + (" (\u00d7" + f"{1 + v:.2f})" if r["rule"] == "tripled" else "")
    if f["type"] == "share":
        return f"{v * 100:.1f}%"
    if f["type"] == "difference" and re.search(r"_ytd\d\d$", k):   # a level (all permits minus PGWPs), not a change
        return f"{v:,.0f}"
    if f["type"] == "difference":
        return ("+" if v >= 0 else MINUS) + f"{abs(v):,.0f}"
    return f"{v:,.0f}"


table(doc, ["Statement in the note", "Shown", "Exact value", "Where to find it"],
      [[r["statement"], r["shown"] or "(in words)", exact(r), r["location"]] for r in nums],
      [2.35, 0.85, 1.15, 2.65], font_pt=7.5)

doc.add_paragraph("Appendix E. Reproducing the analysis", style="Heading 2")
for t in S["appendix_e"]:
    bullet(t)

doc.add_paragraph("Appendix F. Full references", style="Heading 2")
for ref in refs:
    p = doc.add_paragraph(style="Reference")
    p.paragraph_format.left_indent = Inches(0.2); p.paragraph_format.first_line_indent = Inches(-0.2)
    p.add_run(f"[{ref['n']}] {ref['citation']} ")
    add_hyperlink(p, ref["url"], ref["url"], size=8)
    p.add_run(f" (accessed {ref['accessed']})")

for st in doc.styles:
    try:
        if st.font.name and st.font.name != "Calibri":
            st.font.name = "Calibri"
            rf = st.element.rPr.find(qn("w:rFonts"))
            for a in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
                rf.set(qn(a), "Calibri")
    except AttributeError:
        pass
doc.core_properties.author = "Mohsen Fazeli"
doc.core_properties.title = title
doc.save(S["out_docx"])
open(S["used_ids_txt"], "w").write("\n".join(body_ids) + "\n")
print("saved", S["out_docx"], "| facts used in body:", len(body_ids))
