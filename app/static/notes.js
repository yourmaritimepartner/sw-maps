// Review notes: press N (or the Note button), click a spot on a map or the page,
// type a line, press Enter. Notes are pinned to the image (x/y as fractions), so they
// show wherever that image appears and survive re-renders. Queue: /notes.
(() => {
  const PAGE = location.pathname + location.search;
  const PAGE_TARGET = "page:" + location.pathname;
  let mode = false, notes = [], showDone = false, pop = null;
  const el = (tag, props = {}, ...kids) => { const e = Object.assign(document.createElement(tag), props); e.append(...kids); return e; };

  // wrap each noteable image so pins can be positioned over it
  const hosts = [...document.querySelectorAll("img[data-note-target]")].map(img => {
    const host = el("span", { className: "nl-host" });
    host.dataset.target = img.dataset.noteTarget;
    img.replaceWith(host); host.append(img);
    return host;
  });
  const pageHost = el("div", { className: "nl-page" });
  pageHost.dataset.target = PAGE_TARGET;
  document.body.append(pageHost);

  const btn = el("button", { type: "button", className: "nl-btn", title: "Add a note (N)" });
  const done = el("input", { type: "checkbox" });
  document.body.append(el("div", { className: "nl-bar" }, btn,
    el("label", { className: "nl-done" }, done, " Show done"), el("a", { href: "/notes" }, "All notes")));

  const setMode = on => {
    mode = on;
    document.body.classList.toggle("nl-on", on);
    btn.textContent = on ? "Click a spot (Esc to cancel)" : "Note";
    if (!on) btn.dataset.count = notes.filter(n => n.status === "open").length;
  };
  btn.onclick = () => setMode(!mode);
  done.onchange = () => { showDone = done.checked; render(); };
  document.addEventListener("keydown", e => {
    if (e.target.closest("input, textarea, select")) return;
    if (e.key === "n" || e.key === "N") { e.preventDefault(); setMode(!mode); }
    if (e.key === "Escape") { setMode(false); closePop(); }
  });

  // in note mode, a click anywhere drops a note instead of following links
  document.addEventListener("click", e => {
    if (!mode || e.target.closest(".nl-bar, .nl-pop, .nl-pin")) return;
    e.preventDefault(); e.stopPropagation();
    const host = e.target.closest(".nl-host");
    if (host) {
      const r = host.getBoundingClientRect();
      openPop(host, (e.clientX - r.left) / r.width, (e.clientY - r.top) / r.height);
    } else {
      sizePage();
      openPop(pageHost, e.pageX / pageHost.offsetWidth, e.pageY / pageHost.offsetHeight);
    }
  }, true);

  const closePop = () => { if (pop) pop.remove(); pop = null; };
  function openPop(host, x, y) {
    closePop();
    const ta = el("textarea", { rows: 3, placeholder: "What should change here? Enter to save, Shift+Enter for a new line" });
    const save = el("button", { type: "button" }, "Save");
    const cancel = el("button", { type: "button", className: "nl-ghost" }, "Cancel");
    pop = el("div", { className: "nl-pop" }, ta, el("div", { className: "nl-actions" }, cancel, save));
    pop.style.left = x * 100 + "%"; pop.style.top = y * 100 + "%";
    if (x > 0.6) pop.classList.add("nl-left");
    host.append(pop); ta.focus();
    cancel.onclick = closePop;
    const submit = async () => {
      const text = ta.value.trim();
      if (!text) return;
      save.disabled = true;
      const r = await fetch("/api/notes", { method: "POST", headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ page: PAGE, target: host.dataset.target, x, y, text }) });
      if (r.ok) { closePop(); setMode(false); load(); }
      else { save.disabled = false; save.textContent = "Retry"; }
    };
    save.onclick = submit;
    ta.addEventListener("keydown", e => { if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); submit(); } });
  }

  function sizePage() {
    pageHost.style.height = document.documentElement.scrollHeight + "px";
  }
  function render() {
    document.querySelectorAll(".nl-pin").forEach(p => p.remove());
    sizePage();
    for (const host of [...hosts, pageHost]) {
      for (const n of notes.filter(n => n.target === host.dataset.target && (showDone || n.status !== "done"))) {
        const pin = el("a", { className: "nl-pin nl-" + n.status, href: "/notes#n" + n.id }, String(n.id));
        pin.style.left = n.x * 100 + "%"; pin.style.top = n.y * 100 + "%";
        pin.title = `#${n.id} · ${n.who} · ${n.t}\n${n.text}` + (n.resolution ? `\n→ ${n.resolution}` : "");
        host.append(pin);
      }
    }
    setMode(mode);
  }
  async function load() {
    const r = await fetch("/api/notes");
    notes = r.ok ? await r.json() : [];
    render();
  }
  window.addEventListener("load", render);
  load();
})();
