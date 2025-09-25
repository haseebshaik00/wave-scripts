/* MIRAGE Marker Browser — v2
 * - Prefers jotDateTime for Year/Month/Day filters.
 * - Output columns: id, accessCode, date, time, marker, file.
 * - Baseline/Play: keep only PIDs with only that family; dedupe duplicates; form unique start→stop pairs; flag orphans.
 */

const state = {
  rows: [], // normalized rows
  years: new Set(),
  monthsByYear: new Map(),     // year -> Set(month)
  daysByYearMonth: new Map(),  // `${year}-${mm}` -> Set(day)
};

const els = {
  fileInput:  document.getElementById('fileInput'),
  dropZone:   document.getElementById('dropZone'),
  stats:      document.getElementById('stats'),
  year:       document.getElementById('yearSelect'),
  month:      document.getElementById('monthSelect'),
  day:        document.getElementById('daySelect'),
  marker:     document.getElementById('markerSelect'),
  clearBtn:   document.getElementById('clearBtn'),
  results:    document.getElementById('results'),
  resultMeta: document.getElementById('resultMeta'),
};

// ---------- small utils

const pad2 = n => String(n).padStart(2, '0');
const isDate = v => Object.prototype.toString.call(v) === '[object Date]' && !isNaN(v);
const excelSerialToMs = n => Math.round((Number(n) - 25569) * 86400 * 1000); // Excel -> ms
const norm = s => String(s ?? '').toLowerCase().replace(/\s+/g, '').replace(/[_-]/g, '');

function pick(obj, candidates) {
  for (const c of candidates) {
    const k = Object.keys(obj).find(key => norm(key) === norm(c));
    if (k != null) return obj[k];
  }
  return undefined;
}

function safeNumber(x) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

function parseJotMs(row) {
  const raw = pick(row, ['jotDateTime', 'jotdatetime', 'jot_time', 'jottime', 'date', 'datetime']);
  if (raw == null || raw === '') return NaN;

  // If JS Date
  if (isDate(raw)) return raw.getTime();

  // If numeric (Excel serial or epoch)
  if (typeof raw === 'number') {
    if (raw > 1e12) return raw;                // ms epoch
    if (raw > 1e10) return raw / 1000;         // s epoch that slipped in as big?
    if (raw > 59_000) return excelSerialToMs(raw); // Excel serial (typical > 45000 for year >= 2023)
  }

  // String: try US M/D/YYYY HH:mm or general Date parse
  if (typeof raw === 'string') {
    const m = raw.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})\s+(\d{1,2}):(\d{2})(?::(\d{2}))?/);
    if (m) {
      const [_, MM, DD, YYYY, hh, mm, ss] = m.map(Number);
      const dt = new Date(YYYY, MM - 1, DD, hh, mm, ss || 0);
      if (!isNaN(dt.getTime())) return dt.getTime();
    }
    const d2 = new Date(raw);
    if (!isNaN(d2.getTime())) return d2.getTime();
  }
  return NaN;
}

function parseUtcLikeMs(row) {
  const raw = pick(row, ['utc_app', 'ts_app', 'utc', 'timestamp', 'ts']);
  if (raw == null || raw === '') return NaN;
  const n = safeNumber(raw);
  if (!isNaN(n)) return n < 1e11 ? n * 1000 : n;
  return NaN;
}

function ymdFromMs(ms) {
  const d = new Date(ms);
  return {
    y: String(d.getFullYear()),
    m: pad2(d.getMonth() + 1),
    d: pad2(d.getDate()),
    key: `${d.getFullYear()}-${pad2(d.getMonth() + 1)}-${pad2(d.getDate())}`
  };
}

function classifyMarker(raw) {
  const s = String(raw || '').toLowerCase();
  const t = s.replace(/[_:.,/()-]+/g, ' ').replace(/\s+/g, ' ').trim();

  if (/baseline.*(start|stop)/i.test(t)) return 'baseline';
  if (/play.*(start|stop)/i.test(t)) return 'play';
  if (/(interrupt(ed)? by children|children.*interrupt)/i.test(t)) return 'interrupted_children';
  if (/(interrupt(ed)? by adults|adult.*interrupt)/i.test(t)) return 'interrupted_adults';
  if (/(did(\s*not|n'?t)\s*follow\s*(instructions|directions))/i.test(t)) return 'didnt_follow';
  if (/(electrode(s)?(\s*or)?(\s*device)?\s*disconnect(ed)?)/i.test(t)) return 'electrodes';
  return 'other';
}

function isStart(raw) { return /start/i.test(String(raw)); }
function isStop(raw)  { return /stop/i.test(String(raw)); }

function fileLeaf(label) {
  // "20250320_GT2330_mirage_UTCMarker.csv :: Sheet1" -> "20250320_GT2330_mirage_UTCMarker.csv"
  return String(label || '').split('::')[0].trim();
}

// ---------- normalization

function normalizeRow(row, fileLabel) {
  const id          = pick(row, ['id', 'rowid']);
  const pid         = pick(row, ['participantID', 'participantId', 'pid']);
  const accessCode  = pick(row, ['accessCode', 'accesscode', 'access']);
  const assessorID  = pick(row, ['assessorID', 'assessorid', 'assessor']);
  const marker      = pick(row, ['marker', 'event', 'note', 'annotation']);

  // Times
  const jotMs = parseJotMs(row);
  const utcMs = parseUtcLikeMs(row);
  const tsMs  = !isNaN(jotMs) ? jotMs : utcMs;     // prefer jot

  // For filtering, we strictly use the JOT date when present
  const jotDateKey = !isNaN(jotMs) ? ymdFromMs(jotMs) : null;
  const fallbackKey = !isNaN(utcMs) ? ymdFromMs(utcMs) : null;

  // Keep original jot text to show exact string (optional)
  const jotRaw = pick(row, ['jotDateTime', 'jotdatetime', 'jot_time', 'jottime', 'date', 'datetime']);

  if (!marker) return null;

  const primaryKey = jotDateKey || fallbackKey;

  return {
    __file: fileLabel,
    fileName: fileLeaf(fileLabel),
    id: id ?? '',
    pid: pid ?? '',
    accessCode: accessCode ?? '',
    assessorID: assessorID ?? '',
    marker: String(marker),
    markerClass: classifyMarker(marker),

    tsMs: !isNaN(tsMs) ? tsMs : null,

    // For filtering and display (prefer jot)
    jotMs: !isNaN(jotMs) ? jotMs : null,
    jotRaw: jotRaw ?? '',
    pY: primaryKey ? primaryKey.y : '',
    pM: primaryKey ? primaryKey.m : '',
    pD: primaryKey ? primaryKey.d : '',
    pKey: primaryKey ? primaryKey.key : ''
  };
}

function buildCalendarIndexes(rows) {
  state.years.clear();
  state.monthsByYear.clear();
  state.daysByYearMonth.clear();

  for (const r of rows) {
    if (!r.pKey) continue;
    state.years.add(r.pY);

    if (!state.monthsByYear.has(r.pY)) state.monthsByYear.set(r.pY, new Set());
    state.monthsByYear.get(r.pY).add(r.pM);

    const ym = `${r.pY}-${r.pM}`;
    if (!state.daysByYearMonth.has(ym)) state.daysByYearMonth.set(ym, new Set());
    state.daysByYearMonth.get(ym).add(r.pD);
  }
}

function fillYearMonthDaySelects() {
  els.year.innerHTML = `<option value="">All</option>` +
    Array.from(state.years).sort().map(y => `<option value="${y}">${y}</option>`).join('');
  els.month.innerHTML = `<option value="">All</option>`;
  els.day.innerHTML   = `<option value="">All</option>`;
}

function refreshMonthOptions() {
  const y = els.year.value;
  if (!y) {
    els.month.innerHTML = `<option value="">All</option>`;
    els.day.innerHTML   = `<option value="">All</option>`;
    return;
  }
  const months = state.monthsByYear.get(y) || new Set();
  els.month.innerHTML = `<option value="">All</option>` +
    Array.from(months).sort().map(m => `<option value="${m}">${m}</option>`).join('');
  els.day.innerHTML = `<option value="">All</option>`;
}

function refreshDayOptions() {
  const y = els.year.value, m = els.month.value;
  if (!y || !m) { els.day.innerHTML = `<option value="">All</option>`; return; }
  const days = state.daysByYearMonth.get(`${y}-${m}`) || new Set();
  els.day.innerHTML = `<option value="">All</option>` +
    Array.from(days).sort().map(d => `<option value="${d}">${d}</option>`).join('');
}

function updateStats() {
  const fileCount = new Set(state.rows.map(r => r.__file)).size;
  const pidCount  = new Set(state.rows.map(r => r.pid).filter(Boolean)).size;
  els.stats.textContent = `${fileCount} file(s), ${state.rows.length} row(s), ${pidCount} unique PID(s) loaded.`;
}

// ---------- baseline/play pair handling

function collapseConsecutiveDuplicates(arr) {
  // Remove consecutive exact duplicates by (tsMs, marker, accessCode, assessorID, fileName)
  const out = [];
  for (const r of arr) {
    const last = out[out.length - 1];
    const same = last &&
      (last.tsMs === r.tsMs) &&
      (last.marker === r.marker) &&
      (last.accessCode === r.accessCode) &&
      (last.assessorID === r.assessorID) &&
      (last.fileName === r.fileName);
    if (!same) out.push(r);
  }
  return out;
}

function uniquePairsWithOrphans(arr) {
  // Input must already be filtered to one PID and one family (baseline/play), sorted by time
  const deduped = collapseConsecutiveDuplicates(arr);
  const out = [];
  for (let i = 0; i < deduped.length; i++) {
    const cur = deduped[i];
    if (isStart(cur.marker)) {
      const nxt = deduped[i + 1];
      if (nxt && isStop(nxt.marker)) {
        out.push(cur, nxt);
        i += 1; // consume pair
      } else {
        // orphan start
        out.push({ ...cur, __orphan: true });
      }
    } else if (isStop(cur.marker)) {
      // orphan stop (no preceding start)
      out.push({ ...cur, __orphan: true });
    } else {
      // Shouldn't happen for baseline/play family, but keep safe
      out.push(cur);
    }
  }
  return out;
}

// ---------- render

function render() {
  const y = els.year.value, m = els.month.value, d = els.day.value;
  const markerSel = els.marker.value;

  // Filter by primary (jot) date fields
  let base = state.rows.filter(r => {
    if (!r.pKey) return false;
    if (y && r.pY !== y) return false;
    if (m && r.pM !== m) return false;
    if (d && r.pD !== d) return false;
    return true;
  });

  // Marker family filtering
  if (markerSel === 'baseline' || markerSel === 'play') {
    // Only pids whose rows that day contain only that family
    const byPid = groupBy(base, r => r.pid || r.accessCode || 'UNKNOWN');
    const keepRows = [];
    for (const [pid, rows] of byPid) {
      const classes = new Set(rows.map(x => x.markerClass));
      if (classes.size === 1 && classes.has(markerSel)) {
        // sort, dedupe duplicates, return unique start/stop sequence, mark orphans
        const sorted = rows.slice().sort((a,b) => (a.tsMs ?? 0) - (b.tsMs ?? 0));
        const seq = uniquePairsWithOrphans(sorted);
        keepRows.push(...seq);
      }
    }
    base = keepRows;
  } else {
    base = base.filter(r => r.markerClass === markerSel);
  }

  // Group by PID for display and sort time
  const groups = groupBy(base, r => r.pid || r.accessCode || 'UNKNOWN');
  const sortedGroups = Array.from(groups.entries())
    .sort((a, b) => String(a[0]).localeCompare(String(b[0])))
    .map(([pid, arr]) => [pid, arr.slice().sort((x,y) => (x.tsMs ?? 0) - (y.tsMs ?? 0))]);

  // Render
  els.results.innerHTML = '';
  let totalRows = 0;

  for (const [pid, arr] of sortedGroups) {
    totalRows += arr.length;

    const card = document.createElement('div');
    card.className = 'pid-block';

    const head = document.createElement('div');
    head.className = 'pid-head';
    head.innerHTML = `<div class="pid-title">PID: <strong>${escapeHtml(pid)}</strong></div>
                      <div class="pid-meta">${arr.length} row(s)</div>`;
    card.appendChild(head);

    const table = document.createElement('table');
    table.className = 'mini';
    table.innerHTML = `
      <thead>
        <tr>
          <th>id</th>
          <th>Access Code</th>
          <th>Date</th>
          <th>Time</th>
          <th>Marker</th>
          <th>File</th>
        </tr>
      </thead>
      <tbody>
        ${arr.map(r => {
          const ms = r.jotMs ?? r.tsMs;
          const dateStr = ms ? formatDate(ms) : '';
          const timeStr = ms ? formatTime(ms) : '';
          const orphanStyle = r.__orphan ? ' style="background:#ffe5e5;"' : '';
          return `
            <tr${orphanStyle}>
              <td>${escapeHtml(r.id)}</td>
              <td>${escapeHtml(r.accessCode)}</td>
              <td>${escapeHtml(dateStr)}</td>
              <td>${escapeHtml(timeStr)}</td>
              <td>${escapeHtml(r.marker)}</td>
              <td title="${escapeHtml(r.fileName)}">${escapeHtml(shorten(r.fileName))}</td>
            </tr>
          `;
        }).join('')}
      </tbody>
    `;
    card.appendChild(table);
    els.results.appendChild(card);
  }

  els.resultMeta.textContent = `${sortedGroups.length} PID(s) • ${totalRows} row(s)`;
}

function groupBy(arr, keyFn) {
  const m = new Map();
  for (const x of arr) {
    const k = keyFn(x);
    if (!m.has(k)) m.set(k, []);
    m.get(k).push(x);
  }
  return m;
}

function formatDate(ms) {
  const d = new Date(ms);
  return `${d.getFullYear()}-${pad2(d.getMonth()+1)}-${pad2(d.getDate())}`;
}
function formatTime(ms) {
  const d = new Date(ms);
  return `${pad2(d.getHours())}:${pad2(d.getMinutes())}:${pad2(d.getSeconds())}`;
}
function escapeHtml(s) {
  return String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;')
    .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}
function shorten(p, n = 42) {
  const s = String(p || '');
  if (s.length <= n) return s;
  return s.slice(0, Math.floor(n/2)-1) + '…' + s.slice(-Math.ceil(n/2)+1);
}

// ---------- file loading

els.fileInput.addEventListener('change', e => {
  if (e.target.files?.length) loadFiles(e.target.files);
});

['dragenter', 'dragover'].forEach(evt =>
  els.dropZone.addEventListener(evt, e => {
    e.preventDefault(); e.stopPropagation();
    els.dropZone.classList.add('dragging');
  })
);
['dragleave', 'drop'].forEach(evt =>
  els.dropZone.addEventListener(evt, e => {
    e.preventDefault(); e.stopPropagation();
    els.dropZone.classList.remove('dragging');
    if (evt === 'drop' && e.dataTransfer?.files?.length) {
      loadFiles(e.dataTransfer.files);
    }
  })
);

function loadFiles(fileList) {
  const files = Array.from(fileList);
  if (!files.length) return;

  els.stats.textContent = `Parsing ${files.length} file(s)…`;

  const promises = files.map(file => {
    const name = file.name.toLowerCase();
    if (name.endsWith('.csv')) {
      return parseCSV(file, file.name);
    } else if (name.endsWith('.xlsx') || name.endsWith('.xls')) {
      return parseXLSX(file, file.name);
    } else {
      return Promise.resolve([]);
    }
  });

  Promise.all(promises).then(results => {
    const merged = results.flat().map(r => normalizeRow(r.row, r.__file)).filter(Boolean);
    state.rows = merged;

    buildCalendarIndexes(state.rows);
    fillYearMonthDaySelects();
    updateStats();
    render();
  }).catch(err => {
    console.error(err);
    els.stats.textContent = 'Error while parsing files. See console.';
  });
}

function parseCSV(file, filename) {
  return new Promise((resolve, reject) => {
    Papa.parse(file, {
      header: true,
      skipEmptyLines: true,
      complete: res => {
        const out = (res.data || []).map(row => ({ row, __file: filename }));
        resolve(out);
      },
      error: reject
    });
  });
}

function parseXLSX(file, filename) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = e => {
      try {
        const wb = XLSX.read(e.target.result, { type: 'array' });
        const rows = [];
        wb.SheetNames.forEach(sn => {
          const ws = wb.Sheets[sn];
          const json = XLSX.utils.sheet_to_json(ws, { defval: "" });
          json.forEach(row => rows.push({ row, __file: `${filename} :: ${sn}` }));
        });
        resolve(rows);
      } catch (err) { reject(err); }
    };
    reader.onerror = reject;
    reader.readAsArrayBuffer(file);
  });
}

// ---------- filters

els.year.addEventListener('change', () => { refreshMonthOptions(); render(); });
els.month.addEventListener('change', () => { refreshDayOptions(); render(); });
els.day.addEventListener('change', render);
els.marker.addEventListener('change', render);

// ---------- prebundle boot (optional, if mirage-bundle.js present)
if (window.MIRAGE_BUNDLE && Array.isArray(window.MIRAGE_BUNDLE)) {
  try {
    state.rows = window.MIRAGE_BUNDLE
      .map(r => normalizeRow(r.row, r.__file))
      .filter(Boolean);

    const loaderSection = document.querySelector(".loader");
    if (loaderSection) loaderSection.style.display = "none";

    buildCalendarIndexes(state.rows);
    fillYearMonthDaySelects();
    updateStats();
    render();
  } catch (e) {
    console.error("Failed to init from MIRAGE_BUNDLE:", e);
  }
}
