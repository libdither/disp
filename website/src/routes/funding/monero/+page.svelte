<script lang="ts">
  import { base } from "$app/paths";

  const MONERO_ADDRESS =
    "87ErQFwijiDCqcPs1yy8qGdaLBA2BG1BqAy93aE4pGYe2KFbZuxJDiGjjgLtDrzgxFaK98YHv7rctYfyCfptAvmHDWN8MGv";
  // view-only key, published ON PURPOSE: it can reveal incoming donations
  // (that is the point — anyone can audit the ledger) but can never spend.
  const MONERO_VIEW_KEY =
    "e6cf097d491f78f5a8443436c43b54985a0c5a0702e683f2515a57ff29942306";

  let copied = $state(false);
  let copiedTimer: ReturnType<typeof setTimeout> | undefined;
  async function copyAddr() {
    try {
      await navigator.clipboard.writeText(MONERO_ADDRESS);
      copied = true;
      clearTimeout(copiedTimer);
      copiedTimer = setTimeout(() => (copied = false), 1600);
    } catch {}
  }
</script>

<svelte:head>
  <title>Monero · disp</title>
</svelte:head>

<div class="container page">
  <h1><span class="grad-text">Monero</span> donations</h1>

  {#if MONERO_ADDRESS}
    <p class="lede">
      Scan or copy below. Every XMR goes straight into rent, compute, and the
      claude code subscription that co-wrote this very page.
    </p>
    <div class="card addrcard">
      <div class="qr-slot has-qr">
        <img src="{base}/xmr-donate-qr.png" alt="Monero donation QR code" width="168" height="168" />
      </div>
      <div class="addr-col">
        <span class="addr-label">address</span>
        <code class="addr">{MONERO_ADDRESS}</code>
        <button class="btn" onclick={copyAddr}>
          {copied ? "copied ✓" : "copy address"}
        </button>
      </div>
    </div>

    <details class="transparency">
      <summary>Transparency: the ledger is public</summary>
      <p>
        This wallet's <em>view key</em> is published, so anyone can audit
        incoming donations (a view key can see, never spend). Import the
        address + view key below as a view-only wallet in any Monero client to
        verify the totals shown on the <a href="{base}/funding/">funding page</a>.
      </p>
      <code class="addr viewkey">{MONERO_VIEW_KEY}</code>
    </details>
  {:else}
    <p class="lede">
      The wallet is still being set up, which is a fancy way of saying I
      haven't done it yet. Check back soon, or
      <a
        href="https://github.com/libdither/disp/issues"
        target="_blank"
        rel="noopener">open an issue</a
      > and shame me into it.
    </p>
    <div class="card soon">
      <svg viewBox="0 0 64 64" class="seedling" aria-hidden="true">
        <path
          d="M32 52 L32 34 M32 40 C32 30 22 28 16 20 M32 36 C32 28 42 26 48 18"
          stroke="var(--bark)"
          stroke-width="2.5"
          stroke-linecap="round"
          fill="none"
        />
        <circle cx="16" cy="19" r="6" fill="var(--g1)" />
        <circle cx="48" cy="17" r="6" fill="var(--g2)" />
        <circle cx="32" cy="30" r="5" fill="var(--g3)" />
        <ellipse cx="32" cy="54" rx="14" ry="3" fill="var(--bark)" opacity="0.3" />
      </svg>
      <p>
        Future home of a QR code. For now it grows quietly, like everything
        else here.
      </p>
    </div>
  {/if}

  <p class="backlink"><a href="{base}/funding/">← back to funding</a></p>
</div>

<style>
  .page {
    padding-block: 3.5rem 2rem;
    max-width: 700px;
  }
  h1 {
    font-size: clamp(2rem, 5vw, 2.8rem);
    margin: 0 0 0.8rem;
  }
  .lede {
    color: var(--fg-muted);
    font-size: 1.05rem;
  }
  .addrcard {
    display: flex;
    gap: 1.4rem;
    align-items: center;
    margin-top: 1.6rem;
    flex-wrap: wrap;
  }
  .qr-slot {
    width: 168px;
    height: 168px;
    flex: none;
    border: 2px dashed var(--border-strong);
    border-radius: 12px;
    display: grid;
    place-items: center;
    color: var(--fg-faint);
    font-size: 0.8rem;
  }
  .qr-slot.has-qr {
    border-style: solid;
    background: #fff;
    overflow: hidden;
  }
  .qr-slot img {
    width: 100%;
    height: 100%;
    object-fit: contain;
  }
  .transparency {
    margin-top: 1.4rem;
    font-size: 0.9rem;
    color: var(--fg-muted);
  }
  .transparency summary {
    cursor: pointer;
    color: var(--fg);
    font-weight: 550;
  }
  .transparency p {
    margin: 0.6rem 0;
  }
  .viewkey {
    display: block;
    font-size: 0.72rem;
  }
  .addr-col {
    flex: 1;
    min-width: 220px;
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    align-items: flex-start;
  }
  .addr-label {
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    color: var(--fg-faint);
  }
  .addr {
    word-break: break-all;
    font-size: 0.78rem;
    background: var(--bg-code);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 0.6em 0.8em;
    white-space: normal;
  }
  .soon {
    margin-top: 1.6rem;
    display: flex;
    gap: 1.2rem;
    align-items: center;
  }
  .seedling {
    width: 72px;
    height: 72px;
    flex: none;
  }
  .soon p {
    color: var(--fg-muted);
    margin: 0;
  }
  .backlink {
    margin-top: 2rem;
    font-size: 0.9rem;
  }
</style>
