<script lang="ts">
  import { base } from "$app/paths";

  let { data } = $props();
  // by the directory each page sits in
  const groups = $derived.by(() => {
    const by = new Map<string, typeof data.pages>();
    for (const p of data.pages) {
      const dir = p.path.split("/").slice(0, -1).join("/") || "(top level)";
      by.set(dir, [...(by.get(dir) ?? []), p]);
    }
    return [...by];
  });
</script>

<svelte:head><title>Pages · disp</title></svelte:head>

<section class="container" style="padding:3rem 0">
  <h1>Pages</h1>
  <p>Every HTML page in the repository, served from its place in the tree.</p>
  {#each groups as [dir, pages]}
    <h3><code>{dir}</code></h3>
    <ul>
      {#each pages as p}
        <li>
          <a href="{base}/repo/{p.path}" data-sveltekit-reload>{p.title}</a>
          <code style="opacity:0.6">{p.path.split("/").pop()}</code>
        </li>
      {/each}
    </ul>
  {/each}
</section>
