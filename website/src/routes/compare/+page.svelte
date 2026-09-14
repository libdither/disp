<script lang="ts">
  import Compare from "$lib/langs/Compare.svelte";
  import langs from "virtual:awesome-langs";
</script>

<svelte:head>
  <title>Compare · disp</title>
  <meta
    name="description"
    content="Every language and system that aims near disp's goal, scored on the same six axes, with disp's own row beside them."
  />
</svelte:head>

<div class="container page">
  <h1>Progress &amp; <span class="grad-text">Comparisons</span></h1>
  <p class="lede">
    Every language and system that aims near disp's goal, scored on the same
    six axes ({langs.surveyed}) as a percentage of what disp itself needs
    there, with disp's own row at the top. Sort by an axis to see who is
    ahead, pick up to two to compare against disp. The full write-ups live in
    <a href={langs.surveyUrl} target="_blank" rel="noopener"
      >research/awesome-langs</a
    >.
  </p>
  <Compare data={langs} />
</div>

<style>
  .page {
    padding-block: 3rem 3.4rem;
  }
  h1 {
    font-size: clamp(2.2rem, 5vw, 3.2rem);
    margin: 0 0 0.6rem;
  }
  .lede {
    color: var(--fg-muted);
    max-width: 46rem;
    margin: 0 0 1.6rem;
  }
</style>
