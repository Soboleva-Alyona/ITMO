<template>
    <footer>
      <div> {{ usersCnt }} users, {{ postsCnt }} posts</div>
        <a href="/">Codeforces</a> &copy; 2021 by Mike Mirzayanov
    </footer>
</template>

<script>
export default {
    name: "Footer",
    props: ["usersCnt", "postsCnt"]
}
</script>

<style scoped>

</style>