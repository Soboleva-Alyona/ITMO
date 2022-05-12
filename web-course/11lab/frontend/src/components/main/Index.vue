<template>
    <div>
      <PostView v-for="post in posts" :post="post" :key="post.id" :user="whichUser(post)"/>
    </div>
</template>

<script>

import PostView from "./PostView";
export default {
    name: "Index",
    props: ["posts"],
    components: {
      PostView
    },
    methods: {
      whichUser: function (post) {
        return post.user;
      }
    }

}
</script>

<style scoped>

</style>