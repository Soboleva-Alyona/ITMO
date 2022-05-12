<template>
  <div>
    <PostView v-for="post in posts" :post="post" :key="post.id" :commentsCount="countComments(post)"
                :user="whichUser(post)"/>
  </div>

</template>

<script>

import PostView from "./PostView";

export default {
    name: "Index",
    components : {
      PostView
    },
    props: ["posts", "comments", "users"],
    methods: {
      countComments: function (post) {
        return Object.values(this.comments).filter(c => c.postId === post.id).length;
      },
      whichUser: function (post) {
        return this.users[post.userId];
      }
    }

}
</script>

<style scoped>

</style>
