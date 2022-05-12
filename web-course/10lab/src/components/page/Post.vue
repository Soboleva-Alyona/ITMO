<template>
  <div>
    <PostView :post="post" :commentsCount="countComments(post)" :user="this.users[post.userId]"/>
    <PostComments v-for="comment in chooseComments(post.id)" :key="comment.id" :comment="comment" :users="users"/>
  </div>
</template>

<script>
import PostView from "./PostView";
import PostComments from "./PostComments";
export default {
  name: "Post",
  components: {PostView, PostComments},
  props: ["post", "users", "comments"],
  methods : {
    chooseComments : function (postId) {
      return Object.values(this.comments).filter(c => c.postId === postId);
    },
    countComments: function (post) {
      return Object.values(this.comments).filter(c => c.postId === post.id).length;
    },
  }
}
</script>

<style scoped>

</style>