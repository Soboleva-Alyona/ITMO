<template>
  <div>
  <article>
    <div class="title" @click.prevent="onPost(post)">
      {{ post.title }}
    </div>
    <div class="information">
      By {{ user.name }}
    </div>
    <div class="body">
      {{ post.text }}
    </div>
    <div class="footer">
      <div class="left">
        <img src="../../assets/img/voteup.png" title="Vote Up" alt="Vote Up"/>
        <span class="positive-score">+173</span>
        <img src="../../assets/img/votedown.png" title="Vote Down" alt="Vote Down"/>
      </div>
      <div class="right">
        <img src="../../assets/img/comments_16x16.png" title="Comments" alt="Comments"/>
        {{ commentsCount }}
      </div>
    </div>
  </article>
  </div>
</template>

<script>
export default {
  name: "PostView",
  props: ["post", "user", "commentsCount"],
  methods: {
    onPost: function (post) {
      this.$root.$emit("onPost", post);
      this.changePage("Post");
    },
    changePage: function (page) {
      this.$root.$emit("onChangePage", page);
    },
  }
}
</script>

<style scoped>
section {
  margin-bottom: 1rem;
}
</style>