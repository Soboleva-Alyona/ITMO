<template>
  <div class="comment">
    <div class="comment_info">
      Added by {{users[comment.userId].name}}
    </div>
    <div class="comment_text">
      {{comment.text}}
    </div>
  </div>
</template>

<script>
export default {
  name: "PostComments",
  props: ["comment", "users"]
}
</script>

<style scoped>

</style>