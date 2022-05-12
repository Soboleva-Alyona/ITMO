<template>
    <div class="middle">
        <Sidebar :posts="viewPosts"/>
        <main>
            <Index v-if="page === 'Index'" :posts="viewPostsIndex" :comments="comments" :users="users"/>
            <Enter v-if="page === 'Enter'"/>
            <WritePost v-if="page === 'WritePost'"/>
            <EditPost v-if="page === 'EditPost'"/>
            <Register v-if="page === 'Register'"/>
            <Post v-if="page === 'Post'" :post="post" :users="users" :comments="comments"/>
            <Users v-if="page === 'Users'" :users="users"/>
        </main>
    </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./page/Index";
import Enter from "./page/Enter";
import WritePost from "./page/WritePost";
import EditPost from "./page/EditPost";
import Register from "./page/Register";
import Users from "./page/Users";
import Post from "./page/Post";

export default {
    name: "Middle",
    data: function () {
        return {
            page: "Index"
        }
    },
    components: {
        WritePost,
        Enter,
        Register,
        Index,
        Sidebar,
        EditPost,
        Users,
        Post
    },
    props: ["posts", "users", "comments", "post"],

    computed: {
        viewPosts: function () {
            return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
        },
        viewPostsIndex: function () {
          return Object.values(this.posts).sort((a, b) => b.id - a.id);
        },
    }, beforeCreate() {
        this.$root.$on("onChangePage", (page) => this.page = page);
    }
}
</script>

<style scoped>

</style>
