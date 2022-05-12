package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.service.PostService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {

    private final PostService postService;

    public PostPage(PostService postService) {
        this.postService = postService;
    }

    @GetMapping({"/post/{id}", "/post"})
    public String post(Model model, @PathVariable(required = false) String id, HttpSession httpSession) {
        if (id == null) {
            putBadMessage(httpSession, "Please, add post id");
        } else {
            if (id.matches("[0-9]+")) {
                Post post = postService.findById(Long.valueOf(id));
                if (post == null) {
                    putBadMessage(httpSession, "No such post");
                    return "PostPage";
                }
                model.addAttribute("post", post);
                model.addAttribute("comments", post.getComments());
                model.addAttribute("comment", new Comment());
            }
        }
        return "PostPage";
    }

    @PostMapping("/post/{id}")
    public String postComment(@Valid @ModelAttribute("comment") Comment comment, BindingResult bindingResult, @PathVariable String id,
                              HttpSession httpSession) {
        if (getUser(httpSession) == null) {
            putBadMessage(httpSession, "You are not authorized");
            return "redirect:/";
        }


        Post post = postService.findById(Long.valueOf(id));
        if (post == null) {
            putBadMessage(httpSession, "No such post");
            return "PostPage";
        }
        if (bindingResult.hasErrors()) {
            putBadMessage(httpSession, "Comment can't be blank");
        } else {
            comment.setUser(getUser(httpSession));
            postService.addComment(post, comment);

            putMessage(httpSession, "You added new comment");
        }


        return "redirect:/post/" + id;
    }
}
