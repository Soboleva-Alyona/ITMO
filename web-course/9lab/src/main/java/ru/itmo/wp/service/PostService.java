package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.repository.PostRepository;
import ru.itmo.wp.repository.TagRepository;

import java.util.ArrayList;
import java.util.List;

@Service
public class PostService {
    private final PostRepository postRepository;
    private final TagRepository tagRepository;

    public PostService(PostRepository postRepository, TagRepository tagRepository) {
        this.postRepository = postRepository;
        this.tagRepository = tagRepository;
    }

    public List<Post> findAll() {
        return postRepository.findAllByOrderByCreationTimeDesc();
    }

    public Post findById(Long id) {
        return id == null ? null : postRepository.findById(id).orElse(null);
    }

    public void addComment(Post post, Comment comment) {
        post.addComment(comment);
        postRepository.save(post);
    }

    public void create(User author, PostForm postForm) {
        Post post = new Post();
        post.setTitle(postForm.getTitle());
        post.setText(postForm.getText());

        String[] tags = postForm.getTagsString().trim().split("\\s+");

        List<Tag> tagsRes = new ArrayList<>();
        for (String s : tags) {
            Tag tag = tagRepository.findByName(s);
            if (tag != null) {
                if (!tagsRes.contains(tag)) {
                    tagsRes.add(tag);
                }
                continue;
            }
            Tag newTag = new Tag();
            newTag.setName(s);
            if (!tagsRes.contains(newTag)) {
                tagsRes.add(newTag);
                tagRepository.save(newTag);
            }
        }
        post.setTags(tagsRes);

        author.addPost(post);
        postRepository.save(post);
    }
}
