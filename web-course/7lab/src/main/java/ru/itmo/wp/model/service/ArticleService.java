package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;

import java.util.List;

public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();

    public void validateArticle(Article article) throws ValidationException {
        if (Strings.isNullOrEmpty(article.getTitle()) || article.getTitle().trim().length() == 0) {
            throw new ValidationException("Title is required");
        }

        if (Strings.isNullOrEmpty(article.getText()) || article.getText().trim().length() == 0) {
            throw new ValidationException("Text is required");
        }

        if (article.getTitle().length() > 255) {
            throw new ValidationException("Title can't be longer than 255 symbols");
        }

        if (article.getText().length() > 1000) {
            throw new ValidationException("Title can't be longer than 1000 symbols");
        }

    }

    public void save(Article article) {
        articleRepository.save(article);
    }

    public List<Article> sortAllByCreationTime() {
        return articleRepository.sortAllByCreationTime();
    }

    public Article find(Long id) {
        return articleRepository.find(id);
    }


    public List<Article> findArticlesOfUser(Long userId) {
        return articleRepository.findByUserId(userId);
    }

    public void setArticleHidden(Article article, Boolean hidden) {
        articleRepository.setArticleHidden(article, hidden);
    }
}
