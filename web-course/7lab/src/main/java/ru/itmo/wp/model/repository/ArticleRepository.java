package ru.itmo.wp.model.repository;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface ArticleRepository {

    Article find(Long id);

    List<Article>findAll();

    List<Article>sortAllByCreationTime();

    void save(Article article);

    List<Article> findByUserId(Long userId);

    void setArticleHidden(Article article, Boolean hidden);
}
