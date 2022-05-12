package ru.itmo.wp.web.page;

import org.checkerframework.checker.units.qual.A;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;


import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class ArticlePage {
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    private void create(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        Article article = new Article();

        article.setTitle(request.getParameter("title"));
        article.setText(request.getParameter("text"));
        article.setHidden(false);
        User user = (User) request.getSession().getAttribute("user");
        if (user != null) {
            article.setUserId(user.getId());

            articleService.validateArticle(article);
            articleService.save(article);
            request.getSession().setAttribute("message", "Article is successfully added");
            throw new RedirectException("/index");
        } else {
            request.getSession().setAttribute("message", "You are not authorized");
        }
        throw new RedirectException("/index");
    }

}
