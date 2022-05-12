package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class MyArticlesPage {
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (request.getSession().getAttribute("user") != null) {
            User user = (User) request.getSession().getAttribute("user");
            view.put("articles", articleService.findArticlesOfUser(user.getId()));
        }
    }

    private void setArticleHidden(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        String userId = request.getParameter("id");
        long id;
        try {
            id = Long.parseLong(userId, userId.length());
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid id");
        }
        Article article = articleService.find(id);
        User user = (User) request.getSession().getAttribute("user");
        Boolean hidden = Boolean.parseBoolean(request.getParameter("newHidden"));
        if (article != null && user != null && article.getUserId() == user.getId()) {
            articleService.setArticleHidden(article, hidden);
        }
    }


}
