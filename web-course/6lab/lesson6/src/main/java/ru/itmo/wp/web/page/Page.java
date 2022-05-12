package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.service.EventService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public abstract class Page {
    protected final UserService userService = new UserService();
    protected final EventService eventService = new EventService();
    protected HttpServletRequest request;

    protected void action(HttpServletRequest request, Map<String, Object> view) {
        // no operations.
    }

    void before(HttpServletRequest request, Map<String, Object> view) {
        this.request = request;
        User user = getUser();
        if (user != null) {
            view.put("user", user);
        }
        //view.put("userCount", userService.findCount());
    }

    void after(HttpServletRequest request, Map<String, Object> view) {
        view.put("userCount", userService.findCount());
    }

    protected User getUser() {
        return (User) request.getSession().getAttribute("user");
    }

    protected void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

    protected void setMessage(String message) {
        request.getSession().setAttribute("message", message);

    }

}
