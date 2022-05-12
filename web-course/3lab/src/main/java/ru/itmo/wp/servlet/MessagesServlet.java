package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
//import java.util.*;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.ArrayList;
import java.util.List;

public class MessagesServlet extends HttpServlet {
    private final List<Object> messages = new ArrayList<>();
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("application/json");
        String uri = request.getRequestURI();

        Object user;
        Object text;
        Object answer = null;
        HttpSession session = request.getSession();
        switch (uri) {
            case "/message/auth":
                user = request.getParameter("user");
                if (user != null) {
                    session.setAttribute("user", user);
                    answer = user;
                } else if (session.getAttribute("user") == null) {
                    answer = "";
                } else
                    answer = session.getAttribute("user");
                break;
            case "/message/add":
                text = request.getParameter("text");
                user = session.getAttribute("user");
                if (text != null && user != null) {
                    messages.add(Map.of("user", user, "text", text));
                }
                break;
            case "/message/findAll":
                if (session.getAttribute("user") != null)
                    answer = messages;
                break;
            default:
                response.sendError(HttpServletResponse.SC_NOT_FOUND);
        }
        if (answer != null) {
            String json = new Gson().toJson(answer);
//            response.getWriter().print(json);
            response.setCharacterEncoding("utf-8");
            response.getOutputStream().write(json.getBytes(StandardCharsets.UTF_8));
            response.getOutputStream().flush();
        }

    }

}
