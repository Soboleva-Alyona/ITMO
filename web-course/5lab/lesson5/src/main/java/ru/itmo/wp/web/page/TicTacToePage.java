package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {
    // TODO: Implement it.
    private void action(HttpServletRequest request, Map<String, Integer> view) {
        State state = (State) request.getSession().getAttribute("state");

    }

    public static class State {
        private boolean[][] cells = new boolean[3][3];



    }
}
