package ru.itmo.wp.lesson8.controller;

import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import ru.itmo.wp.lesson8.service.UserService;

@Controller
public class UserPage extends Page{
    private final UserService userService;

    public UserPage(UserService userService) {
        this.userService = userService;
    }


    @GetMapping({"/user/{id}", "/user"})
    public String user(Model model, @PathVariable (required = false) String id) {
        if (id == null) {
            return "redirect:/";
        } else {
            if (id.matches("[1-9]+")) {
                model.addAttribute("user", userService.findById(Long.valueOf(id)));
            }
            return "UserPage";
        }
    }

}
