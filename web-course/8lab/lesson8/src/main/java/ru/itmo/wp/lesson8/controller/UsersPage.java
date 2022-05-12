package ru.itmo.wp.lesson8.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.lesson8.domain.User;
import ru.itmo.wp.lesson8.form.DisableForm;
import ru.itmo.wp.lesson8.form.NoticeForm;
import ru.itmo.wp.lesson8.form.UserCredentials;
import ru.itmo.wp.lesson8.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class UsersPage extends Page {
    private final UserService userService;

    public UsersPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/users/all")
    public String users(Model model) {
        model.addAttribute("users", userService.findAll());
        return "UsersPage";
    }

    @PostMapping("/users/all")
    public String enableOrDisableUser(Model model, @Valid DisableForm disableForm,
                                      HttpSession httpSession) throws NumberFormatException{
        if (getUser(httpSession) == null) {
            setMessage(httpSession, "You are not authorized");
            return "redirect:/";
        }
        if (!disableForm.getDisabled().equals("disable") && !disableForm.getDisabled().equals("enable")) {
            setMessage(httpSession, "You are a bad guy");
            return "redirect:/";
        }
        if (!disableForm.getUserId().matches("[1-9]+")) {
            setMessage(httpSession, "You are a bad guy");
            return "redirect:/";
        }
        enableOrDisableUser(httpSession, Long.parseLong(disableForm.getUserId()), disableForm.getDisabled());
        if (getUser(httpSession).getId() == Long.parseLong(disableForm.getUserId())) {
            unsetUser(httpSession);
            return "redirect:/";
        }

        return "redirect:/users/all";
    }
}
