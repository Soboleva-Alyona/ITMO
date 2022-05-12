package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import com.google.common.hash.Hashing;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

/** @noinspection UnstableApiUsage*/
public class UserService {
    private final UserRepository userRepository = new UserRepositoryImpl();
    private static final String PASSWORD_SALT = "177d4b5f2e4f4edafa7404533973c04c513ac619";

    public void validateRegistration(User user, String password, String passwordConfirmation) throws ValidationException {
        if (Strings.isNullOrEmpty(user.getLogin())) {
            throw new ValidationException("Login is required");
        }
        if (!user.getLogin().matches("[a-z]+")) {
            throw new ValidationException("Login can contain only lowercase Latin letters");
        }
        if (user.getLogin().length() > 8) {
            throw new ValidationException("Login can't be longer than 8 letters");
        }
        if (userRepository.findByLogin(user.getLogin()) != null) {
            throw new ValidationException("Login is already in use");
        }

        if (Strings.isNullOrEmpty(password)) {
            throw new ValidationException("Password is required");
        }
        if (password.length() < 4) {
            throw new ValidationException("Password can't be shorter than 4 characters");
        }
        if (password.length() > 12) {
            throw new ValidationException("Password can't be longer than 12 characters");
        }

        if (!password.equals(passwordConfirmation)) {
            throw new ValidationException("Passwords are different");
        }

        if (Strings.isNullOrEmpty(user.getEmail())) {
            throw new ValidationException("Email is required");
        }
        if (!user.getEmail().matches("[^@]*@[^@]*")) {
            throw new ValidationException("Email should contain exactly one\'@\'");
        }
        if (userRepository.findByEmail(user.getEmail()) != null) {
            throw new ValidationException("User with such email is already registered");
        }
    }

    public void register(User user, String password) {
        userRepository.save(user, getPasswordSha(password));
    }

    private String getPasswordSha(String password) {
        return Hashing.sha256().hashBytes((PASSWORD_SALT + password).getBytes(StandardCharsets.UTF_8)).toString();
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }

    public void validateEnter(String login, String password) throws ValidationException {
        if (Strings.isNullOrEmpty(login) || Strings.isNullOrEmpty(password)) {
            throw new ValidationException("Fields should be not null");
        }

        User user = userRepository.findByLoginAndPasswordSha(login, getPasswordSha(password));
        if (user == null) {
            user = userRepository.findByEmailAndPasswordSha(login, getPasswordSha(password));
            if (user == null) {
                throw new ValidationException("Invalid login/email or password");
            }
        }
    }

    public User findByLoginAndPassword(String loginOrEmail, String password) {
        return userRepository.findByLoginAndPasswordSha(loginOrEmail, getPasswordSha(password)) != null ? userRepository.findByLoginAndPasswordSha(loginOrEmail, getPasswordSha(password))
                : userRepository.findByEmailAndPasswordSha(loginOrEmail, getPasswordSha(password));
    }

    public int findCount() {
        return userRepository.findCount();
    }
}
