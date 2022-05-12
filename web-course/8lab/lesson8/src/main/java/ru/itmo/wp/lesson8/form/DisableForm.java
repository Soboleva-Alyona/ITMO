package ru.itmo.wp.lesson8.form;

import org.springframework.beans.factory.annotation.Value;

import javax.validation.constraints.*;

@SuppressWarnings("unused")
public class DisableForm {
    @NotNull
    @NotEmpty
    @NotBlank
    private String userId;

    @NotBlank
    @NotNull
    private String disabled;

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getUserId() {
        return this.userId;
    }

    public void setDisabled(String disabled) {
        this.disabled = disabled;
    }

    public String getDisabled() {
        return this.disabled;
    }
}
