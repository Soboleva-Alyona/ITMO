package ru.itmo.wp.form;

import javax.validation.constraints.*;

public class PostForm {
    @NotNull
    @NotEmpty
    @NotBlank
    private String title;

    @NotNull
    @NotEmpty
    @NotBlank
    private String text;

    @Size(min = 0, max = 60)
    @Pattern(regexp = "(\\s*[a-z]+\\s*)*", message = "Expected latin lowercase words split by whitespaces")
    private String tagsString;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getTagsString() {
        return tagsString;
    }

    public void setTagsString(String tagsString) {
        this.tagsString = tagsString;
    }

}
