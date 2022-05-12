<#import "/spring.ftl" as spring>
<#import "commons.ftlh" as c>

<@c.page>

    <div class="notice form-box">
        <div class="header">Notice</div>
        <div class="body">
            <form method="post" action="">
                <div class="field">
                    <div class="name">
                        <label for="content">Notice content:</label>
                    </div>
                    <div class="value">
                        <textarea id="content" name="content" type="content" value=""/></textarea>
                    </div>

                </div>
                <div class="button-field">
                    <input type="submit" value="Create">
                </div>
            </form>
        </div>
    </div>

</@c.page>
