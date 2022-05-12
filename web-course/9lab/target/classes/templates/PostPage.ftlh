<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->
<#import "commons.ftlh" as c>
<#import "/spring.ftl" as spring>

<@c.page>
    <#if post??>
        <@c.post post=post/>
        <#if user??>
            <div class="form">
                <div class="header">New comment</div>
                <div class="body">
                    <form method="post" action="">
                        <div class="field">
                            <div class="value">
                                <input id="text" name="text" value="${comment.text!}"></input>
                            </div>
                            <@c.error "comment.text"/>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Add">
                        </div>
                    </form>
                </div>
            </div>
        </#if>
        <div class="comments">Comments</div>
        <#if comments?size!=0>
            <#list comments as comment>
                <@c.comment comment=comment/>
            </#list>
        <#else>
            No comments
        </#if>
    <#else>
        No such post
    </#if>
</@c.page>
