<#import "commons.ftlh" as c>

<@c.page>
    <#if user??>
        <div class="user-info">
            <div>User id: ${user.id}</div>
            <div>User login : ${user.login}</div>
            <div>User registration time: ${user.creationTime}</div>
        </div>
    <#else>
        No such user
    </#if>
</@c.page>