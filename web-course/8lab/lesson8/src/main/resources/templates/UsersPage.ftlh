<#import "commons.ftlh" as c>

<@c.page>
    <#if users?size==0>
        <tr class="noData">
            No users registered
        </tr>
    <#else>
    <div class="users datatable">
        <div class="caption">User</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Login</th>
                    <th>Created</th>
                    <th>Disabled/enabled</th>
                </tr>
                </thead>
                <tbody>
                    <#list users as user>
                    <tr>
                        <td class="id">${user.id}</td>
                        <td class="login">${user.login}</td>
                        <td class="creationTime">${user.creationTime}</td>
                        <td class="disabled">
                        <form method="post">
                                <input name="userId" value="${user.id}" hidden>
                                <#if user.isDisabled()>
                                    <#assign text="enable">
                                <#else>
                                    <#assign text="disable">
                                </#if>
                                <input class="button" name="disabled" type="submit" value="${text}">

                        </form>
                        </td>
                    </tr>
                </#list>
                </tbody>
            </table>



    </div>
    </#if>
</@c.page>
