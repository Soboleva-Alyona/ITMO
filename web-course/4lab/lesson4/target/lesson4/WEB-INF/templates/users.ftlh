<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>

    <div class="datatable">
        <table>
            <thead>
            <tr>
                <th>Users</th>
            </tr>
            </thead>
            <tbody>

            <#list users as u>
                <tr>
                    <#if user?? && user.id == u.id>*</#if>
                    <td><@c.userlink u/></td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
</@c.page>
