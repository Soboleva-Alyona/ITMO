<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="user_id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if handle?has_content>
        <#assign u = c.findBy(users, "handle", handle)!/>
    <#else>
        <#if user_id?has_content && user_id?is_number>
            <#assign u = c.findBy(users, "id", user_id)!/>
        </#if>
    </#if>

    <#if u?has_content>
        <div class="userPage">
            <img src="img/no-title.jpg" alt="user-icon">
            <div class="userInfo">

                <h3><u>${u.name}</u></h3>
                <ul>
                <li>id : ${u.id} </li>
                <li>handle: ${u.handle} </li>
                <li>name: ${u.name} </li>
                </ul>

            </div>
        </div>

        <#if u == users[0]>
            <div class="strelki">
                <span class="str_grey"> &larr; </span>
                <a class="strelka_to_right" href="/user?handle=${users[1].handle}#"> &rarr; </a>
            </div>
        <#elseif u == users[users?size - 1]>
            <div class="strelki">
                <a class="strelka_to_left" href="/user?handle=${users[users?size - 2].handle}#">&larr;</a>
                <span class="str_grey"> &rarr; </span>
            </div>

        <#else>
            <div class="strelki">
                <a class="strelka_to_left" href="/user?handle=${c.getPrevUser(u).handle}#">&larr;</a>
                <a class="strelka_to_right" href="/user?handle=${c.getNextUser(u).handle}#"> &rarr; </a>
            </div>
        </#if>


    <#else>
        No such user
    </#if>

</@c.page>

