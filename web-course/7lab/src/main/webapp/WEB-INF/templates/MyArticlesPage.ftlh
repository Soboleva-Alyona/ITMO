<#-- @ftlvariable name="title" type="java.lang.String" -->
<#-- @ftlvariable name="text" type="java.lang.String" -->
<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <#if user??>
        <#if articles[0]??>
            <div class="articles datatable">
                <div class="caption">My articles</div>

                    <table>
                        <thead>
                        <tr>
                            <th>Id</th>
                            <th>Title</th>
                            <th>Show/Hide</th>
                        </tr>
                        </thead>
                        <tbody>

                            <#list articles as article>
                                <tr>
                                    <td class="article-id">${article.id}</td>
                                    <td class="article-title">${article.title}</td>
                                    <td class="show-hide">
                                        <#if article.hidden>
                                            <input article_id="${article.id}" type="submit" value="Show">
                                        <#else>
                                            <input article_id="${article.id}" type="submit" value="Hide">
                                        </#if>
                                    </td>
                                </tr>
                            </#list>
                        </tbody>
                    </table>

            </div>
                <script>
                    $(function () {
                            $(".show-hide input").click(function () {
                                const $form = $(this);
                                let statusValue = $form.attr('value');
                                if (statusValue !== 'Hide' && statusValue !== 'Show') {
                                    return;
                                }
                                const status = (statusValue === "Hide");
                                let id =  $form.attr('article_id');
                                if (!id.match(/^\d+$/)) {
                                    return;
                                }
                                const data = {
                                    action: "setArticleHidden",
                                    id,
                                    newHidden: status
                                }

                                const success = function (response) {
                                    $form.attr('value', !status ? "Hide" : "Show")
                                }

                                ajax(data, success);
                                return false;
                            })
                        })
                </script>
                <#else >
                You haven't written any post yet
        </#if>
    <#else>
        You haven't login
    </#if>


</@c.page>