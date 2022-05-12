<#import "commons.ftlh" as c>

<@c.page>
    Welcome on Codeforces!

        <div class="articles">
            <template>
                <article>
                    <div class="article-title"></div>
                    <div class="info"></div>
                    <div class="article-text"></div>
                </article>
            </template>
            <div class="noData">
                No articles
            </div>
        </div>

        <script>
            $(function () {
                const $noData = $(".articles .noData");
                $noData.text("Loading...");

                $.ajax({
                    url: "index",
                    dataType: "json",
                    data: {
                        action: "findArticles",
                    },
                    success: function(response) {
                        response["articles"].forEach(function(article) {
                            $noData.hide();
                            const $tpl = $($(".articles template").prop("content")).clone();
                            $tpl.find(".article-title").text(article.title);
                            $tpl.find(".info").text("Done on " + article.creationTime + " by " + response["usersId"][article["userId"]]["login"]);

                            $tpl.find(".article-text").text(article.text);
                            $tpl.find("tpl").attr("data-articleId", article.id);
                            const $isHidden = $tpl.find(".is-hidden").text(article.hidden);
                            if ($isHidden != true) {
                                $(".articles").append($tpl);
                            }
                        });
                    },
                    type: "POST"
                });
            })
        </script>

</@c.page>
