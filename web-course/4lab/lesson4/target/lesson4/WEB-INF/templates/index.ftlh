<#-- @ftlvariable name="id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#assign p = c.findPost(posts, "id", id)>
    <article>

                <div class="title">${p.title}</div>
                <div class="information">By ${p.userId}</div>
                <div class="body">
                    <p>${p.text}</p>
                </div>

                <div class="footer">
                    <div class="left">
                        <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                        <span class="positive-score">+173</span>
                        <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                    </div>
                    <div class="right">
                        <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                        2 days ago
                        <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                        <a href="#">68</a>
                    </div>
                </div>

    </article>

</@c.page>