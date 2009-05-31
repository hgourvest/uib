<html>
<body>
  <table>
    <th>
      <tr><td>Country</td>
      <td>Currency</td>
      <td>Suppr</td>
      <td>Edit</td></tr></th>
  <% for k, v in ipairs(data) do %>
  <tr>
      <td><%=v.COUNTRY%></td>
      <td><%=v.CURRENCY%></td>
      <td><a href="/country/del/<%=v.COUNTRY%>">suppr</a></td>
      <td><a href="/country/edit/<%=v.COUNTRY%>">edit</a></td></tr>
 <% end %>
 </table>
<form action="/country/add" method="POST"><input type="text" name="country"/><input type="text" name="currency"/><input type="submit"/></form>
</body>
</html>