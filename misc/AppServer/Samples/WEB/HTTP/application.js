/**
 * @author Henri Gourvest <hgourvest@progdigy.com>
 */

var id = 0;

function JsonRpc(service, methode, params, doSuccess){
	new Ajax.Request(service, {
		method:'post',
		requestHeaders: {'Accept': 'application/json', 'Content-Type': 'application/json'},
		postBody: $H({'method': methode, 'params': params, 'id': ++id}).toJSON(),
  		onSuccess: function(transport){
			var json = transport.responseText.evalJSON(true);
			if (json.error == null) 
				doSuccess(json.result); else
				alert(json.error);
  		}
	});	
}

function echo(sender, str)
{
	JsonRpc('application', 'echo', [str], function(result){
		sender.value = result[0];
	});
}

function getdata(div, t){
    function dce(data){return document.createElement(data);}
    function dct(data){return document.createTextNode(data);}
    function dca(data){return document.createAttribute(data);}
    //function dcet(data, str){return dce(data).appendChild(dct(str)).parent;}

	JsonRpc('application', 'getdata', [t], function(r){
		div.innerHTML = '';
		var table = div.appendChild(dce('table'));
		var tbody = table.appendChild(dce('tbody'));
		var tr = tbody.appendChild(dce('tr'));
		
		for(var i = 0; i < r.meta.length; i++){
			tr.appendChild(dce('th')).appendChild(dct(r.meta[i]));
		}
		
		for(var i = 0; i < r.data.length; i++)
		{
		  var tr = tbody.appendChild(dce('tr'));
		  for(var j = 0; j < r.data[i].length; j++){
		  	tr.appendChild(dce('td')).appendChild(dct(r.data[i][j]));
		  }
		}
	});	
}
	