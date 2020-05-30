/* 
 * html_onclick - JavaScript library for extend HTML and create widgets such as dropdown menu, counters, tabs, editable fields...
 * 
 * Version: 1.0
 * License: MIT
 * 
 *  Copyright (c) 2013-2020 Saemon Zixel <saemonzixel@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy of this software *  and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

if("html_onclick" in window == false)
function html_onclick(event) {
	var ev = event || window.event;
	var trg1 = ev.target || ev.srcElement || document.body.parentNode;
	if (trg1.nodeType && trg1.nodeType == 9) trg1 = trg1.body.parentNode; // #document
	if (trg1.nodeType && trg1.nodeType == 3) trg1 = trg1.parentNode; // #text
	var trg1p = (trg1.parentNode && trg1.parentNode.nodeType != 9) ? trg1.parentNode : {className:'', nodeName:'', getAttribute:function(){return ''}};
	var trg1pp = (trg1p.parentNode && trg1p.parentNode.nodeType != 9) ? trg1p.parentNode : {className:'', nodeName:'', getAttribute:function(){return ''}};
	
	// удобная функция поиска близкого элемента по BEM
	function find_near(class_name, if_not_found_return, start_node, prefix) {
		// определим префикс блока
		if(!prefix && (prefix = class_name.indexOf('!')+1) > 0) { 
			var prefix1 = class_name.substring(0, prefix-1);
			class_name = prefix1 + class_name.substring(prefix);
		} else
			var prefix1 = prefix || class_name.replace(/^([a-z]+-[^-]+).*$/, '$1');
		var regexp = new RegExp(class_name+'( |$)','');
		
		// поищем среди соседей сначало
		for(var i = 0, root = start_node || trg1; i < root.parentNode.childNodes.length; i++)
			if((root.parentNode.childNodes[i].className||'').match(regexp))
				return root.parentNode.childNodes[i];
		
		// найдём корневой node в блоке, заодно возможно встретим искомый элемент
		for(; root.parentNode.className.indexOf(prefix1) > -1; root = root.parentNode)
			if(root.parentNode.className.indexOf(class_name) > -1 && root.parentNode.className.match(regexp))
				return root.parentNode;
		
		// перебираем всё, что ниже root
		var nodes = root.getElementsByTagName('*');
		for(var i=0; i<nodes.length; i++)
			if(nodes[i].className && nodes[i].className.indexOf(class_name) > -1 && nodes[i].className.match(regexp)) 
				return nodes[i];
			
		return if_not_found_return;
	}

	// для расширений
	html_onclick.ev = ev;
	html_onclick.ev_processed = true;
	html_onclick.trg1 = trg1; html_onclick.trg1p = trg1p; html_onclick.trg1pp = trg1pp;
	html_onclick.find_near = find_near;
	
	// [outerclick] event
	if (html_onclick.outer_click_hooks.length && ev.type == 'click') {
		var save_click_hooks = [];
		for (var i = 0; i < html_onclick.outer_click_hooks.length; i++)
			if (html_onclick.outer_click_hooks[i](event, trg1))
				save_click_hooks.push(html_onclick.outer_click_hooks[i]);
		html_onclick.outer_click_hooks = save_click_hooks;
	}
	
	// g-show_hide-by_id-*
	if(trg1.className.indexOf('g-show_hide-by_id') > -1) {
		var id = trg1.className.match(/g-show_hide-by_id-([-0-9a-zA-Z_]+)/);
		var elem = document.getElementById(id[1]);
		if(elem && elem.style.display == 'none') {
			elem.style.display = 'block';
			if(elem.getAttribute('onshow'))
				(new Function('event', elem.getAttribute('onshow'))).call(trg1, ev);
		} else
		if(elem && elem.style.display != 'none') {
			elem.style.display = 'none';
			if(elem.getAttribute('onhide'))
				(new Function('event', elem.getAttribute('onhide'))).call(trg1, ev);
		}
	}

	// расширения
	for (var i in html_onclick.extensions)
	if(html_onclick.extensions[i] instanceof Function)
		html_onclick.extensions[i](ev, trg1, trg1p, trg1pp, find_near);
}

/* install html_onclick events (if first load) */
if(!html_onclick.extensions) {
	html_onclick.extensions = [];
	html_onclick.outer_click_hooks = [];
	
	document.documentElement.addEventListener("click", html_onclick);
	document.documentElement.addEventListener("input", html_onclick);

	document.addEventListener("DOMContentLoaded", function(event) {
		if(document.readyState != 'complete') return;
		html_onclick({type: 'DOMContentLoaded', target: document});
	});
}

/* 
 * html_onkey - JavaScript library for extend HTML and create widgets such as dropdown menu, counters, tabs, editable fields...
 * 
 * Version: 1.0
 * License: MIT
 * 
 *  Copyright (c) 2013-2020 Saemon Zixel <saemonzixel@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy of this software *  and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

if("html_onkey" in window == false)
function html_onkey(event) {
	var ev = event || window.event;
	var trg1 = ev.target || ev.srcElement || document.body.parentNode;
	if (trg1.nodeType && trg1.nodeType == 9) trg1 = trg1.body.parentNode; // #document
    if (trg1.nodeType && trg1.nodeType == 3) trg1 = trg1.parentNode; // #text
	var trg1p = (trg1.parentNode && trg1.parentNode.nodeType != 9) ? trg1.parentNode : {className:'', nodeName:'', getAttribute:function(){return ''}};
	var trg1pp = (trg1p.parentNode && trg1p.parentNode.nodeType != 9) ? trg1p.parentNode : {className:'', nodeName:'', getAttribute:function(){return ''}};
	
	// для расширений
	html_onkey.ev = ev;
	html_onkey.trg1 = trg1; html_onkey.trg1p = trg1p; html_onkey.trg1pp = trg1pp;
	
	// расширения
	for (var i in html_onkey.extensions) 
	if(html_onkey.extensions[i] instanceof Function)
		html_onkey.extensions[i](ev, trg1, trg1p, trg1pp);
	
	// для совместимости со старым кодом
	return window.html_onkey_custom ? html_onkey_custom(ev, trg1, trg1p, trg1pp) : undefined;
}

/* install html_onkey events (if first load) */
if(!html_onkey.extensions) {
	html_onkey.extensions = [];
	
	document.documentElement.addEventListener("keyup", html_onkey);
	document.documentElement.addEventListener("keypress", html_onkey);
	document.documentElement.addEventListener("keydown", html_onkey);
	document.documentElement.addEventListener("change", html_onkey);
}

/* 
 * html_onmouse - JavaScript library for extend HTML and create widgets such as trackbars, windows, draggable elements...
 * 
 * Version: 1.0
 * License: MIT
 * 
 *  Copyright (c) 2013-2020 Saemon Zixel <saemonzixel@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy of this software *  and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

if("html_onmouse" in window == false)
function html_onmouse(event) {
	var ev = event || window.event || { type:'', target: document.body.parentNode };
    var trg1 = ev.target || ev.srcElement || document.body.parentNode;
	if (trg1.nodeType && trg1.nodeType == 9) trg1 = trg1.body.parentNode; // #DOCUMENT
    if (trg1.nodeType && trg1.nodeType == 3) trg1 = trg1.parentNode; // #TEXT
	var trg1p = (trg1.parentNode && trg1.parentNode.nodeType != 9) ? trg1.parentNode : {className:'', nodeName:'', getAttribute:function(){return ''}};

	// mouseout-window
	if( ev.type == "mouseout" && ev instanceof Event) {
		e = ev.originalEvent || ev; // if jQuery.Event
		active_node = (e.relatedTarget) ? e.relatedTarget : e.toElement;
		if(!active_node) {
// 				html_onmouse({type:'mouseout-window',target:(document.body||{}).parentNode||window});
			html_onmouse({type:"mouseout-window", target: trg1});
			return;
		}
	}
	
	function find_near(class_name, if_not_found_return, start_node, prefix) {
		// определим префикс блока
		if(!prefix && (prefix = class_name.indexOf('!')+1) > 0) { 
			var prefix1 = class_name.substring(0, prefix-1);
			class_name = prefix1 + class_name.substring(prefix);
		} else
			var prefix1 = prefix || class_name.replace(/^([a-z]+-[^-]+).*$/, '$1');
			
		// найдём корневой node в блоке
		for(var root = start_node || trg1; root.nodeName != 'HTML' && root.parentNode.className.indexOf(prefix1) > -1; root = root.parentNode);
		
		if(root.className.indexOf(class_name) > -1) return root;
		
		var nodes = root.getElementsByTagName('*');
		for(var i=0; i<nodes.length; i++)
			if(nodes[i].className && nodes[i].className.indexOf(class_name) > -1) 
				return nodes[i];
			
		return if_not_found_return;
	}
	
	// для расширений
	html_onmouse.ev = ev;
	html_onmouse.trg1 = trg1; html_onmouse.trg1p = trg1p;
	html_onmouse.find_near = find_near;
	
	// .g-draggable [START...]
	if(ev.type == 'mousedown' && trg1.className.indexOf('g-draggable') > -1) {
		var ev_pageX = ev.pageX || (ev.clientX+document.documentElement.scrollLeft);
		var ev_pageY = ev.pageY || (ev.clientY+document.documentElement.scrollTop);

		// возмём целевой для перетаскивания
		var trg = trg1;
		if(trg.className.indexOf('g-draggable-parent4') >= 0) trg = trg.parentNode.parentNode.parentNode.parentNode;
		else if(trg.className.indexOf('g-draggable-parent3') >= 0) trg = trg.parentNode.parentNode.parentNode;
		else if(trg.className.indexOf('g-draggable-parent2') >= 0) trg = trg.parentNode.parentNode; 
		else if(trg.className.indexOf('g-draggable-parent') >= 0) trg = trg.parentNode;
	
		// отменим всплытие, и запретим выделение
		if(ev.stopPropagation) ev.stopPropagation();
		else ev.cancelBubble = true;
		document.ondragstart = function(){ return false; }
		document.body.onselectstart = function() { return false } // IE8
		ev.preventDefault(); // new browsers
		
		// стартовые кординаты мыши
		trg.setAttribute('data-mousedown_point', ev_pageX+' '+ev_pageY+' '+ev.clientX+' '+ev.clientY);
		
		var trg_offset = trg.getBoundingClientRect();
		var trg_style = trg.currentStyle || window.getComputedStyle(trg, null);
		var start_params = [
			trg.offsetHeight, trg.offsetWidth,
			parseInt(trg_style.left == 'auto' ? '0' : trg_style.left),
			parseInt(trg_style.top == 'auto' ? '0' : trg_style.top),
			trg_offset.left, trg_offset.top,
			Math.round(trg_offset.left) + (window.pageXOffset || document.documentElement.scrollLeft || document.body.scrollLeft || 0),
			Math.round(trg_offset.top) + (window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0),
		];
		trg.setAttribute('data-start_params', start_params.join(' '));
		trg.style.position = trg_style.position;
		
		html_onmouse.cursor.dragging_element = trg;
		return false; // подавим активацию выделения
	}
	
	// .g-draggable [...MOVE...]
	if(ev.type == 'mousemove' && html_onmouse.cursor['dragging_element']) {
		var trg = html_onmouse.cursor.dragging_element;
		var base_point = trg.getAttribute('data-mousedown_point').split(' ');
		var start_params = trg.getAttribute('data-start_params').split(' ');
		switch(trg.style.position) {
			case 'fixed':
				trg.style.left = parseInt(start_params[4])+ev.clientX-parseInt(base_point[2])+'px';
				trg.style.top = parseInt(start_params[5])+ev.clientY-parseInt(base_point[3])+'px';
				break;
			default:
			case 'static':
				trg.style.position = 'relative';
			case 'relative':
			case 'absolute':
				var ev_pageX = ev.pageX || (ev.clientX+document.documentElement.scrollLeft);
				var ev_pageY = ev.pageY || (ev.clientY+document.documentElement.scrollTop);
				trg.style.left = parseInt(start_params[2])+ev_pageX-parseInt(base_point[0])+'px';
				trg.style.top = parseInt(start_params[3])+ev_pageY-parseInt(base_point[1])+'px';
				break;
		}
		
		if(ev.stopPropagation) ev.stopPropagation();
		else ev.cancelBubble = true;
	}

	// .g-draggable [...END]
	if(ev.type == 'mouseup') {
		for(var f in html_onmouse.cursor)
			if(f.indexOf('dragging_') === 0)
				delete html_onmouse.cursor[f];
			
		document.ondragstart = null;
		document.body.onselectstart = null; // IE8
	}
	
	// .win-splitter [START...]
	if(ev.type == 'mousedown' && trg1.className.indexOf('win-splitter') > -1) {
		
		// стартовые кординаты мыши
		trg1.setAttribute('data-mousedown-point', (ev.pageX || (ev.clientX+document.documentElement.scrollLeft))+' '+(ev.pageY || (ev.clientY+document.documentElement.scrollTop)));
		
		// стартовое положение
		trg1.setAttribute('data-start-left-top', parseInt(trg1.style.left) + ' ' + parseInt(trg1.style.top));
		
		// найдём родительское окно
		for(trg1.win = trg1.parentNode; 
			!trg1.win.className.match(/win( |$)/); 
			trg1.win = trg1.win.parentNode)
			if(trg1.win.parentNode.nodeType != 1) break;

		// сгенерируем уникальный id если нет
		trg1.id = trg1.id || trg1.win.id+'_splitter'+(new Date())*1;
			
		// запомним, кого мы перетаскиваем
		html_onmouse.cursor.dragging_splitter = trg1;
			
		// запретим выделение
		document.body.onselectstart = function() { return false }
		document.ondragstart = function(){ return false; }
		
		// подавляем начало выделения мышью
		if(ev.preventDefault) ev.preventDefault();
		else ev.returnValue = false;
	}

	// .win-splitter [...DRAG]
	if(ev.type == 'mousemove' && html_onmouse.cursor['dragging_splitter']) {
		var trg = html_onmouse.cursor['dragging_splitter'];
		var ev_pageX = ev.pageX || (ev.clientX+document.documentElement.scrollLeft);
		var ev_pageY = ev.pageY || (ev.clientY+document.documentElement.scrollTop);
		
		var mousedown_point = trg.getAttribute('data-mousedown-point');
		mousedown_point = mousedown_point ? mousedown_point.split(' ') : [ev_pageX||0, ev_pageY||0];

		var start_left_top = trg.getAttribute('data-start-left-top');
		start_left_top = start_left_top ? start_left_top.split(' ') : [trg.style.left||'0', trg.style.top||'0'];
		
		// вычесляем отклонение
		var delta_x = ev_pageX - mousedown_point[0];
		var delta_y = ev_pageY - mousedown_point[1];
		
		if(trg.className.indexOf('type_vertical') > -1) {
			trg.force_delta_right = trg.force_delta_left = parseInt(start_left_top[0]) + delta_x - parseInt(trg.style.left||'0');
// 			trg.style.left = parseInt(start_left_top[0]) + delta_x + 'px';
		} else {
			trg.force_delta_top = trg.force_delta_bottom = parseInt(start_left_top[1]) + delta_y - parseInt(trg.style.top||'0');
// 			trg.style.top = parseInt(start_left_top[1]) + delta_y + 'px';
		}

		// запустим resize окна, чтоб зависимые элементы изменили свой размер и положение
		html_onmouse({type: 'winresize', target: trg.win});
		
		delete trg.force_delta_left; delete trg.force_delta_right;
		delete trg.force_delta_top; delete trg.force_delta_bottom;
	}
	
	// .win-resizer [mousedown]
	if(ev.type == 'mousedown' && trg1.className.indexOf('win-resizer') > -1) {
		
		// стартовые кординаты мыши
		trg1.setAttribute('data-mousedown_point', (ev.pageX || (ev.clientX+document.documentElement.scrollLeft))+' '+(ev.pageY || (ev.clientY+document.documentElement.scrollTop)));
		trg1.removeAttribute('data-start_size');
		
		html_onmouse.cursor.dragging_window_resizer = trg1;
		
		// запретим выделение
		document.body.onselectstart = function() { return false } // old IE
		document.ondragstart = function(){ return false; } // old browsers
		ev.preventDefault();
	}

	// .win-resizer [mousemove]
	if(ev.type == 'mousemove' && html_onmouse.cursor['dragging_window_resizer']) {
		var trg = html_onmouse.cursor.dragging_window_resizer || trg1;
		var parent_div = trg.parentNode;
		var ev_pageX = ev.pageX || (ev.clientX+document.documentElement.scrollLeft);
		var ev_pageY = ev.pageY || (ev.clientY+document.documentElement.scrollTop);
		
		var start_point = trg.getAttribute('data-mousedown_point');
		if(!start_point) trg.setAttribute('data-mousedown_point', (start_point = (ev_pageX||0)+' '+(ev_pageY||0)));
		start_point = start_point.split(' ');
			
		var start_size = trg.getAttribute('data-start_size');
		if(!start_size) trg.setAttribute('data-start_size', (start_size = trg.parentNode.offsetWidth + ' ' + trg.parentNode.offsetHeight));
		start_size = start_size.split(' ');
		
		// новоая ширина, но не менее минимальной у окна если задана
		var new_width = parseInt((trg.parentNode.currentStyle || window.getComputedStyle(trg.parentNode, null)).minWidth||'0');

		new_width = Math.max(new_width, parseInt(start_size[0]) + (ev_pageX||0) - parseInt(start_point[0]));
		
		var new_height = Math.max(0, parseInt(start_size[1]) + (ev_pageY||0) - parseInt(start_point[1]));

		// старый или новый обработчик?
		ev.fieldsets_found = trg.parentNode.getElementsByTagName('FIELDSET');
	}
	
	// [winresize] ?
	if(ev.type == 'winresize' /*&& trg1.className.indexOf('win-resizer') > -1*/) {
		var parent_div = trg1;
		var new_width = ev.width || ev.new_width || parseInt(parent_div.style.width || (parent_div.offsetWidth+''));
		var new_height = ev.height || ev.new_height || parseInt(parent_div.style.height || (parent_div.offsetHeight+''));
		
		// старый или новый обработчик?
		ev.fieldsets_found = trg1.getElementsByTagName('FIELDSET');
	}
	
	// [winresize] OLD
	if((ev.type == 'winresize' || ev.type == 'mousemove') &&
		('fieldsets_found' in ev && ev.fieldsets_found.length == 0)) {
		
		// соберём список видимых и подходящих строк окна
		var nodes = [];
		for(var i = 0; i < parent_div.childNodes.length; i++)
			if(parent_div.childNodes[i] == trg 
			|| parent_div.childNodes[i].style.display == 'none'
			|| parent_div.childNodes[i].className.indexOf('c-tabs-hidden_tab') > -1
			|| parent_div.childNodes[i].nodeType != 1) continue;
			else nodes.push(parent_div.childNodes[i]);
			
		var need_correct_width = false, flexible = 0;
		for(var ii = 0; ii < nodes.length; ii++) {
			var first_child = nodes[ii].firstElementChild;
			
			// уменьшим ширину, а потом проверим, не выпирает ли
			nodes[ii].style.width = new_width + 'px';
			if(first_child && first_child.offsetWidth > new_width) {
				new_width = first_child.offsetWidth;
				need_correct_width = true;
			}
			
			// учтём жёсткие высоты
			if(nodes[ii].getAttribute('data-win-row-height')) {
				new_height -= parseInt(nodes[ii].getAttribute('data-win-row-height'));
				if(!nodes[ii].style.height) nodes[ii].style.height = nodes[ii].getAttribute('data-win-row-height') + 'px';
				continue;
			}
			
			// резиновые высоты
			flexible++;
			nodes[ii].style.height = '1px';
			if(first_child) new_height -= first_child.offsetHeight; 
		}
		
		// проставим резиновые высоты, и расчитам окончательную высоту
		var din_height = Math.max(0, new_height) / Math.max(flexible, 1);
		var final_height = 0;
		for(var ii = 0; ii < nodes.length; ii++) {
			var first_child = nodes[ii].firstElementChild;

			if(need_correct_width)
				nodes[ii].style.width = new_width + 'px';
			if( ! nodes[ii].getAttribute('data-win-row-height'))
				try { nodes[ii].style.height = (first_child.offsetHeight || 1) + din_height + 'px'; } 
				catch(e) { nodes[ii].style.height = 1 + din_height + 'px'; }
			final_height += nodes[ii].offsetHeight;
		}
		if(final_height == 0) 
			final_height = new_height;
			
		parent_div.style.width = new_width + 'px';
		parent_div.style.height = final_height + 'px';
	}
	
	// [winresize] NEW
	if((ev.type == 'winresize' || ev.type == 'mousemove') &&
		('fieldsets_found' in ev && ev.fieldsets_found.length > 0)) {
// console.log(ev);
	
		var win = parent_div;	
	
		// вычеслим текущий хеш из настроек привязок
		var current_hash = [];
		for(var i = 0; i < ev.fieldsets_found.length; i++)
			current_hash.push('['+i+']'+ev.fieldsets_found[i].getAttribute('data-win-relocations')||'(empty)');
		current_hash = current_hash.join('');
			
		// хеши не совпали или старого хеша/функции нет вообще, строим новую функцию обработки привязок
		if(current_hash != win['onresize_hash']) {
			
			// первыми идут обновления размера окна
			func_src = [
				'/* win */',
				'var win_delta_left = 0, win_delta_top = 0;',
				'var win_delta_right = new_width - parseInt(win.style.width);',
				'var win_delta_bottom = new_height - parseInt(win.style.height);',
				'win.style.width = new_width + "px";',
				'win.style.height = new_height + "px";'
			];
			var checklist = {
				win_delta_top: true, win_delta_right: true, 
				win_delta_bottom: true, win_delta_left: true};
			
			// Составляем список найденных привязок для обработки
			for(var i = 0; i < ev.fieldsets_found.length; i++) {
				var fieldset = ev.fieldsets_found[i];
				var relocations = fieldset.getAttribute('data-win-relocations')||'';
				fieldset.id = fieldset.id || (win.id+'_fieldset'+i);
			
				func_src.push(''); func_src.push('/* '+fieldset.id+' */');
				func_src.push('var '+fieldset.id+' = fieldsets['+i+'];');
				
				// TOP
				var reloc = relocations.match(/(top): *#?([0-9a-zA-Z_]+)[()]* +([a-z]+) *([.0-9]*)/);
				if(!reloc) { 
					func_src.push('var '+fieldset.id+'_delta_top = '+fieldset.id+'.force_delta_top || 0;');
					checklist[fieldset.id+'_delta_top'] = true;
				} else {
					checklist[fieldset.id+'_delta_top'] = [fieldset.id, reloc[1], reloc[2], reloc[3], reloc[4], reloc[5]];
				}
				
				// BOTTOM
				var reloc = relocations.match(/(bottom): *#?([0-9a-zA-Z_]+)[()]* +([a-z]+) *([.0-9]*)/);
				if(!reloc) { 
					func_src.push('var '+fieldset.id+'_delta_bottom = '+fieldset.id+'.force_delta_bottom || 0;');
					checklist[fieldset.id+'_delta_bottom'] = true;
				} else {
					reloc[0] = fieldset.id;
					checklist[fieldset.id+'_delta_bottom'] = [fieldset.id, reloc[1], reloc[2], reloc[3], reloc[4], reloc[5]];
				}
				
				// LEFT
				var reloc = relocations.match(/(left): *#?([0-9a-zA-Z_]+)[()]* +([a-z]+) *([.0-9]*)/);
				if(!reloc) { 
					func_src.push('var '+fieldset.id+'_delta_left = '+fieldset.id+'.force_delta_left || 0;');
					checklist[fieldset.id+'_delta_left'] = true;
				} else {
					reloc[0] = fieldset.id;
					checklist[fieldset.id+'_delta_left'] = [fieldset.id, reloc[1], reloc[2], reloc[3], reloc[4], reloc[5]];
				}
				
				// RIGHT
				var reloc = relocations.match(/(right): *#?([0-9a-zA-Z_]+)[()]* +([a-z]+) *([.0-9]*)/);
				if(!reloc) { 
					func_src.push('var '+fieldset.id+'_delta_right = '+fieldset.id+'.force_delta_right || 0;');
					checklist[fieldset.id+'_delta_right'] = true;
				} else {
					reloc[0] = fieldset.id;
					checklist[fieldset.id+'_delta_right'] = [fieldset.id, reloc[1], reloc[2], reloc[3], reloc[4], reloc[5]];
				}
			}
			
			// начинаем расчитывать привязки по мере разрешения зависимостей
			for(var prt_cnt = 1; prt_cnt < 10; prt_cnt++) {
				func_src.push(''); func_src.push('/* '+prt_cnt+' */');
				
				var been_processed = 0;
				for(var f in checklist) if(checklist[f] != true) {
					var fieldset_id = checklist[f][0];
					
					// TOP
					if(checklist[f][1] == 'top') {
						var reloc_target = checklist[f][2]+'_delta_'+(checklist[f][3]||'bottom');
						
						// пропускаем если ещё не посчитан элемент к которому привязываемся
						if(checklist[reloc_target] != true) continue; 
						
						// посчитаем нашу делту от делты привязываемого
						func_src.push('var '+f+' = '+fieldset_id+'.force_delta_top || ('+reloc_target+' * '+parseFloat(checklist[f][4]||'1.0')+');');

						// устанавливаем новую высоту
						func_src.push(fieldset_id+'.style.top = parseFloat('+fieldset_id+'.style.top||'+fieldset_id+'.offsetTop+".0") + '+f+'+"px";');
						
						// отмечаем, что расчитали
						checklist[f] = true;
						been_processed++;
					}
					
					// BOTTOM
					else if(checklist[f][1] == 'bottom') {
						var reloc_target = checklist[f][2]+'_delta_'+(checklist[f][3]||'top');
						var fieldset_top = checklist[f][0]+'_delta_top';
						
						// пропускаем если ещё не посчитан элемент к которому привязываемся
						// и наш top т.к. надо будет его вычесть из высоты
						if(checklist[reloc_target] != true) continue; 
						if(checklist[fieldset_top] != true) continue; 
						
						// посчитаем нашу делту от делты привязываемого
						func_src.push('var '+f+' = '+fieldset_id+'.force_delta_bottom || ('+reloc_target+' * '+parseFloat(checklist[f][4]||'1.0')+');');
						
						// устанавливаем новую высоту
						func_src.push(fieldset_id+'.style.height = parseFloat('+fieldset_id+'.style.height||'+fieldset_id+'.offsetHeight+".0") + '+f+' - '+fieldset_top+' + "px";');
						
						// отмечаем, что расчитали
						checklist[f] = true;
						been_processed++;
					}
					
					// LEFT
					else if(checklist[f][1] == 'left') {
						var reloc_target = checklist[f][2]+'_delta_'+(checklist[f][3]||'right');
						
						// пропускаем если ещё не посчитан элемент к которому привязываемся
						if(checklist[reloc_target] != true) continue; 
						
						// посчитаем нашу делту от делты привязываемого
						func_src.push('var '+f+' = '+fieldset_id+'.force_delta_left || ('+reloc_target+' * '+parseFloat(checklist[f][4]||'1.0')+');');

						// устанавливаем новую высоту
						func_src.push(fieldset_id+'.style.left = parseFloat('+fieldset_id+'.style.left||'+fieldset_id+'.offsetLeft+".0") + '+f+'+"px";');
						
						// отмечаем, что расчитали
						checklist[f] = true;
						been_processed++;
					}
					
					// RIGHT
					else if(checklist[f][1] == 'right') {
						var reloc_target = checklist[f][2]+'_delta_'+(checklist[f][3]||'left');
						var fieldset_left = checklist[f][0]+'_delta_left';
						
						// пропускаем если ещё не посчитан элемент к которому привязываемся
						// и наш top т.к. надо будет его вычесть из высоты
						if(checklist[reloc_target] != true) continue; 
						if(checklist[fieldset_left] != true) continue; 
						
						// посчитаем нашу делту от делты привязываемого
						func_src.push('var '+f+' = '+fieldset_id+'.force_delta_right || ('+reloc_target+' * '+parseFloat(checklist[f][4]||'1.0')+');');
						
						// устанавливаем новую высоту
						func_src.push(fieldset_id+'.style.width = parseFloat('+fieldset_id+'.style.width||'+fieldset_id+'.offsetWidth+".0") + '+f+' - '+fieldset_left+' + "px";');
						
						// отмечаем, что расчитали
						checklist[f] = true;
						been_processed++;
					} 
					
					// неизвестный парамтр ???
					else {
						debugger;
					}
					
					func_src.push('');
				}
				
				// еси нечего не обработали, то обрабатывать нечего
				if(been_processed == 0) break;
			}
// littlelisp_ide_main_window_src.value = func_src.join('\n');
			win.onresize_func = new Function('win, new_width, new_height, fieldsets', func_src.join('\n'));
			win.onresize_hash = current_hash;
		}
		
		if(win.onresize_func)
			win.onresize_func(win, new_width, new_height, ev.fieldsets_found);
	}
	
	// [winresize] settings
	if(ev.type == 'winresize') {
		// сохраним параметры окна, если указано где
		if(parent_div.getAttribute('data-win-settings-inLocalStorage')) {
			var key = ev.trg.parentNode.getAttribute('data-win-settings-inLocalStorage');
			var win_settings = JSON.parse(localStorage[key]||'{}') || {};
			win_settings.height = parseInt(ev.trg.parentNode.style.height);
			win_settings.width = parseInt(ev.trg.parentNode.style.width);
			localStorage[key] = JSON.stringify(win_settings);
		}
	}

	// .win-close-btn, .win-close-button
	if(ev.type == 'mouseup' && (trg1.className.indexOf('win-close-btn') > -1 || trg1.className.indexOf('win-close-button') > -1)) {
		var trg = trg1.parentNode;
		while(trg.parentNode.className.indexOf('win') > -1) trg = trg.parentNode;
		
		return html_onmouse({type:'winclose', target: trg});
	}
	
	// .win-minimize-btn, .win-minimize-button
	if(ev.type == 'mouseup' && (trg1.className.indexOf('win-minimize-btn') > -1 || trg1.className.indexOf('win-minimize-button') > -1)) {
		var trg = trg1.parentNode;
		while(trg.parentNode.className.indexOf('win') > -1) trg = trg.parentNode;
		trg.style.display = "none";
	}

	// [wincreate]
	if(ev.type == 'wincreate') {
		var html = [ev.innerHTML||''];
		for(var i = 0; i < (ev.header_rows||[]).length; i++) {
			if(ev.header_rows[i].match(/[" ]?win-row[" ]/))
				html.push('<div class="win-row-wrp type_caption g-draggable-parent">'+ev.header_rows[i]+'</div>');
			else
				html.push('<div class="win-row-wrp type_caption g-draggable-parent"><div class="win-row g-draggable-parent2">'+ev.header_rows[i]+'</div></div>');
		}
		
		for(var i = 0; i < (ev.rows||[]).length; i++) {
			if(ev.rows[i].match(/[" ]?win-row-wrp[" ]/))
				html.push(ev.rows[i]);
			else if(ev.rows[i].match(/[" ]?win-row[" ]/))
				html.push('<div class="win-row-wrp">'+ev.rows[i]+'</div>');
			else
				html.push('<div class="win-row-wrp"><div class="win-row">'+ev.rows[i]+'</div></div>');
		}
		
		for(var i = 0; i < (ev.rows_footer||[]).length; i++) {
			if(ev.rows_footer[i].match(/[" ]?win-row[" ]/))
				html.push('<div class="win-row-wrp type_footer">'+ev.rows_footer[i]+'</div>');
			else
				html.push('<div class="win-row-wrp type_footer"><div class="win-row">'+ev.rows_footer[i]+'</div></div>');
		}
		
		if((ev.rows||[]).length == 0)
			html[html.length-1] = html[html.length-1].replace(/win-row-wrp/, 'win-row-wrp no_body');
			
		
		if((ev['resizer']||ev['resize']||ev['resizable']) != false)
			html.push('<div class="win-resizer"></div>');

		var win = document.createElement('DIV');
		win.id = ev.id||('win'+(new Date)*1);
		win.style.display = 'none';
		win.className = 'win g-draggable';
		win.innerHTML = html.join('');
		
		if(ev.attributes)
		for(var attr_name in ev.attributes) {
			win.setAttribute(attr_name, ev.attributes[attr_name]);
		}
		
		if('width' in ev) win.style.width = ev.width + 'px';
		if('height' in ev) win.style.height = ev.height + 'px';
		
		document.body.appendChild(win);
	}
	
	// [winshow]
	if(ev.type == 'winshow') {
		if(trg1.style.display != 'block')
			trg1.style.display = 'block';
		
		if(!ev['position'] || ev.position == 'center' || ev.position == 'center center') {
			trg1.style.left = (window.innerWidth - trg1.offsetWidth) / 2 + 'px';
			trg1.style.top = (window.innerHeight - trg1.offsetHeight) / 2 + 'px';
		}
		
		if(ev['with_background']) {
			var cover = document.getElementById('win_backcover') || document.createElement('DIV');
			cover.style.cssText = 'height: 100%;left:0;position:fixed;top:0;width:100%;z-index: 98;background:'+(typeof ev['with_background'] == 'boolean' ? 'rgba(0, 0, 0, 0.7)' : ev['with_background']);
			
			if( ! cover.id) {
				cover.id = 'win_backcover';
				document.body.appendChild(cover);
			}
		}
	}
	
	// [winremove]
	if(' windestroy windelete winremove winclose'.indexOf(ev.type) > -1 && trg1.className.indexOf('win') > -1) {
		trg1.style.display = 'none';
		trg1p.removeChild(trg1);
		
		var win_backcover = document.getElementById('win_backcover')
		if(win_backcover)
			win_backcover.parentNode.removeChild(win_backcover);
	}
		
	// отклуючаем любые перетаскивания и включаем обратно выделения
	if(ev.type == 'mouseup' || ev.type == 'mouseout-window') {
		for(var f in html_onmouse.cursor)
			if(f.indexOf('dragging_') === 0)
				delete html_onmouse.cursor[f];
			
//		document.ondragstart = null;
		document.body.onselectstart = null; // IE8
	}
	
	// расширения
	for (var i in html_onmouse.extensions)
	if(html_onmouse.extensions[i] instanceof Function)
		html_onmouse.extensions[i](ev, trg1, trg1p, trg1p.parentNode, find_near);
}

/* install html_onmouse events (if first load) */
if(!html_onmouse.extensions) {
	html_onmouse.extensions = [];
	html_onmouse.cursor = {};
	
	document.documentElement.addEventListener("mousedown", html_onmouse);
	document.documentElement.addEventListener("mousemove", html_onmouse);
	document.documentElement.addEventListener("mouseup", html_onmouse);
	document.documentElement.addEventListener("mouseout", html_onmouse);
	document.documentElement.addEventListener("mouseover", html_onmouse);
}

/* 
 * LittleLisp.js IDE
 * 
 * Version: 1.0
 * License: MIT
 * 
 *  Copyright (c) 2020 Saemon Zixel <saemonzixel@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy of this software *  and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

function littlelisp_ide_onclick(event) {
	var ev = event;
	var trg1 = arguments[1] || document.head.parentNode;
	var trg1p = arguments[2] || document.head.parentNode;
	var trg1pp = arguments[3] || document.head.parentNode;
	var find_near = (html_onclick||{}).find_near || function(){ alert("Error! html_onclick.find_near not found!"); };

	// c-object_props
	if(trg1.className.indexOf('c-object_props') > -1 || trg1p.className.indexOf('c-object_props') > -1) {
		// откорректируем trg1 (иконка в кнопке например)
		trg1 = trg1.className.indexOf('c-object_props') > -1 ? trg1 : trg1p;
		
		// CSS внедрим стили виджита если нет
		if(ev.type == 'load') {
			var style_tag = document.getElementById("c_object_props_css");
			if(!style_tag) {
				style_tag = document.createElement("STYLE");
				style_tag.id = "c_object_props_css";
				style_tag.type="text/css";
				style_tag.appendChild(document.createTextNode(
				".c-object_props { position: relative; }"+
				".c-object_props-cols { width: 100%; height: 100%; overflow-y: auto; overflow-x: hidden; display: inline-block; vertical-align: top; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; border-top: 32px solid #fff; position: relative; border-bottom: 1px solid silver; }"+
				".c-object_props-cols.mode_without_toolbar { border-top: none; }"+
				/*         .c-object_props-col1 { width: 20%; min-height: 100%; display: inline-block; border: 1px solid silver; vertical-align: top; background: #EEE; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; } */
				".c-object_props-col1 { position: absolute; left: 0px; top: 0px; width: 20%; min-height: 100%; display: block; border: 1px solid silver; vertical-align: top; background: #EEE; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; }"+
				/*         .c-object_props-col2 { width: 79.9%; min-height: 100%; display: inline-block; border: 1px solid silver; border-left: none; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; } */
				".c-object_props-col2 { position: absolute; left: 20%; top: 0px; right: 0px; min-height: 100%; display: block; border: 1px solid silver; border-left: none; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; }"+
				".c-object_props-key, .c-object_props-value { border-bottom: 1px solid silver; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; padding: 1px 3px; cursor: default; text-overflow: ellipsis; white-space: nowrap; overflow: hidden; }"+
				".c-object_props-value:empty { min-height: 1.4em }"+
				".c-object_props-value { white-space: nowrap; overflow: hidden; cursor: text; }"+
				".c-object_props-key.state_active { background: silver; }"+
				".c-object_props-key.mod_error, .c-object_props-value.mod_error { background-color: #f2dede }"+
				".c-object_props-editor { display: none; position: absolute; width: 79.9%; height: 100%; left: 20%; top: 0; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; border-top: 32px solid white; }"+
				".c-object_props-cols.mode_without_toolbar + .c-object_props-editor { border-top: 0; }"+
				".c-object_props-editor-textarea { height: 100%; width: 100%; max-width: 100%; max-height: 100%; border: 1px solid silver; border-left: 0px solid silver; background: white; margin: 0; border-radius: 0px; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; overflow-y: scroll; }"+
				".c-object_props.mod_one_prop > .c-object_props-editor { display: block; }"+
				".c-object_props.mod_one_prop > .c-object_props-cols { width: 20%; }"+
				".c-object_props.mod_one_prop > .c-object_props-cols > .c-object_props-col1 { width: 100%; }"+
				".c-object_props.mod_one_prop > .c-object_props-cols > .c-object_props-col2 { display: none; }"+
				".c-object_props-toolbar { position: absolute; top: 0; left: 0; right: 1px; border: 1px solid #ddd; }"+
				".c-object_props-toolbar-btn { display: inline-block; vertical-align: top; height: 22px; margin: 1px; border: none; background: transparent; }"+
				".c-object_props-toolbar-btn:hover { background: silver; }"+
				".c-object_props-toolbar-btn > i { background: transparent no-repeat 50% 50%; display: inline-block; width: 20px; height: 20px; vertical-align: bottom; }"+
				".c-object_props-toolbar-divider { width: 1px; margin: 1px 1px; overflow: hidden; background-color: #e5e5e5; height: 20px; display: inline-block; vertical-align: middle; }"+
				".c-object_props-editor-btns { position: absolute; right: 2px; bottom: 2px; }"+
				".c-object_props-editor-btn { display: inline-block; }"+
				".c-object_props-btn_back { width: 100%; height: 100%; text-align: left; border-bottom: 1px solid silver; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box; padding: 1px 3px; cursor: default; text-overflow: ellipsis; white-space: nowrap; overflow: hidden; user-select: none; -webkit-user-select: none; -moz-user-select: none; }"+
				".c-object_props-link { color: -webkit-text; }"));
				document.querySelector("head").insertBefore(style_tag, document.querySelector("head > style"));
			}
		}
		
		// [load]
		if(ev.type == 'load') {
			var root = find_near('c-object_props');
			
			// создадим историю состояний если нет
			if('states' in root == false) {
				root.states = [];
			}
			
			// ...иначе обновим в ней текущее стостояние
			else {
				root.states[0].scrollTop = find_near('c-object_props-cols').scrollTop;
				for(var curr = find_near('c-object_props-col1').firstElementChild; curr; curr = curr.nextElementSibling)
					if(curr.className.indexOf('state_active') > -1) {
						root.states[0].selected_key = curr.innerHTML;
						break;
					}
			}
			
			// загрузим уже существующее состояние
			if("state_to_load" in ev) {
				var obj = ev.state_to_load.object;
			} 
			
			// загрузим новое состояние и новый объект в историю
			else { 
				if("key_to_load" in ev)
					var obj = root.states[0].object[ev.key_to_load];
				else {
					var obj = ev.object_to_load;
					root.states = [];
				}
				
				root.states.unshift({
					object: obj, 
					selected_key: undefined, 
					scrollTop: 0});
				
				delete root.previous_states; // удалим состояния для кнопки "Next"
			}

			var col1_html = [], col2_html = [], row_class = '';
			
			// кнопка назад и путь
			if(root.states.length > 1) {
				var path = [];
				
				var root_obj = root.states[root.states.length-1].object;
				path.push('<a class="c-object_props-link cmd_object_props_prev_state" href="javascript:void('+(root.states.length-2)+')" data-state-num="'+(root.states.length-2)+'">' + (root_obj.toString().indexOf("[object ") === 0 ? root_obj.toString() : ({}).toString.apply(root_obj)) + '</a>');
				
				for(var i = root.states.length-1; i > 0; i--) {
					var num = i - 2;
					path.push('.<a class="c-object_props-link cmd_object_props_prev_state" href="javascript:void('+num+')" data-state-num="'+num+'">' + root.states[i].selected_key + '</a>');
				}
			
				col1_html.push('<div class="c-object_props-btn_back">&larr;..&nbsp;</div>');
				col2_html.push('<div class="c-object_props-value">'+path.join("")+'</div>');
			}
			
			// пригодится для вывода содежимого объектов
			function json_enc(obj, max_deep) {
				switch(typeof obj) {
					case "object":
						if(obj == null) return "null";
						
						if((max_deep == undefined ? 1 : max_deep) < 1) 
							return ({}).toString.apply(obj);
						
						var json = [];
						if(obj instanceof Array) {
							for(var i = 0; i < obj.length; i++)
								json.push(json_enc(obj[i], max_deep-1));
							return '['+json.join(',')+']';
						}
						for(var fname in obj) {
							json.push(fname+": "+json_enc(obj[fname], max_deep-1));
							if (json.length > 15) return JSON.stringify(obj);
						}
						return "{"+json.join(', ')+"}";
					case "number":
						return obj.toString();
					case "boolean":
						return obj ? "true" : "false";
					case "undefined":
						return "undefined";
					default:
						return '"'+String(obj)+'"';
				}
			}
			
			// сгенерируем HTML со содержимым объекта
			var selected_key = ('state_to_load' in ev) ? ev.state_to_load.selected_key : '';
			for(var f in obj)
			if(col1_html.length > (root.getAttribute("data-rows-limit")||100)) {
				col1_html.push('<div class="c-object_props-key">&nbsp;</div>');
				col2_html.push('<div class="c-object_props-value">Limit '+(root.getAttribute("data-rows-limit")||100)+' rows excedded!...</div>');
				break;
			}
			else {
				row_class = '';
				
				switch(typeof obj[f]) {
					case "undefined":
						col2_html.push('<div class="c-object_props-value type_undefined" data-key="'+f+'" title="'+(({}).toString.call(obj[f]))+'">&nbsp;</div>');
						break;
					case "string":
						col2_html.push('<div class="c-object_props-value type_string" data-key="'+f+'" title="'+(({}).toString.call(obj[f]))+'">'+obj[f].replace(/</g, '&lt;').replace(/^ $/, "&nbsp;")+'&nbsp;</div>');
						break;
					case "function":
						col2_html.push('<div class="c-object_props-value type_func_src" data-key="'+f+'" title="'+(({}).toString.call(obj[f]))+'">'+obj[f].toString().replace(/</g, '&lt;')+'</div>');
						break;
					default:
						try {
							col2_html.push('<div class="c-object_props-value" data-key="'+f+'" title="'+(({}).toString.call(obj[f]))+'">' +
							json_enc(obj[f],1).replace(/</g, '&lt;') + '</div>');
						} catch(ex) {
							col2_html.push('<div class="c-object_props-value" data-key="'+f+'" title="!!! Error: '+ex.message+' !!!">'+(({}).toString.call(obj[f]))+'</div>');
							row_class = ' mod_error';
						}
				}
				
				col1_html.push('<div class="c-object_props-key '+(/*selected_key==f?'state_active':*/'')+row_class+'">'+f+'</div>');
			}
			
			var html = ['<div class="c-object_props-cols '+(ev.show_toolbar?'':'mode_without_toolbar')+'"><div class="c-object_props-col1">',
				col1_html.join('\n'),
				'</div><div class="c-object_props-col2">',
				col2_html.join('\n'),
				'</div></div>',
				'<div class="c-object_props-editor"><textarea class="c-object_props-editor-textarea" onkeyup="console.log(event); if(event.keyCode == 27) html_onclick({type:\'click\',target:this.parentNode.querySelector(\'.cmd_object_props_discard_changes\')})"></textarea><div class="c-object_props-editor-btns"><button type="button" class="c-object_props-editor-btn cmd_object_props_apply_changes">Apply</button><button type="button" class="c-object_props-editor-btn cmd_object_props_discard_changes">Cancel</button></div></div>'];
			
			/* if(ev.show_toolbar)
				html.push('<div class="c-object_props-toolbar">'+
				'<button class="c-object_props-toolbar-btn cmd_object_props_prev_state"><i class="ico-prev"></i></button>'+
				'<button class="c-object_props-toolbar-btn cmd_object_props_next_state"><i class="ico-next"></i></button>'+
				'<span class="c-object_props-toolbar-divider"></span>'+
				'<button class="c-object_props-toolbar-btn cmd_object_props_apply_changes"><i class="ico-apply"></i>Apply</button>'+
				'<button class="c-object_props-toolbar-btn cmd_object_props_discard_changes"><i class="ico-discard"></i>Discard</button>'+
				'</div>'
				); */
			
			root.innerHTML = html.join('');
			
			// прокрутим список как раньше было
			if('state_to_load' in ev) {
				find_near('c-object_props-cols').scrollTop = ev.state_to_load.scrollTop || 0;
			} 
			
			// уберём редактор
			root.className = root.className.replace(/ *mod_one_prop/, '');
			
			// повесим обработчик 2ного клика
			root.ondblclick = html_onclick;
			
			// onchange
			root.onchange = ev.onchange;
		}
		
		// [dblclick] .c-object_props-key/-value
		if(ev.type == 'dblclick' && trg1.className.match(/c-object_props-key|c-object_props-value/)) {
			var root = find_near('c-object_props');
			
			// не трогаем пустые строки (limit...)
			if(trg1.innerHTML.match(/^(&nbsp;| *)$/)) return;
			
			// сохраним название поля в которое опускаемся
			root.states[0].selected_key = trg1.innerHTML;
			
			html_onclick({
				type: "load", 
				target: root, 
				show_toolbar: find_near("c-object_props-toolbar"),
				key_to_load: trg1.innerHTML});
		}
		
		// [click] .c-object_props-value
		if(ev.type == 'click' && trg1.className.match(/c-object_props-value/)) {
			
			// если выделили мышкой текст
			if(window.getSelection().toString() != "")
				return;
			
			// если кликнули по c-object_props-value то переключимся на соответствующий c-object_props-key
			if(trg1.className.indexOf("c-object_props-value") > -1) {
				for(var pos = 0; trg1.previousElementSibling; pos++)
					trg1 = trg1.previousElementSibling;
				trg1 = trg1.parentNode.previousElementSibling.firstElementChild;
				for(;pos > 0 && trg1.nextElementSibling; pos--) 
					trg1 = trg1.nextElementSibling;
			}
			
			for(var curr = trg1.parentNode.firstElementChild; curr; curr = curr.nextElementSibling) {
				if(curr.className.indexOf(' state_active') > -1) {
					// уберём активность
					curr.className = curr.className.replace(/ *state_active/, '');
					
					// если кликнули на активный элемент, уберём редактор и вернёмся в режим списка
					if(curr == trg1) {
						var root = find_near('c-object_props', {previous_states:[], current_state:0});
						root.className = root.className.replace(/ *mod_one_prop/, '');
						continue;
					}
				}

				if(curr == trg1) {
					curr.className += ' state_active';
					
					// загрузим текущее значение в редактор
					var editor = find_near('c-object_props-editor-textarea');
					var root = trg1.parentNode.parentNode.parentNode;
					var state = root.states[0];
					var obj = state.object;
					
					state.selected_key = curr.innerHTML;
					
					// загрузим на редактирование в зависимости от типа
					editor.className = editor.className.replace(/ *type_[-a-z0-9_]+/, '');
					switch(typeof obj[curr.innerHTML]) {
						case 'undefined':
							editor.value = 'undefined';
							editor.className += ' type_undefined';
							break;
						case 'string':
							editor.value = obj[curr.innerHTML];
							editor.className += ' type_string';
							break;
						case 'function':
							editor.value = obj[curr.innerHTML].toString();
							editor.className += ' type_func_src';
							break;
						case 'boolean':
							editor.value = JSON.stringify(obj[curr.innerHTML]);
							editor.className += ' type_boolean';
							break;
						case 'object':
							if(obj[curr.innerHTML] === null) {
								editor.value = JSON.stringify(obj[curr.innerHTML]);
								editor.className += ' type_null';
								break;
							}
							if('length' in obj[curr.innerHTML]) {
								editor.value = JSON.stringify(obj[curr.innerHTML]);
								editor.className += ' type_array';
								break;
							}
						default:
							editor.value = JSON.stringify(obj[curr.innerHTML]);
							editor.className += ' type_'+(typeof obj[curr.innerHTML]);
					}
					
					// покажем редактор
					root.className = root.className.replace(/ *mod_one_prop/, '') + ' mod_one_prop';
					
					// поставим фокус в него
					editor.focus();
				}
			}
			
		}
		
		// [click] .cmd_object_props_prev_state
		// [dblclick] .c-object_props-btn_back
		// TODO remove state_active
		if(ev.type == 'click' && trg1.className.indexOf('cmd_object_props_prev_state') > -1
		|| (ev.type == 'dblclick' && trg1.className.indexOf('c-object_props-btn_back') > -1)) {
			var root = find_near('c-object_props');
			
			// история состояний пуста
			if((root.states||[]).length == 0) return;
			
			// более предыдущего состояния нет
			if(!root.states[1]) return;
			
			// переключимся на запрошенное состояние по порядковому номеру
			for(var num = trg1.getAttribute("data-state-num") || 0; 
				num >= 0; num--) {
				
				// перенесём текущее состояние вы специальный список для кнопки "Next"
				if('previous_states' in root == false) 
					root.previous_states = [];
				root.previous_states.unshift(root.states.shift());
				
				if(num == 0)
					html_onclick({type: 'load', target: root, state_to_load: root.states[0]});
			}
		}
		
		// [click] .cmd_object_props_next_state
		if(ev.type == 'click' && trg1.className.indexOf('cmd_object_props_next_state') > -1) {
			var root = find_near('c-object_props');
			
			if('previous_states' in root == false) return;
			if( ! root.previous_states[0]) return;
			
			html_onclick({type: 'load', target: root, state_to_load: root.previous_states[0]});
			
			// перенесём в стек текущих состояний загруженное состояние
			root.states.unshift(root.previous_states.shift());
		}
		
		// [click] .cmd_object_props_apply_changes
		if(ev.type == 'click' && trg1.className.indexOf('cmd_object_props_apply_changes') > -1) {
			var root = find_near('c-object_props');
			
			if(root.className.indexOf('mod_one_prop') < 0) return;
			
			// так-же надо будет подправить значение в списке
			var list_item = {};
			for(var curr = find_near('c-object_props-col2').firstElementChild; curr; curr = curr.nextElementSibling)
				if(curr.getAttribute('data-key') == root.states[0].selected_key) {
					list_item = curr;
					break;
				}
			
			// запомним старое значение
			var old_value = root.states[0].object[root.states[0].selected_key];
			
			// оповестим об изменении значения в поле
			var editor = find_near('c-object_props-editor-textarea');
			html_onclick({
				type:'change', 
				target: root,
				editor: editor,
				object: root.states[0].object,
				key: root.states[0].selected_key, 
				value: editor.value, 
				value_type: editor.className.match(/type_[a-z_A-Z0-9]+/)[0],
				value_old: old_value});

			// отобразим новое значение
			switch(editor.className.match(/type_[a-z_A-Z0-9]+/)[0]) {
				case 'type_string':
// 					root.states[0].object[root.states[0].selected_key] = editor.value;
					list_item.innerHTML = editor.value;
					break;
				case 'type_undefined':
					if(editor.value == 'undefined') {
// 						root.states[0].object[root.states[0].selected_key] = undefined;
						list_item.innerHTML = editor.value;
						break;
					}
				case 'type_func_src':
				default:
// 					root.states[0].object[root.states[0].selected_key] = eval('('+editor.value+')');
					list_item.innerHTML = editor.value;
					break;
			}
			
			// скроем редактор
			root.className = root.className.replace(/ *mod_one_prop/, "");
			
			// уберём state_active
			var list_item = root.querySelector(".c-object_props-key.state_active")||{className:""};
			list_item.className = list_item.className.replace(/ *state_active/," ");
			root.states[0].selected_key = undefined;
		}
		
		// [click] .cmd_object_props_discard_changes
		if(ev.type == 'click' && trg1.className.indexOf('cmd_object_props_discard_changes') > -1) {
			var root = find_near('c-object_props');
			
			if(root.className.indexOf('mod_one_prop') < 0) return;
			root.className = root.className.replace(/ *mod_one_prop/, "");
			
			// уберём state_active
			var list_item = root.querySelector(".c-object_props-key.state_active")||{className:""};
			list_item.className = list_item.className.replace(/ *state_active/," ");
			root.states[0].selected_key = undefined;
			
/*			html_onclick({
				type: "click",
				target: root.querySelector(".c-object_props-key.state_active")
			});*/
		}
		
	}

	// cmd_code_editor_save
	// TODO target not my
	if(ev.type == "click" && (trg1.className.indexOf('cmd_code_editor_save') > -1 || trg1p.className.indexOf('cmd_code_editor_save') > -1)) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var win = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = win.getAttribute('data-file_name') || document.getElementById('loacalStorage_files').value;
		var new_code = win.value || win.innerHTML;
		
		// сохранение файла в главном окне
		if(win_id == "littlelisp_ide_main_window") {
			if(file_name == "") { 
				alert("File not selected!");
				file_name = prompt("New filename:", "");
				if (!file_name) return;
				
				// проверим, чтоб имя не совпало с уже существующем файлом
				if ("littlelisp_file_"+file_name in localStorage 
				&& confirm("File "+file_name+" already exists!\nOverwrite?") == false)
					return;
			}
			
			html_onclick({type: "save_file", file_name: file_name, file_content: new_code, target: trg1});
			
			return;
		}
		
		// сохранение в дебагере (если это текст всего файла)
		if(win_id.match(/^littlelisp_debugger_/) && win.getAttribute('data-file_name')) {
			
			html_onclick({type: "save_file", file_name: file_name, file_content: new_code, target: trg1});
			
			// TODO restart debugger?
			return;
		}
		
		/* если сохранение в дебагере или в классброузере */
		var func_name = (win.getAttribute("data-full-name")||"").replace(/^window\./, "");
		
		// новый класс или метод?
		if(!func_name) {
			var func_name = new_code.match(/[ \t\n]*\([ \t\n]*def[^ \t\n]+[ \t\n]+([a-zA-Z_0-9.]+)/);
			if (!func_name) return alert("Error! Can`t save! (unknown declaration)");
			func_name = func_name[1];
		}
		
		// если не указано название файла, то запросим его ввести
		if(file_name == "") { 
			alert("File not selected!");
			file_name = prompt("New filename:", "");
			if (!file_name) return;
			
			// если файл новый, то заменим содержимое и всё
			if ("littlelisp_file_"+file_name in localStorage == false) {
				html_onclick({type: "save_file", file_name: file_name, file_content: new_code, target: trg1});
				return;
			}
		}
		
		// сформируем регулярку поиска старой записи в тексте
		if (func_name.indexOf(".prototype.") > -1)
			var regexp_func_decl = new RegExp("\\([ \t\n]*(defmeth[ \t\n]+"+func_name.replace(".prototype.",".")+"|defun[ \t\n]+"+func_name+")[ \t\n]+");
		else if (new_code.match(/[ \t\n]*\([ \t\n]*defclass/))
			var regexp_func_decl = new RegExp("\\([ \t\n]*defclass[ \t\n]+"+func_name+"[ \t\n]+");
		else
			var regexp_func_decl = new RegExp("\\([ \t\n]*(defmeth-static[ \t\n]+"+func_name+"|defun[ \t\n]+"+func_name+")[ \t\n]+");
		
		// возмём содержимое файла и найдём обьявление старого метода/класа
		var file_content = localStorage.getItem('littlelisp_file_'+file_name);
		var entry_start = file_content.search(regexp_func_decl);
		if(entry_start < 0) return alert("Error! Declaration for "+func_name+" not found in file "+file_name);
		
		// поищем всё обьявление
		var parse_result = littleLisp.parse(file_content, entry_start);
		var start_len = parse_result[parse_result.length-1];
		
		// сгенерируем новое содержимое файла
		var new_file_content = file_content.substring(0, start_len >> 16) + new_code + file_content.substring((start_len >> 16) + (start_len & 65535));
		
		html_onclick({type: "save_file", file_name: file_name, file_content: new_file_content, target: trg1});
		
		// TODO eval in class_browser?
		
		return;
	}

	// cmd_code_editor_run
	if(ev.type == "click" && (trg1.className.indexOf('cmd_code_editor_run') > -1 || trg1p.className.indexOf('cmd_code_editor_run') > -1)) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var littlelisp_ide_main_window = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = littlelisp_ide_main_window.getAttribute('data-file_name') || document.getElementById('loacalStorage_files').value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		
		if(littlelisp_ide_main_window.selectionStart == littlelisp_ide_main_window.selectionEnd) 
			var src = littlelisp_ide_main_window.value;
		else
			var src = littlelisp_ide_main_window.value.substring(littlelisp_ide_main_window.selectionStart, littlelisp_ide_main_window.selectionEnd);
		

		try {
			// если в дебагере, то на основе текущего контекста создаём новый дебагер
			if(win_id.match(/^littlelisp_debugger_/))
				var littlelisp_debugger = littleLisp.debug(src, file_name /* +"-debug"+littlelisp_ide_onclick.debugger_win_next_num */, document.getElementById(win_id).littlelisp_debugger.ctx);
			
			// иначе в глобальной области видимости
			else 
				var littlelisp_debugger = littleLisp.debug(src, file_name);
		} 
		catch(ex) {
			console.error(ex);
			alert(ex.stack);
			return;
		}
		
		// запустим
		try {
			var result = littlelisp_debugger.continue();
		} 
		catch(ex) {
			if(ex != "Breakpoint!") {
				alert(ex.stack);
				console.info(ex.stack);
			}
			
			var button = document.createElement('BUTTON');
			button.className = "cmd_code_editor_debugger";
			button.innerHTML = "<i></i>Debugger #"+(littlelisp_ide_onclick.debugger_win_next_num++);
			button.littlelisp_debugger = littlelisp_debugger;
			button.ex = ex;
		
			if(document.getElementById("littlelisp_ide_panel")) {
				document.getElementById("littlelisp_ide_panel").insertBefore(button, document.getElementById("littlelisp_ide_panel").firstElementChild);
			}
			else {
				button.className = "c-panel-btn cmd_code_editor_debugger";
				document.querySelector("#littlelisp_ide_main_window .c-toolbar").appendChild(button);
			}
		
			button.click();
		
			return;
		}
		
		try {
			alert("RESULT: " + (result instanceof Function 
				? result : JSON.stringify(result)))
		} catch(ex) {
			alert("RESULT: " + result);
		}
		return;
	}
	
	// cmd_code_editor_start_debug
	if(trg1.className.indexOf('cmd_code_editor_start_debug') > -1 || trg1p.className.indexOf('cmd_code_editor_start_debug') > -1) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var editor = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = editor.getAttribute('data-file_name') || document.getElementById('loacalStorage_files').value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		var src = editor.selectionStart == editor.selectionEnd 
			? editor.value 
			: editor.value.substring(editor.selectionStart, editor.selectionEnd);
		
		// если в дебагере, то на основе текущего контекста создаём новый дебагер
		if(win_id.match(/^littlelisp_debugger_/))
			var littlelisp_debugger = littleLisp.debug(src, file_name /* +"-debug"+littlelisp_ide_onclick.debugger_win_next_num */, document.getElementById(win_id).littlelisp_debugger.ctx);
		
		// иначе в глобальной области видимости
		else 
			var littlelisp_debugger = littleLisp.debug(src, file_name);
		
		var button = document.createElement('BUTTON');
		button.className = "cmd_code_editor_debugger";
		button.innerHTML = "<i></i>Debugger #"+(littlelisp_ide_onclick.debugger_win_next_num++);
		button.littlelisp_debugger = littlelisp_debugger;
		
		if(document.getElementById("littlelisp_ide_panel")) {
			document.getElementById("littlelisp_ide_panel").insertBefore(button, document.getElementById("littlelisp_ide_panel").firstElementChild);
		}
		else {
			button.className = "c-panel-btn cmd_code_editor_debugger";
			document.querySelector("#littlelisp_ide_main_window .c-toolbar").appendChild(button);
		}
		
		button.click();
		
		return;
	}
	
	// cmd_code_editor_debugger
	if(ev.type == "click" && trg1.className.indexOf('cmd_code_editor_debugger') > -1 || trg1p.className.indexOf('cmd_code_editor_debugger') > -1) {
		var button = trg1p.className.indexOf('cmd_code_editor_debugger') > -1 ? trg1p : trg1;
		var id = button.innerHTML.replace(/.+?#/, '');
		var win = document.getElementById("littlelisp_debugger_"+id);
		if(!win) {
			html_onclick({
			type: "show_debugger_win",
			target: button});
		}
		else if(win.style.display == "none") {
			win.style.display = "block";
			win.querySelector(".c-code_editor-textarea").focus();
		}
		else 
			win.style.display = "none";
	}
	
	// cmd_code_editor_continue
	if(ev.type == "click" && (trg1.className.indexOf('cmd_code_editor_continue') > -1 || trg1p.className.indexOf('cmd_code_editor_continue') > -1)) {
	
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		
		var littlelisp_debugger = document.getElementById(win_id).littlelisp_debugger;
		
		try {
			littlelisp_debugger.continue(true);
		} catch(ex) {
			if(ex != "Breakpoint!") {
				alert(ex + "\n" + ex.stack);
				console.info(ex.stack);
			}
		}
		
		// выделяем фрагмент кода текущий
		var littlelisp_ide_main_window = document.getElementById(win_id+'_code_editor');
		littlelisp_ide_main_window.setSelectionRange(
			littlelisp_debugger.ctx.range(1), 
			littlelisp_debugger.ctx.range(2));
		
		html_onclick({
			type: "refresh_dbg_debugger",
			target: document.getElementById(win_id)});
	}
	
	// cmd_code_editor_restart
	if(trg1.className.indexOf('cmd_code_editor_restart') > -1 || trg1p.className.indexOf('cmd_code_editor_restart') > -1) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var editor = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = editor.getAttribute('data-file_name') || document.getElementById('loacalStorage_files').value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		var littlelisp_debugger = document.getElementById(win_id).littlelisp_debugger;
		
		// пересоздаём контекст заново на основе текущего текста/исходника
		// TODO restart only function context
		littlelisp_debugger.ctx = new littleLisp.Context(window, littlelisp_debugger.ctx.parent, littleLisp.parse(editor.value), undefined, editor.value, file_name, littlelisp_debugger.ctx.type);
		
		html_onclick({
			type: "refresh_dbg_debugger",
			target: document.getElementById(win_id)});
	}
	
	// cmd_code_editor_step_in/over/out
	if(trg1.className.indexOf('cmd_code_editor_step_in') > -1 || trg1p.className.indexOf('cmd_code_editor_step_in') > -1 || trg1.className.indexOf('cmd_code_editor_step_over') > -1 || trg1p.className.indexOf('cmd_code_editor_step_over') > -1 || trg1.className.indexOf('cmd_code_editor_step_out') > -1 || trg1p.className.indexOf('cmd_code_editor_step_out') > -1) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		
		var littlelisp_debugger = document.getElementById(win_id).littlelisp_debugger;
		
		// переключим дебагер на выбранный контекст в callstack
		var num = 0;
		var selected_ctx = littlelisp_debugger.ctx;
		var call_stack_select = document.getElementById(win_id+'_callstack').firstElementChild;
		for(var curr_ctx = littlelisp_debugger.ctx; curr_ctx; curr_ctx = curr_ctx.parent) {
			if (num++ == call_stack_select.selectedIndex) { 
				littlelisp_debugger.ctx = curr_ctx;
				break;
			}
		}
		
		// сделаем шаг
		try {
		if(trg1.className.indexOf('cmd_code_editor_step_over') > -1 || trg1p.className.indexOf('cmd_code_editor_step_over') > -1)
			littlelisp_debugger.step_over();
		else if(trg1.className.indexOf('cmd_code_editor_step_out') > -1 || trg1p.className.indexOf('cmd_code_editor_step_out') > -1)
			littlelisp_debugger.step_out();
		else 
			littlelisp_debugger.step_in();
		} catch(ex) {
			console.error(ex.stack || ex);
			alert(ex.stack || ex);
		}
		
		// выделяем фрагмент кода текущий
		var littlelisp_ide_main_window = document.getElementById(win_id+'_code_editor');
		littlelisp_ide_main_window.setSelectionRange(
			littlelisp_debugger.ctx.range(1), 
			littlelisp_debugger.ctx.range(2));
		
		html_onclick({
			type: "refresh_dbg_debugger",
			target: document.getElementById(win_id)});
	}

	// cmd_code_editor_set_breakpoint
	if(trg1.className.indexOf('cmd_code_editor_set_breakpoint') > -1 || trg1p.className.indexOf('cmd_code_editor_set_breakpoint') > -1) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var littlelisp_ide_main_window = document.getElementById(win_id).querySelector('textarea.c-code_editor-textarea');
		var select1 = document.getElementById('loacalStorage_files');
		var file_name = littlelisp_ide_main_window.getAttribute('data-file_name') || select1.value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		
		if(file_name in littleLisp.breakpoints == false)
			littleLisp.breakpoints[file_name] = {};
		
		var atom_alphabet = 
		"qwfpgjluyarstdhneiozxcvbkmQWFPGJLUYARSTDHNEIOZXCVBKM" + // Latin
		"+*/%<>=!|&" + // arithmetic + logic
		":@'`," + // symbol chars + quote + backquote
		"яжфпгйлуыюшщарстдхнеиоэзчцвбкмъьЯЖФПГЙЛУЫЮШЩАРСТДХНЕИОЭЗЧЦВБКМЬЪ" + // Cyrillic
		"_.?"+
		"1234567890"; // additional atom chars
		
		// TODO String?
		// вычеслим начало токена
		for(var tok_start = littlelisp_ide_main_window.selectionStart; tok_start >= 0; tok_start--)
			if(atom_alphabet.indexOf(littlelisp_ide_main_window.value[tok_start]) > -1) continue;
			else { tok_start++; break; }
			
		// и конец токена
		for(var tok_end = littlelisp_ide_main_window.selectionStart; tok_end < littlelisp_ide_main_window.value.length; tok_end++)
			if(atom_alphabet.indexOf(littlelisp_ide_main_window.value[tok_end]) > -1) continue;
			else break;
		
		var token = (tok_start << 16) + (tok_end-tok_start);
		
		// если breapoint уже такой есть, то уберём его
		if (token in littleLisp.breakpoints[file_name])
			delete littleLisp.breakpoints[file_name][token];
		
		// иначе на него всего и повесим breakpoint
		else
			littleLisp.breakpoints[file_name][token] = true;
		
		// включим подсветку breakpoints
		littlelisp_ide_main_window.selectionEnd = littlelisp_ide_main_window.selectionStart;
		html_onmouse({type:"mouseover", target:trg1});
	}

	// cmd_code_editor_inspect_it
	if(trg1.className.indexOf('cmd_code_editor_inspect_it') > -1 || trg1p.className.indexOf('cmd_code_editor_inspect_it') > -1) {
	
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var editor = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = editor.getAttribute('data-file_name') || document.getElementById('loacalStorage_files').value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		
		if(editor.selectionStart == editor.selectionEnd) 
			var src = editor.value;
		else
			var src = editor.value.substring(editor.selectionStart, editor.selectionEnd);

			try {
				
			// если в дебагере, то на основе текущего контекста создаём новый дебагер
			if(win_id.match(/^littlelisp_debugger_/))
				var littlelisp_debugger = littleLisp.debug(src, file_name, document.getElementById(win_id).littlelisp_debugger.ctx);
			
			// иначе в глобальной области видимости
			else 
				var littlelisp_debugger = littleLisp.debug(src, file_name);
			
			} catch(ex) {
				alert(ex.stack);
				return;
			}
			
			try {
				var result = littlelisp_debugger.continue();
				
				// удалим дебагер из списка дебагеров
				for (var i in littleLisp.debuggers)
					if (littleLisp.debuggers[i] == littlelisp_debugger)
						delete littleLisp.debuggers[i];
				
			} catch(ex) {
				if(ex != "Breakpoint!") {
					alert(ex);
					console.info(ex.stack);
				}
				
				var button = document.createElement('BUTTON');
				button.className = "cmd_code_editor_debugger";
				button.innerHTML = "<i></i>Debugger #"+(littlelisp_ide_onclick.debugger_win_next_num++);
				button.littlelisp_debugger = littlelisp_debugger;
				button.ex = ex;
			
				if(document.getElementById("littlelisp_ide_panel")) {
					document.getElementById("littlelisp_ide_panel").insertBefore(button, document.getElementById("littlelisp_ide_panel").firstElementChild);
				}
				else {
					button.className = "c-panel-btn cmd_code_editor_debugger";
					find_near("c-toolbar").appendChild(button);
				}
			
				button.click();
			
				return;
// 			}
		}

// 				alert("RESULT: " + (result instanceof Function ? result : JSON.stringify(result)))
// 				return;
		
		for(var id = 1;;id++)
			if(document.getElementById("littlelisp_inspector_"+id)) continue;
			else break;
		
		var div = document.createElement("DIV");
		div.innerHTML = '<fieldset id="title_{id}" style="position:absolute;left:0;top:0;height:35px;width:460px;" class="win-header g-draggable-parent" data-win-relocations="right: win() right"><div class="win-header-title g-draggable-parent2">Inspect {obj_type}</div> <a class="win-close-btn" href="javascript:void(0)">x</a></fieldset>'+
		'<fieldset id="{id}_raw" class="win-fieldset" style="position:absolute;left:20px;top:60px;height:55px;width:420px" data-win-relocations="right: win() right;bottom:#{id}_splitter bottom;">'+
		'<textarea class="c-textarea" style="width:100%;height:100%;max-width:100%;max-height:100%;box-sizing:border-box" data-win-id="{id}"></textarea>'+
		'</fieldset>'+
		'<fieldset id="{id}_splitter" class="win-splitter type_horizontal" style="position:absolute;left:20px;top:115px;height:5px;width:420px;" data-win-relocations="right:win() right; top: win() bottom; bottom: win() bottom;"></fieldset>'+
		'<fieldset id="{id}_watch" class="win-fieldset" style="position:absolute;left:20px;top:120px;height:420px;width:420px" data-win-relocations="right: win() right; bottom: win() bottom; top:#{id}_splitter bottom;">'+
		'<div class="c-object_props" style="width:100%;height:100%;outl1ine:1px solid silver;" data-win-id="{id}"></div>'+
		'</fieldset>';
		var form = [];
		for(var i = 0; i < div.childNodes.length; i++) 
		if(div.childNodes[i].nodeName == "FIELDSET")
			form.push(div.childNodes[i].outerHTML
				.replace("{num}", id)
				.replace("{obj_type}", {}.toString.call(result))
				.replace("{id}", "littlelisp_inspector_"+id)
				.replace("{id}", "littlelisp_inspector_"+id)
				.replace("{id}", "littlelisp_inspector_"+id));
	
		var win = {
			type: 'wincreate',
			id: 'littlelisp_inspector_'+id,
			attributes: {},
			resizable: true,
			width: 460, height: 570,
			innerHTML: form.join('')};

		html_onmouse(win);
		
		// RAW значение
		try {
			document.querySelector("#littlelisp_inspector_"+id+"_raw > textarea").value = (result !== undefined ? result.toString() : "undefined");
		} catch(ex) {
			document.querySelector("#littlelisp_inspector_"+id+"_raw > textarea").value = ({}.toString).apply(result);
		}
		
		// загрузим контекст в watch
		html_onclick({
			type: 'load', 
			target: document.getElementById("littlelisp_inspector_"+id+'_watch').firstElementChild,
// 					show_toolbar: true,
			object_to_load: result,
			onchange: function(event, state) { return html_onclick({type:'onchange', target: this.parentNode.firstElementChild, state: state}); }
		});
		
		html_onmouse({
			type: 'winshow', 
			target: document.getElementById(win.id)});
			
		return;
	}

	// cmd_code_editor_show_class_browser
	if(trg1.className.indexOf('cmd_code_editor_show_class_browser') > -1 || trg1p.className.indexOf('cmd_code_editor_show_class_browser') > -1) {
		var id = 1;
		
		// если есть окно, то покажем/скроем его
		var win = document.getElementById("littlelisp_class_browser_"+id);
		if(win) {
			win.style.display = win.style.display == "none" ? "block" : "none";
			return;
		}
		
		var div = document.createElement("DIV");
		div.innerHTML = '<fieldset id="{id}_title" style="position:absolute;left:0;top:0;height:35px;width:640px;" class="win-header g-draggable-parent" data-win-relocations="right: win() right"><div class="win-header-title g-draggable-parent2">Class Browser</div> <a class="win-minimize-btn" href="javascript:void(0)" title="Minimize (ESC)">_</a><a class="win-close-btn" href="javascript:void(0)" title="Close">x</a></fieldset>'+
		'<fieldset id="{id}_class_hierarchy" style="position:absolute;top:60px;left:20px;width:300px;height:165px;-moz-box-sizing:border-box;box-sizing:border-box;" data-win-relocations="right:#{id}_splitter1 left; bottom: #{id}_splitter2 bottom;"><select style="width:100%;height:100%;background:white;-moz-box-sizing:border-box;box-sizing:border-box;" multiple="true">'+
		'<optgroup id="{id}_js_classes" label="(unknown_file)"></optgroup></select></fieldset>'+
		'<fieldset id="{id}_class_object_methods" style="position:absolute;top:60px;left:325px;width:300px;height:165px;-moz-box-sizing:border-box;box-sizing:border-box;" data-win-relocations="bottom: #{id}_splitter2 bottom; left: #{id}_splitter1 right; right: win() right;"><select style="width:100%;height:100%;background:white;-moz-box-sizing:border-box;box-sizing:border-box;" multiple="true">'+
		'<optgroup id="{id}_class_methods" label="class (static methods)"></optgroup>'+
		'<optgroup id="{id}_object_methods" label="object"></optgroup>'+
		'</select></fieldset>'+
		'<fieldset id="{id}_code_toolbar" style="position:absolute;top:230px;left:20px;width:600px;height:32px;" data-win-relocations="bottom:win() bottom; top: #{id}_splitter2 bottom; right:win() right;">'+
		'<div class="c-toolbar" data-win-id="{id}">'+
		'<button class="c-toolbar-btn cmd_code_editor_save" title="Save changes"><i class="ico-save"></i>Save</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_run" title="Evalute selected fragment"><i class="ico-play"></i>RunIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_start_debug" title="Debug selected fragment"><i class="ico-bug"></i>DebugIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_inspect_it" title="Evalute selected fragment and explore result"><i class="ico-eye"></i>ExploreIt</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_set_breakpoint"><i class="ico-add-brk"></i>Set breakpoint</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_new_class"><i class="ico-plus"></i>Class</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_new_method"><i class="ico-plus"></i>Method</button>'+
		'</div></fieldset>'+
		'<fieldset id="{id}_code" class="c-code_editor" style="position:absolute;top:255px;left:20px;width:600px;height:220px;" data-win-relocations="top:#{id}_splitter2 bottom; bottom: win() bottom; right:win() right;">'+
		'<pre id="{id}_brkpnts" class="c-code_editor-bg" style="height:100%;width:100%;display:block;background:white;position:absolute;top:0;left:0;z-index:-1;color:transparent;"></pre>'+
		'<textarea id="{id}_code_editor" class="c-code_editor-textarea" style="width:100%;height:100%;background:transparent;"></textarea></fieldset>'+
		'<fieldset id="{id}_splitter1" class="win-splitter type_vertical" style="position:absolute;left:320px;top:60px;height:165px;width:5px;" data-win-relocations="bottom: #{id}_splitter2 bottom; right: win() right; left: win() right;"></fieldset>'+
		'<fieldset id="{id}_splitter2" class="win-splitter type_horizontal" style="position:absolute;left:20px;top:225px;height:5px;width:600px;" data-win-relocations="top: win() bottom; bottom: win() bottom; right: win() right;"></fieldset>';
				
		var form = [];
		for(var i = 0; i < div.childNodes.length; i++) 
		if(div.childNodes[i].nodeName == "FIELDSET")
			form.push(div.childNodes[i].outerHTML
				.replace("{num}", id)
				.replace("{id}", "littlelisp_class_browser_"+id)
				.replace("{id}", "littlelisp_class_browser_"+id)
				.replace("{id}", "littlelisp_class_browser_"+id)
				.replace("{id}", "littlelisp_class_browser_"+id)
				.replace("{id}", "littlelisp_class_browser_"+id));
	
		var win = {
			type: 'wincreate',
			id: 'littlelisp_class_browser_'+id,
			attributes: {},
			resizable: true,
			width: 640, height: 500,
			innerHTML: form.join('')};

		html_onmouse(win);
		html_onmouse({
			type: 'winshow', 
			target: document.getElementById(win.id)});
		
		// TODO firefox - no classes
		var js_class_names = ["<option>Number</option>", "<option>String</option>", "<option>Boolean</option>", "<option>Symbol</option>", "<option>Date</option>", "<option>Array</option>", "<option>Object</option>", "<option>Function</option>"];
		var lisp_class_names = {};
		for(var name in window) {
			if(window[name] && window[name].prototype && "QWFPGJLUYARSTDHNEIOZXCVBKMЯЖФПГЙЛУЫЮШЩАРСТДХНЕИОЬЗЧЦВБКМЁЪ".indexOf(name[0]) > -1) {
				if(window[name].__file && window[name].__source) {
					if(window[name].__file in lisp_class_names == false)
						lisp_class_names[window[name].__file] = [];
					lisp_class_names[window[name].__file].push("<option>"+name+"</option>");
				}
				else
					js_class_names.push("<option>"+name+"</option>");
			}
		}

		// загружаем классы не распределённые по файлам
		var optgroup = document.getElementById("littlelisp_class_browser_"+id+"_js_classes");
		js_class_names.sort();
		optgroup.innerHTML = js_class_names.join("");

		// выведем классы по файлам
		var select_classes = optgroup.parentNode;
		for(var file_name in lisp_class_names) {
			lisp_class_names[file_name].sort();
			optgroup = document.createElement("OPTGROUP");
			optgroup.id = "littlelisp_class_browser_" + id + "_lisp_classes_" + file_name;
			optgroup.label = file_name;
			optgroup.innerHTML = lisp_class_names[file_name].join("");
			select_classes.insertBefore(optgroup, select_classes.lastElementChild);
		}
		
		// добавим кнопку на панель
		if(document.getElementById("littlelisp_ide_panel") && !document.querySelector("#littlelisp_ide_panel .cmd_code_editor_show_class_browser")) {
			var button = document.createElement("BUTTON");
			button.innerHTML = "Class Browser";
			button.className = "cmd_code_editor_show_class_browser";
			document.getElementById("littlelisp_ide_panel").insertBefore(button, document.getElementById("littlelisp_ide_panel").lastElementChild);
		}
	}
	
	// cmd_code_editor_new_class
	if(trg1.className.indexOf('cmd_code_editor_new_class') > -1 || trg1p.className.indexOf('cmd_code_editor_new_class') > -1) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		
		var editor = document.getElementById(win_id+"_code_editor");
		editor.value = "(defclass NewClass \n\t:extends SuperClass\n\t:instvars \"var1 var2...\"\n\t:constructor :defmeth_name\n\t:classvars \"\"\n)\n)";
	}
	
	// cmd_code_editor_new_method
	if(trg1.className.indexOf('cmd_code_editor_new_method') > -1 || trg1p.className.indexOf('cmd_code_editor_new_method') > -1) {
		var dbg_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		
		var editor = document.getElementById(dbg_id+"_code_editor");
		editor.value = "(defmeth method_name (arg1 arg1...) (\n\t;; method body hear\n))";
	}

	// #littleLisp_class_browser_NNN_js/lisp_classes
	if(trg1.nodeName == 'OPTION' && (trg1p.id||"").match(/_(js|lisp)_classes/)) {
		var win_id = trg1p.id.replace(/_(js|lisp)_classes.*/, "");
		var clazz = window[trg1.innerHTML];
		
		var methods = [];
		var list = Object.getOwnPropertyNames(clazz.prototype);
		for(var i = 0; i < list.length; i++)
		if(clazz.prototype[list[i]] instanceof Function) {
			methods.push("<option data-full-name=\"window."+trg1.innerHTML+".prototype."+list[i]+"\">"+list[i]+"</option>");
		}
		if(clazz.prototype.__proto__)
		for(var meth_name in clazz.prototype.__proto__)
		if(clazz.prototype.__proto__[meth_name] instanceof Function) {
			methods.push("<option data-full-name=\"window."+trg1.innerHTML+".prototype."+meth_name+"\">"+meth_name+"</option>");
		}
		methods.sort();
		document.getElementById(win_id+"_object_methods").innerHTML = methods.join("");
		
		var methods = [];
		var list = Object.getOwnPropertyNames(clazz);
		for(var i = 0; i < list.length; i++)
		if(clazz[list[i]] instanceof Function) {
			methods.push("<option data-full-name=\"window."+trg1.innerHTML+"."+list[i]+"\">"+list[i]+"</option>");
		}
		methods.sort();
		document.getElementById(win_id+"_class_methods").innerHTML = methods.join("");
		
		// загрузим описание класса в редактор кода
		var code_editor = document.getElementById(win_id+"_code_editor");
		code_editor.setAttribute("data-full-name", "window."+trg1.innerHTML);
		if ("source" in clazz)
			code_editor.value = clazz.source;
		else
			code_editor.value = clazz.toString();
		if ("file" in clazz)
			code_editor.setAttribute("data-file_name", clazz.file);
		else
			code_editor.removeAttribute("data-file_name");
	}
	
	// #littleLisp_class_browser_NNN_class/object_methods
	if(trg1.nodeName == 'OPTION' && (trg1p.id||"").match(/_(class|object)_methods/)) {
		var full_name = trg1.getAttribute("data-full-name");
		var func = eval("("+full_name+")");
		
		// загрузим исходный код метода в редактор кода
		var id = trg1p.id.replace(/_(class|object)_methods/, "");
		var code_editor = document.getElementById(id+"_code_editor");
		if ("__args_names" in func == false) {
			code_editor.value = func.toString();
			code_editor.setAttribute("data-source-offset", 0);
			code_editor.setAttribute("data-full-name", full_name);
			code_editor.removeAttribute("data-file_name");
		} 
		else {
		
			// ищем открывающую скобку всего определения метода
			for(var start = (func.__args_names[func.__args_names.length-1] >> 16)-1; start >= 0; start--)
				if(func.__source[start] == '(') break;
				
			// ...и закрывающую
			for(var end = (func.__body[func.__body.length-1] >> 16) + (func.__body[func.__body.length-1] & 65535) + 1; end > func.__source.length; end++)
				if(func.__source[end] == ')') break;
			
			// вырежем и загрузим фрагмент кода-определения метода
			code_editor.value = func.__source.substring(start, end);
			code_editor.setAttribute("data-source-offset", start);
			code_editor.setAttribute("data-full-name", full_name);
			code_editor.getAttribute("data-file_name", func.__file);
		}
			
	}

	// #loacalStorage_files
	if(trg1.id == 'loacalStorage_files') {

		var littlelisp_ide_main_window = document.getElementById('littlelisp_ide_main_window_src');

		if(trg1.value == '(new...)') {
			var name = prompt('File name:', '');
			if(!!name) {
				localStorage.setItem('littlelisp_file_'+name, '');
				(trg1.insertBefore(document.createElement('OPTION'), trg1.lastChild)).innerHTML = name;
				trg1.selectedIndex = trg1.options.length - 2;
				littlelisp_ide_main_window.value = "";
				littlelisp_ide_main_window.focus();
			}
		}
		else {
			littlelisp_ide_main_window.value = localStorage.getItem('littlelisp_file_'+trg1.value);
		}
		
		// запомним, что переключились на файл
		var config = JSON.parse(localStorage.littlelisp_config || "{}");
		config.last_file = trg1.options[trg1.selectedIndex].innerHTML;
		localStorage.littlelisp_config = JSON.stringify(config);
	}

	// #littlelisp_debugger_NNN_callstack > select
	if(ev.type == 'click' && trg1.nodeName == 'SELECT' && (trg1p.id||'').indexOf('_callstack') > -1) {
		html_onclick({
			type: "refresh_dbg_debugger",
			target: document.getElementById(trg1.getAttribute("data-win-id")),
			selected_ctx_num: trg1.selectedIndex});
	}

	// [change] .c-object_props
	if(ev.type == 'change' && trg1.className.indexOf('c-object_props') > -1) {
// 		var dbg_id = find_near("c-object_props", {getAttribute:function(){}}).getAttribute("data-win-id");
// 		var littlelisp_debugger = document.getElementById(dbg_id).littlelisp_debugger;
// 		littlelisp_debugger.ctx[ev.key] = JSON.parse(ev.value);
	
		ev.object[ev.key] = eval("("+ev.value+")");
// 		debugger;
	}

	// [show_debugger_win]
	if(ev.type == "show_debugger_win") {
		var button = trg1;
		
		var id = button.innerHTML.replace(/.+?#/, '');
		
		var div = document.createElement("DIV");
		div.innerHTML = '<fieldset id="title_{id}" style="position:absolute;left:0;top:0;height:35px;width:640px;" class="win-header g-draggable-parent" data-win-relocations="right: win() right"><div class="win-header-title g-draggable-parent2">Debugger #{num}</div> <a class="win-minimize-btn" href="javascript:void(0)" title="Minimize (ESC)">_</a><a class="win-close-btn" href="javascript:void(0)" title="Close">x</a></fieldset>'+
		'<fieldset id="{id}_callstack" class="win-fieldset" style="position:absolute;left:20px;top:60px;width:600px;height:20px;overflow:hidden" data-win-relocations="right: win() right">'+
		'<select style="width:100%;height:100%" data-win-id="{id}"></select></fieldset>'+
		'<fieldset id="{id}_code_toolbar" style="position:absolute;top:90px;left:20px;width:600px;height:32px;" data-win-relocations="bottom:win() bottom;right:win() right;">'+
		'<div class="c-toolbar" data-win-id="{id}">'+
		'<button class="c-toolbar-btn cmd_code_editor_save" title="Save changes"><i class="ico-save"></i>Save</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_continue" title="Continue"><i class="ico-play"></i></button>'+
		'<button class="c-toolbar-btn cmd_code_editor_step_over" title="Step over"><i class="ico-step_over"></i></button>'+
		'<button class="c-toolbar-btn cmd_code_editor_step_in" title="Step in"><i class="ico-step_in"></i></button>'+
		'<button class="c-toolbar-btn cmd_code_editor_step_out" title="Step out"><i class="ico-step_out"></i></button>'+
		'<button class="c-toolbar-btn cmd_code_editor_restart" title="Restart"><i class="ico-restart"></i></button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_run" title="Evalute selected fragment"><i class="ico-play"></i>RunIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_start_debug" title="Debug selected fragment"><i class="ico-dbg-start"></i>DebugIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_inspect_it" title="Evalute selected fragment and explore result"><i class="ico-eye"></i>ExploreIt</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_set_breakpoint"><i class="ico-add-brk"></i>Set breakpoint</button>'+
		'</div></fieldset>'+
		'<fieldset id="{id}_code" class="c-code_editor" style="position:absolute;top:115px;left:20px;width:600px;height:340px;" data-win-relocations="bottom:#{id}_splitter bottom; right:win() right;">'+
		'<pre id="{id}_brkpnts" class="c-code_editor-bg" style="height:100%;width:100%;display:block;background:white;position:absolute;top:0;left:0;z-index:-1;color:transparent;"></pre>'+
		'<textarea id="{id}_code_editor" class="c-code_editor-textarea" style="width:100%;height:100%;background:transparent;"></textarea></fieldset>'+
		'<fieldset id="{id}_splitter" class="win-splitter type_horizontal" style="position:absolute;left:20px;top:455px;height:5px;width:600px;" data-win-relocations="right:win() right; top: win() bottom; bottom: win() bottom;"></fieldset>'+
		'<fieldset id="{id}_watch" class="win-fieldset" style="position:absolute;left:20px;top:460px;height:150px;width:600px" data-win-relocations="right: win() right; bottom: win() bottom; top: #{id}_splitter bottom">'+
		'<div class="c-object_props" style="width:100%;height:100%;outl1ine:1px solid silver;" data-win-id="{id}"></div></fieldset>';
		
		var form = [];
		for(var i = 0; i < div.childNodes.length; i++) 
		if(div.childNodes[i].nodeName == "FIELDSET")
			form.push(div.childNodes[i].outerHTML
				.replace("{num}", id)
				.replace("{id}", "littlelisp_debugger_"+id)
				.replace("{id}", "littlelisp_debugger_"+id)
				.replace("{id}", "littlelisp_debugger_"+id)
				.replace("{id}", "littlelisp_debugger_"+id)
				.replace("{id}", "littlelisp_debugger_"+id));
		
		var win = {
			type: 'wincreate',
			id: 'littlelisp_debugger_'+id,
			attributes: {},
			resizable: true,
			width: 640, height: 625,
			innerHTML: form.join('')};

		html_onmouse(win);
		html_onmouse({
			type: 'winshow', 
			target: document.getElementById(win.id)});
		
		// привяжем объект для управления дебагом
		document.getElementById(win.id).littlelisp_debugger = button.littlelisp_debugger;
		document.getElementById(win.id).ex = button.ex;
		
		html_onclick({
			type: "refresh_dbg_debugger",
			target: document.getElementById("littlelisp_debugger_"+id)});
	}
	
	// [restore_main_window_content]
	if(ev.type == "restore_main_window_content" /* || ev.type == "load" && ev.target == document */) {

		// считаем config
		try {
			var config = localStorage["littlelisp_config"];
			config = JSON.parse(config);
		}
		catch(ex) {
// 			alert(ex);
			var config = undefined;
		}

		// если config повреждён или его нет, то создадим новый
		if(!config) {
			localStorage["littlelisp_config"] = "{\"last_file\":\"workspace1\"}";
			config = {last_file: "workspace1"};
			if("littlelisp_file_workspace1" in localStorage == false)
				localStorage["littlelisp_file_workspace1"] = document.getElementById('littlelisp_ide_main_window_src').value;
		}
		
		// загрузим список сохранёных файлов
		var select1 = document.getElementById("loacalStorage_files");
		for(var f in localStorage) if(f.match(/^littlelisp_file_/)) {
			(select1.appendChild(document.createElement('OPTION'))).innerHTML = f.replace(/^littlelisp_file_/, '');
		}
		(select1.appendChild(document.createElement('OPTION'))).innerHTML = '(new...)';
		
		// загрузим в редактор последний редактированный файл
		var last_file_content = localStorage["littlelisp_file_"+config.last_file];
		if(typeof last_file_content == "undefined")
			document.getElementById('littlelisp_ide_main_window_src').value = "";
		else
			document.getElementById('littlelisp_ide_main_window_src').value = last_file_content;
		document.getElementById('littlelisp_ide_main_window_src').focus();
			
		// выделим редактируемый файл если загрузили успешно
		if(typeof last_file_content != "undefined")
		for(var i = select1.options.length-1; i >= 0; i--) {
			select1.options[i].selected = select1.options[i].innerHTML == config.last_file;
		}
	}
		
	// [refresh_dbg_debugger]
	if(ev.type == "refresh_dbg_debugger") {
		var dbg_id = ev.dbg_id || trg1.id;
		var selected_ctx_num = ev.selected_ctx_num;
		
		var littlelisp_debugger = document.getElementById(dbg_id).littlelisp_debugger;
		var ex = document.getElementById(dbg_id).ex;
		
		// очистим стек вызовов
		var call_stack_select = document.getElementById(dbg_id+'_callstack').firstElementChild;
		call_stack_select.innerHTML = '';
		
		// загрузим стек вызовов
		var selected_ctx = littlelisp_debugger.ctx
		for(var curr_ctx = littlelisp_debugger.ctx; curr_ctx; curr_ctx = curr_ctx.parent) {
			
			// создадим option и заполним её параметрами
			var option = document.createElement('OPTION');
			call_stack_select.appendChild(option);
			option.value = call_stack_select.options.length;
			
			// надпись
			var start_offset = curr_ctx.input[0] >> 16;
			var option_caption = curr_ctx.file + ':' + curr_ctx.range(1) + ' - ' + curr_ctx.source.substring(start_offset, start_offset+100);
			option.innerHTML = option_caption.length > 100 
				? option_caption.substring(0, 100) + '...' 
				: option_caption;
				
			// возмём выделенный контекст если попросили
			if (selected_ctx_num === call_stack_select.options.length-1) { 
				selected_ctx = curr_ctx;
				option.selected = true;
			}
		}
		
		// загрузим контекст в watch
		html_onclick({
			type: 'load', 
			target: document.getElementById(dbg_id+'_watch').firstElementChild,
			show_toolbar: false,
			object_to_load: ((ex && ex != "Breakpoint!") ? selected_ctx : selected_ctx.scope ),
			clear_before_load: true
		});
		
		var editor = document.getElementById(dbg_id+'_code_editor');
		
		// загрузим исходный код нужного файла если нужно
		if(editor.getAttribute('data-file_name') != selected_ctx.file) {
			editor.value = selected_ctx.source;
			editor.setAttribute('data-file_name', selected_ctx.file);
		}
				
		// выделим нужный фрагмент
		editor.setSelectionRange(
			selected_ctx.range(1),
			selected_ctx.range(1));
		editor.blur();
		editor.focus();
		editor.setSelectionRange(
			selected_ctx.range(1),
			selected_ctx.range(2));
	}

	// [show_hide_littlelisp_js_ide]
	if(ev.type == "show_hide_littlelisp_js_ide") {
		var style_tag = document.getElementById("littlelisp_ide_css_styles");
		if(!style_tag) {
			style_tag = document.createElement("STYLE");
			style_tag.id = "littlelisp_ide_css_styles";
			style_tag.type="text/css";
			style_tag.appendChild(
				document.createTextNode(littlelisp_ide_onclick.css_styles));
			document.querySelector("head").insertBefore(style_tag, document.querySelector("head > style"));
		}
		
		// build and show IDE interface
		var ide_win = document.getElementById("littlelisp_ide_main_window");
		if ( ! ide_win) {
			var div = document.createElement("DIV");
			div.innerHTML = '<div id="littlelisp_ide_main_window" class="c-code_editor" style="width:100%;height:100%;position:fixed;top:0;left:0;right:0;bottom:0;display:none;background:white">'+
			'<pre id="littlelisp_ide_main_window_brkpnts" class="c-code_editor-bg" style="height:100%;width:100%;border-top:26px solid transparent;display:block;background:white;position:absolute;top:0;left:0;color:transparent;"></pre>'+
		'<textarea id="littlelisp_ide_main_window_src" class="c-code_editor-textarea" contenteditable="true" style="height:100%;width:100%;border-top:26px solid transparent;display:block;background:transparent;position:relative" data-win-id="littlelisp_ide_main_window">'+(window['default_lisp_code']||"")+'</textarea>'+
		'<div class="c-toolbar" style="position:absolute;top:0;width:100%;" data-win-id="littlelisp_ide_main_window">'+
		'<select id="loacalStorage_files" class="c-toolbar-select"></select>'+
		'<button class="c-toolbar-btn cmd_code_editor_save" title="Save to LocalStorage (Alt+S)"><i class="ico-save"></i>Save</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_run" title="Execute selected fragment or all text (Alt+R)"><i class="ico-play"></i>RunIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_start_debug" title="Start debugging selected fragment or all text (Alt+W)"><i class="ico-bug"></i>DebugIt</button>'+
		'<button class="c-toolbar-btn cmd_code_editor_inspect_it" title="Evalute selected fragment or all text and explore result (Alt+A,Ctrl+I)"><i class="ico-eye"></i>ExploreIt</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_set_breakpoint" title="Set/unset breakpoint (Alt+T)"><i class="ico-add-brk"></i>Set breakpoint</button>'+
		'<span class="c-toolbar-divider"></span>'+
		'<button class="c-toolbar-btn cmd_code_editor_show_class_browser"><i class="ico-class-browser" title="Open Class Browser"></i>Class Browser</button>'+
		'<button class="c-toolbar-btn" id="littlelisp_ide_main_window_settings" title="Settings" style="display:none"><i class="ico-cog"></i></button>'+
		'<span class="c-toolbar-divider"></span></div></div>';
		
			document.body.appendChild(div.firstElementChild);
			ide_win = document.getElementById("littlelisp_ide_main_window");
			ide_win.style.display = "block";
		
			html_onclick({type: "restore_main_window_content"});
			
			// скроем кнопку показа IDE
			(document.getElementById("littlelisp_ide_main_window_show")||{}).innerHTML = "Hide IDE";
		}
		else {
			
			if(ide_win.style.display == "block") {
				ide_win.style.display = "none";
				(document.getElementById("littlelisp_ide_main_window_show")||{}).innerHTML = "Show IDE";
			}
			else {
				ide_win.style.display = "block";
				(document.getElementById("littlelisp_ide_main_window_show")||{}).innerHTML = "Hide IDE";
			}
		}
	}
	
	// [hide_littlelisp_js_ide]
	if(ev.type == "hide_littlelisp_js_ide" || (ev.type == "click" && trg1.id == "littlelisp_ide_main_window_show_hide")) {
		var ide_win = document.getElementById("littlelisp_ide_main_window");
		if (ide_win) ide_win.style.display = "none";
		
		// покажем кнопку
		(document.getElementById("littlelisp_ide_main_window_show")||{}).innerHTML = "Show IDE";
	}

	// [save_file]
	if(ev.type == "save_file") {
		// дадим знать, что идёт сохранение
		var trg = trg1p.className.indexOf('cmd_code_editor_save') > -1 ? trg1p : trg1;
		trg.style.opacity = 0.4;
		setTimeout(function(){ trg.style.opacity = 1; }, 500);
		
		if (!ev.file_content) {
			alert("New content is empty! (canceled saving)");
			debugger;
			return;
		}
		
		// сохраним
		localStorage.setItem("littlelisp_file_"+ev.file_name, ev.file_content);
		
		// также обновим контент в главном окне если открыто
		if (document.getElementById('loacalStorage_files').value == ev.file_name) {
			var editor1 = document.getElementById("littlelisp_ide_main_window").querySelector(".c-code_editor-textarea");
			if (editor1.value != ev.file_content)
				editor1.value = ev.file_content;
		}
	}
	
	// [debugger.*]
	if(ev.type.indexOf("debugger.") > -1) {
		var panel = document.getElementById("littlelisp_ide_panel");
		
		// поищем кнопку, и если есть откроем окно дебагера
		for (var ii = 0; ii < panel.childNodes.length; ii++) {
			var button = panel.childNodes[ii];
			if (button.littlelisp_debugger == ev.debugger) {
				var id = button.innerHTML.replace(/.+?#/, '');
				if (! document.getElementById("littlelisp_debugger_"+id))
					button.click();
				else
					html_onclick({type: "refresh_dbg_debugger", dbg_id: "littlelisp_debugger_"+id});
				return
			}
		}
		
		// иначе добавим кнопку на панель
		var button = document.createElement('BUTTON');
		button.className = "cmd_code_editor_debugger";
		button.innerHTML = "<i></i>Debugger #"+(littlelisp_ide_onclick.debugger_win_next_num++);
		button.littlelisp_debugger = ev.debugger;

		if(document.getElementById("littlelisp_ide_panel")) {
			panel.insertBefore(button, panel.firstElementChild);
		}
		else {
			button.className = "c-panel-btn cmd_code_editor_debugger";
			document.querySelector("#littlelisp_ide_main_window .c-toolbar").appendChild(button);
		}
		
		// и нажмём на неё, чтоб дебагер открылся
		button.click();
	}
}

littlelisp_ide_onclick.debugger_win_next_num = 1;

// подключим расширение
html_onclick.extensions.push(littlelisp_ide_onclick);

function littlelisp_ide_onkey(event) {
	var ev = (html_onkey||{}).ev || event;
	var trg1 = (html_onkey||{}).trg1 || event.target;
	var trg1p = (html_onkey||{}).trg1p || trg1.parentNode;
// 	console.log(ev);

	// [textarea] ТАВ
	if(ev.keyCode == 9 && ev.type == "keydown") {
// 				document.execCommand("styleWithCSS", true, null);
	
		if(trg1.nodeName = 'TEXTAREA') {
			var textEvent = document.createEvent('TextEvent');
			textEvent.initTextEvent('textInput', true, true, null, "\t");
			trg1.dispatchEvent(textEvent);
			
			if(ev.stopPropagation) ev.stopPropagation();
			else ev.cancelBubble = true;
		
			if(ev.preventDefault) ev.preventDefault();
			else ev.returnValue = false;
		
			return false;
		}
	
		if(ev.shiftKey)
			document.execCommand('outdent', true, null);
		else
			document.execCommand('indent', true, null);

		if(ev.stopPropagation) ev.stopPropagation();
		else ev.cancelBubble = true;
		
		if(ev.preventDefault) ev.preventDefault();
		else ev.returnValue = false;
		
		return false;
	}

	// [win] ESC
	if(ev.keyCode == 27 && ev.type == "keydown") {
		var windows = document.querySelectorAll(".win");
		for(var i = windows.length-1; i >= 0 ; i--)
			if(windows[i].style.display != "none") {
				if(windows[i].querySelector(".win-minimize-btn"))
					html_onmouse({type: "mouseup", target: windows[i].querySelector(".win-minimize-btn")});
				else
					html_onmouse({type: "mouseup", target: windows[i].querySelector(".win-close-btn")});
				return;
			}

		// если нету окон, то скроем IDE
		html_onclick({type: "show_hide_littlelisp_js_ide"});
	}
	
	// [textarea] ALT+A,CTR+I - InstpectIt
	if(ev.keyCode == 65 && ev.type == "keydown" && ev.altKey || 
	   ev.keyCode == 73 && ev.type == "keydown" && ev.ctrlKey) {
		html_onclick({
			type:"click", 
			target: (document.getElementById(trg1.id.replace(/_(src|code_editor)$/, ""))||{querySelector:function(){}}).querySelector(".cmd_code_editor_inspect_it")});
	}
	
	// [textarea] ALT+R - DoIt
	if(ev.keyCode == 84 && ev.type == "keydown" && ev.altKey) {
		html_onclick({
			type:"click", 
			target: (document.getElementById((ev.target.id).replace(/_src$/, ""))||{querySelector:function(){}}).querySelector(".cmd_code_editor_run")});
	}
	
	// [textarea] ALT+W - DebugIt
	if(ev.keyCode == 87 && ev.type == "keydown" && ev.altKey) {
		html_onclick({
			type:"click", 
			target: (document.getElementById((ev.target.id).replace(/_src$/, ""))||{querySelector:function(){}}).querySelector(".cmd_code_editor_start_debug")});
	}
	
	// [textarea] ALT+S - Save
	if(ev.keyCode == 83 && ev.type == "keydown" && ev.altKey) {
		html_onclick({
			type:"click", 
			target: (document.getElementById((ev.target.id).replace(/_src$/, ""))||{querySelector:function(){}}).querySelector(".cmd_code_editor_save")});
	}
	
	// [textarea] ALT+T - Set/unset breakpoint
	if(ev.keyCode == 116 && ev.type == "keydown" && ev.altKey) {
		html_onclick({
			type:"click", 
			target: (document.getElementById((ev.target.id).replace(/_src$/, ""))||{querySelector:function(){}}).querySelector(".cmd_code_editor_set_breakpoint")});
	}
}

html_onkey.extensions.push(littlelisp_ide_onkey);

function littlelisp_ide_onmouse(event) {
	var ev = (html_onmouse||{}).ev || event;
	var trg1 = (html_onmouse||{}).trg1 || event.target;
	var trg1p = (html_onmouse||{}).trg1p || trg1.parentNode;
	var find_near = (html_onmouse||{}).find_near || function(){ alert("Error! html_onmouse.find_near not found!"); };

	// show breapoints background
	if((ev.type == "mouseover" || ev.type == "mouseout" || ev.type == "mouseout-window") && (trg1.className.indexOf("c-toolbar-btn cmd_code_editor_set_breakpoint") > -1 || trg1p.className.indexOf("c-toolbar-btn cmd_code_editor_set_breakpoint") > -1)) {
		var win_id = find_near("c-toolbar", {getAttribute:function(){}}).getAttribute("data-win-id");
		var textarea = document.getElementById(win_id).querySelector(".c-code_editor-textarea");
		var file_name = textarea.getAttribute("data-file_name") || document.getElementById("loacalStorage_files").value;
		file_name = file_name == "(new...)" ? "(no file)" : file_name;
		
		// скроем подложку с breakpoints
		if(ev.type == "mouseout") {
			(textarea.previousElementSibling||{style:{}}).style.display = "none";
			return;
		}
		
		// если ничего не выделено, то покажем breakpoints
		if(file_name in littleLisp.breakpoints) {
			var points = [];
			for(var brk in littleLisp.breakpoints[file_name])
				points.push(brk);
			points.sort();
			
			var result = textarea.value;
			for(var i = points.length-1; i >= 0; i--) {
				var off = points[i]*1 >> 16;
				var len = points[i]*1 & 65535;
				result = result.substring(0, off) + "<strike style=\"background:red\">" + result.substring(off, off+len) + "</strike>" + result.substring(off+len);
			}
			
			textarea.previousElementSibling.innerHTML = result;
		}
		
		textarea.previousElementSibling.style.display = "block";
		textarea.previousElementSibling.scrollTop = textarea.scrollTop;
	}

	// [winclose]
	if(ev.type == "winclose") {
		var win_id = (trg1.id||"").match(/^littlelisp_debugger_[0-9]+$/);
		if (win_id) {
			win_id = new RegExp(win_id[0].replace("littlelisp_debugger_", "Debugger #")+"$");
			var buttons = document.querySelectorAll("button.cmd_code_editor_debugger");
			for (var i in buttons)
			if (buttons[i].innerHTML.match(win_id)) {
				buttons[i].parentNode.removeChild(buttons[i]);
				break;
			}
		}
	}
	
}

html_onmouse.extensions.push(littlelisp_ide_onmouse);

// кнопка показа IDE
// var littlelisp_ide_button_loop = setInterval(function(){
if (!document.getElementById("littlelisp_ide_main_window_show")) {
	var div = document.createElement("DIV");
	div.id = "littlelisp_ide_panel";
	div.setAttribute("style", "position:fixed; right: 2px; bottom: 2px; display: block; z-index: 999; text-align: right;");
	div.innerHTML = '<button id="littlelisp_ide_main_window_show" onclick="html_onclick({type: \'show_hide_littlelisp_js_ide\'})">Show IDE</button>';
	
	try {
		document.querySelector("body").appendChild(div);
// 		clearInterval(littlelisp_ide_button_loop);
// 		delete window["littlelisp_ide_button_loop"];
	}
	catch(ex) {
		try { 
			document.querySelector("html").appendChild(div);
		} catch(ex2) {
			alert(ex2);
			console.log(ex2);
		}
	}
}
// }, 500);

littlelisp_ide_onclick.css_styles = 
".cmd_code_editor_debugger > i {  background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2aWV3Qm94PSIwIDAgMjQgMjQiIHdpZHRoPSIxOCIgaGVpZ2h0PSIxOCI+CiAgICA8cGF0aCBkPSJNIDguOTA2MjUgMiBMIDcuNzE4NzUgMy41OTM3NSBMIDkuMjUgNC43NSBDIDkuMDgzOTE2NyA1LjEyNzc5NjkgOSA1LjU0OTE0NjkgOSA2IEwgMTUgNiBDIDE1IDUuNTY0OTExIDE0LjkzNjQ2OSA1LjE0ODg0OSAxNC43ODEyNSA0Ljc4MTI1IEwgMTYuMzQzNzUgMy42MjUgTCAxNS4xNTYyNSAyIEwgMTMuMzc1IDMuMzQzNzUgQyAxMi45NjIzMTYgMy4xMzU0MTU5IDEyLjUwODIzNCAzIDEyIDMgQyAxMS41MDc1MyAzIDExLjA1OTcwNSAzLjExNjE4OSAxMC42NTYyNSAzLjMxMjUgTCA4LjkwNjI1IDIgeiBNIDggNyBDIDcuNDQ1NTkzOSA3IDYuOTU2OTI3NyA3LjY3ODM5MjkgNi41OTM3NSA4Ljc4MTI1IEwgNC4xNTYyNSA3LjM3NSBMIDMuMTU2MjUgOS4wOTM3NSBMIDYuMTI1IDEwLjgxMjUgQyA2LjAzNjUxODUgMTEuNDg4NDE2IDYgMTIuMjMyODUxIDYgMTMgTCAzIDEzIEwgMyAxNSBMIDYuMTI1IDE1IEMgNi4yNDIyMDE3IDE1Ljc5NzI3OSA2LjQzOTA0NTcgMTYuNDgxMzQ0IDYuNjg3NSAxNy4wNjI1IEwgNC4wOTM3NSAxOS4wMzEyNSBMIDUuMzEyNSAyMC42MjUgTCA3Ljc4MTI1IDE4Ljc1IEMgOC42NzI0NzAzIDE5LjY3OTAzOSA5LjgyMjE1NTIgMjAuMTg2MDM0IDExIDIwLjU5Mzc1IEwgMTEgMTMgTCAxMyAxMyBMIDEzIDIwLjY4NzUgQyAxNC40NDM3NSAyMC4yNSAxNS41ODMzNzQgMTkuNzc2ODA3IDE2LjQwNjI1IDE4LjkwNjI1IEwgMTguNzE4NzUgMjAuNjU2MjUgTCAxOS45Mzc1IDE5LjA5Mzc1IEwgMTcuNDM3NSAxNy4xNTYyNSBDIDE3LjY1NjQ0MiAxNi41NTI0ODMgMTcuNzgzOTc3IDE1Ljg0ODM5IDE3Ljg3NSAxNSBMIDIxIDE1IEwgMjEgMTMgTCAxOCAxMyBDIDE4IDEyLjIwODk1NiAxNy45Mzc1MzggMTEuNDQzMTY2IDE3Ljg0Mzc1IDEwLjc1IEwgMjAuNzUgOS4wNjI1IEwgMTkuNzUgNy4zMTI1IEwgMTcuMzc1IDguNjg3NSBDIDE3LjAxNDY1NiA3LjY0MjgyMDggMTYuNTM3NjAyIDcgMTYgNyBMIDggNyB6Ii8+Cjwvc3ZnPgo=') !important; margin-right: 1px; background-size: 20px 20px; }\n"+

".ico-add-brk { margin-right: 1px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiBoZWlnaHQ9IjIwIiB3aWR0aD0iMjAiIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj4KPHBhdGggZD0ibTEuMTIzMSA5LjM5NzZoMTEuMzIxbDIuNjgwNiAzLjAyNS0yLjY4MDYgMi45MzUxaC0xMS4zMjF6Ii8+CjxwYXRoIGQ9Im0xNC4xMTcgNi4zODU1aDIuMDEwNnYtMS45OTMyaC45ODc5NHYxLjk5MzJoMi4wMDE5di45OTY2MWgtMi4wMDE5djIuMDE5MmgtLjk4Nzk0di0yLjAxOTJoLTIuMDEwNnoiLz4KPC9zdmc+Cg==') !important; }\n"+

".ico-eye { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cGF0aCBkPSJtMTAgNC41NDU1Yy0zLjYzNjQgMC02Ljc0MTggMi4yNjE4LTggNS40NTQ1IDEuMjU4MiAzLjE5MjcgNC4zNjM2IDUuNDU0NSA4IDUuNDU0NXM2Ljc0MTgtMi4yNjE4IDgtNS40NTQ1Yy0xLjI1ODItMy4xOTI3LTQuMzYzNi01LjQ1NDUtOC01LjQ1NDV6bTAgOS4wOTA5Yy0yLjAwNzMgMC0zLjYzNjQtMS42MjkxLTMuNjM2NC0zLjYzNjRzMS42MjkxLTMuNjM2NCAzLjYzNjQtMy42MzY0IDMuNjM2NCAxLjYyOTEgMy42MzY0IDMuNjM2NC0xLjYyOTEgMy42MzY0LTMuNjM2NCAzLjYzNjR6bTAtNS44MTgyYy0xLjIwNzMgMC0yLjE4MTguOTc0NTUtMi4xODE4IDIuMTgxOCAwIDEuMjA3My45NzQ1NSAyLjE4MTggMi4xODE4IDIuMTgxOCAxLjIwNzMgMCAyLjE4MTgtLjk3NDU1IDIuMTgxOC0yLjE4MTggMC0xLjIwNzMtLjk3NDU1LTIuMTgxOC0yLjE4MTgtMi4xODE4eiIvPjwvc3ZnPgo=') !important; margin: 0 2px 0 -4px; }\n"+

".cmd_code_editor_start_debug > i { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiBoZWlnaHQ9IjE4IiB3aWR0aD0iMTgiIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDI0IDI0Ij4KPHBhdGggZD0ibTYuNjc5NyAxLjA3NjJsLS44OTA2IDEuMTk1MyAxLjE0ODQuODY3MmMtLjEyNDYuMjgzMy0uMTg3NS41OTkzLS4xODc1LjkzNzVoNC41YzAtLjMyNjMtLjA0OC0uNjM4NC0uMTY0LS45MTQxbDEuMTcyLS44NjcyLS44OTEtMS4yMTg3LTEuMzM2IDEuMDA3OGMtLjMwOTMtLjE1NjMtLjY0OTgtLjI1NzgtMS4wMzEtLjI1NzgtLjM2OTQgMC0uNzA1Mi4wODcxLTEuMDA3OC4yMzQzbC0xLjMxMjUtLjk4NDN6bS0uNjc5NyAzLjc1Yy0uNDE1OCAwLS43ODIzLjUwODgtMS4wNTQ3IDEuMzM1OWwtMS44MjgxLTEuMDU0Ny0wLjc1IDEuMjg5MSAyLjIyNjYgMS4yODljLS4wNjY0LjUwNzAtLjA5MzggMS4wNjUzLS4wOTM4IDEuNjQwN2gtMi4yNXYxLjQ5OThoMi4zNDM4Yy4wODc5LjU5OC4yMzU1IDEuMTExLjQyMTggMS41NDdsLTEuOTQ1MyAxLjQ3Ny45MTQxIDEuMTk1IDEuODUxNS0xLjQwNmMuNjY4NS42OTYgMS41MzA3IDEuMDc3IDIuNDE0MSAxLjM4MnYtNi4wNjJoMS41di4wMDk4bDMuOTE2IDEuODU3MmgyLjA4NHYtMS40OTk4aC0yLjI1YzAtLjU5MzMtLjA0Ny0xLjE2NzctLjExNy0xLjY4NzVsMi4xNzktMS4yNjU3LS43NTAtMS4zMTI1LTEuNzgxIDEuMDMxM2MtLjI3MC0uNzgzNS0uNjI4LTEuMjY1Ni0xLjAzMS0xLjI2NTZoLTZ6IiB0cmFuc2Zvcm09InNjYWxlKDEuMzMzMykiLz4KPHBhdGggZD0ibTIxLjc4NiAxOC40NS05LjMzMzMtNS4zMzMzdjEwLjY2NyIvPgo8L3N2Zz4K') !important; }\n"+

".cmd_code_editor_show_class_browser > i { margin: 0 2px 0 -4px; background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2ZXJzaW9uPSIxLjEiIHZpZXdCb3g9IjAgMCAyMCAyMCI+PHBhdGggZD0ibSAyLjAxODUsMTYgMTUuOTYzLDAgMCwtNi4wOTIzNTQxIC0xNS45NjMsMCB6IG0gMCwtNy42MDkwOTU5IDcuMTk4MjgxNSwwIEwgOS4yMTY3ODE1LDQgMi4wMTg1LDQgWiBNIDEwLjgyNjIxOSw0IGwgMCw0LjM5MDkwNDEgNy4xNTUyODEsMCBMIDE3Ljk4MTUsNCBaIi8+PC9zdmc+Cg==') !important; }\n"+

".ico-discard { background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH3wsdFwII35vlTAAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAmUlEQVQY023PsQ2CQBQG4I8LvYPYkLgArRXBCbR0Kp0AYkXLCDQkdG5g4gJg4RkJub+6vPte8r8Mjqf6hgJl17TvONuhx9A17SWL6OybAWV893EZ7jkO/ikisEJwCKgxbvAajagDnqg2eI0qPEPXtDNeWBJwwatr2jlbXVdIZ0AZEmhMdO4DpkSnbecpxxUB+1/x+FnhEReuH/OUL1Zkgg+nAAAAAElFTkSuQmC') !important; }"+

".ico-save { background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAA7EAAAOxAGVKw4bAAAAB3RJTUUH4AcFCy4mijH+MAAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAvklEQVQoz2OcM2eOZ19v77YfP34w8PDwMDAzMzOgg1+/fjFISEgwbNy0iY+bm5uBQU5W9r+IsPB/EWHh/+fPn/+PDcyfP/+/iLDw//i4uP///v1jY/r27RsDLuDi7Mzg4uwM52/dupWhv69vKhMDiWDbtm0pJGtiYGBgQNF0/do1FMk9e/cy7N6zh+HG9eso4owiwsL/SbFFX1+fgXLnDUJNHBwcJGng4ORkYCqvqAhiZ2cnSoOAgABDdnZ2KgCnNkvad65KiwAAAABJRU5ErkJggg==') !important; }"+

".ico-play { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyLjgybW0iIGhlaWdodD0iMi44Mm1tIiB2aWV3Qm94PSIwIDAgMTAgMTAiPjxwYXRoIGQ9Im0gMTAsNSAtMTAsLTUgMCwxMCIvPjwvc3ZnPgo=') !important; }"+

".ico-plus { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cGF0aCBkPSJtMTAuNzY3IDVoLTEuNTM0djQuMjMzaC00LjIzM3YxLjUzNGg0LjIzM3Y0LjIzM2gxLjUzNHYtNC4yMzNoNC4yMzN2LTEuNTM0aC00LjIzM3YtNC4yMzN6Ii8+PC9zdmc+Cg==') !important; }"+

".cmd_code_editor_restart > i { margin: 0 -4px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cGF0aCBkPSJtMTAgMy40MTQydjJjLTIuNzYgMC01IDIuMjQtNSA1czIuMjQgNSA1IDUgNS0yLjI0IDUtNWgtMS44NGMtLjA0IDEuNzItMS40MyAzLjA5LTMuMTYgMy4wOS0xLjc1IDAtMy4xNi0xLjQtMy4xNi0zLjE1czEuNDEtMy4xNiAzLjE2LTMuMTZ2Mi4yMmw0LjUtMy00LjUtM3oiLz48L3N2Zz4K') !important; }\n"+

".ico-step_over { margin: 0 -4px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1LjAxNDA4OTZtbSIgaGVpZ2h0PSIyLjcyNDk5MDFtbSIgdmlld0JveD0iMCAwIDE3Ljc2NjQ1OSA5LjY1NTQ3NjgiPjxnIHRyYW5zZm9ybT0idHJhbnNsYXRlKDEuMTg3NTc3NWUtOCwtMTA0Mi43MDY3KSI+PHBhdGggZD0ibSA1Ljk0NjQ1OTQsMTA0OS44NjIyIGMgMCwxLjEgMC45LDIgMiwyIDEuMSwwIDIsLTAuOSAyLC0yIDAsLTEuMSAtMC45LC0yIC0yLC0yIC0xLjEsMCAtMiwwLjkgLTIsMiIgaWQ9InBhdGgzNjEzIi8+PHBhdGggZD0ibSAxLjE5NjQ1OTQsMTA1MC4zOTIyIGMgMi41NSwtOC40MyAxMS4zOTk5OTk2LC04LjczIDEzLjkzOTk5OTYsMCIgc3R5bGU9ImZpbGw6bm9uZTtzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6Mi41Ii8+PHBhdGggZD0ibSAxNS42MjY0NTksMTA1Mi4zNjIyIC00LjU0LC0yLjc2IDYuNjgsLTIuMSIvPjwvZz48L3N2Zz4K') !important; }"+

".ico-step_out { margin: 0 -4px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyLjU0bW0iIGhlaWdodD0iMy42NjhtbSIgdmlld0JveD0iMCAwIDguOTkgMTMiPjxnIHRyYW5zZm9ybT0idHJhbnNsYXRlKDEuMTg3NTc3M2UtOCwtMTAzOS4zNjIyKSI+PHBhdGggZD0ibSAyLjUsMTA1MC4zNjIyIGMgMCwxLjEgMC45LDIgMiwyIDEuMSwwIDIsLTAuOSAyLC0yIDAsLTEuMSAtMC45LC0yIC0yLC0yIC0xLjEsMCAtMiwwLjkgLTIsMiIvPjxwYXRoIGQ9Im0gNC41LDEwMzkuMzYyMiAtNC41MCw0IEwgMywxMDQzLjM2MjIgbCAwLDQgMywwIDAsLTQgMywwIC00LjUsLTQgeiIvPjwvZz48L3N2Zz4K') !important; }"+

".ico-step_in { margin: 0 -4px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyLjU0bW0iIGhlaWdodD0iMy42NjhtbSIgdmlld0JveD0iMCAwIDguOTkgMTMiPjxwYXRoIGQ9Ik0gMi40OSwxMC45OSBDIDIuNDksMTIuMDkgMy4zOSwxMyA0LjQ5LDEzIGMgMS4xLDAgMiwtMC45IDIsLTIuMDAwNDg1IDAsLTEuMTAgLTAuOTAsLTIgLTIsLTIgLTEuMSwwIC0yLDAuOSAtMiwyIi8+PHBhdGggZD0ibSAyLjk5LDAgMCw0IC0zLDAgNC41LDQgNC41LC00IC0zLDAgMCwtNCAtMywwIHoiLz48L3N2Zz4K') !important; }"+

".ico-replay { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB2ZXJzaW9uPSIxLjEiIHZpZXdCb3g9IjAgMCAyMCAyMCI+CjxnIHRyYW5zZm9ybT0idHJhbnNsYXRlKDIuMTY5NSAxLjgzMDUpIj4KPHBhdGggZD0ibTIuNTg1OCAxMy44OTFjLTEuNTg5OC0xLjQ2Mi0yLjU4NTgtMy41Ni0yLjU4NTgtNS44OTEgMC00LjQyIDMuNTgtOCA4LThzOCAzLjU4IDggOC0zLjU4IDgtOCA4di0xLjZjMy41MjggMCA2LjQtMi44NzIgNi40LTYuNHMtMi44NzItNi40LTYuNC02LjQtNi40IDIuODcyLTYuNCA2LjRjMCAxLjkzNTcuODY0NjEgMy42NzQgMi4yMjc4IDQuODQ4N2wxLjY5MjUtMS40Mi4wMTUxIDQuNDYzLTQuMzkyNS0uNzkwIDEuNDQyOS0xLjIxMXptMy45MjA5LTIuNDYyIDQuNTcxMy0zLjQyOS00LjU3MTMtMy40Mjg2djYuODU3MXoiLz48L2c+Cjwvc3ZnPgo=') !important; }"+

".ico-cog { background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cGF0aCBkPSJtMTAuMTA0IDEyLjE3MmMtMS4xOCAwLTIuMTMtLjk2LTIuMTMtMi4xMyAwLTEuMTguOTYtMi4xMyAyLjEzLTIuMTMgMS4xOCAwIDIuMTMuOTYgMi4xMyAyLjEzIDAgMS4xOC0uOTYgMi4xMy0yLjEzIDIuMTNtOC0zLjczLTIuOTMtLjUzIDEuNzItMi4zOS0yLjI2LTIuMjYtMi40NiAxLjc5LS40Ny0zaC0zLjJsLS40IDMuMTMtMi41My0xLjkyLTIuMjYgMi4yNiAxLjk5IDIuNTktMy4yLjMzdjMuMmwzLjMzLjMzLTIuMTIgMi41OSAyLjI2IDIuMjYgMi41My0yLjEyLjQgMy4zM2gzLjJsLjQtMy4yIDIuNTMgMS45OSAyLjI2LTIuMjYtMS43OS0yLjQ1IDMtLjQ3di0zLjJ6Ii8+PC9zdmc+Cg==') !important; }"+

".win { position: fixed; left: 0; top: 0; display: block; min-width: 32px; min-height: 32px; background: #fff; box-shadow: 0 0 6px rgba(0,0,0,0.7); z-index: 99;font:12px/14px arial,helvetica,tahoma,sans-serif; border-radius:0; }"+
".win-header { padding: 15px 20px; font:normal 16px Arial,Helvetica,sans-serif; cursor: default; -webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: -moz-none; -ms-user-select: none; user-select: none; }"+
".win-header-title { border-bottom: 1px solid #b7b6b1; color:#6f6563; padding: 15px 20px 10px; }"+
".win-close-btn, .win-close-btn:hover { position:absolute; top:0px; right:0px; width:20px; height:20px; text-indent:-1000em; overflow:hidden; background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyMCAyMCIgdmVyc2lvbj0iMS4xIj48cGF0aCBkPSJtIDE1LDYuMzI5OTk5OCAtMS4zMywtMS4zMyAtMy42NywzLjY3IC0zLjY2OTk5OTksLTMuNjcgLTEuMzMsMS4zMyAzLjY3LDMuNjcwMDAwMiAtMy42NywzLjY3IDEuMzMsMS4zMyBMIDEwLDExLjMzIDEzLjY3LDE1IDE1LDEzLjY3IDExLjMzLDEwIDE1LDYuMzI5OTk5OCBaIiAvPjwvc3ZnPgo=') 50% 50% no-repeat; border: 12px solid transparent; border-left-width: 2px; }"+
".win-minimize-btn { position:absolute; top:0px; right:34px; width:20px; height:20px; text-indent:-1000em; overflow:hidden; background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cmVjdCB5PSIxMyIgeD0iNCIgd2lkdGg9IjEyIiByeT0iMCIgaGVpZ2h0PSIyIiAvPjwvc3ZnPgo=') 50% 50% no-repeat; border: 12px solid transparent; border-right-width: 2px; border-left-width: 8px; }"+
".win-close-btn:hover, .win-minimize-btn:hover { opacity: 0.5; }"+
".win-minimize-btn.mod_first { right: 0px; border-right-width: 12px; }" +
".win-footer { text-align: center; }"+
".win-header, .win-fieldset, .win-footer { -moz-box-sizing:border-box; -webkit-box-sizing:border-box; box-sizing:border-box; }"+
".win-resizer { position: absolute; right: -9px; bottom: -10px; width: 20px; height: 20px; border-radius: 3px; cursor:se-resize; -moz-user-select:none; -webkit-user-select:none; -ms-user-select:none; }"+
".win-resizer:hover { border-bottom: 4px solid silver; border-right: 4px solid silver; }"+
".win-splitter { background: #eee; }"+
".win-splitter:hover { background: silver; cursor:col-resize; }"+
".win-splitter.type_horizontal:hover { background: silver; cursor:row-resize; }"+
".win-fieldset, .win fieldset { border: none; padding: 0; }"+
".win-fieldset > .row { padding: 5px 17px; clear:both; }"+
".win-fieldset > .row > label { float: left; width: 100px; }"+
		
".c-toolbar { background: white; border: 1px solid #ddd; box-sizing: border-box; white-space: nowrap; overflow-y: hidden; }"+
".c-toolbar-btn { display: inline-block; vertical-align: top; height: 22px; margin: 1px; border: none; background: transparent; line-height: 20px; }"+
".c-toolbar-btn:hover { background: silver; cursor: pointer; }"+
".c-toolbar-btn > i, .ico-apply { background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAALCAYAAACksgdhAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH3wsdFwECFGNfkQAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAqklEQVQoz5XRPU4CYRAG4OcDeiylpLAgFHgEWmkUTmBjjUcgnICT7EK1lHsEGiKFicZ4gb0A2owF8pPlbed9kslMUiMPk3EbC7wWWV6lmqDEPTYYNq8AcItOugLAFqNWFAaY4bnI8uoCeMR3ClDiJnZ+wvIM+CiyfN/CPIAofv7b9ABAA9MYnMoR+ENfMdjWAZDiEA10sUL/EoAmvL/tfu56vQrr+MXLOQC/RH9Eb3kwTjIAAAAASUVORK5CYII='); background-position: 50% 50%; background-repeat: no-repeat; display: inline-block; width: 20px; height: 20px; vertical-align: bottom; }"+
".c-toolbar-select, .c-toolbar-input { display: inline-block; vertical-align: top; height: 22px; margin: 1px; background: transparent; line-height: 20px; }"+
".c-toolbar-divider { width: 1px; margin: 1px 1px; overflow: hidden; background-color: #e5e5e5; height: 20px; display: inline-block; vertical-align: middle; }"+

"#littlelisp_ide_main_window_show_hide { position:absolute; right: 0; top: 0; width: 30px; height: 25px;  border: none; border-left: 1px solid #e5e5e5; /* background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyMCAyMCIgdmVyc2lvbj0iMS4xIj48cGF0aCBkPSJtIDE1LDYuMzI5OTk5OCAtMS4zMywtMS4zMyAtMy42NywzLjY3IC0zLjY2OTk5OTksLTMuNjcgLTEuMzMsMS4zMyAzLjY3LDMuNjcwMDAwMiAtMy42NywzLjY3IDEuMzMsMS4zMyBMIDEwLDExLjMzIDEzLjY3LDE1IDE1LDEzLjY3IDExLjMzLDEwIDE1LDYuMzI5OTk5OCBaIiAvPjwvc3ZnPgo=') 50% 50% no-repeat; */ background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZlcnNpb249IjEuMSIgdmlld0JveD0iMCAwIDIwIDIwIj48cmVjdCB5PSIxMyIgeD0iNCIgd2lkdGg9IjEyIiByeT0iMCIgaGVpZ2h0PSIyIiAvPjwvc3ZnPgo=') 50% 50% no-repeat; }"+
"#littlelisp_ide_main_window_show_hide:hover { background-color: silver; } "+

".cmd_code_editor_continue > i { margin: 0 -4px; }"+
".cmd_code_editor_next_step > i { margin: 0 -4px; background-image: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIj8+PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyLjU0bW0iIGhlaWdodD0iMy42NjhtbSIgdmlld0JveD0iMCAwIDguOTkgMTMiPjxnIHRyYW5zZm9ybT0idHJhbnNsYXRlKDEuMTg3NTc3M2UtOCwtMTAzOS4zNjIyKSI+PHBhdGggZD0ibSAyLjUsMTA1MC4zNjIyIGMgMCwxLjEgMC45LDIgMiwyIDEuMSwwIDIsLTAuOSAyLC0yIDAsLTEuMSAtMC45LC0yIC0yLC0yIC0xLjEsMCAtMiwwLjkgLTIsMiIvPjxwYXRoIGQ9Im0gMywxMDM5LjM2MjIgMCw0IC0zLDAgTCA0LjUsMTA0Ny4zNjIyIGwgNC41LC00IC0zLDAgMCwtNCAtMywwIHoiLz48L2c+PC9zdmc+Cg=='); }"+

"#littlelisp_ide_main_window_show { /* position:fixed; right: 2px; bottom: 2px; display: block;*/ }"+

"#littlelisp_ide_main_window_settings { position:absolute; right: 0px; top: 0px; border: none; border-left: 1px solid #e5e5e5; padding-left: 3px; padding-right: 3px; }"+
"#littlelisp_ide_main_window_settings:hover, #littlelisp_ide_main_window_settings:hover > i { background-color: silver; opacity: 1; }"+
"#littlelisp_ide_main_window_settings > i { opacity: 0.2; width: 18px; } "+

".c-code_editor-bg, .c-code_editor-textarea { font: 13px/16px monospace; padding: 2px; margin:0; border: 1px solid silver; -moz-box-sizing:border-box; box-sizing: border-box; overflow: auto; white-space: pre-wrap; word-wrap: break-word; }"+

"";

