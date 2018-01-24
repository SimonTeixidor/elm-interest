var chartData = [];
function initElm() {
	var node = document.getElementById('main-app');
	var app = Elm.Main.embed(node, window.location.href.concat("=").split('=')[1]);
	app.ports.chartist.subscribe(function(data) {
		chartData = data;
		initPlot();
	});
}
function initPlot() {
	const shortMonthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
	new Chartist.Line('#chart', {
		series: [
			{
				name: 'series-1',
				data: chartData.map(p => ({x:p[0], y:p[1]}))
			}
		]
	}, {
		axisX: {
			type: Chartist.FixedScaleAxis,
			divisor: 4,
			labelInterpolationFnc: function(value) {
				const date = new Date(value);
				return shortMonthNames[date.getMonth()]+date.getFullYear().toString().slice(2);
			}
		},
		axisY: {
			labelInterpolationFnc: function(value) {
				if(value.toString().length < 6) {
					return value;
				} else {
					var retVal = "";
					var significant = 0;
					while(retVal.length < 5) {
						retVal = value.toExponential(significant++);
					}
					return retVal;
				}
			}
		}
	});
}
