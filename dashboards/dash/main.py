import dash
import dash_core_components as dcc
import dash_html_components as html
import os
import netCDF4 as nc
import plotly.graph_objs as go

degree_sign = u"\N{DEGREE SIGN}"

# -------------------------- PYTHON FUNCTIONS ---------------------------- #


def build_banner():
    return html.Div(
        id='banner',
        className='banner'
    )

def empty_figure():
    fig = go.Figure(go.Scatter(x=[], y = []))
    fig.update_layout(template = None)
    fig.update_xaxes(showgrid = False, showticklabels = False, zeroline=False)
    fig.update_yaxes(showgrid = False, showticklabels = False, zeroline=False)
    
    return fig


# -------------------------- LOAD DATA ---------------------------- #


dataFile = os.path.join('data/2021-01.nc')
ds = nc.Dataset(dataFile)
xarr = ds.variables['time'][:]
refVar = 'RH'

varList = []
varLongNameByName = {}
for varName in list(ds.variables.keys()):
    # Include only variables with same shape as the reference variable and with units defined
    if (ds.variables[varName].shape != ds.variables[refVar].shape) or 'units' not in ds.variables[varName].ncattrs():
        continue
    longName = varName
    if 'long_name' in ds.variables[varName].ncattrs():
        longName = ds.variables[varName].long_name
    varLongNameByName[varName] = longName
sortedByLongName = sorted(varLongNameByName.items(), key=lambda x: x[1])

for listItem in sortedByLongName:
    varList.append({'label': listItem[1], 'value': listItem[0]})


# -------------------------- TEXT ---------------------------- #


dash_text = os.path.basename(dataFile)



# -------------------------- DASH ---------------------------- #


external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets, assets_folder='assets')
server = app.server

app.config.suppress_callback_exceptions = True


# -------------------------- PROJECT DASHBOARD ---------------------------- #


app.layout = html.Div(children=[
    html.H1(
        children=[
            build_banner(),
            html.P(
                id='instructions',
                children=dash_text),
            ]
    ),

    dcc.Dropdown(
        id='demo-dropdown',
        placeholder='Select a variable',
        options=varList,
        style={"width": "50%"},
    ),

    html.Div(id='dd-output-container'),

    dcc.Graph(
        id='example-graph',
        figure = empty_figure()      
    )
])

# -------------------------- CALLBACKS ---------------------------- #

@app.callback(
    [dash.dependencies.Output('example-graph', 'figure'),
    dash.dependencies.Output('dd-output-container', 'children')],
    [dash.dependencies.Input('demo-dropdown', 'value')],
    [dash.dependencies.State('example-graph', 'figure')])
def update_output(value, existing_state):

    if value is None:
        return existing_state, ""

    yarr = ds.variables[value][:,0,0]

    figure={
        'data': [
            go.Scatter(x=xarr, y=yarr, mode='markers')
        ],
        'layout': {
            'title': varLongNameByName[value] + ' at ' + str(ds.variables['lat'][0]) + " " + degree_sign + "N" + " "
                                + str(ds.variables['lon'][0]) + " " + degree_sign + "E",
            'xaxis':{
                'title':ds.variables['time'].units
            },
            'yaxis':{
                'title':ds.variables[value].units
            }
        }
    }

    return figure, ""


# -------------------------- MAIN ---------------------------- #


if __name__ == '__main__':
    app.run_server(host='0.0.0.0', port=8080, debug=True, use_reloader=True)
    #dev_tools_silence_routes_logging = False)