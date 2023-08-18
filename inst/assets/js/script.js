launchTutorial = function(id,tutorialName){
    Shiny.setInputValue(id +"-TutorialName", tutorialName);
  }

ShowBoxplotGroupOptions = function (gd,tutorialName) {

  id = gd.id.split('-',1).toString();  

  comparisonElementName = id + "-" + "GroupAnalysisOptions";
  document.getElementById(comparisonElementName).scrollIntoView();
    
  }


clearBoxplotGroups = function(gd){

  id = gd.id.split('-',1).toString();
  traces = gd._fullData;
  
  for (var i = 0; i < traces.length; i++) {
      traceName = traces[i].name;
      if(traceName.startsWith("Group")) {
        fullTraceName = id +"-"+traceName.replace(' ','');
        Shiny.setInputValue(fullTraceName, "");
      }
    }
}

launchTutorialFromModebar = function(gd,tutorialName){
  //console.log('tutorial event triggered for ' + nsid +' '+ tutorialName);
  id = gd.id.split('-',1).toString();
  console.log(gd.id);
  Shiny.setInputValue(id +"-TutorialName", tutorialName);
}

openSideBarPanel = function(gd) {

  id = gd.id.split('-',1).toString();
  elementName = id + '-sidebarLinks';
  document.getElementById(elementName).click();
  
}

annotatePointByKey = function(plotName,keyName,annotationsToKeep) {
  
  var el = document.getElementById(plotName);
  var data = el.data;
  var trace = -1;
  var keyIndex = -1;

  if (data !== undefined) {
    
    selectedIndex = Array.from({ length: data.length }, (v, i) => []);
    
    // for each trace
    for (var i = 0; i < data.length; i++) {
      
      keys = data[i].key;
      
      if(keys !== undefined) {
        // for each key in trace, find key index based on key name
        for(var j = 0; j < keys.length; j++) {
          if (keys[j] == keyName) {
            trace = i;
            keyIndex = j;
            break;
          }
        }
      }
      
    }

    annotations = el.layout.annotations.slice(0,annotationsToKeep) || [];

    if(trace > -1 & keyIndex > -1) {

      selectedIndex[trace][0] = keyIndex;
    
      Plotly.update(plotName, {selectedpoints: selectedIndex});
      
      var textArray = -1
      if(Array.isArray(data[trace].text)) {
        textArray = data[trace].text[keyIndex].split('<');
      }
      else {
        textArray = data[trace].text.split()[keyIndex].split('<');
      }
      
      var analyteIndex = textArray.findIndex(x => x.includes("Analyte"));
      var annotationText = textArray[analyteIndex].split(':')[1].trim();
    
      annotation = {
        text: annotationText,
        x: data[trace].x[keyIndex],
        y: parseFloat(data[trace].y[keyIndex].toPrecision(4)), 
        showarrow : true, 
        arrowhead : 1, 
        startarrowhead : 1, 
        arrowside : 'end', 
        arrowcolor : '#e74c3c',
        ax : 20, 
        ay : -40,
        font : {color : 'Black', family : 'Arial', size : 16}, 
        bgcolor : '#abb2b9', 
        standoff : 4
      }

      annotations.push(annotation);
              
    }
        
    Plotly.relayout(plotName,{annotations: annotations})

  }

}

updateSelectedKeys = function(plotName, keys) {

  var el = document.getElementById(plotName);
  var data = el.data;
  var selectedKeys = keys.split('|');

  if (data !== undefined) {

    var selectedIndicies = [];

    for (var i = 0; i < data.length; i++) {

      traceKeys = data[i].key;
      traceMatches = [];

      for (var k = 0; k < traceKeys.length; k++) {
        for (var j = 0; j < selectedKeys.length; j++) {
          if (traceKeys[k] === selectedKeys[j]) {
            traceMatches.push(k);
          }
        }
      }

      selectedIndicies.push(traceMatches)
    
    }

    Plotly.restyle(plotName, { selectedpoints : selectedIndicies });

  }

}

clearSelectedKeys = function(plotName) {

  var el = document.getElementById(plotName);
  var data = el.data;

  if (data !== undefined) {

    var traceCount = data.length;

    var selectedIndicies = Array.from(Array(traceCount), () => new Array());

    Plotly.restyle(plotName, { selectedpoints : selectedIndicies });
  
  }

}

toggleModebarButtons = function(plotName,buttonList,action) {
  
  var plotlyElements = document.getElementById(plotName).children[0].children[0].children;
  
  for(var i = 0; i < plotlyElements.length; i++) {
    item = plotlyElements.item(i);
    if(item.classList == "modebar-container" ) {
      modebarButtons = item.children[0].children; 
      for(var j = 0; j < modebarButtons.length; j++) {
        modeBarName = modebarButtons.item(j).children[0].getAttribute("data-title");
        if(buttonList.includes(modeBarName)) {
          if(action=="hide") {
            modebarButtons.item(j).classList.add("hidden");
          } 
          else {
            modebarButtons.item(j).classList.remove("hidden");
          }
        }
      }
    }
  }
}

openTab = function (tabName) {
  
  $('a', $('.sidebar')).each(function() {
    
    if(this.getAttribute('data-value') == tabName) {
      
      this.click()
    };
    
  })
  
}

PurgePlot = function (plotName) {

  var el = document.getElementById(plotName);
 
  if(typeof el !== null && el !== 'undefined' ) {
    
    Plotly.purge(plotName);

  }
  
}

removeExcessPlotTraces = function(plotName, expectedTraceCount) {

  var el = document.getElementById(plotName);

  if(typeof el !== null && el !== 'undefined' ) {

    var data = el.data;

    var traceCount = data.length;
    
    if(traceCount > expectedTraceCount) {
      for(var i = expectedTraceCount; i < data.length; i++) {
        Plotly.deleteTraces(el, i);
      }
    }

  }
}

removeExcessAnnotations = function(plotName,annotationsToKeep) {

  var el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {

    var annotations = el.layout.annotations.slice(0,annotationsToKeep) || [];

    Plotly.relayout(plotName,{annotations: annotations})

  }
  
}

cloneTraceByKeys = function(plotName,keys,newTraceName) {
  
  var el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {

    var selectedKeys = keys.split("|");

    var data = el.data;

    traceKeys = [];
    xVals = [];
    yVals = [];
    key = [];
    text = [];
    
    for (var i = 0; i < data.length; i++) {
      
      traceKeys = data[i].key;
      
      for (var k = 0; k < traceKeys.length; k++) {
        for (var j = 0; j < selectedKeys.length; j++) {
          if (traceKeys[k] === selectedKeys[j]) {
            xVals.push(data[i].x[k]);
            yVals.push(data[i].y[k]);
            key.push(data[i].key[k]);
            text.push(data[i].text[k]);
          }
        }
      }

    }

    var newTrace = {
      type : "scatter",
      name: newTraceName,
      x: xVals,
      xaxis: "x",
      y: yVals,
      yaxis: "y",
      key: key,
      text: text,
      hoverinfo: "text",
      mode: "markers",
      marker: {
        color: "red",
        size: 10, 
        line: {
          color: "rgba(62,153,205,1)"
        },
        symbol: ["circle"]
      },
      visible: true
    }
    
    Plotly.addTraces(plotName, newTrace);

  }

}

resetPlotTraceVisibility = function(plotName) {

  var el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {
 
    var data = el.data;

    if (data !== undefined) {
      for (var i = 0; i < data.length; i++) {
        data[i].visible = true;
      }
    }
  }
}

isolateTraceVisibility = function(plotName, traceName) {

  var el = document.getElementById(plotName);

  if(typeof el !== null && el !== 'undefined' ) {

    var data = el.data;

    if (data !== undefined) {
      for (var i = 0; i < data.length; i++) {
        if(data[i].name == traceName) {
          data[i].visible = true;
        }
        else {
          data[i].visible = "legendonly";
        }
      }
    }   
  }
}

limitDataTableSelections = function(table, max_selections, table_row_selected_input, target_input) {
  
  setTimeout(function() {

    var selectedindexes = table.rows({selected:true}).indexes();
    var selectedindices = Array(selectedindexes.length);
    var unselectedindexes = table.rows({selected:false}).indexes();
    var unselectedindices = Array(unselectedindexes.length);

    for(var i = 0; i < selectedindices.length; ++i){
      selectedindices[i] = selectedindexes[i]+1;
    }

    for(var i = 0; i < unselectedindices.length; ++i){
      unselectedindices[i] = unselectedindexes[i];
    }

    if(selectedindexes.length <= max_selections) {

      table.$('td:first-child').each(function() {

        $(this).removeClass('notselectable');
        $(this).removeClass('selectable');

      });

      Shiny.setInputValue(table_row_selected_input, selectedindices);

      if(selectedindexes.length == 0) {

        Shiny.setInputValue(target_input,null, {priority:'event'});

      }

    }

    if(selectedindexes.length == max_selections) {

      table.$('td:first-child').each(function() {

        if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

          $(this).removeClass('notselectable');
          $(this).removeClass('selectable');
          $(this).addClass('notselectable');

        }

      });

    }

    if(selectedindexes.length > max_selections ) {

      table.$('td:first-child').each(function() {

        if($.inArray($(this)[0]._DT_CellIndex.row,unselectedindices)!= -1) {

          $(this).removeClass('selectable');
          $(this).addClass('notselectable');

        }

      });

    }

  }, 0);
}