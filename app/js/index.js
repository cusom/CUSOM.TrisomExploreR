export function launchTutorial(id,tutorialName){
    Shiny.setInputValue(id +"-TutorialName", tutorialName);
  }

export function ShowBoxplotGroupOptions(gd,tutorialName) {

  id = gd.id.split('-',1).toString();  

  comparisonElementName = id + "-" + "GroupAnalysisOptions";
  document.getElementById(comparisonElementName).scrollIntoView();
    
  }


export function clearBoxplotGroups(gd){

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

export function launchTutorialFromModebar(gd,tutorialName){
  //console.log('tutorial event triggered for ' + nsid +' '+ tutorialName);
  id = gd.id.split('-',1).toString();
  console.log(gd.id);
  Shiny.setInputValue(id +"-TutorialName", tutorialName);
}

export function openSideBarPanel(gd) {

  id = gd.id.split('-',1).toString();
  elementName = id + '-sidebarLinks';
  document.getElementById(elementName).click();
  
}

export function annotatePointByKey(plotName, trace, keyIndex, annotationText, annotationsToKeep) {
  
  const el = document.getElementById(plotName);
  const data = el.data;

  if (data !== undefined) {
    
    const selectedIndex = Array.from({ length: data.length }, (v, i) => []);
  
    const annotations = el.layout.annotations.slice(0,annotationsToKeep) || [];

    if(trace > -1 & keyIndex > -1) {

      selectedIndex[trace][0] = keyIndex;
    
      Plotly.update(plotName, {selectedpoints: selectedIndex});
    
      const annotation = {
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

export function updateSelectedKeys(plotName, keys) {

  const el = document.getElementById(plotName);
  const data = el.data;
  const selectedKeys = keys.split('|');

  if (data !== undefined) {

    const selectedIndicies = [];

    for (var i = 0; i < data.length; i++) {

      let traceKeys = data[i].key;
      let traceMatches = [];

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

export function clearSelectedKeys(plotName) {

  const el = document.getElementById(plotName);
  const data = el.data;

  if (data !== undefined) {

    const traceCount = data.length;

    const selectedIndicies = Array.from(Array(traceCount), () => new Array());

    Plotly.restyle(plotName, { selectedpoints : selectedIndicies });
  
  }

}

export function toggleModebarButtons(plotName,buttonList,action) {
  
  const plotlyElements = document.getElementById(plotName).children[0].children[0].children;
  
  for(var i = 0; i < plotlyElements.length; i++) {
    const item = plotlyElements.item(i);
    if(item.classList == "modebar-container" ) {
      const modebarButtons = item.children[0].children; 
      for(var j = 0; j < modebarButtons.length; j++) {
        const modeBarName = modebarButtons.item(j).children[0].getAttribute("data-title");
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

export function openTab(tabName) {
  
  $('a', $('.sidebar')).each(function() {
    
    if(this.getAttribute('data-value') == tabName) {
      
      this.click()
    };
    
  })
  
}

export function PurgePlot(plotName) {

  const el = document.getElementById(plotName);

  if(typeof el !== null && el !== 'undefined' ) {
    
    Plotly.purge(plotName);

  }
  
}

export function removeExcessPlotTraces(plotName, expectedTraceCount) {

  const el = document.getElementById(plotName);

  if(typeof el !== null && el !== 'undefined' ) {

    const data = el.data;
    const traceCount = data.length;
    
    if(traceCount > expectedTraceCount) {
      for(var i = expectedTraceCount; i < data.length; i++) {
        Plotly.deleteTraces(el, i);
      }
    }

  }
}

export function removeExcessAnnotations(plotName,annotationsToKeep) {

  const el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {

    const annotations = el.layout.annotations.slice(0,annotationsToKeep) || [];
    Plotly.relayout(plotName,{annotations: annotations})

  }
  
}

export function cloneTraceByKeys(plotName,keys,newTraceName) {
  
  const el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {

    const selectedKeys = keys.split("|");
    const data = el.data;

    let traceKeys = [];
    let xVals = [];
    let yVals = [];
    let key = [];
    let text = [];
    
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

    const newTrace = {
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

export function resetPlotTraceVisibility(plotName) {

  const el = document.getElementById(plotName);

  if (typeof el !== null && el !== 'undefined' ) {

    const data = el.data;
    if (data !== undefined) {
      for (var i = 0; i < data.length; i++) {
        data[i].visible = true;
      }
    }
  }
}

export function isolateTraceVisibility(plotName, traceName) {

  const el = document.getElementById(plotName);

  if(typeof el !== null && el !== 'undefined' ) {

    const data = el.data;
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

export function limitDataTableSelections(table, max_selections, table_row_selected_input, target_input) {
  
  setTimeout(function() {

    const selectedindexes = table.rows({selected:true}).indexes();
    const selectedindices = Array(selectedindexes.length);
    const unselectedindexes = table.rows({selected:false}).indexes();
    const unselectedindices = Array(unselectedindexes.length);

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