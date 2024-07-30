let oUnit_css = "/**************************************************************************/\n/* The OUnit library                                                      */\n/*                                                                        */\n/* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           */\n/* Copyright (C) 2010 OCamlCore SARL                                      */\n/* Copyright (C) 2013 Sylvain Le Gall                                     */\n/*                                                                        */\n/* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  */\n/* and Sylvain Le Gall.                                                   */\n/*                                                                        */\n/* Permission is hereby granted, free of charge, to any person obtaining  */\n/* a copy of this document and the OUnit software (\"the Software\"), to    */\n/* deal in the Software without restriction, including without limitation */\n/* the rights to use, copy, modify, merge, publish, distribute,           */\n/* sublicense, and/or sell copies of the Software, and to permit persons  */\n/* to whom the Software is furnished to do so, subject to the following   */\n/* conditions:                                                            */\n/*                                                                        */\n/* The above copyright notice and this permission notice shall be         */\n/* included in all copies or substantial portions of the Software.        */\n/*                                                                        */\n/* The Software is provided ``as is'', without warranty of any kind,      */\n/* express or implied, including but not limited to the warranties of     */\n/* merchantability, fitness for a particular purpose and noninfringement. */\n/* In no event shall Maas-Maarten Zeeman be liable for any claim, damages */\n/* or other liability, whether in an action of contract, tort or          */\n/* otherwise, arising from, out of or in connection with the Software or  */\n/* the use or other dealings in the software.                             */\n/*                                                                        */\n/* See LICENSE.txt for details.                                           */\n/**************************************************************************/\n\nh1 {\n  font-size: 26px;\n  margin-right: 15px;\n  padding-left: 0px;\n}\n\nh2 {\n  font-size: 20px;\n  margin-right: 15px;\n  padding-left: 5px;\n}\n\n#ounit-current h2 {\n  text-decoration: underline;\n}\n\n#ounit-results-started-at {\n  width: 100%;\n}\n\n.ounit-results-content div {\n  width: 150px;\n  margin-top: 1px;\n}\n\n.ounit-results-content .number {\n  text-align: right;\n  display: inline-block;\n  float: right;\n  width: 50px;\n}\n\n.ounit-results-verdict.ounit-failure {\n  color: red;\n}\n\n.ounit-success h2,\n.ounit-results-successes .number {\n  background-color: #4a4;\n}\n\n.ounit-failure h2,\n.ounit-results-failures .number {\n  background-color: #f66;\n}\n\n.ounit-error h2,\n.ounit-results-errors .number {\n  background-color: #000;\n  color: #fff;\n}\n\n.ounit-skip h2,\n.ounit-results-skips .number {\n  background-color: #fd0;\n}\n\n.ounit-todo h2,\n.ounit-results-todos .number {\n  background-color: #aaf;\n}\n\n.ounit-timeout h2,\n.ounit-results-timeouts .number {\n  background-color: #888;\n}\n\n.ounit-conf h2,\n.ounit-results h2 {\n  background-color: #aaa;\n}\n\n.ounit-log,\n.ounit-conf-content {\n  font-family: Lucida Console, Monaco, Courier New, monospace;\n  white-space: nowrap;\n  font-size: 16px;\n  color: #666;\n  margin-left: 20px;\n}\n\n.ounit-duration,\n.ounit-started-at,\n.ounit-results-content {\n  margin-bottom: 10px;\n  margin-left: 15px;\n}\n\n.ounit-started-at {\n  margin-bottom: 0;\n}\n\nspan.ounit-timestamp {\n  display: inline-block;\n  width: 70px;\n}\n\n.ounit-log .ounit-result,\n.ounit-results-verdict {\n  font-weight: bold;\n  margin-top: 5px;\n}\n\n#navigation {\n  position: fixed;\n  top: 0;\n  right: 0;\n  background-color: #fff;\n  padding: 9px;\n  border: 1px solid #000;\n  border-top: none;\n  border-right: none;\n};\n";;
     let oUnit_js = "/**************************************************************************/\n/* The OUnit library                                                      */\n/*                                                                        */\n/* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           */\n/* Copyright (C) 2010 OCamlCore SARL                                      */\n/* Copyright (C) 2013 Sylvain Le Gall                                     */\n/*                                                                        */\n/* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  */\n/* and Sylvain Le Gall.                                                   */\n/*                                                                        */\n/* Permission is hereby granted, free of charge, to any person obtaining  */\n/* a copy of this document and the OUnit software (\"the Software\"), to    */\n/* deal in the Software without restriction, including without limitation */\n/* the rights to use, copy, modify, merge, publish, distribute,           */\n/* sublicense, and/or sell copies of the Software, and to permit persons  */\n/* to whom the Software is furnished to do so, subject to the following   */\n/* conditions:                                                            */\n/*                                                                        */\n/* The above copyright notice and this permission notice shall be         */\n/* included in all copies or substantial portions of the Software.        */\n/*                                                                        */\n/* The Software is provided ``as is'', without warranty of any kind,      */\n/* express or implied, including but not limited to the warranties of     */\n/* merchantability, fitness for a particular purpose and noninfringement. */\n/* In no event shall Maas-Maarten Zeeman be liable for any claim, damages */\n/* or other liability, whether in an action of contract, tort or          */\n/* otherwise, arising from, out of or in connection with the Software or  */\n/* the use or other dealings in the software.                             */\n/*                                                                        */\n/* See LICENSE.txt for details.                                           */\n/**************************************************************************/\n\nvar successHidden = true;\n\nfunction displaySuccess(display) {\n  var div = document.getElementsByClassName('ounit-success');\n  for (var i = 0; i < div.length; i++) {\n    div[i].style.display = display;\n  };\n};\n\nfunction toggleSuccess() {\n  if (successHidden) {\n    displaySuccess('block');\n  } else {\n    displaySuccess('none');\n  };\n  successHidden = ! successHidden;\n  var button = document.getElementById('toggleVisibiltySuccess');\n  if (successHidden) {\n    button.textContent = 'Show success';\n  } else {\n    button.textContent = 'Hide success';\n  };\n};\n\nfunction resetTestCurrent() {\n  var div = document.getElementById('ounit-current');\n  if (div) {\n    div.removeAttribute('id');\n  };\n};\n\nfunction setTestCurrent(div) {\n  resetTestCurrent();\n  div.id = \"ounit-current\";\n  div.scrollIntoView(true);\n};\n\nfunction nextTest() {\n  var div = document.getElementsByClassName('ounit-test');\n  var found = false;\n  var foundCurrent = false;\n  var idx = 0;\n  if (div) {\n    for (; !found && idx < div.length; idx++) {\n      if (foundCurrent && div[idx].style.display != 'none') {\n        found = true;\n      };\n      if (div[idx].id == \"ounit-current\") {\n        foundCurrent = true;\n      };\n    };\n    if (!foundCurrent && div.length > 0) {\n      setTestCurrent(div[0]);\n    } else if (found) {\n      setTestCurrent(div[idx - 1]);\n    } else {\n      resetTestCurrent();\n    };\n  };\n};\n\nfunction gotoTop() {\n  window.scrollTo(0,0);\n  resetTestCurrent();\n};\n";;