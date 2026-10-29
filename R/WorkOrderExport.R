library(shiny)
library(DT)
library(shinyWidgets)

WorkOrderExport_all_columns_ui <- c(
  'Work OrderID',
  'Work OrderId2',
  'Production Date',
  'Sales OrderID',
  'Delivery Location',
  'Sales OrderID2',
  'PN',
  'Product Name',
  'Serial Number',
  'Sales OrderQty',
  'Delivery Date',
  'Total DeliveryQty'
)
#default columns
WorkOrderExport_default_columns_ui <- c(
  'Work OrderID',
  'PN',
  'Product Name',
  'Serial Number',
  'Production Date',
  'Sales OrderID',
  'Delivery Location'
)

#' Export 页签（三栏式，结构与 WorkOrderUI 一致）
#'
#' @param tabTitle  标题
#' @param colTitles 三栏标题
#' @param widthRates 左右比例
#' @param func_left 左函数（过滤条件）
#' @param func_right 右函数（列设置）
#' @param func_bottom 下面一栏（结果 + 导出）
#'
#' @return 返回值
#' @export
#'
#' @examples
#' WorkOrderExportUI()
WorkOrderExportUI <- function(tabTitle = 'Export',
                                     colTitles = c('Operation Area', 'Column Setting', 'Result Area'),
                                     widthRates = c(6, 6, 12),
                                     func_left = WorkOrderExportUI_left,
                                     func_right = WorkOrderExportUI_right,
                                     func_bottom = WorkOrderExportUI_bottom) {
  res = tsui::uiGen3(tabTitle = tabTitle, colTitles = colTitles, widthRates = widthRates,
                     func_left = func_left, func_right = func_right, func_bottom = func_bottom)
  return(res)
}
#' 左栏：动态过滤条件（字段 / 比较 / 值 / 并且-或者）
#' @export
#' @examples
#' WorkOrderExportUI_left()
WorkOrderExportUI_left <- function() {
  res <- tagList(
    tags$head(tags$style(HTML(paste0(
      ".dlf-row{display:flex;flex-wrap:nowrap;overflow-x:auto;gap:6px;align-items:flex-end;padding:4px 6px;margin-bottom:4px;border-radius:3px;}",
      ".dlf-row.is-active{background:#e9f2fa;}",
      ".dlf-row .form-group{margin-bottom:0;}",
      ".dlf-toolbar{display:flex;gap:14px;margin-bottom:6px;}"
    )))),
    div(class = "dlf-toolbar",
        actionLink("btn_WorkOrderExport_filter_add", "Add Row"),
        actionLink("btn_WorkOrderExport_filter_del", "Delete Row"),
        actionLink("btn_WorkOrderExport_filter_clear", "Delete All"),
        actionLink("btn_WorkOrderExport_filter_insert", "Insert Row"),
        actionLink("btn_WorkOrderExport_filter_copy", "Copy Row")
    ),
    div(id = "WorkOrderExport_filter_rows"),
    tags$hr(),
    shiny::actionButton(inputId = "btn_WorkOrderExport_view", label = "Search"),
    tsui::mdl_download_button(id = 'dl_WorkOrderExport', label = 'Export')
  )
  return(res)
}
#' 右栏：列设置（与 DeliveryLocationUI_right 一致）
#' @export
#' @examples
#' WorkOrderExportUI_right()
WorkOrderExportUI_right <- function() {
  res <- tagList(
    pickerInput(
      inputId = "WorkOrderExport_column_selector",
      label = "Selected Columns:",
      choices = WorkOrderExport_all_columns_ui,
      selected = WorkOrderExport_default_columns_ui,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} Columns Seleced",
        size = 10,
        `live-search` = TRUE,
        `live-search-style` = "contains"
      ),
      multiple = TRUE,
      choicesOpt = list(
        style = rep(("color: black;"), 100)
      )
    ),
    actionButton("btn_WorkOrderExport_select_all", "Select All",
                 class = "btn-primary btn-sm",
                 style = "margin-right: 5px;"),
    actionButton("btn_WorkOrderExport_deselect_all", "Deselect All",
                 class = "btn-secondary btn-sm"),
    actionButton("btn_WorkOrderExport_defaultValue", "Default Value",
                 class = "btn-primary btn-sm"),
    verbatimTextOutput("WorkOrderExport_selection_info")
  )
  return(res)
}

#' 下栏：结果表格
#' @export
#' @examples
#' WorkOrderExportUI_bottom()
WorkOrderExportUI_bottom <- function() {
  res <- tagList(
    tsui::uiScrollX(tsui::mdl_dataTable(id = 'WorkOrderExport_resultView', label = 'Export Preview'))
  )
  return(res)
}
