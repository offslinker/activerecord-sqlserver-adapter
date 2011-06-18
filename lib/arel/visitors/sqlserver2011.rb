require 'arel'

module Arel
  module Visitors
    class SQLServer < Arel::Visitors::ToSql

      private

      # SQLServer ToSql/Visitor (Overides)

      def visit_Arel_Nodes_SelectStatement(o)
        core = o.cores.first
        find_and_fix_uncorrelated_joins_in_select_statement(o)
        projections = core.projections
        groups = core.groups
        offset = o.offset
        top = core.top

        if limit = o.limit
          if allow_set_top?(o)
            top = Arel::Nodes::Top.new(o.limit.expr)
            limit = nil
          else
            offset ||= Arel::Nodes::Offset.new(0)
          end
        end

        orders = if offset
          top = nil
          rowtable_orders(o)
        else
          o.orders.uniq
        end
        
	if eager_limiting_select_statement?(o)
          groups = projections.map { |x| projection_without_expression(x) }
          projections = projections.map { |x| projection_without_expression(x) }
          orders = orders.map do |x|
            expr = Arel.sql projection_without_expression(x.expr)
            if projections.include?(expr)
              x
            elsif x.descending?
              order_with_null_last(Arel::Nodes::Min.new([expr]), :desc)
            else
              order_with_null_last(Arel::Nodes::Max.new([expr]), :asc)
            end
          end.flatten
        elsif top_one_everything_for_through_join?(o)
          projections = projections.map { |x| projection_without_expression(x) }
        end

        [ (visit(o.with) if o.with),
	  ("SELECT"),
          (visit(top) if top),
          (projections.map{ |x| visit(x) }.join(', ')),
          (source_with_lock_for_select_statement(o)),
          ("WHERE #{core.wheres.map{ |x| visit(x) }.join ' AND ' }" unless core.wheres.empty?),
          ("GROUP BY #{groups.map { |x| visit x }.join ', ' }" unless groups.empty?),
          (visit(core.having) if core.having),
          ("ORDER BY #{orders.map{ |x| visit(x) }.join(', ')}" if !orders.empty?),
          (visit(offset) if offset),
          (visit(limit) if limit)
        ].compact.join ' '
      end

      # override visitors
      def visit_Arel_Nodes_Top(o)
        current_top = visit(o.expr)
        if current_top.is_a?(Integer)
          "TOP (#{current_top})"
        else
          "TOP(CAST(#{current_top} as INT))"
        end
      end

      def visit_Arel_Nodes_Offset(o)
        current_offset = visit(o.expr)
        if current_offset.is_a?(Integer)
          "OFFSET #{current_offset} ROWS"
        else
          "OFFSET (CAST(#{current_offset} as INT)) ROWS"
        end
      end

      def visit_Arel_Nodes_Limit(o)
        current_limit = visit(o.expr)
        if current_limit.is_a?(Integer)
	  "FETCH FIRST #{current_limit} ROWS ONLY"
        else
	  "FETCH FIRST (CAST(#{current_limit} as INT)) ROWS ONLY"
        end
      end

      # change conditions
      def allow_set_top?(o)
        o.offset.nil? && !single_distinct_select_statement?(o)
      end

      def eager_limiting_select_statement?(o)
        core = o.cores.first
        single_distinct_select_statement?(o) &&
          core.groups.empty? && 
          !single_distinct_select_everything_statement?(o) &&
          !o.orders.empty?
      end

      def order_with_null_last(expr, order=:desc)
	if expr.is_a? Arel::Nodes::Node
          order_with_null_last(visit(expr), order)
        else
          [Arel::Nodes::Ordering.new(Arel.sql("CASE WHEN (#{expr} is null) then 1 else 0 end"), :desc), Arel::Nodes::Ordering.new(Arel.sql(expr), order)]
        end
      end
    end
  end

end

Arel::Visitors::VISITORS['sqlserver'] = Arel::Visitors::SQLServer
