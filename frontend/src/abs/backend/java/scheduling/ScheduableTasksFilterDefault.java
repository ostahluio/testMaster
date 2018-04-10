/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.List;

import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

public class ScheduableTasksFilterDefault implements ScheduableTasksFilter {

    @Override
    public List<TaskInfo> filter(List<TaskInfo> scheduableTasks) {
        return scheduableTasks;
    }

}
